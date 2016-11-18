/*	Sorting of columns of a dataset by alphabetical order	*/
%macro SortColTable(lib, old_DB_name, new_DB_name);
	%if %sysevalf(&old_DB_name.=&new_DB_name.) %then %do;				/*	Necessary copy if sorting in the same table	*/
		%let new_DB_name=&old_DB_name._temp;
	%end;
	proc sql noprint;
		select name into : listVar separated by ', '
		from dictionary.columns
		where libname = %upcase("&lib.") and memname = %upcase("&old_DB_name.")
		order by name ;			/*	Variables list of the dataset	*/
		create table &lib..&new_DB_name. as
			select &listVar.
			from &lib..&old_DB_name. ;
	quit ;
	%if %sysevalf(&old_DB_name._temp=&new_DB_name.) %then %do;
		proc sql noprint;	drop table &lib..&old_DB_name.;	quit;			/*	Delete of the old table	*/
		data &lib..&old_DB_name.;	set &lib..&new_DB_name.;	run;		/*	Re-creation of the old table based on the new sorted table	*/
		proc sql noprint;	drop table &lib..&new_DB_name.;	quit;			/*	Delete of the new sorted table (redundand)	*/
	%end;
%mend SortColTable; 


/* Transformation of "bound-pourcentage" variable of a dataset*/
%macro TransPourBorn(old_DB, new_DB, listVar);
	%let eps=0.000001;
	data &new_DB;
		set &old_DB;
	%do i=1 %to %eval(%sysfunc(countc(&listVar., " "))+1);
		%let var1=%scan(&listVar., &i.);
		%put &var1;
		/*if &var1.=100 then &var1.=100-&eps.;
		else if &var1.=0 then &var1.=&eps.;
		&var1.=log(&var1./100/(1-&var1./100));*/
	%end;
		id=_N_;
	run;
%mend TransPourBorn;

/*	Analyzing of univariate distribution of a list of quantitativ variables */
%macro ManyUnivar(listVar);
	%do i=1 %to %eval(%sysfunc(countc(&listVar., " "))+1);
		%let var1=%scan(&listVar, &i);
		%put &var1;
		proc univariate data=Quapri.Base1;
			var &var1;
			histogram &var1 / kernel;
		run;
	%end;
%mend ManyUniVar;

/*	Display residuals in relation with observation in a genmod proc	*/
%macro DisplayVarRes(lib, DB, varResp, varQuali, varQuanti, dist, link);
	proc genmod data=&lib..&DB.;
		class &varQuali.; 
		model &varResp.%if &dist.=binomial %then %do; /pop_tot_17_oesect %end ; =&varQuali. &varQuanti. /dist=&dist. link=&link.;
		output out=&lib..resGLM p=pred l=lower u=upper;
	run;
	data &lib..resGLM;
		set &lib..resGLM(keep=&varResp. pred lower upper);
		res=&varResp.- /*%if &dist.=binomial %then %do;1000%end; **/pred;
	run;
	proc univariate data=&lib..resGLM;
 		histogram res/ kernel ;
 	run;
	proc gplot data=&lib..resGLM;
		plot res*&varResp.;
	run;
%mend DisplayVarRes;

%macro CompareMod(lib, db, vResp, vQuali, vQuanti, dist, link, f_inv);
	/* Value assignation fot default parameters	*/
	%let dist=%sysfunc(coalescec(&dist., normal));
	%let link=%sysfunc(coalescec(&link., id));
	%let f_inv=%sysfunc(coalescec(&f_inv.,id));

	/*	Table creation if absent	*/
	%if not(%sysfunc(exist(&lib..CompareMod))) %then %do;
		
		proc sql;
			create table &lib..CompareMod
				(	numModel num format=12.0 label='Numeral of tested model (key)',
					vResp char(20) label='Response variable',
					dist char(20) label='Distribution',
					link char(    20) label='Link fonction used',
					transI char(20) label='Inverse of response variable transformation',
					Log_Likehood num format=12.4 label='Log Likehood',
					AIC num format=12.4 label='AIC (smaller is better)',
					r2param num format=12.4 label='Determination coefficient of estimated parameter (R²)',
					r2paramA num format=12.4 label='Adjusted determination coefficient of estimated parameter (R²)',
					r2obs num format=12.4 label='Determination coefficient of observations',
					r2obsA num format=12.4 label='Adjusted determination coefficient of observations',
					NbFactSi num format=12.4 label='Number of significant factors');
		quit;
	%end;

	/*	Statistical model	*/
	ods output ModelFit=&lib..fitMod_1 ParameterEstimates=&lib..fitMod_2 Type3=&lib..fitMod_4;
	proc genmod data=&lib..&db.;
		class &vQuali.; 
		model &vResp.
		%if &dist.=binomial or &dist.=gamma 	%then %do;		/pop_tot_17_oesect		%end;
		=&vQuali. &vQuanti. / type3 dist=&dist.
		%if &dist. NE gamma 					%then %do; 		link=&link. 			%end;
		;
		/* If distribution is Poisson, necessity of specify number of inhabitants as weight	*/
		%if &dist.=poisson %then %do; weight pop_tot_17_oesect; %end;
		output out=&lib..fitMod_3 p=pred l=lower u=upper;
	run;
	ods output close;
	
	
	data &lib..fitMod_3;
		set Quapri.Fitmod_3 (keep=&vResp. pop_tot_17_oesect pred) ;
		/*	Case where response variable is a transformation of interest parameter	*/
		/*	obsAbs		:	observation of the number of hospitalisation 	(absolute)
			predAbs 	:	prediction of the number of hospitalisation 	(absolute)
			obsRate		:	observation of the rate of hospitalisation		(rate)
			predRate	:	prediction of the rate of hospitalisation		(rate)	*/
		%if &dist.=binomial or &dist.=gamma %then %do;
			obsAbs=&vResp.;						predAbs=pred*pop_tot_17_oesect;	obsRate=1000*&vResp./pop_tot_17_oesect;	predRate=pred;
		%end;
		%else %if &dist.=poisson %then %do;
			obsAbs=&vResp.;						predAbs=pred;					obsRate=1000*&vResp./pop_tot_17_oesect;	predRate=1000*pred/pop_tot_17_oesect;
		%end;
		%else %do;
			obsAbs=&vResp.*pop_tot_17_oesect/1000;	predAbs=pred*pop_tot_17_oesect/1000;	obsRate=&vResp.;					predRate=pred;
		%end;
		/*	Case where a transformation has been applied to response variable	*/
		%if &f_inv. =exp %then %do;
			obsAbs=&f_inv.(obsAbs);				predAbs=&f_inv.(predAbs);
		%end;
		%else %if %sysfunc(substr(&f_inv., 1,5))=power %then %do;
			%let k=%sysfunc(compress(&f_inv, "power"));
			obsAbs=obsAbs**&k.;					predAbs=predAbs**&k.;
		%end;
			resRate=predRate-obsRate;
		run;

	/*	Computation of several information	*/
	proc sql noprint;
		select count(*) into : nbRow from &lib..CompareMod;															/*	Number of rows inf CompareMod		*/
		select count(*) into : nbObs from &lib..FitMod_3;															/*	Number of observations				*/
		select count(*) into : 
from &lib..fitmod_4;														/*	Number of variables					*/
		select count(*) into : nbVarSi from &lib..FitMod_4 where ProbChiSq<0.01 and ProbChiSq NE .;					/*	Number of significant variables		*/
		select Value into	:	llh from &lib..FitMod_1 where Criterion="Log Likelihood";							/*	Log-likelihood of model				*/
		select Value into	:	aic from &lib..FitMod_1 where Criterion="AIC (smaller is better)";					/*	Log-likelihood of model				*/
		select 1-VAR(predRate-obsRate)/VAR(obsRate) into : r2param from &lib..FitMod_3 ;							/*	R² of estimated parameters			*/
		select 1-VAR(predAbs-obsAbs)/VAR(obsAbs) into : r2obs from &lib..FitMod_3 ;									/*	R² of estimated observations		*/
		insert into &lib..CompareMod values (																		/*	Filling of result tables			*/
			%sysevalf(&nbRow.+1),
			"&vResp.",
			"&dist.",
			"&link.",
			"&f_inv.",
			&llh.,
			&aic.,
			&r2param., %sysevalf(&r2param.-&nbVar.*(1-&r2param.)/(&nbObs.-&nbVar.-1)),
			&r2obs., %sysevalf(&r2obs.-&nbVar.*(1-&r2obs.)/(&nbObs.-&nbVar.-1)),
			&nbVarSi.);
		/*drop table &lib..FitMod_1, &lib..FitMod_2, &lib..FitMod_3;*/													/* Supression of useless tables		*/
	quit;
	
	ods listing;		/*	Necessary instruction to save graph (Why ???)	*/
	%let path=C:\Users\urceco\Documents\Maxime Oriol_URC-eco_Documents\400 Survey\420 Quapri-Psy;
	/*	Saving of the graphic verification	*/
	goptions reset=all gsfname=loc gsfmode=replace device=bmp ;

	filename loc "&path.\421 Graphics outputs\histo.bmp";
	proc univariate data=&lib..Fitmod_3 noprint;
 		histogram resRate/ kernel ;
	run;
	filename loc "&path.\421 Graphics outputs\qqplot.bmp";
	proc univariate data=&lib..Fitmod_3 noprint;
		qqplot resRate /normal(mu=est sigma=est color=red l=1);
 	run;
	filename loc "&path.\421 Graphics outputs\res_vs_pred.bmp";
	proc gplot data=&lib..FitMod_3;
		plot resRate*predRate;
	run;
	filename loc clear;
	ods listing close;

	/*	Export to Excel	*/
	proc export data=&lib..CompareMod outfile="&path.\423 Sas results\CompareMod.xls" DBMS=EXCEL REPLACE LABEL;
		sheet="Comparative table";
	proc export data=&lib..FitMod_2 outfile="&path.\423 Sas results\CompareMod.xls" DBMS=EXCEL REPLACE LABEL;
		sheet="Effects evaluation";
	proc export data=&lib..FitMod_4 outfile="&path.\423 Sas results\CompareMod.xls" DBMS=EXCEL REPLACE LABEL;
		sheet="Type 3 Effects evaluation";
	run;
%mend CompareMod;

%macro TestVar(lib, dtInt, dtPlan, vResp, vQuali, vQuanti);

    data &lib..plan2;
        set &lib..&dtPlan.;
        rownum=_N_;
    run;
    %let listVarQt=;                                                                /*  Initialisation of variables     */
    %let listVarQl=;

    %let listVar1Qt=;
    %let listVar1Ql=;
    proc sql noprint;
		select count(*) into : nbIndiv from &lib..&dtInt.;	/*	Number of individuals (for adjusted R2)	*/
        /*  Extraction of number of quantitativ and qualitativ variables            */
        select count(name) into : nbVarQt  from dictionary.columns where libname = upcase("&lib.") and memname = upcase("&dtPlan.") and name like "varQt%";
        select count(name) into : nbVarQl  from dictionary.columns where libname = upcase("&lib.") and memname = upcase("&dtPlan.") and name like "varQl%";
    quit;
        /*  Building of a table of unduplicated variables to test   */
        %do i=1 %to &nbVarQt.;
            proc sql noprint;
                create table t&i. as select distinct varQt&i. as ssDoub from &lib..&dtPlan.;
            quit;
            %let listVar1Qt= &listVar1Qt. t&i.;
        %end;
        proc sql noprint;
            create table t0 (ssDoub char(100) label='Variable');
        quit;
        %do i=1 %to &nbVarQl.;
            proc sql noprint;
                create table t&i. as select distinct varQl&i. as ssDoub from &lib..&dtPlan.;
            quit;
            %let listVar1Ql=&listVar1Ql. t&i.;
        %end;
        data &lib..uniqVar;
            set t0 &listVar1Qt. &listVar1Ql.;
            rownum=_N_;
        run;

        proc sql noprint;
            select distinct count(*) into : nbVarWOD from &lib..UniqVar;
            /*  Formating of data of experimental design for computing facilities       */
            select name into : listVarQt separated by ', ' from dictionary.columns where libname = upcase("&lib.") and memname = upcase("&dtPlan.") and name like "varQt%" order by name;
            select name into : listVarQl separated by ', ' from dictionary.columns where libname = upcase("&lib.") and memname = upcase("&dtPlan.") and name like "varQl%" order by name;
            /*  Test of presence of qualitativ and quantitativ variables (take care about comma !)  */
        %if %length(&listVarQt.)>0 and %length(&listVarQl.)>0 %then %do;
            create table &lib..plan2 as select *, catx(" ", &listVarQt.) as varQT, catx(" ", &listVarQl.) as varQL from &lib..plan2;
        %end;
        %else %if %length(&listVarQt.)>0 %then %do;
            create table &lib..plan2 as select *, catx(" ", &listVarQt.) as varQT, "" as varQL from &lib..plan2;
        %end;
        %else %if %length(&listVarQl.)>0 %then %do;
            create table "" as varQT, &lib..plan2 as select *, catx(" ", &listVarQl.) as varQL from &lib..plan2;
        %end;
    /*  Table creation if absent    */
    %if not(%sysfunc(exist(&lib..CompareVar))) %then %do;
            create table &lib..CompareVar
                (   numModel num format=12.0 label='Key',
                    varResp char(30) label='Response variable',
                    combVarQL char(200) label='Qualitativ variables combinaisons ',
                    combVarQT char(200) label='Quantitativ variables combinaisons',
                    Log_Likehood num format=12.4 label='Log Likehood',
                    AIC num format=12.4 label='AIC (smaller is better)',
					R2 num format=12.4 label='Coefficient of correlation',
					adjR2 num format=12.4 label='Adjusted coefficient of correlation', 
					rETP_est num format=12.4 label='Estimation of RATIOETP',
					rETP_pv num format=12.4 label='P-value of RATIOETP');
            %do i=1 %to &nbVarWOD.;
                select ssDoub into : vNameTemp from &lib..Uniqvar where rownum=&i.;
                alter table &lib..CompareVar add &vNameTemp. num format=12.4 label="&vNameTemp.";
            %end;
    %end;
    %else %do;
        delete from &lib..CompareVar;
    %end;
        select count(*) into : nbTest from &lib..Plan2;
    quit;

    %do i=1  %to &nbTest.;
        proc sql noprint;
            select varQT into : varTempQt from &lib..plan2 where rownum=&i.;
            select varQL into : varTempQl from  &lib..plan2 where rownum=&i.;
        quit;
		
        ods output ModelFit=&lib..out1_LH /*Likelihooh and AIC of model */;
        proc genmod data=&lib..&dtInt.;
            class &vQuali. &varTempQl.;
            model &vResp.=&vQuanti. &varTempQt. &vQuali. &varTempQl. / type3;
        run;
		quit;
		ods output close;
		ods output ParameterEstimates=&lib..out2_est /* Parameters estimates*/ ModelANOVA=&lib..out3_t3 /* Type 3 effect */ FitStatistics=&lib..out4_r2	/* R2	*/;
		proc glm data=&lib..&dtInt.;
            class &vQuali. &varTempQl.;
            model &vResp.=&vQuanti. &varTempQt. &vQuali. &varTempQl. / solution e3;
		run;
		quit;
		ods output close;

        proc sql noprint;
            select Value into   : llh from &lib..out1_LH where Criterion="Log Likelihood";
            select Value into   : aic from &lib..out1_LH where Criterion="AIC (smaller is better)";
			select RSquare into	: r2 from &lib..out4_r2;
			select count(*) into : nbVar from &lib..out3_t3;	/* Number of variables, for adjusted R square	*/
			select Estimate into : rETP_est from &lib..out2_est where Parameter="RATIOETP";
			select ProbF into : rETP_pv from &lib..out3_t3 where Source="RATIOETP";
            insert into &lib..CompareVar(numModel, varResp, combVarQL, combVarQT, Log_Likehood, AIC, R2, adjR2, rETP_est, rETP_pv)
            values (&i., "&vResp.", "&varTempQl.", "&varTempQt.", &llh., &aic., &r2., %sysevalf(1-(1-&r2.)*(&nbIndiv.-1)/(&nbIndiv.-&nbVar.)), &rETP_est., &rETP_pv.);
        quit;


        %do j=1 %to &nbVarWOD.;
            %let vTestTmp=;
            proc sql noprint;
                select ssDoub into : vNameTmp from &lib..Uniqvar where rownum=&j.;
                select ProbF into : vTestTmp from &lib..out3_t3 where Source="&vNameTmp.";
            quit;
            %if %length(&vTestTmp.)>0 %then %do;
                %if %datatyp(&vTestTmp.)=NUMERIC %then %do;
                    proc sql;
                        update &lib..CompareVar set &vNameTmp.=&vTestTmp. where numModel=&i.;
                    quit;
                %end;
                %else %if %datatyp(&vTestTmp.)=CHAR %then %do;
                    proc sql;
                        update &lib..CompareVar set &vNameTmp.=0.0001 where numModel=&i.;
                    quit;
                %end;
            %end;
        %end;
        /*ods output close;*/
    %end;
	/*proc sql noprint;
		drop table &lib..out1_LH, &lib..out2_est, &lib..out3_t3, &lib..out4_r2;
	quit;*/
	%let path=C:\Users\urceco\Documents\Maxime Oriol_URC-eco_Documents\400 Survey\420 Quapri-Psy;
	proc export data=&lib..Comparevar outfile="&path.\423 Sas results\CompareVar.xls" DBMS=EXCEL REPLACE LABEL;
		sheet="&vResp.";
	proc export data=&lib..out2_est outfile="&path.\423 Sas results\CompareVar.xls" DBMS=EXCEL REPLACE LABEL;
		sheet="Estim &vResp.";
	run;
%mend TestVar;
