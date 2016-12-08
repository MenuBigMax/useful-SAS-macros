/*	Modify the table 'base' to see all modalities of variables 'v1' and 'v2' even if their crossed frequency is 0. 'Table' ideally needs to be issued by a proc freq	*/
%macro Add_missing_freq(v1=, v2=, base=);
	proc sql noprint;
		/*	Distinct values of 'v1' and 'v2'	*/
		create table temp_A as select distinct &v1. from &base.;
		create table temp_B as select distinct &v2. from &base.;
		/*	Cross table with all levels of 'v1' and 'v2'	*/
 		create table temp_C as select * from temp_A cross join temp_B order by &v1., &v2.;
		/*	Addition of frequencies of 'base'	*/
		create table &base. as
			select temp_C.*, &base..count
			from temp_C left join &base.	/*	All levels of C joined variables will be in the table (even if their frequency is missing in 'base') */
			on temp_C.&v1.=&base..&v1. and temp_C.&v2.=&base..&v2.;
		drop table temp_A, temp_B, temp_C;
	quit;
	data &base.;
		set &base.;
		if count=. then count=0; 
	run;
%mend Add_mod_missing_freq;

/* Produce a dataset called 'freq_by_....' where each row concerns a distinct modality of var01 and contains the proportion of women */
%macro Prop_by_var(var01_prop=&y., mod_of_interest=&event_noformat., var_ventil=, base=&b.);
	/*	Contingengy table : stored with a one number of individuals by row	*/
	proc freq data=&base. noprint;
		tables &var_ventil.*&var01_prop./nocol norow nopercent out=freq;
	run;
	%Add_missing_freq(v1=&var_ventil., v2=&var01_prop., base=freq);
	/* Calculation of percentage	*/
	proc sql noprint; 
		delete from freq
			where &var_ventil. is missing or &var01_prop. is missing;
		create table freq_by_&var_ventil. as
			select *, sum(COUNT) as total label "Total de : &var01_prop.", COUNT/sum(COUNT) AS percent2 label "Pourcentage de &mod_of_interest. dans &var01_prop."
			from freq
			group by &var_ventil.;
		drop table freq;
		delete
			from freq_by_&var_ventil.
			where not &var01_prop="&mod_of_interest.";
		alter table freq_by_&var_ventil.
		drop percent, &var01_prop.;
	quit;
%mend Prop_by_var;

/* To display a plot with proportion of females depending on an continuous indicator */
%macro Plot_01_vs_Continuous(var01_prop=&y., mod_of_interest=&event_noformat., var_ventil=, base=&b.);
	%Prop_by_var(var01_prop=&var01_prop., mod_of_interest=&mod_of_interest., var_ventil=&var_ventil., base=&base.);
	/*	Plot						*/
	proc sgplot data=freq_by_&var_ventil.;
		bubble X=&var_ventil. Y=percent2 size=count;
		reg x=&var_ventil. Y=percent2 / weight=count nomarkers;
		loess x=&var_ventil. Y=percent2 / weight=count nomarkers;
		yaxis label="Proportion de &mod_of_interest. dans &var01_prop.";
		xaxis label="&var_ventil.";
	run;
	proc sql noprint;
		drop table freq_by_&var_ventil.;
	quit;
%mend Plot_01_vs_Continuous;


/* Displays a plot between 2 continuous variables */
%macro Plot_MeanContinu_vs_Continu(vary=, varx=, base=&b.);
	proc sql noprint;
		create table &vary._vs_&varx. as
			select &varx., mean(&vary.) as moy_&vary. label "Moyenne de &vary.", var(&vary.) as var_&vary. label "Variance de &vary.", 1/var(&vary.) as inv_var_&vary. label "Inverse de la variance de &vary."
			from &base.
			group by &varx.;
	quit;
	proc sgplot data= &vary._vs_&varx.;
		bubble X=&varx. Y=moy_&vary. size=var_&vary.;
		reg x=&varx. Y=moy_&vary. / weight=inv_var_&vary. nomarkers;
		loess x=&varx. Y=moy_&vary. / weight=inv_var_&vary. nomarkers;
		yaxis label="Moyenne de &vary.";
		xaxis label="&varx";
	run;
	proc sql noprint;
		drop table &vary._vs_&varx.;
	quit;
%mend Plot_MeanContinuous_vs_Continuous;

%macro TestChisqCramer(listVar=, base=&b.);
	
	/* Creation of the results rable		*/
	proc sql noprint;
			create table TestChisqCramer
				(	var1 char(40) label='Variable 1',
					var2 char(40) label='Variable 2',
					pvalue num format=pvalue6.4 label='Chi Square P-value',
					vcramer num format=10.4 label='V de Cramer');
	quit;

	
	%do i=1 %to %eval(%sysfunc(countc(&listVar., " "))+1);
		%let var_i=%scan(&listVar., &i.);
		%do j=%sysevalf(&i.+1) %to %eval(%sysfunc(countc(&listVar., " "))+1);
			%let var_j=%scan(&listVar., &j.);
			/*	Contingency table enabling test statistics calculation	*/
			ods output ChiSq=test_procfreq;
			proc freq data=&base.;
				tables &var_i.*&var_j./chisq noprint;
			run; 
			ods output close;
			data test_procfreq;
				set test_procfreq;
				format _all_;
			run;
			/*	Add of results into the result table	*/
			proc sql noprint;
				select Prob into :i_pval from test_procfreq where statistic="Khi-2";
				select Value into :i_cramer from test_procfreq where statistic="V de Cramer";
				insert into TestChisqCramer set var1="&var_i.", var2="&var_j.", pvalue=&i_pval., vcramer=&i_cramer.;
			quit;
			%put &var_i. &var_j. &i_pval. &i_cramer.;
		%end;
	%end;
	proc sql;
		drop table test_procfreq;
	quit;
%mend TestChisqCramer;

%macro TestPearsonSpearman(listVar=, base=&b.);
	
	/* Creation of the results rable		*/
	proc sql noprint;
			create table TestPearsonSpearman
				(	var1 char(40) label='Variable 1',
					var2 char(40) label='Variable 2',
					c_pearson num format=10.4 label='Coefficient de corrélation de Pearson',
					pval_pearson num format=pvalue6.4 label='P-value test de de nullité de la corrélation de Pearson',
					c_spearman num format=10.4 label='Coefficient de corrélation de Spearman',
					pval_spearman num format=pvalue6.4 label='P-value test de de nullité de la corrélation de Spearman');
	quit;

	
	%do i=1 %to %eval(%sysfunc(countc(&listVar., " "))+1);
		%let var_i=%scan(&listVar., &i.);
		%do j=%sysevalf(&i.+1) %to %eval(%sysfunc(countc(&listVar., " "))+1);
			%let var_j=%scan(&listVar., &j.);
			/*	Contingency table enabling test statistics calculation	*/
			ods output PearsonCorr=corr_pearson  SpearmanCorr=corr_spearman;
			proc corr data=&base. pearson spearman nosimple;
				var &var_i. &var_j.;
			run; 
			ods output close;
			data corr_pearson;
				set corr_pearson;
				format _all_;
			run;
			data corr_spearman;
				set corr_spearman;
				format _all_;
			run;
			/*	Add of results into the result table	*/
			proc sql noprint;
				select &var_i. into :i_c_pearson from corr_pearson where upcase(Variable)=upcase("&var_j.");
				select P&var_i. into :i_pval_pearson from corr_pearson where upcase(Variable)=upcase("&var_j.");
				select &var_i. into :i_c_spearman from corr_spearman where upcase(Variable)=upcase("&var_j.");
				select P&var_i. into :i_pval_spearman from corr_spearman where upcase(Variable)=upcase("&var_j.");
				%put &var_i. &var_j. &i_c_pearson. &i_pval_pearson. &i_c_spearman. &i_pval_spearman.;
				insert into TestPearsonSpearman
					set var1="&var_i.",
						var2="&var_j.",
						c_pearson=&i_c_pearson.,
						pval_pearson=&i_pval_pearson.,
						c_spearman=&i_c_spearman.,
						pval_spearman=&i_pval_spearman.;
			quit;
		%end;
	%end;
	proc sql;
		drop table corr_pearson, corr_spearman;
	quit;
%mend TestPearsonSpearman;

%macro TestKruskallWallis(listVarQuali=, listVarConti=, base=&b.);
	
	/* Creation of the results rable		*/
	proc sql;
			create table TestKruskallWallis
				(	var_quali char(40) label='Variable qualitative',
					var_conti char(40) label='Variable continue',
					prob num format=pvalue6.4 label='P-value');
	quit;

	
	%do i=1 %to %eval(%sysfunc(countc(&listVarQuali., " "))+1);
		%let var_i=%scan(&listVarQuali., &i.);
		%do j=1 %to %eval(%sysfunc(countc(&listVarConti., " "))+1);
			%let var_j=%scan(&listVarConti., &j.);
			/*	Contingency table enabling Kruskall-Wallis test statistics calculation	*/
			proc npar1way data=&base. wilcoxon noprint;
				class &var_i.;
				var &var_j.;
				output out=kruskall_wallis wilcoxon;
			run;
			/*	Add of results into the result table	*/
			proc sql noprint;
				select p_KW into : pVal from kruskall_wallis where upcase(_VAR_)=upcase("&var_j.");
				insert into TestKruskallWallis values ("&var_i.", "&var_j.", &pVal.);
			quit;
			%put "&var_i.", "&var_j.", &pVal.;
		%end;
	%end;
	proc sql;
		drop table kruskall_wallis;
	quit;
%mend TestKruskallWallis;

/*	Produces a table with cumulative proportion of individuals presents in the base 'base' depending on 2 categorical variables */
%macro PropCumulativeTable(varX=, varY=, base=&b., illustrative=nothing);
		/*	Generates the contingency table which must be import by an ODS. Otherwise zero aren't accounted. */
	ods output CrossTabFreqs=&varY._vs_&varX._2;
		proc freq data=&base.;
			tables &varY.*&varX./nofreq norow nocol cumcol nopercent;
		run;
	ods output off;
	proc sql noprint;
		delete from &varY._vs_&varX._2
		where &varY. is missing or &varX. is missing;
	quit;
	/*	Transpose to have a table contains as many column as 'varY' modalities */
		proc sort data=&varY._vs_&varX._2;
			by &varX. &varY.;
	proc transpose data=&varY._vs_&varX._2 out=&varY._vs_&varX.;
			by &varX.;
			var CumColPercent;
			id &varY.;
	run;
	proc sql noprint;
		/*	Cleaning of tables	*/
		drop table &varY._vs_&varX._2;
		alter table &varY._vs_&varX. drop column _NAME_;
		alter table &varY._vs_&varX. drop column _LABEL_;
	quit;
%mend PropCumulativeTable;


/* Produces a table with basic statistics of a continuous variable depending on another variable	*/
%macro BasicStatY_vs_X(varY=, varX=, base=&b.);
	proc sql noprint;
		create table &varY._vs_&varX. as
			select &varX., avg(&varY.) as moy label "Moyenne de &varY.", min(&varY.) as min label "Minimum", max(&varY.) as max label "Maximum", var(&varY.) as var label "Variance"
				from &base.
				group by &varX.
				order by moy desc;
			delete from &varY._vs_&varX.
				where &varX. is missing;
	quit;
%mend BasicStatY_vs_X;

/*	Produces a plot with different proportions of 'varY' depending on 'varX'	*/
%macro CumulativePlot(varX=, varY=, base=&b.);
	%PropCumulativeTable(varX=&varX., varY=&varY., base=&base.);
	proc sql noprint;
		/*	Extraction of particular data	*/
		select name into : listVarY separated by ' '
			from dictionary.columns
			where libname = %upcase("work") and memname = %upcase("&varY._vs_&varX.") and upcase(name) ne upcase("&varX.");
		select count(name) into : nb_var separated by ' '
			from dictionary.columns
			where libname = %upcase("work") and memname = %upcase("&varY._vs_&varX.") and upcase(name) ne upcase("&varX.");
	quit;
	/*	Plot parametrization	*/
	axis1 label=("&varX.");
	axis2 label=('Proportion cumulée');
	symbol1 interpol=join;
	symbol2 interpol=join;
	proc gplot data=&varY._vs_&varX.;
		plot (&listVarY.)*Annee /overlay areas=&nb_var. legend vaxis=axis2 haxis=axis1;
	run;
	quit;	
%mend CumulativePlot;


/*	Generates a table named 'ini_*' with 'nbRow' rows and with only one identification column	*/
%macro init_table(nbRow=);
	data init_&nbRow.r;
		format id best12.;
		%do id=1 %to &nbRow.;
			id=&id.;
			output;
		%end;
	run;
%mend init_table;


/*	Generates a table names 'new_*' which is the table 'base' with new values for the 'varToAd' variable */
%macro add_new_values(base=, varToAd=, varToMakeMissing=, Xmax_pred=);
	proc sql noprint;
		select max(&varToAd.) into : maxX from &base.;		/*	Maximum of the variable X	(to know when we begin to predict)	*/
	quit;
	%let nb_pred=%eval(&Xmax_pred.-&maxX.);					/*	Number of new values to add	*/
	/*	Construction of a dataset with missing responses for the levels of variable which we want to predict	*/
	data learning_data;								
		set &base. (keep=&varToAd. &varToMakeMissing.);
	run;
	%init_table(nbRow=&nb_pred.);										/*	Artificial table generated to be sure to have the asked number of rows	*/
	data new_&varToAd. (keep=&varToAd.);
		set init_&nb_pred.r (obs=&nb_pred.);								
		&varToAd.=&maxX.+_n_;
	data new_&varToAd.;											/*	Add of missing responses at the end of the learning table */
		set learning_data new_&varToAd.;
	run;
	proc sql noprint;
		drop table learning_data, init_&nb_pred.r;
	quit;
%mend add_new_values;


/*	Creates a table named 'pred_wanted' containing predictions of the probabilities ot varY01 depending on varX	*/
%macro pred_logi(varY01= , mod_event=, mod_event_noformat=, varX=, base=, Xmax_pred=, alpha=0.05);
	%add_new_values(base=&base., varToAd=&varX., varToMakeMissing=&varY01., Xmax_pred=&Xmax_pred.);
	
	/*	Machin learning	*/
	proc logistic data=new_&varX.;
		class &varY01.;
		model &varY01.(event="&mod_event.")=&varX.;
		output out=pred_wanted p=estimate l=lower u=upper / alpha=&alpha. ;
	run;
	
	%let aff_alpha=%sysevalf((1-&alpha.)*100);	/*	%sysevalf for decimals, %eval for integers			*/
	
	%Prop_by_var(var01_prop=&varY01., mod_of_interest=&mod_event_noformat., var_ventil=&varX., base=&base.);
	proc sql noprint;
		drop table learning_data, new_&varX.;
		/*	Deleting of duplicates X variables */
		create table pred_wanted as
		select distinct annee label "Année", estimate label "Prédiction", lower label "Borne sup de l'IC à &aff_alpha.%", upper label "Borne inf de l'IC à &aff_alpha.%"
		from pred_wanted;
		/*	Adding of real proportions of 'varY01'	*/
		create table pred_wanted as
			select a.annee, a.estimate, a.upper, a.lower, b.percent2 label "Taux réel de &mod_event.", b.total label "Nombre total de &varY01."
			from pred_wanted as a full join freq_by_annee as b on a.annee=b.annee;
	quit;	
%mend pred_logi;

/*	Generates a plot containing predictions of 'pred_logi' macro	*/
%macro plot_pred_logi(varY01= , mod_event=, mod_event_noformat=, varX=, base=, Xmax_pred=, alpha=0.05, by=);
	%pred_logi(varY01=&varY01., mod_event=&mod_event., mod_event_noformat=&mod_event_noformat., varX=&varX., base=&base., Xmax_pred=&Xmax_pred., alpha=&alpha.);
	/*	Searching threshold values of varX where lower and upper probabilities of varY01 exceed 50%	*/
	proc sql inobs=1 noprint;
		select &varX. into : lower50 from pred_wanted where lower>0.5 order by &varX. asc;
		select &varX. into : upper50 from pred_wanted where upper>0.5 order by &varX. asc;
	quit;
	proc sql noprint;
		select max(&varX.) into : max from pred_wanted;
		select min(&varX.) into : min from pred_wanted;
	quit;
	%let min2=%eval(%sysfunc(round(&min./10))*10);
	%put &min2.;
	/*	Plot parametrization	*/
	%let aff_alpha=%sysevalf((1-&alpha.)*100);	/*	%sysevalf for decimals, %eval for integers			*/
	proc sgplot data=pred_wanted;
		band x=&varX. lower=lower upper=upper /legendlabel="IC à &aff_alpha.%";
		bubble x=&varX. y=percent2 size=total / BRADIUSMIN=1 BRADIUSMAX=8 TRANSPARENCY=0.6;
		series x=&varX. y=estimate;
		refline &lower50. &upper50./ axis=x;
		refline 0.5 /axis=y;
		yaxis label="Probabilité de la modalité &mod_event. pour la base &base.";
		xaxis values=(&min2. to &max. by &by.);
	run;
%mend plot_pred_logi;
