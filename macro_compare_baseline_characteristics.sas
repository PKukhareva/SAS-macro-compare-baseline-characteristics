*  MACRO:        Compare_baseline_characteristics
*  DESCRIPTION:  Compares baseline characteristics by a specified variable 
*  SOURCE:       CSCC, UNC at Chapel Hill
*  PROGRAMMER:   Polina Kukhareva
*  DATE:         05/13/2013
*  HISTORY:      Compare_baseline_characteristics -- Kukhareva -- 05/13/2013
*  LANGUAGE:     SAS VERSION 9.3
*  INPUT:        SAS data set
*  OUTPUT:       RTF file with table, SAS data set
*******************************************************************;

/* Examples:

*/
%macro Compare_characteristics_all(
_DATA_IN=/*Name of the data set containing initial data, e.g. data_in */,
_DATA_OUT=/*data set containing results, e.g. data_out*/,
_GROUP=/*by variable, e.g. treatment*/,
_CHARACTERISTICS=/*List of variables to be included in a table separated by blanks*/, 
_N_PCT_CHISQUARED='NONE'/*List of ALL the categorical variables separated by blanks*/, 
_MEAN_STD_TTEST = 'NONE' /*List of ALL the continuous characteristic variables separated by blanks*/,
_MEAN_STDERR_TTEST = 'NONE' /*List of ALL the continuous characteristic variables separated by blanks*/,
_MEAN_STDERR_WILCOXON = 'NONE' /*List of ALL the continuous characteristic variables separated by blanks*/,
_MEDIAN_IQR_WILCOXON = 'NONE' /*List of ALL the continuous characteristic variables separated by blanks*/,
_WEIGHTED='NONE' /*List of ALL the weighted continuous variables separated by blanks*/,
_WEIGHT='NONE' /*Weight variable*/,
_MASKED_MEAN='NONE' /*Variables, such as cost which should be masked by dividing by average*/,
_DOLLAR_FORMAT='NONE'/*List of ALL the variables for which we estimate mean and std and use Wilcoxon test*/,
_PCT_FORMAT='NONE'/*List of ALL the variables for which we estimate mean and std and use T test but represent results with pct %*/,
_FOOTNOTE=%str(&sysdate, &systime -- produced by macro program) /*Footnote which appears in the rtf file */,
_TITLE1=Comparison of Characteristics/*Title which appears in the rtf file*/,
_SUFFIX=a/*Characters which will appear in the name of rtf file, e.g. bc_macro_1*/,
_PAT_ID='NONE'/*patient identifier for reporting the number of patients*/
)/ minoperator;

options nodate mprint pageno=1 mergenoby=warn MISSING=' ' validvarname=upcase;
ods listing close;

%let _CHARACTERISTICS=%upcase(&_CHARACTERISTICS);
%let _N_PCT_CHISQUARED=%upcase(&_N_PCT_CHISQUARED);
%let _MEAN_STD_TTEST=%upcase(&_MEAN_STD_TTEST);
%let _MEAN_STDERR_TTEST=%upcase(&_MEAN_STDERR_TTEST);
%let _MEAN_STDERR_WILCOXON=%upcase(&_MEAN_STDERR_WILCOXON);
%let _MEDIAN_IQR_WILCOXON=%upcase(&_MEDIAN_IQR_WILCOXON);
%let _WEIGHTED=%upcase(&_WEIGHTED);
%let _MASKED_MEAN=%upcase(&_MASKED_MEAN);
%let _DOLLAR_FORMAT=%upcase(&_DOLLAR_FORMAT);
%let _PCT_FORMAT=%upcase(&_PCT_FORMAT);

proc format;
   value pvalue_best
      0-<0.1=[pvalue5.3] 
      Other=[5.2] ;
run;

/*Producing an internal work data set with categorical group variable*/
data internal_dataset;
   set &_DATA_IN;
      length categorical_group $100;
      if Vtype(&_GROUP)='C' then categorical_group=&_GROUP;
         else categorical_group=strip(input(&_GROUP, best12.));
      if missing (&_GROUP) then delete;
run;
proc sort data=internal_dataset;
   by categorical_group;
run;
/*Creating a macro variable containing group variable values*/
proc sql NOPRINT;
   select distinct categorical_group into :distinct_groups separated by '~'
      from internal_dataset;
quit;
%let number_of_distinct_groups= %eval(%sysfunc(countw(%str(&distinct_groups),~)));


/*producing observation counts as first line in the output table*/

proc sql NOPRINT; 
	create table &_DATA_OUT as
    select "Number of observations" length = 90 as characteristic,
	"" length = 90 as CHARACTERISTIC_VALUE ,
	strip(put(count (*),8.0)) length = 90 as stat_all,
	%do i=1 %to &number_of_distinct_groups; 
		strip(put(sum (categorical_group="%scan(&distinct_groups,&i,~)"),8.0)) length = 90 as stat_&i , 
	%end;
	. as PVALUE format = pvalue_best.,
	"" length = 90 as STATS_USED
	from internal_dataset;
quit;
%if &_PAT_ID NE 'NONE' %then %do;
	proc sql NOPRINT; 
		create table PATIENT_COUNTS_&_SUFFIX as
	    select "Number of patients" length = 90 as characteristic,
		"" length = 90 as CHARACTERISTIC_VALUE ,
		strip(put(count (distinct &_PAT_ID),8.0)) length = 90 as stat_all,
		%do i=1 %to &number_of_distinct_groups; 
			strip(put(count_&i,8.0)) length = 90 as stat_&i ,
		%end;
		. as PVALUE format = pvalue_best.,
		"" length = 90 as STATS_USED
		from internal_dataset
		%do i=1 %to &number_of_distinct_groups; 
		, (select count (distinct &_PAT_ID) as count_&i from internal_dataset where categorical_group="%scan(&distinct_groups,&i,~)")
		%end;
		;
	quit;
	proc append BASE=&_DATA_OUT DATA=PATIENT_COUNTS_&_SUFFIX force; run;
%end;

/*We are iterating through all the characteristics in given order to compare their values */
%do  all_count=1 %to %sysfunc(countw(&_CHARACTERISTICS));
	%let var=%scan(&_CHARACTERISTICS, &all_count,%str( ));
	%let var=%UNQUOTE(&var);
	%let weight='NONE';
	%let masked='NONE';
	/*replacing numeric binary variables with categorical*/
	%if %sysfunc(find(&_N_PCT_CHISQUARED,&var,i)) ge 1 %then %do;
	    data internal_dataset;
		set internal_dataset;
	         length &var._c $90;
	         if Vtype(&var)='C' then &var._c=&var;
	         else &var._c=strip(put(&var, 8.0));
	      run; 
		%let var=&var._c;
		%let statistic1=N; 
		%let statistic2=PCT;
		%let statTest=CHISQUARED;
		%let format=%QUOTE(percent8.1);
	%end;

	/*setting parameters for continuous variables*/
	%if %sysfunc(find(&_MEAN_STD_TTEST,&var,i)) ge 1 %then %do;
		%let statistic1=MEAN; 
		%let statistic2=STD;
		%let statTest=TTEST;
		%let format=%QUOTE(8.2);
	%end;
	%if %sysfunc(find(&_MEAN_STDERR_TTEST,&var,i)) ge 1 %then %do;
		%let statistic1=MEAN; 
		%let statistic2=STDERR;
		%let statTest=TTEST;
		%let format=%QUOTE(8.2);
	%end;
	%if %sysfunc(find(&_MEAN_STDERR_WILCOXON,&var,i)) ge 1 %then %do;
		%let statistic1=MEAN; 
		%let statistic2=STDERR;
		%let statTest=WILCOXON;
		%let format=%QUOTE(8.2);
	%end;
	%if %sysfunc(find(&_MEDIAN_IQR_WILCOXON,&var,i)) ge 1 %then %do;
		%let statistic1=MEDIAN; 
		%let statistic2=IQR;
		%let statTest=WILCOXON;
		%let format=%QUOTE(8.1);
	%end;
	%if %sysfunc(find(&_PCT_FORMAT,&var,i)) ge 1 %then %do;
		%let format=%QUOTE(percent8.2);
	%end;
	%if %sysfunc(find(&_DOLLAR_FORMAT,&var,i)) ge 1 %then %do;
		%let format=%QUOTE(dollar8.2);
	%end;
	%if %sysfunc(find(&_WEIGHTED,&var,i)) ge 1 %then %do;
		%let weight=&_WEIGHT;
	%end; 	
	%if %sysfunc(find(&_MASKED_MEAN,&var,i)) ge 1 %then %do;
		%let masked=YES;
	%end; 	
	%Compare_characteristics(
	_DATA_IN=internal_dataset,
	_CATEGORICAL_GROUP_VALUES=&distinct_groups,
	_NUMBER_OF_DISTINCT_GROUPS=&number_of_distinct_groups,
	_CHARACTERISTIC=&var, 
	_STATISTIC1=&statistic1, 
	_STATISTIC2=&statistic2, 
	_STAT_TEST=&statTest,
	_WEIGHT=&weight,
	_MASKED_MEAN=&masked,
	_FORMAT=&format
	)
	/*appending results for each characteristic*/;
	proc append BASE=&_DATA_OUT DATA=out_&var force; run;

	proc datasets lib=work memtype=data noprint;
	delete 
		%do i=1 %to &number_of_distinct_groups; 
			stat1_&var._&i
			stat2_&var._&i
		%end;
		stat1_&var
		stat2_&var
		stat1_&var._all
		stat2_&var._all
		stat_&var
		p_&var
		out_&var
		; 
	run; 
	quit;
%end;

proc datasets lib=work memtype=data noprint; 
	delete internal_dataset ; 
run; quit;

footnote;
title;

%mend Compare_characteristics_all;

%macro Compare_characteristics(
_DATA_IN=/*Name of the data set containing initial data, e.g. rq.simcox */,
_DATA_OUT=/*data set containing results, e.g. data_out1*/,
_CATEGORICAL_GROUP_VALUES=/*by variable values, e.g. group1~group2*/,
_NUMBER_OF_DISTINCT_GROUPS=/*number of categories in the by variable, e.g. 2*/,
_CHARACTERISTIC=/*variable of interest, e.g. GENDER*/, 
_STATISTIC1='NONE'/*descriptive statistic, e.g. N, MEDIAN, MEAN*/, 
_STATISTIC2='NONE'/*descriptive statistic, e.g. PCT, IQR, SD, SR*/, 
_STAT_TEST='NONE' /*statistical test, e.g. CHISQUARED, WILCOXON, TTEST*/,
_WEIGHT='NONE' /*Weight variable*/,
_MASKED_MEAN='NONE' /*Variables, such as cost which should be masked by dividing by average*/,
_FORMAT=%QUOTE(8.2)/*Format for mean, e.g. %QUOTE(8.2), %QUOTE(percent.)*/
)/ minoperator;
/*statistic1*/ 
	/*N*/ 
   %if &_STATISTIC1 = N %then %do;
   /*by group*/
      %do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; 
         proc sql NOPRINT;
            create table stat1_&_CHARACTERISTIC._&i as
				select &_CHARACTERISTIC as CHARACTERISTIC_VALUE,
	            put(sum(^missing(&_CHARACTERISTIC) and categorical_group="%scan(&_CATEGORICAL_GROUP_VALUES,&i,~)"),8.0) as stat1_&i
            from internal_dataset
            group by &_CHARACTERISTIC;
         quit;
      %end;
	/*overall*/
      proc sql NOPRINT;
            create table stat1_&_CHARACTERISTIC._all as
				select &_CHARACTERISTIC as CHARACTERISTIC_VALUE,
	            put(sum(^missing(&_CHARACTERISTIC)),8.0) as stat1_all
            from internal_dataset
            group by &_CHARACTERISTIC;
      quit;
   %end;
	/*MEAN*/ 
   %if &_STATISTIC1 in %QUOTE(MEAN MEDIAN) %then %do;
   /*by group*/
      %do i=1 %to &number_of_distinct_groups; 
	  proc means data = internal_dataset nonobs maxdec=2 nway   N MEAN MEDIAN STD STDERR Q1 Q3 NOPRINT;
			var &_CHARACTERISTIC;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
			output out = stat1_&_CHARACTERISTIC._&i &_STATISTIC1 = num_stat1_&i;
			where categorical_group="%scan(&_CATEGORICAL_GROUP_VALUES,&i,~)";
		run;
   %if &_MASKED_MEAN NE 'NONE' %then %do;
   	proc sql NOPRINT; select num_stat1_1 into :num_stat1_1  from stat1_&_CHARACTERISTIC._1;
   %end;
		data stat1_&_CHARACTERISTIC._&i;
	  		set stat1_&_CHARACTERISTIC._&i;
			stat1_&i = put(num_stat1_&i, &_FORMAT);
			%if &_MASKED_MEAN NE 'NONE' %then %do; stat1_&i = put(num_stat1_&i/&num_stat1_1, &_FORMAT);   %end;
		run;
      %end;
	/*overall*/
	  proc means data = internal_dataset nonobs maxdec=2 nway   N MEAN MEDIAN STD STDERR Q1 Q3 NOPRINT;
			var &_CHARACTERISTIC;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
			output out = stat1_&_CHARACTERISTIC._all &_STATISTIC1 = num_stat1_all;
	  run;
	  data stat1_&_CHARACTERISTIC._all;
	  	set stat1_&_CHARACTERISTIC._all;
		stat1_all = put(num_stat1_all, &_FORMAT);
		%if &_MASKED_MEAN NE 'NONE' %then %do; stat1_all = put(num_stat1_all/&num_stat1_1, &_FORMAT);   %end;

	run;
   %end;

	/*merging (by group + overall)*/
	  data stat1_&_CHARACTERISTIC;
         %do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; set stat1_&_CHARACTERISTIC._&i; %end; 
				set stat1_&_CHARACTERISTIC._all;
	  run;

/*statistic2*/
	/*PCT*/
   %if &_STATISTIC2 = PCT %then %do;
    /*by group*/
      %do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; 
         proc sql NOPRINT;
            create table stat2_&_CHARACTERISTIC._&i as
				select &_CHARACTERISTIC as CHARACTERISTIC_VALUE,
	            catx('','(',put(sum(^missing(&_CHARACTERISTIC) and categorical_group="%scan(&_CATEGORICAL_GROUP_VALUES,&i,~)")/subtotal,&_FORMAT),')') as stat2_&i
            from internal_dataset, 
            (select sum(^missing(&_CHARACTERISTIC)) as subtotal from internal_dataset
            where categorical_group="%scan(&_CATEGORICAL_GROUP_VALUES,&i,~)")
            	group by &_CHARACTERISTIC ;
         quit;
      %end;
	 /*overall*/
      proc sql NOPRINT;
            create table stat2_&_CHARACTERISTIC._all as
				select &_CHARACTERISTIC as CHARACTERISTIC_VALUE,
	            catx('','(',put(sum(^missing(&_CHARACTERISTIC))/subtotal,&_FORMAT),')') as stat2_all
            from internal_dataset,
			(select sum(^missing(&_CHARACTERISTIC)) as subtotal from internal_dataset)
            	group by &_CHARACTERISTIC ;
      quit;

   %end; 
   	/*SD, SE*/ 
   %if &_STATISTIC2 in %QUOTE(STD STDERR) %then %do;
   /*by group*/
      %do i=1 %to &number_of_distinct_groups; 
	  proc means data = internal_dataset nonobs maxdec=2 nway   N MEAN MEDIAN STD STDERR Q1 Q3 NOPRINT;
			var &_CHARACTERISTIC;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
			output out = stat2_&_CHARACTERISTIC._&i &_STATISTIC2 = num_stat2_&i;
			where categorical_group="%scan(&_CATEGORICAL_GROUP_VALUES,&i,~)";
		run;
		data stat2_&_CHARACTERISTIC._&i;
	  		set stat2_&_CHARACTERISTIC._&i;
			stat2_&i = catx('',' \u0177\~ ',put(num_stat2_&i, &_FORMAT));
		%if &_MASKED_MEAN NE 'NONE' %then %do; stat2_&i = catx('',' \u0177\~ ',put(num_stat2_&i/&num_stat1_1, &_FORMAT));   %end;
		run;
      %end;
	/*overall*/
	  proc means data = internal_dataset nonobs maxdec=2 nway   N MEAN MEDIAN STD STDERR Q1 Q3 NOPRINT;
			var &_CHARACTERISTIC;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
			output out = stat2_&_CHARACTERISTIC._all  &_STATISTIC2 = num_stat2_all;
	  run;
	  data stat2_&_CHARACTERISTIC._all;
	  	set stat2_&_CHARACTERISTIC._all;
		stat2_all = catx('',' \u0177\~ ',put(num_stat2_all, &_FORMAT));
		%if &_MASKED_MEAN NE 'NONE' %then %do; stat2_all = catx('',' \u0177\~ ',put(num_stat2_all/&num_stat1_1, &_FORMAT));   %end;

	run;
   %end;
/*IQR*/
   %if &_STATISTIC2 in %QUOTE(IQR)  %then %do;
   /*by group*/
      %do i=1 %to &number_of_distinct_groups; 
	  proc means data = internal_dataset nonobs maxdec=2 nway N MEAN MEDIAN STD STDERR Q1 Q3 NOPRINT
			%if &_WEIGHT NE 'NONE' %then %do; VARDEF=WEIGHT %end;
			;
			var &_CHARACTERISTIC;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
			output out = stat2_&_CHARACTERISTIC._&i q1 = q1_stat2_&i q3 = q3_stat2_&i;
			where categorical_group="%scan(&_CATEGORICAL_GROUP_VALUES,&i,~)";
		run;
		data stat2_&_CHARACTERISTIC._&i;
	  		set stat2_&_CHARACTERISTIC._&i;
		stat2_&i = catx('','(',put(q1_stat2_&i, &_FORMAT),put(q3_stat2_&i, &_FORMAT),')');
		run;
      %end;
	/*overall*/
	  proc means data = internal_dataset nonobs maxdec=2 nway  N MEAN MEDIAN STD STDERR Q1 Q3 NOPRINT
	  		%if &_WEIGHT NE 'NONE' %then %do; VARDEF=WEIGHT %end;
			;
			var &_CHARACTERISTIC;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
			output out = stat2_&_CHARACTERISTIC._all  q1 = q1_stat2_all q3 = q3_stat2_all;
	  run;
	  data stat2_&_CHARACTERISTIC._all;
	  	set stat2_&_CHARACTERISTIC._all;
	stat2_all = catx('','(',put(q1_stat2_all, &_FORMAT),put(q3_stat2_all, &_FORMAT),')');
	run;
   %end;

	/*merging (by group + overall)*/
	  data stat2_&_CHARACTERISTIC;
         %do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; set stat2_&_CHARACTERISTIC._&i; %end; 
		set stat2_&_CHARACTERISTIC._all;
	  run;

/*test*/
	  /*CHISQUARED*/
   %if &_STAT_TEST = CHISQUARED %then %do;
         proc freq data=internal_dataset NOPRINT;
         table categorical_group*&_CHARACTERISTIC/chisq;
         output out=p_&_CHARACTERISTIC (keep=p_pchi rename=(p_pchi=pvalue)) pchi;
      run;
   %end;
	  /*TTEST*/
%if &_STAT_TEST = TTEST %then %do;
      ods output OverallANOVA=p_&_CHARACTERISTIC  (keep = source probf where=(source='Model') rename=(probf=pvalue));
		proc anova data=internal_dataset;
         	class categorical_group;
            model  &_CHARACTERISTIC=categorical_group;
			%if &_WEIGHT NE 'NONE' %then %do; weight &_WEIGHT; %end;
      	run;
	  ods output close;
   %end;
   	  /*WILCOXON*/
%if &_STAT_TEST = WILCOXON %then %do;
      proc npar1way data=baseline_characteristics_ds wilcoxon;
         var &CHECK_VAR;
         class categorical_group;
         output out=p (keep=P_KW rename=(P_KW=pvalue)) Wilcoxon;
      run;
   %end;

/*statistic1 + statistic2 + pvalue*/
	  data stat_&_CHARACTERISTIC;
	  length CHARACTERISTIC STATS_USED CHARACTERISTIC_VALUE STAT_ALL 
		 %do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; 
			stat_&i
		 %end;
		$90;
         set stat1_&_CHARACTERISTIC;
		set stat2_&_CHARACTERISTIC;
		 %do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; 
			stat_&i = catx('','{',stat1_&i,stat2_&i,'}'); 
		 %end;
		 stat_all=catx('','{',stat1_all,stat2_all,'}');
		CHARACTERISTIC = "&_CHARACTERISTIC";
		STATS_USED = catx("_","&_STATISTIC1","&_STATISTIC2","&_STAT_TEST") ;
	  run;
      %if (%sysfunc(exist(p_&_CHARACTERISTIC)))=0 %then %do;
         data p_&_CHARACTERISTIC;
            length pvalue 8.;
         run;
      %end;
	  proc sql NOPRINT; create table out_&_CHARACTERISTIC as 
		select STAT_ALL, 
			CHARACTERISTIC , 
			CHARACTERISTIC_VALUE, 
			%do i=1 %to &_NUMBER_OF_DISTINCT_GROUPS; 
			stat_&i,
			%end;
			PVALUE,
			STATS_USED 
	  	from  stat_&_CHARACTERISTIC
		LEFT JOIN  p_&_CHARACTERISTIC ON ^missing(PVALUE);
		quit;

%mend Compare_characteristics;

