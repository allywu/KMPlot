
/*
Data requirements:
  1. Input dataset assumed to be one record per subject. 
  2. Stratification (macro var _stratavar) needs a numeric variable. 
     Data need another variable <_stratavar>_c has corresponding label for 
     proc format. e.g. TRTPN/TRTPN_C: 1/Drug A, 2/Placebo.    
*/

%macro mKMplot ( _indata=       /*Required.Input dataset assumed to be one record per 
                                  subject*/ 
                ,_timevar=      /*Required. Time variable*/ 
                ,_censorvar=    /*Required. Censoring variable, should include value 
                                  to identify a censored observation e.g: HYPER(0), 
                                  0 is censor*/
                ,_stratavar=    /*Permissible. Analysis stratification for KM,
                                  usually treatment group.*/    
                ,_stratalis=    /*Permissible. Study stratification groups*/
                ,_datafl=       /*Permissible. Data filter for LIFETEST and PHREG
                                  (where=(&datafl)), please use SAS logic. e.g. 
                                  GROUP=1*/                        
                ,_tinterval=    /*Permissible. Time interval for displaying. 
                                  Default =3*/
                ,_header=       /*Permissible. The header of the figure*/
                ,_Xlabel=       /*Permissible. The label of X-axis (Time interval)*/                                 
                ,_Ylabel=       /*Permissible. The label of Y-axis (Survival)*/                   
                ,_ARtitle=      /*Permissible. The label of Number of subjects at
                                  risk*/
                ,_Refline=      /*Permissible. Present reference line for median
                                  survival rate. Default=N.*/
                ,_ref=          /*Permissible. Study reference group*/
                ,_fnmed=        /*Permissible. The footnote shows survival median or
                                  not. Default=N.*/
                ,_fnhr=         /*Permissible. The footnote shows hazard ratio or
                                  not. Default=N.*/
                ,_fnp=          /*Permissible. The footnote shows pvalue for hazard
                                  ratio or not. Default=N.*/ 
                ,_outtype= rtf  /*Permissible. Output document type. Default=rtf.*/ );
                                  

/*-----------------------*/
/*   ticks for X-axis    */
/*-----------------------*/
%if &_TINTERVAL = %then %let _TINTERVAL = 3; /*default=3*/

proc sql noprint;
    select &_TINTERVAL.*ceil((max(&_TIMEVAR.)/&_TINTERVAL.)) into: _maxi
    from &_INDATA.
    ;
run;
quit;


/*-----------------------*/
/*       strata          */
/*-----------------------*/
%if &_STRATAVAR.= %then %do;
	data &_INDATA.;
		set &_INDATA.;
		DMY = 1;
		DMY_C = "All Subjects";
	run;
	%let _STRATAVAR = DMY;
%end;

/*-----------------------------*/
/*  label for strata: STRATA   */
/*-----------------------------*/
proc sort data= &_INDATA out = fmt nodupkey; 
    by &_STRATAVAR.;
run;

%let _STRATAVAR_C = %sysfunc(cats(%scan(&_STRATAVAR.,1),_C));%put &_STRATAVAR_C;
 

data fmt (keep=fmtname type start label &_STRATAVAR.);
    set fmt end=eos;
    by &_STRATAVAR.;
    
    retain fmtname "STRATA"  type "N";
    length start end $8. label $40.;

    start =  strip(put(_N_,best.));
    end   =  strip(put(_N_,best.));
    label = &_STRATAVAR_C.;

	if eos then call symputx ('_Nstrata', _N_);
run;

proc format library=work cntlin=fmt;
run;

/*-------------------------------------------------------------------------------*/
/*PROC LIFETEST for KM-plot, subject at risk, survival rate median, event numbers*/
/*-------------------------------------------------------------------------------*/
ods graphics / reset; 
ods exclude all;
ods trace on;

ods output survivalplot=zz_plotdata CensoredSummary=zz_esum Quartiles=zz_qtp 
           %if &_Nstrata.>= 2 %then HomTests=zz_htest ;;
proc lifetest data=&_INDATA %if %bquote(&_DATAFL.) ne %then %do; %str((where=(&_DATAFL.))) %end; 
    method=pl
    plots=survival(atrisk(outside)=0 to &_maxi. by &_TINTERVAL.);;
    ods select SurvivalPlot CensoredSummary Quartiles %if &_Nstrata.>= 2 %then Homtests;;
    time &_TIMEVAR. * &_CENSORVAR.;
    strata &_STRATAVAR.;;
run;

data zz_plotdata;
    set zz_plotdata;    
    by STRATUM;
    retain CUMEVENT SURVPROB;
    if first.STRATUM then do;
        CUMEVENT=0;
        SURVPROB=1;
    end;
    else do;
        if EVENT ^= . then CUMEVENT=CUMEVENT+EVENT;
        if SURVIVAL ^= . then SURVPROB=SURVIVAL;
    end;
run;

/*-----------------------*/
/*    Data for plot      */ 
/*-----------------------*/
data fSURVIVAL;
    set zz_plotdata;   
    format STRATUMNUM strata.;
run; 

/*------------------------*/
/* footnote event/ median */
/*------------------------*/
proc sql;
    create table fn1_0 as
    select a.&_STRATAVAR., a.STRATUM, a.TOTAL, a.FAILED, b.ESTIMATE, b.LOWERLIMIT, b.UPPERLIMIT
    from zz_esum as a
        left join zz_qtp (where=(PERCENT=50)) as b
        on a.&_STRATAVAR.=b.&_STRATAVAR.    
    having ^missing(&_STRATAVAR.)
    order by STRATUM
    ;
quit;

%if &_REF= %then %do;
    data _null_;
        set fmt end=eos;
        by START;
		if eos then call symputx ('_REF', input(&_STRATAVAR.,best.));
    run;
%end;

proc sql;
    select LABEL into: _REFGP
    from fmt
    where &_STRATAVAR. = &_REF
    ;
quit;

data fn1;
    set fn1_0;
    by STRATUM;

    /*length setting- for output space*/
    %if &_fnmed=Y %then %do; length x $100.; %end;
    %else %do; length x $120.; %end;
    
    if missing(TOTAL) then TOTAL=0;
    if missing(FAILED) then FAILED=0;
    
    array zero1(*) ESTIMATE LOWERLIMIT UPPERLIMIT;
    array zero2(*) $8. ESTIMATE_ LOWERLIMIT_ UPPERLIMIT_;
    do i=1 to dim(zero1);
        if missing(zero1(i)) then zero2(i)="NA";
        else zero2(i) = strip(put(zero1(i),5.1));
    end;
    
    x1 = cats("Events : ", put(FAILED,best.) , "/", put(TOTAL,best.));
    x2 = cats("Median : ", ESTIMATE_, " (", LOWERLIMIT_, ",", UPPERLIMIT_, ")" );
    %if &_fnmed=Y %then %do; x = catx(", ", x1, x2); %end;
    %else %do; x = x1; %end;

    if &_stratavar = &_ref then call symputx ('_FAILED', FAILED); /*Failed - event number of ref group*/
run;

/* depends on statification numbers */
%do i=1 %to &_Nstrata.;
    %global _event_&i;
    data _null_;
        set fn1;
        if _n_=&i. then call symput("_event_&i.",x);
    run;
%end;


/*footnote HR and P, at least Straum >1*/
%if &_Nstrata.>= 2 %then %do;

ods output  ParameterEstimates=zz_hazard;
proc phreg data=&_INDATA. %if %bquote(&_DATAFL.) ne %then %str((where=(&_DATAFL.)));;
    ods select ParameterEstimates;
    class &_STRATAVAR. (ref="&_REF.") &_STRATALIS.;
    model &_TIMEVAR.* &_CENSORVAR.=&_STRATAVAR./ties=EXACT risklimits alpha=0.05 ;
    strata &_STRATALIS.; 
run;
ods exclude none;

proc sql;
    create table fn2_0 as
	select a.*,b.LABEL as _STRATANAME, monotonic() as _N_
	from zz_hazard as a
	    left join FMT as b
		on input(a.CLASSVAL0,best.)=b.&_STRATAVAR.
    ;;
quit; 

%do i=1 %to %eval(&_Nstrata.-1);
    proc sql;
	    select _STRATANAME into: _STRATANAME_&i 
	    from fn2_0
	    where _N_ = &i.
        ;
    quit; 
    %put &&_STRATANAME_&i ;
%end; 

data fn2;
    set fn2_0;

    array zero1(*) HAZARDRATIO HRLOWERCL HRUPPERCL PROBCHISQ;
    array zero2(*) $8. HAZARDRATIO_ HRLOWERCL_ HRUPPERCL_ PROBCHISQ_;
    do k=1 to dim(zero1);
        %if &_FAILED. ^= 0 %then %do; 
            if missing(zero1(k)) then zero2(k)="NA";
            else zero2(k) = strip(put(zero1(k),5.2));
        %end;
    end;

    if . < PROBCHISQ < 0.001 then PROBCHISQ_ = "<0.001";
    else if PROBCHISQ > 0.999 then PROBCHISQ_ = ">0.999";
/*	else if 0.001 <= PROBCHISQ < 0.01 then PROBCHISQ_ = strip(put(PROBCHISQ,5.3));*/

    %if &_fnhr=Y %then %do; 
		
   	    %do i=1 %to %eval(&_Nstrata.-1); 
            %global _hr1_&i _hr2_&i _hr3_&i _hr4_&i;
            if _n_ = &i then do;
			    %if &_Nstrata. = 2 %then
                    %let _hr1_&i = %str(Hazard ratio (reference=%trim(&_REFGP.)) : ); 
			    %if &_Nstrata. > 2 %then
                    %let _hr1_&i = %str(Hazard ratio for %trim(&&_STRATANAME_&i) (reference=%trim(&_REFGP.)) : ); 
                %if &_FAILED. = 0 %then %do; 
                    %let _hr2_&i = {unicode '221e'x};
                    call symput ("_hr3_&i" , " (NA,NA)");
                %end;
                %else %do; 
                    call symput ("_hr2_&i" , cats(HAZARDRATIO_));
					call symput ("_hr3_&i" , " (" || cats(HRLOWERCL_, ",", HRUPPERCL_, ")"));
                %end;
                %if &_fnp=Y %then call symput ("_hr4_&i" , cats(", p-value : ", PROBCHISQ_));;
             end; 
             %put &&_hr1_&i &&_hr2_&i &&_hr3_&i &&_hr4_&i;
        %end; 
    %end;
run;

%end;


/*------------------------*/
/*       for output       */
/*------------------------*/

%let _lev2=%sysevalf((%sysevalf(&_Nstrata)*.04)+.01);
%let _lev3=%sysevalf(&_lev2);
%let _lev4=%sysevalf((%sysevalf(&_Nstrata-1)*.04)+.01);
%let _lev1=%sysevalf(1-&_lev2-&_lev3-&_lev4);

%put &_lev1 &_lev2 &_lev3 &_lev4;

%macro temp;

proc template;     
    define statgraph f_SURVIVAL; 
    dynamic TOTAL FAILED ;
    mvar _Nstrata _ARtitle _Ylabel _Xlabel _header  _lev1 _lev2 _lev3 _lev4;

    begingraph / border=FALSE designwidth=20cm designheight=14cm; 

    layout lattice / rows=4 rowweights=(&_lev1 &_lev2 &_lev3 &_lev4) rowgutter=0 columndatarange=union ;
    
    /*1st cell- KM plot*/
    cell;
    layout overlay / border=false
        yaxisopts=(offsetmin=0 linearopts=(thresholdmin=0 viewmin=0 
                   tickvaluesequence=(start=0 end=1 increment=.1)) 
                   %if &_Ylabel^= %then %do; label=&_Ylabel. labelattrs=(size=9pt) %end;)
        xaxisopts=(offsetmin=0 display=(line label ticks tickvalues) 
                   %if &_Xlabel^= %then %do; label=&_Xlabel. labelattrs=(size=9pt) %end;
                   linearopts=(thresholdmin=0 tickvaluesequence=(start=0 end=%eval(&_maxi) increment=%eval(&_tinterval) ))); 
        scatterplot x=TIME y=CENSORED / group=STRATUMNUM name="scat";
        stepplot x=TIME y=SURVIVAL / group=STRATUMNUM name="step";
        %if &_Refline=Y %then %do; referenceline y=0.5 / lineattrs=(color=lightgray); %end;
    endlayout;
    endcell;
    
    /*2nd cell- AT Risk*/   
    cell; 
    %if &_ARtitle^= %then %do;
    cellheader;
        entry halign=left textattrs=(size=8pt weight=bold) &_ARtitle. / border=FALSE;
    endcellheader; 
    %end;
    layout overlay/ walldisplay=none xaxisopts=(display=none);
        blockplot x=TATRISK block=ATRISK / class=STRATUMNUM blockindex=STRATUMNUM
                                           repeatedvalues=true display=(label values) 
                                           valuehalign=start valuefitpolicy=truncate valueattrs=graphdatatext(size=8pt) 
                                           labelposition=left labelattrs=graphvaluetext labelattrs=(size=8pt) includemissingclass=false;                  
    endlayout;
    endcell;

    /*3rd cell- Event Number*/
    cell; 
    layout overlay/ walldisplay=none xaxisopts=(display=none) pad=(top=0 left=5 bottom=0); 
    discretelegend "step" / valueattrs=(size=8pt) border=false displayclipped=true halign=left;          
    layout gridded / rows=&_Nstrata.  border=false;    
        %do i=1 %to &_Nstrata.;
            entry halign=left  textattrs=(size=8pt)  "%nrbquote(&&&_event_&i)" ; 
        %end; 
    endlayout;
    endlayout;
    endcell;

    /*4th cell- Hazard Ratio*/
    %if &_Nstrata. >=2 %then %do; 
    cell;
    layout overlay/ walldisplay=none xaxisopts=(display=none);
	layout gridded / rows=%eval(&_Nstrata.-1)  border=false halign=left;    
	    %do i=1 %to %eval(&_Nstrata.-1);
            %if &_FAILED. ^= 0 %then %do; entry halign=left textattrs=(size=8pt)  " " "&&_hr1_&i" "&&_hr2_&i" "&&_hr3_&i" "&&_hr4_&i"; %end;
            %else %if &_FAILED. = 0 %then %do; entry halign=left textattrs=(size=8pt)  " " "&&_hr1_&i" &&_hr2_&i "&&_hr3_&i" "&&_hr4_&i"; %end;
        %end;	
    endlayout;
	endlayout;
    endcell;    
    %end;

    endlayout;
    endgraph;
    end; 
run;
quit;

%mend temp;
%temp;


options orientation=landscape papersize=letter nodate nonumber;
ods noproctitle escapechar="`";
ods listing style=listing;


/*%let outputname = %str(&out..&_outtype); */
/*%mcroutputinfo(outputname=&outputname); */
/*%mcrrtftitle;*/
/*%mcrrtffoot;*/

ods graphics / reset noborder imagename="fSURVIVAL" ;
ods listing gpath="&_outputPath\";
ods &_OutType file ="&_outputPath\survival.&_outtype"  
    style=listing nogfootnote nogtitle bodytitle toc_data  headery=720 footery=720;

proc sgrender data=fSURVIVAL template=f_SURVIVAL
    %if &_HEADER^= %then description=&_HEADER.;;
run;


ods &_OutType close;
ods listing ;


%mend mKMplot;

