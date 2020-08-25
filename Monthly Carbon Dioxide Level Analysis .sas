data work.co2;
infile 'C:\Users\my57617\Desktop\Project2\co2.dat' firstobs=2;
input y;
run;

data work.co2;
set work.co2;
year=1994+int((_n_+1-1)/12-0.001);
month=MOD(_n_+1-1,12);
if month=0 then month=12;
time=MDY(month,1,year);
format time monyy.;
drop year month;
n=_n_;
run;
quit;
data work.co2new;
set work.co2;
if n > 120 then delete; 
  run;
  quit;

  proc gplot data=work.co2;
  plot y*time/ vaxis=axis1 haxis=axis2 frame grid;
  title2 "co2 vs time";
  axis1 label = (a=90 'co2');
  axis2 label=('time');
  symbol1 v=dat h=.1 i=join ci=blue ;
run;
quit;

proc transreg data=work.co2;
title2 'BOX-COX';
model boxcox(y/lambda=0to 10 by 0.1) = identity(time);
output out=trans;
run;
*1 is in 95CI no transformation;

title2 "SARIMA models_1 for Y without Intercept";
proc arima data=work.co2new;
  identify var = y(1,12) nlag=60; 
  estimate p=(1,2)(12,24) noint printall grid method=ml plot;
  estimate p=(1,2) q=(0)(12,24) noint printall grid method=ml plot;
  estimate p=(1,2)(12) q=(0)(12) noint printall grid method=ml plot;
  estimate p=(1,2) q=(0)(12) noint printall grid method=ml plot;
  estimate p=(1,2)(12) noint printall grid method=ml plot;
  forecast out=out1 id=time alpha=0.05 lead=0; 
run;
quit;

title2 "SARIMA models_2 for Y without Intercept";
proc arima data=work.co2new;
  identify var = y(1,12) nlag=60; 
  estimate p=(0)(12,24) q=(1,2) noint printall grid method=ml plot;
  estimate q=(1,2)(12,24) noint printall grid method=ml plot;
  estimate p=(0)(12) q=(1,2)(12) noint printall grid method=ml plot;
  estimate q=(1,2)(12) noint printall grid method=ml plot;
  estimate p=(0)(12) q=(1,2) noint printall grid method=ml plot;
  forecast out=out2 id=time alpha=0.05 lead=0; 
run;
quit;

title2 "SARIMA models_3 for Y without Intercept";
proc arima data=work.co2new;
  identify var = y(1,12) nlag=60; 
  estimate p=(1)(12,24)q=(1) noint printall grid method=ml plot;
  estimate p=(1) q=(1)(12,24) noint printall grid method=ml plot;
  estimate p=(1)(12) q=(1)(12) noint printall grid method=ml plot;
  estimate p=(1) q=(1)(12) noint printall grid method=ml plot;
  estimate p=(1)(12) q=(1)noint printall grid method=ml plot;
  forecast out=out3 id=time alpha=0.05 lead=0; 
run;
quit;

title2 "SARIMA models_4 for Y without Intercept";
proc arima data=work.co2new;
  identify var = y(1,12) nlag=60; 
  estimate p=(1)(12,24)noint printall grid method=ml plot;
  estimate p=(1) q=(0)(12,24) noint printall grid method=ml plot;
  estimate p=(1)(12) q=(0)(12) noint printall grid method=ml plot;
  estimate p=(1) q=(0)(12) noint printall grid method=ml plot;
  estimate p=(1)(12)noint printall grid method=ml plot;
  forecast out=out4 id=time alpha=0.05 lead=0; 
run;
quit;

title2 "SARIMA models_5 for Y without Intercept";
proc arima data=work.co2new;
  identify var = y(1,12) nlag=60; 
  estimate p=(0)(12,24)q=(1) noint printall grid method=ml plot;
  estimate q=(1)(12,24) noint printall grid method=ml plot;
  estimate p=(0)(12) q=(1)(12) noint printall grid method=ml plot;
  estimate q=(1)(12) noint printall grid method=ml plot;
  estimate p=(0)(12) q=(1)noint printall grid method=ml plot;
  forecast out=out5 id=time alpha=0.05 lead=0; 
run;
quit;

*best 3 models (0,1,1)x(0,1,2),(1,1,1),(0,1,1);
proc arima data=work.co2new;
identify var = y(1,12) nlag=60; 
  estimate q=(1)(12,24) noint printall grid method=ml plot;
  *drop significant lag will be last model;
  forecast out=best1 id=time alpha=0.05 lead=0; 
run;

proc arima data=work.co2new;
identify var = y(1,12) nlag=60; 
  estimate p=(0)(12) q=(1)(12) noint printall grid method=ml plot;
*drop significant lag will be last model;
forecast out=best2 id=time alpha=0.05 lead=0; 
run;

proc arima data=work.co2new;
identify var = y(1,12) nlag=60; 
  estimate q=(1)(12) noint printall grid method=ml plot;
  forecast out=best3 id=time alpha=0.05 lead=12; 
run;*best;
data best3;
 merge best3 work.co2;
run;
quit;
proc gplot data=best3;
 where n >120;
  plot y*time forecast*time L95*time U95*time / overlay haxis=axis1
         vaxis=axis2 frame legend=legend1 grid;
  axis1 label = ("time");
  axis2 label = (a=90 "y_t");
  title2 "Forecast plot for y_t: SARIMA(0,1,1)x(0,1,1)_12";
  symbol1 i=join h=.1 v=dot l=1 ci=black;
  symbol2 i=join h=.1 v=square l=2 ci=red;
  symbol3 i=join l=1 r=2 ci=blue;
  legend1  label = none
           position = (bottom center outside)
           across = 4
           down = 1
           mode = reserve
           frame
           offset =(0.5, 0.5)
           value = (j=l h=0.3 'Observed' 'Forecast' 'C.I.' 'C.I.');
run;
quit;

title2 "MAPE Computations for SARIMA (0,1,1)x(0,1,1)_12 without Intercept";
data forecastbest;
 set best3;
 where n> 120;
 keep forecast n;
run;
quit;

data PartCompbest;
 set work.co2;
 where n > 120;
 keep y n;
run;
quit;


data MAPEbest;
 merge forecastbest PartCompbest;
 ratio = (abs(y-forecast))/y;
proc print;
run;
quit;

proc means data=MAPEbest noprint;
 var ratio;
output out=mape mean=mape;
run;

proc print;
title2 "MAPE value for SARIMA (0,1,1)x(0,1,1)_12 for logY without Intercept";
run;
quit;


*second best;
proc arima data=work.co2new;
identify var = y(1,12) nlag=60; 
  estimate q=(1,18)(12) noint printall grid method=ml plot;
  forecast out=best31 id=time alpha=0.05 lead=12; 
run;*best of best;

data best31;
 merge best31 work.co2;
run;
quit;
data best31;
  set best31;
  forecast2 = forecast;
  L952 = L95;
  U952 = U95;
run;
quit;

proc gplot data=best31;
 where n >120;
  plot y*time forecast2*time L952*time U952*time / overlay haxis=axis1
         vaxis=axis2 frame legend=legend1 grid;
  axis1 label = ("time");
  axis2 label = (a=90 "y_t");
  title2 "Forecast plot for y_t: SARIMA(0,1,(1,18))x(0,1,1)_12";
  symbol1 i=join h=.1 v=dot l=1 ci=black;
  symbol2 i=join h=.1 v=square l=2 ci=red;
  symbol3 i=join l=1 r=2 ci=blue;
  legend1  label = none
           position = (bottom center outside)
           across = 4
           down = 1
           mode = reserve
           frame
           offset =(0.5, 0.5)
           value = (j=l h=0.3 'Observed' 'Forecast' 'C.I.' 'C.I.');
run;
quit;

title2 "MAPE Computations for SARIMA (0,1,(1,18))x(0,1,1)_12 without Intercept";
data forecastbest2;
 set best31;
 where n> 120;
 keep forecast2 n;
run;
quit;

data PartCompbest2;
 set work.co2;
 where n > 120;
 keep y n;
run;
quit;


data MAPEbest2;
 merge forecastbest2 PartCompbest2;
 ratio2 = (abs(y-forecast2))/y;
proc print;
run;
quit;

proc means data=MAPEbest2 noprint;
 var ratio2;
output out=mape2 mean=mape2;
run;

proc print;
title2 "MAPE value for SARIMA (0,1,(1,18))x(0,1,1)_12 for logY without Intercept";
run;
quit;


*develop second more;
proc arima data=work.co2new;
identify var = y(1,12) nlag=60; 
  *estimate q=(1,2,3,4,5,6,7,8,9,10,11,18)(12) noint printall grid method=ml plot;
  estimate q=(1,9,18)(12) noint printall grid method=ml plot;
  forecast out=best32 id=time alpha=0.05 lead=12; 
run;*best of best;

data best32;
 merge best32 work.co2;
run;
quit;
data best32;
  set best32;
  forecast2 = forecast;
  L952 = L95;
  U952 = U95;
run;
quit;

proc gplot data=best31;
 where n >120;
  plot y*time forecast2*time L952*time U952*time / overlay haxis=axis1
         vaxis=axis2 frame legend=legend1 grid;
  axis1 label = ("time");
  axis2 label = (a=90 "y_t");
  title2 "Forecast plot for y_t: SARIMA(0,1,(1,18))x(0,1,1)_12";
  symbol1 i=join h=.1 v=dot l=1 ci=black;
  symbol2 i=join h=.1 v=square l=2 ci=red;
  symbol3 i=join l=1 r=2 ci=blue;
  legend1  label = none
           position = (bottom center outside)
           across = 4
           down = 1
           mode = reserve
           frame
           offset =(0.5, 0.5)
           value = (j=l h=0.3 'Observed' 'Forecast' 'C.I.' 'C.I.');
run;
quit;

title2 "MAPE Computations for SARIMA (0,1,(1,18))x(0,1,1)_12 without Intercept";
data forecastbest2;
 set best31;
 where n> 120;
 keep forecast2 n;
run;
quit;

data PartCompbest2;
 set work.co2;
 where n > 120;
 keep y n;
run;
quit;


data MAPEbest2;
 merge forecastbest2 PartCompbest2;
 ratio2 = (abs(y-forecast2))/y;
proc print;
run;
quit;

proc means data=MAPEbest2 noprint;
 var ratio2;
output out=mape2 mean=mape2;
run;

proc print;
title2 "MAPE value for SARIMA (0,1,(1,18))x(0,1,1)_12 for logY without Intercept";
run;
quit;
