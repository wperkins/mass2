#!MC 900
$!READDATASET  '"Fplot.nc" "Vzbot" "Vhp1" "Vhp2" "Vgp12" "Vucart" "Vvcart" "Vdepth" "Vuvel" "Vvvel" "Vvmag" "Vwsel" "Vshear" "Vcourant" "Vfroude" "Visdry" "S1" "E1"' 
  DATASETREADER = 'MASS2 Loader' 
$!ALTERDATA 
  EQUATION = '{xm} = {x}*0.3048' 
$!ALTERDATA 
  EQUATION = '{ym} = {y}*0.3048' 
$!TWODAXIS XVAR = 18
$!TWODAXIS YVAR = 19
$!WRITEDATASET  "metric.plt" 
  INCLUDETEXT = NO
  INCLUDEGEOM = NO
  INCLUDECUSTOMLABELS = NO
  BINARY = YES
  USEPOINTFORMAT = NO
  PRECISION = 9
