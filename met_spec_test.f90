  ! ----------------------------------------------------------------
  ! file: met_spec_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created March  5, 2013 by William A. Perkins
  ! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
  ! ----------------------------------------------------------------
PROGRAM met_spec_test

  USE met_zone

  IMPLICIT NONE

  CALL met_zone_read_specs("met_spec_test.dat")
  CALL met_zone_summary(6)

END PROGRAM met_spec_test
