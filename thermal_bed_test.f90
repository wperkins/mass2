  ! ----------------------------------------------------------------
  ! file: thermal_bed_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created March 19, 2013 by William A. Perkins
  ! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
  ! ----------------------------------------------------------------
PROGRAM thermal_bed_test

  USE thermal_bed

  IMPLICIT NONE

  TYPE (thermal_bed_rec) :: tbed
  DOUBLE PRECISION :: t, deltat, tmax
  DOUBLE PRECISION :: depth, spgrav, kond, spheat
  INTEGER :: i, layers, l, tsteps

  DOUBLE PRECISION :: phi_delta, phi_o, phi_w

  layers = 10
  depth = 2.0
  phi_o = 10.0
  phi_delta = 5.0

  spgrav = 1.6
  spheat = 800
  kond = 0.3
  

  CALL thermal_bed_initialize(tbed, depth, layers, kond, spheat, spgrav, phi_o, phi_o)

  tsteps = 24
  deltat = 3600.0
  t = 0.0
  tmax = deltat*tsteps

  CALL printit(t, tbed)

  DO WHILE (t .LT. 2*tmax)
     t = t + deltat
     phi_w = phi_o + phi_delta*SIN(2.0*3.14159*t/tmax)
     CALL thermal_bed_solve(tbed, deltat, phi_w)
     CALL printit(t, tbed)
  END DO

  CALL thermal_bed_release(tbed)

END PROGRAM thermal_bed_test

! ----------------------------------------------------------------
! SUBROUTINE printit
! ----------------------------------------------------------------
SUBROUTINE printit(t, tbed)

  USE thermal_bed

  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: t
  TYPE (thermal_bed_rec), INTENT(IN) :: tbed

  DOUBLE PRECISION :: z, flux
  INTEGER :: layers(1), l

  layers = UBOUND(tbed%dbed%phi)
  z = (layers(1)-1)*tbed%dbed%dz
  
  flux = thermal_bed_flux(tbed)
  DO l = 1, layers(1)
     WRITE (*, '(F5.1, F5.1, F5.1, G15.6)') t/3600.0, z, tbed%dbed%phi(l), flux
     z = z - tbed%dbed%dz
  END DO
  WRITE (*, *)
END SUBROUTINE printit
