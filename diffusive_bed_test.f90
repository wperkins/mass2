  ! ----------------------------------------------------------------
  ! file: diffusive_bed_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created March 19, 2013 by William A. Perkins
  ! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
  ! ----------------------------------------------------------------
PROGRAM diffusive_bed_test

  USE diffusive_bed

  IMPLICIT NONE

  TYPE (diffusive_bed_rec) :: dbed
  DOUBLE PRECISION :: t, kdiff, deltat, tmax, depth
  INTEGER :: i, layers, l, tsteps

  DOUBLE PRECISION :: phi_delta, phi_o, phi_w

  layers = 10
  kdiff = 2.52e-06
  depth = 2.0
  phi_o = 10.0
  phi_delta = 5.0

  CALL diffusive_bed_initialize(dbed, depth, layers, kdiff, phi_o, phi_o)

  tsteps = 24
  deltat = 3600.0
  t = 0.0
  tmax = deltat*tsteps

  CALL printit(t, dbed)

  DO WHILE (t .LT. 2*tmax)
     t = t + deltat
     phi_w = phi_o + phi_delta*SIN(2.0*3.14159*t/tmax)
     CALL diffusive_bed_solve(dbed, deltat, phi_w)
     CALL printit(t, dbed)
  END DO

  CALL diffusive_bed_release(dbed)

END PROGRAM diffusive_bed_test

! ----------------------------------------------------------------
! SUBROUTINE printit
! ----------------------------------------------------------------
SUBROUTINE printit(t, dbed)

  USE diffusive_bed

  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: t
  TYPE (diffusive_bed_rec), INTENT(IN) :: dbed

  DOUBLE PRECISION :: z, flux
  INTEGER :: layers(1), l

  layers = UBOUND(dbed%phi)
  z = (layers(1)-1)*dbed%dz
  
  flux = diffusive_bed_flux(dbed)
  DO l = 1, layers(1)
     WRITE (*, '(F5.1, F5.1, F5.1, G15.6)') t/3600.0, z, dbed%phi(l), flux
     z = z - dbed%dz
  END DO
  WRITE (*, *)
END SUBROUTINE printit
