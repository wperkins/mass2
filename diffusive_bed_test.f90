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

  DOUBLE PRECISION :: phi_hi, phi_lo, phi_o, phi_w

  layers = 10
  kdiff = 1.54e-04
  depth = 10.0
  phi_o = 10.0
  phi_hi = 30.0
  phi_lo = 10.0

  CALL diffusive_bed_initialize(dbed, depth, layers, kdiff, phi_o, phi_o)

  tsteps = 24
  deltat = 3600.0
  t = 0.0
  tmax = deltat*tsteps

  CALL printit(t, dbed)

  DO i = 1, tsteps
     t = t + deltat
     IF (i <= 12) THEN
        phi_w = (phi_hi - phi_lo)/(tmax/2.0 - 0.0)*(t - 0.0) + phi_lo
     ELSE IF (i > 12) THEN
        phi_w = (phi_lo - phi_hi)/(tmax - tmax/2.0)*(t - tmax/2.0) + phi_hi
     END IF

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

  DOUBLE PRECISION :: z
  INTEGER :: layers(1), l

  layers = UBOUND(dbed%phi)
  z = (layers(1)-1)*dbed%dz
  
  DO l = 1, layers(1)
     WRITE (*, *) t, z, dbed%phi(l)
     z = z - dbed%dz
  END DO
  WRITE (*, *)
END SUBROUTINE printit
