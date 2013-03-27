  ! ----------------------------------------------------------------
  ! file: diffusive_bed.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created March 18, 2013 by William A. Perkins
  ! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE diffusive_bed
! ----------------------------------------------------------------
MODULE diffusive_bed

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE diffusive_bed_rec
     DOUBLE PRECISION :: kdiff
     DOUBLE PRECISION :: phi_inf
     DOUBLE PRECISION :: dz
     DOUBLE PRECISION, ALLOCATABLE :: phi(:)
  END type diffusive_bed_rec

  PRIVATE tridag, solveit

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE diffusive_bed_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE diffusive_bed_initialize(dbed, depth, layers, kdiff, phi_inf, phi_o)

    IMPLICIT NONE

    TYPE (diffusive_bed_rec), INTENT(INOUT) :: dbed
    DOUBLE PRECISION, INTENT(IN) :: depth, kdiff, phi_inf, phi_o
    INTEGER, INTENT(IN) :: layers

    dbed%kdiff = kdiff
    dbed%phi_inf = phi_inf
    dbed%dz = depth/REAL(layers)
    ALLOCATE(dbed%phi(layers+1))
    dbed%phi = phi_o

  END SUBROUTINE diffusive_bed_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE diffusive_bed_release
  ! ----------------------------------------------------------------
  SUBROUTINE diffusive_bed_release(dbed)

    IMPLICIT NONE

    TYPE (diffusive_bed_rec), INTENT(INOUT) :: dbed
    
    DEALLOCATE(dbed%phi)

  END SUBROUTINE diffusive_bed_release


  ! ----------------------------------------------------------------
  ! SUBROUTINE diffusive_bed_solve
  ! ----------------------------------------------------------------
  SUBROUTINE diffusive_bed_solve(dbed, deltat, phi_w)

    IMPLICIT NONE
    
    TYPE (diffusive_bed_rec), INTENT(INOUT) :: dbed
    DOUBLE PRECISION, INTENT(IN) :: deltat, phi_w
    INTEGER :: layers(1), l

    layers = UBOUND(dbed%phi)

    CALL solveit(layers(1), deltat, dbed%dz, dbed%kdiff, dbed%phi_inf, phi_w, dbed%phi)

  END SUBROUTINE diffusive_bed_solve

  ! ----------------------------------------------------------------
  ! SUBROUTINE solveit
  ! ----------------------------------------------------------------
  SUBROUTINE solveit(n, dt, dz, k, phi_o, phi_w, phi)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION, INTENT(INOUT), DIMENSION(n) :: phi
    DOUBLE PRECISION, INTENT(IN) :: dt, dz, k, phi_o, phi_w
    DOUBLE PRECISION, DIMENSION(n) :: a, b, c, d, x
    INTEGER :: l
    DOUBLE PRECISION :: Fo, h, Bi

    Fo = k*dt/dz/dz
    h = 1.0e00
    Bi = h*dz/k

    ! a is the west coefficient (l-1)
    ! b is the central coefficient (l)
    ! c is the east coefficient (l+1)
    ! d is the RHS

    ! l is 1 @ the top (bed/water interface)
    ! l is n @ the bottom 
    
    DO l = 1, n
       a(l) = Fo
       c(l) = Fo
       b(l) = 1.0 + 2.0*Fo
       d(l) = phi(l)
       IF (l .EQ. 1) THEN
          ! b(l) = 1 + 2.0*Fo + 2*Fo*Bi
          ! d(l) = 2*Fo*Bi*phi_w + phi(l)
          a(l) = 0.0
          c(l) = 0.0
          b(l) = 1.0
          d(l) = phi_w
       ELSE IF (l .EQ. n) THEN
          !a(l) = -2.0*Fo
          !b(l) = 1.0 + 2*Fo
          !d(l) = phi(l)
          a(l) = 0.0
          b(l) = 1.0
          c(l) = 0.0
          d(l) = phi_o
       END IF
    END DO

    CALL tridag(n, b, c, a, d, x)

    phi = x

  END SUBROUTINE solveit


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION diffusive_bed_flux
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION diffusive_bed_flux(dbed)

    IMPLICIT NONE

    TYPE (diffusive_bed_rec), INTENT(INOUT) :: dbed

    diffusive_bed_flux = 0.0
  END FUNCTION diffusive_bed_flux


  ! ----------------------------------------------------------------
  ! SUBROUTINE tridiag
  !
  ! a is the central coefficient
  ! b is the east coefficient
  ! c is the west coefficient
  ! d is the RHS
  ! ----------------------------------------------------------------
  SUBROUTINE tridag(n, a, b, c, d,sol)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION, DIMENSION(n), INTENT(IN) :: a,b,c,d
    DOUBLE PRECISION, DIMENSION(n), INTENT(OUT) :: sol
    DOUBLE PRECISION :: ptemp(0:n), qtemp(0:n)
    INTEGER :: i

    ptemp = 0.0
    qtemp = 0.0

    DO i=1,n
       ptemp(i) = b(i)/(a(i) - c(i)*ptemp(i-1))
       qtemp(i) = (d(i) + c(i)*qtemp(i-1))/(a(i)-c(i)*ptemp(i-1))
     END DO
    sol(n) = qtemp(n)

    DO i=n-1,1,-1
       sol(i) = ptemp(i)*sol(i+1) + qtemp(i)
    END DO

  END SUBROUTINE tridag


END MODULE diffusive_bed
  
