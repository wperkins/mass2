! ----------------------------------------------------------------
! file: solver_tdma.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 16, 2003 by William A. Perkins
! Last Change: Tue Jan 28 12:33:40 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL


! ----------------------------------------------------------------
! SUBROUTINE solve_tdma
! ----------------------------------------------------------------
SUBROUTINE solve_tdma(sweeps, x_beg, x_end, y_beg, y_end, &
     &ap, aw, ae, as, an, bp, x)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: sweeps, x_beg, x_end, y_beg, y_end
  DOUBLE PRECISION, DIMENSION(x_beg:x_end, y_beg:y_end), INTENT(IN) :: &
       &ap, aw, ae, as, an, bp
  DOUBLE PRECISION, INTENT(INOUT) :: x(x_beg:x_end, y_beg:y_end)

  INTEGER :: sweep, i
  DOUBLE PRECISION, DIMENSION(y_beg:y_end) :: aa, bb, cc, dd, tt

  DO sweep=1,sweeps
     DO i=x_beg,x_end
        cc(y_beg:y_end) = as(i,y_beg:y_end)
        aa(y_beg:y_end) = ap(i,y_beg:y_end)
        bb(y_beg:y_end) = an(i,y_beg:y_end)
        dd(y_beg:y_end) = bp(i,y_beg:y_end)
        IF (i+1 .LE. x_end) dd(y_beg:y_end) = dd(y_beg:y_end) + &
             &ae(i,y_beg:y_end)*x(i+1,y_beg:y_end)
        IF (i-1 .GE. x_beg) dd(y_beg:y_end) = dd(y_beg:y_end) + &
             &aw(i,y_beg:y_end)*x(i-1,y_beg:y_end)
        CALL tridag(y_beg,y_end,aa,bb,cc,dd,tt)
        x(i,y_beg:y_end) = tt(y_beg:y_end)
     END DO
  END DO
END SUBROUTINE solve_tdma

! Tridiangonal Matrix Solution
SUBROUTINE tridag(start, finish, a, b, c, d,sol)

IMPLICIT NONE

INTEGER :: i,last,ifp1,k,start,finish
DOUBLE PRECISION, DIMENSION(start:finish) :: a,b,c,d,sol
DOUBLE PRECISION :: ptemp(start-1:finish), qtemp(start-1:finish)

DO i=start,finish
ptemp(i) = b(i)/(a(i) - c(i)*ptemp(i-1))
qtemp(i) = (d(i) + c(i)*qtemp(i-1))/(a(i)-c(i)*ptemp(i-1))
END DO
!!$FORALL (i=start:finish)
!!$   ptemp(i) = b(i)/(a(i) - c(i)*ptemp(i-1))
!!$   qtemp(i) = (d(i) + c(i)*qtemp(i-1))/(a(i)-c(i)*ptemp(i-1))
!!$END FORALL

sol(finish) = qtemp(finish)

DO i=finish-1,start,-1
sol(i) = ptemp(i)*sol(i+1) + qtemp(i)
END DO
!!$FORALL (i=finish-1:start:-1)
!!$   sol(i) = ptemp(i)*sol(i+1) + qtemp(i)
!!$END FORALL


END SUBROUTINE tridag
!----------------------------------------------------------------------------

