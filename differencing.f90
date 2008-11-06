! ----------------------------------------------------------------
! file: differencing.f90
!
! A module with routines to do differencing by various schemes
!
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October  6, 2008 by William A. Perkins
! Last Change: Thu Nov  6 11:57:29 2008 by William A. Perkins <d3g096@bearflag.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE differencing
! ----------------------------------------------------------------
MODULE differencing

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PARAMETER :: diff_num_methods = 5
  INTEGER, PARAMETER :: &
       &DIFF_UPWIND = 1, &
       &DIFF_CDS = 2, &
       &DIFF_SOU = 3, &
       &DIFF_MSOU = 4, &
       &DIFF_MUSCL = 5
  CHARACTER*(*), PARAMETER :: diff_method(diff_num_methods) = &
       &(/ 'UPWIND', 'CDS   ', 'SOU   ', 'MSOU  ', 'MUSCL ' /)

  INTEGER :: diff_uv
  DOUBLE PRECISION :: blend_uv

CONTAINS

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION differ_method_from_string
  ! ----------------------------------------------------------------
  INTEGER FUNCTION differ_method_from_string(s)

    IMPLICIT NONE

    CHARACTER, INTENT(IN) :: s*(*)
    INTEGER :: i

    differ_method_from_string = 0

    DO i = 1, diff_num_methods
       IF (TRIM(s) .EQ. TRIM(diff_method(i))) THEN
          differ_method_from_string = i
       END IF
    END DO

  END FUNCTION differ_method_from_string

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION msou_limiter
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION msou_limiter(r)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: r
    msou_limiter = MAX(0.0d0, MIN(2.0d0*r, 1.0d0), MIN(1.0d0*r, 2.0d0))
  END FUNCTION msou_limiter


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION muscl_limiter
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION muscl_limiter(r)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: r
    muscl_limiter = (r + ABS(r))/(1 + ABS(r))
  END FUNCTION muscl_limiter


  ! ----------------------------------------------------------------
  ! SUBROUTINE differ2
  !
  ! Compute coefficents in one direction (w-e or s-n) using the
  ! specified differencing scheme.
  ! 
  ! ----------------------------------------------------------------
  SUBROUTINE differ2(method,&
       &flux_w, diffu_w, flux_e, diffu_e, &
       &phiww, phiw, phip, phie, phiee, &
       &aw, aw2, aww, ae, ae2, aee)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: method
    DOUBLE PRECISION, INTENT(IN) :: flux_w, diffu_w, flux_e, diffu_e
    DOUBLE PRECISION, INTENT(IN) :: phiww, phiw, phip, phie, phiee
    DOUBLE PRECISION, INTENT(OUT) :: aw, aw2, aww, ae, ae2, aee

    DOUBLE PRECISION :: rwminus, rwplus, reminus, replus
    DOUBLE PRECISION :: psywminus, psywplus, psyeminus, psyeplus


    rwminus = 0.0
    rwplus = 0.0
    reminus = 0.0
    replus = 0.0

    ! Default: Upwind Scheme
    
    aw = diffu_w + max(0d+00, flux_w)
    ae = diffu_e + max(0d+00, -flux_e)

    SELECT CASE (method)
    CASE (DIFF_CDS)

       ! Central Difference
    
       aw2 = diffu_w + 0.5*flux_w
       ae2 = diffu_e - 0.5*flux_e

       aww = 0.0
       aee = 0.0

    CASE (DIFF_SOU)

       ! From Norris

       aw2 = diffu_w + 1.5*max(0d+00, flux_w) + 0.5*max(0d+00, flux_e)
       ae2 = diffu_e + 1.5*max(0d+00, -flux_e) + 0.5*max(0d+00, -flux_w)
       aww = -0.5*max(0d+00, flux_w) 
       aee = -0.5*max(0d+00, -flux_e)

    CASE (DIFF_MSOU, DIFF_MUSCL)


       ! Darwish approach

       ! IF (ABS(phiw - phip) .GT. 0.0) THEN
       !    rwminus = (phip - phie)/(phiw - phip)
       !    rwplus = (phiw - phiww)/(phip - phiw)
       ! END IF
       
       ! IF (ABS(phip - phie) .GT. 0.0) THEN
       !    reminus = (phie - phiee)/(phip - phie)
       !    replus = (phip - phiw)/(phie - phip)
       ! END IF

       ! alternate Darwish approach

       IF (ABS(phip - phiw) .GT. 0.0) THEN
          replus = (phie - phip)/(phip - phiw)
       END IF
       IF (ABS(phie - phiee) .GT. 0.0) THEN
          reminus = (phip - phie)/(phie - phiee)
       END IF
       IF (ABS(phip - phie) .GT. 0.0) THEN
          rwminus = (phiw - phip)/(phip - phie)
       END IF
       IF (ABS(phiw - phiww) .GT. 0.0) THEN
          rwplus = (phip - phiw)/(phiw - phiww)
       END IF

       SELECT CASE (method)
       CASE (DIFF_SOU) ! maybe
          psywminus = 1
          psywplus = 1
          psyeminus = 1
          psyeplus = 1
       CASE (DIFF_MSOU)
          psywminus = msou_limiter(rwminus)
          psywplus = msou_limiter(rwplus)
          psyeminus = msou_limiter(reminus)
          psyeplus = msou_limiter(replus)
       CASE (DIFF_MUSCL)
          psywminus = muscl_limiter(rwminus)
          psywplus = muscl_limiter(rwplus)
          psyeminus = muscl_limiter(reminus)
          psyeplus = muscl_limiter(replus)
       END SELECT


       ! Darwish approach
       ! aw2 = diffu_w + MAX(flux_w, 0.0d0) - 0.5*MAX(-flux_w, 0.0d0)*psywminus - &
       !      &0.5*MAX(flux_w, 0.0d0)*psywplus
       ! ae2 = diffu_e + MAX(-flux_e, 0.0d0) - 0.5*MAX(-flux_e, 0.0d0)*psyeminus -&
       !      &0.5*MAX(flux_e, 0.0d0)*psyeplus
       ! aww = 0.0
       ! aee = 0.0

       aw2 = diffu_w + MAX(flux_w, 0.0d0) + 0.5*MAX(flux_w, 0.0d0)*psywplus + 0.5*MAX(flux_e, 0.0d0)*psyeplus
       ae2 = diffu_e + MAX(-flux_e, 0.0d0) + 0.5*MAX(-flux_e, 0.0d0)*psyeminus + 0.5*MAX(-flux_w, 0.0d0)*psywminus
       aww = -0.5*MAX(flux_w, 0.0d0)*psywplus
       aee = -0.5*MAX(-flux_e, 0.0d0)*psyeminus

    CASE DEFAULT

       aw2 = 0.0
       ae2 = 0.0

       aww = 0.0
       aee = 0.0

    END SELECT
    
  END SUBROUTINE differ2


  ! ----------------------------------------------------------------
  ! SUBROUTINE differ
  !
  ! Compute coefficents in one direction (w-e or s-n) using the
  ! specified differencing scheme.
  ! 
  ! ----------------------------------------------------------------
  SUBROUTINE differ(method, flux_w, diffu_w, flux_e, diffu_e, aw, aw2, aww, ae, ae2, aee)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: method
    DOUBLE PRECISION, INTENT(IN) :: flux_w, diffu_w, flux_e, diffu_e
    DOUBLE PRECISION, INTENT(OUT) :: aw, aw2, aww, ae, ae2, aee

    ! Default: Upwind Scheme
    
    aw = diffu_w + max(0d+00, flux_w)
    ae = diffu_e + max(0d+00, -flux_e)

    SELECT CASE (method)
    CASE (DIFF_CDS)

       ! Central Difference
    
       aw2 = diffu_w + 0.5*flux_w
       ae2 = diffu_e - 0.5*flux_e

       aww = 0.0
       aee = 0.0

    CASE (DIFF_SOU)

       aw2 = diffu_w + 1.5*max(0d+00, flux_w) + 0.5*max(0d+00, flux_e)
       ae2 = diffu_e + 1.5*max(0d+00, -flux_e) + 0.5*max(0d+00, -flux_w)
       aww = -0.5*max(0d+00, flux_w) 
       aee = -0.5*max(0d+00, -flux_e)

    CASE DEFAULT

       aw2 = 0.0
       ae2 = 0.0

       aww = 0.0
       aee = 0.0

    END SELECT
    
  END SUBROUTINE differ


END MODULE differencing
