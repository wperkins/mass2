! ----------------------------------------------------------------
! file: elevtbl.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 17, 2000 by William A. Perkins
! Last Change: Thu Nov  9 08:58:28 2000 by William A. Perkins <perk@dora.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE elevtbl
! ----------------------------------------------------------------
MODULE elevtbl

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE elevpt_rec
     REAL :: x
     REAL :: z
  END TYPE elevpt_rec

  INTEGER, PRIVATE :: elevcnt
  TYPE (elevpt_rec), POINTER, PRIVATE :: elevpt(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE elevread
  ! The points need to be in ascending x order
  ! ----------------------------------------------------------------
  SUBROUTINE elevread(filename)

    IMPLICIT NONE
    CHARACTER (LEN=*) :: filename
    INTEGER :: count, istat, i 
    REAL :: x

    OPEN(14,FILE=filename, STATUS='old', IOSTAT=istat, FORM='formatted')
    IF (istat .NE. 0) THEN
       WRITE(*,*) 'FATAL ERROR: cannot open bottom elevation list file ', &
            &TRIM(filename)
       CALL EXIT(8)
    END IF
    WRITE(*, *) 'reading bottom elevations from ', &
         &TRIM(filename)

                                ! count the number of points and
                                ! allocate accordingly
    
    count = 0
    DO WHILE (.TRUE.) 
       READ(14, *, END=100) x, x
       count = count + 1
    END DO
100 CONTINUE

    IF (count .LT. 2) THEN 
       WRITE(*,*) 'FATAL ERROR: too few points in bottom elevation file ', &
            &TRIM(filename)
       CALL EXIT(8)
    END IF

    ALLOCATE(elevpt(count))

    REWIND(14)

    DO i = 1, count
       READ(14, *) elevpt(i)%x, elevpt(i)%z
    END DO

    WRITE(*, *) 'successfully read ', count, ' bottom elevations from ', &
         &TRIM(filename)

    elevcnt = count

  END SUBROUTINE elevread


  ! ----------------------------------------------------------------
  ! REAL FUNCTION elevinterp
  ! ----------------------------------------------------------------
  REAL FUNCTION elevinterp(x)

    IMPLICIT NONE
    REAL :: x
    INTEGER :: i

    IF (x .LE. elevpt(1)%x) THEN
       i = 1
    ELSE IF (x .GE. elevpt(elevcnt)%x) THEN
       i = elevcnt - 1
    ELSE
       DO i = 1, elevcnt - 1 
          IF ((x .GE. elevpt(i)%x) .AND. (x .LE. elevpt(i+1)%x)) EXIT
       END DO
    END IF
    
    IF ((elevpt(i+1)%x - elevpt(i)%x) .NE. 0.0) THEN
       elevinterp = (x - elevpt(i)%x)/(elevpt(i+1)%x - elevpt(i)%x)*&
            &(elevpt(i+1)%z - elevpt(i)%z) + elevpt(i)%z
    ELSE
       elevinterp = elevpt(i)%z
    END IF

  END FUNCTION elevinterp

  ! ----------------------------------------------------------------
  ! SUBROUTINE elevdone
  ! ----------------------------------------------------------------
  SUBROUTINE elevdone()

    IMPLICIT NONE

    IF (ASSOCIATED(elevpt)) DEALLOCATE(elevpt)

  END SUBROUTINE elevdone


END MODULE elevtbl
