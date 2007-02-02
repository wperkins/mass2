! ----------------------------------------------------------------
! file: solver_sixpack.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 23, 2002 by William A. Perkins
! Last Change: Mon Jul 21 14:10:11 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE solver_module
! ----------------------------------------------------------------
MODULE solver_module

  USE solver_common
  USE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=80), PRIVATE :: sixpack_config = "sixpack.cfg"

  INTERFACE
     
     SUBROUTINE Solver5( t,ae,aw,an,as,ap,b,solver, &
          &omega,alpha,npreit,gamma,nsmit,nsolit,depthmx,&
          &mindim,resmx,nitmx,nitmn,resout,noit,residual,&
          &nres,nx,ny,set,ix,iy,zeroed,stat )
       IMPLICIT none
       INTEGER :: nx,ny
       INTEGER :: ix,iy
       INTEGER :: nitmx,nitmn,noit,nres,npreit,stat
       INTEGER :: gamma,nsmit,nsolit,depthmx,mindim
       DOUBLE PRECISION, DIMENSION(nx,ny) :: t,ae,aw,an,as,ap,b
       DOUBLE PRECISION :: omega,alpha,resmx,residual
       LOGICAL :: resout,set,zeroed
       CHARACTER(LEN=*) :: solver
     END SUBROUTINE Solver5
  END INTERFACE
  
  INTEGER, PRIVATE :: nitmx, nitmn,noit,nres,npreit,stat
  INTEGER, PRIVATE :: gamma,nsmit,nsolit,depthmx,mindim
  DOUBLE PRECISION, PRIVATE :: omega,alpha,resmx,residual
  LOGICAL, PRIVATE :: resout, set, zeroed
  CHARACTER(LEN=80), PRIVATE :: solvername, msg

CONTAINS

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_initialize
  ! Returns 0 if all is well
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_initialize(blocks, xmax, ymax, do_flow, do_transport)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: blocks, xmax(blocks), ymax(blocks)
    LOGICAL, INTENT(IN) :: do_flow, do_transport

                                ! set solver parameters

    solvername = 'sip'     ! solver name
    omega = 1.8                 ! relaxation parameter
    alpha = 0.5                 ! relaxation for sip and msi
    npreit = 1                  ! preconditional iterations
    gamma = 0                   ! recursive runs (for mg)
    nsmit = 0                   ! number of mg smoothing iterations
    nsolit = 0                  ! number of mg iterations at finest level
    depthmx = 0                 ! maximum depth for mg
    mindim = 10                 ! minimum array size for mg
    nitmn = 0                   ! minimum number of solution iterations
    nres = 1                    ! calculate the residual every iteration
    resout = .FALSE.            ! print out residual or not
    set = .FALSE.               ! do not set a solution value
    zeroed = .FALSE.            ! do not zero solution first

    CALL solver_config()

    solver_initialize = 0
  END FUNCTION solver_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE solver_config
  ! ----------------------------------------------------------------
  SUBROUTINE solver_config()

    IMPLICIT NONE

    LOGICAL :: opened
    CHARACTER (LEN=1024) :: buf, msg, word, value
    INTEGER :: iounit = 1

    CALL open_existing(sixpack_config, iounit, fatal=.FALSE., result=opened)

    IF (.NOT. opened) THEN
       msg = 'sixpack: Cannot open configuration file "' // &
            &TRIM (sixpack_config) // '", continuing with defaults'
       CALL status_message(msg)
       RETURN
    END IF
  
    DO WHILE (.TRUE.)
       READ (iounit, '(A1024)', END=100) buf
       IF (LEN_TRIM(buf) .GT. 0) THEN
          READ(buf, *) word, value
          SELECT CASE (word)
          CASE ("solver")
             solvername = value
             msg = 'sixpack: solver set to "' // TRIM(solvername) // '"'
             CALL status_message(msg)
          CASE ('scalar_tolerance')
             READ(value, *) scalar_atol
             WRITE(msg, *) 'sixpack: scalar solution tolerance (abs) set to "', scalar_atol
             CALL status_message(msg)
          CASE ('depth_tolerance')
             READ(value, *) depth_atol
             WRITE(msg, *) 'sixpack: depth correction solution tolerance (abs) set to "', depth_atol
             CALL status_message(msg)
          CASE ('precond_iterations')
             READ(value, *) npreit
             WRITE(msg, *) 'sixpack: preconditioner iterations set to ', npreit
             CALL status_message(msg)
          CASE ('alpha')
             READ(value, *) alpha
             WRITE(msg, *) 'sixpack: alpha relaxation factor set to ', alpha
             CALL status_message(msg)
          CASE DEFAULT
             WRITE(msg, *) 'sixpack: configuration line not understood: ', TRIM(buf)
             CALL error_message(msg, fatal=.FALSE.)
          END SELECT
       END IF
    END DO
100 CONTINUE
  END SUBROUTINE solver_config


  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver(blk, ieq, x_start, x_end, y_start, y_end, its, &
       &ap, aw, ae, as, an, bp, x)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: blk, ieq, x_start, x_end, y_start, y_end, its
    DOUBLE PRECISION, INTENT(IN), &
         &DIMENSION(x_start:x_end,y_start:y_end) :: &
         &ap, aw, ae, as, an, bp
    DOUBLE PRECISION, INTENT(INOUT), &
         &DIMENSION(x_start:x_end,y_start:y_end) :: x

    DOUBLE PRECISION, DIMENSION(1:x_end-x_start+3, 1:y_end-y_start+3) ::&
         &aplocal, awlocal, aelocal, aslocal, anlocal, bplocal, xlocal

    INTEGER :: nx, ny, ilocal, jlocal, i, j
    INTEGER :: ix = 2, iy = 2

                                ! aw, ae, as, an need to be on the
                                ! other side of the equation, use this
                                ! to change their sign

    DOUBLE PRECISION, PARAMETER :: sign = -1.0
    
                                ! sixpack expects, but does not use
                                ! ghost cell values, hence we add 2
                                ! extra to the dimensions

    nx = x_end-x_start+3
    ny = y_end-y_start+3

                                ! initialize the coefficents (should
                                ! only affect ghost values)

    aplocal = 1.0
    awlocal = 0.0
    aelocal = 0.0
    aslocal = 0.0
    anlocal = 0.0
    bplocal = 0.0
    xlocal = 0.0

                                ! copy the coefficients and solution
                                ! to the local arrays

    DO ilocal = 2, nx - 1
       DO jlocal = 2, ny - 1
          i = x_start + (ilocal - 2)
          j = y_start + (jlocal - 2)
          aplocal(ilocal, jlocal) = ap(i,j)
          awlocal(ilocal, jlocal) = sign*aw(i,j)
          aelocal(ilocal, jlocal) = sign*ae(i,j)
          aslocal(ilocal, jlocal) = sign*as(i,j)
          anlocal(ilocal, jlocal) = sign*an(i,j)
          bplocal(ilocal, jlocal) = bp(i,j)
          xlocal(ilocal, jlocal) = x(i,j)
       END DO
    END DO

                                ! set equation specific solver
                                ! parameters

    SELECT CASE (ieq) 
    CASE (SOLVE_DP)
       nitmx = depth_sweep
       resmx = depth_atol
    CASE DEFAULT
       nitmx = scalar_sweep
       resmx = scalar_atol
    END SELECT
                                ! solve

    CALL Solver5(xlocal,aelocal,awlocal,anlocal,aslocal,aplocal,bplocal,&
         &solvername, omega,alpha,npreit,gamma,nsmit,nsolit,depthmx,&
         &mindim,resmx,nitmx,nitmn,resout,noit,residual,&
         &nres,nx,ny,set,ix,iy,zeroed,stat)

                                ! copy local solution back to caller

    x(x_start:x_end,y_start:y_end) = xlocal(2:nx-1, 2:ny-1)

    IF (.FALSE.) THEN
       SELECT CASE (stat)
       CASE (0)
          WRITE (*,*) 'Successful solution after ', noit, ' iterations'
       CASE (1)
          WRITE (*,*) 'Solution did not converge after ', noit, ' iterations (final residual = ', residual, ')'
       CASE DEFAULT
          WRITE (*,*) 'Solution failed after ', noit, ' iterations, reason: ', stat
       END SELECT
    END IF
    IF (stat .GT. 1) THEN
       WRITE(msg, *) 'FATAL ERROR: internal solver error (', stat, '), cannot continue'
       CALL error_message(msg, .TRUE.)
    END IF
    solver = stat
  END FUNCTION solver
  

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_finalize
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_finalize()

    IMPLICIT NONE
    solver_finalize = 0

  END FUNCTION solver_finalize
END MODULE solver_module
