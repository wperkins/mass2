!
!  mass2 main driver program
!

PROGRAM mass2

USE mass2_main_025
USE misc_vars

IMPLICIT NONE

CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"

INTEGER :: maxsteps
INTEGER :: status_flag = 1

WRITE(*,*)'calling mass2_main' 

CALL fpinit()
CALL time_series_module_init()
CALL date_time_flags()

CALL open_new('status.out', utility_status_iounit)
CALL open_new('error-warning.out', utility_error_iounit)
CALL open_new('output.out', output_iounit)
CALL start_up(status_flag)


!----------------------------------------------------------------------------------
! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
!----------------------------------------------------------------------------------
! Time Marching Loop

maxsteps = INT((end_time%time - current_time%time)/(delta_t/86400.0d0))
time_step_count = 0
DO WHILE(current_time%time .LT. end_time%time) 

   IF(do_flow)THEN
      CALL hydro(status_flag)
   ENDIF

   IF(do_transport)THEN
      CALL transport(status_flag)
   ENDIF

   ! update the old time level values of the independent variables
   CALL update(status_flag)

   !---------------------------------------------------------------------------
   !update model time prior to output 
   !   since we are advancing in time the dependent variables are now at the next
   !   time level

   time_step_count = time_step_count + 1

   ! update decimal julian day time
   current_time%time = start_time%time + DBLE(time_step_count)*delta_t/86400.000000d0 ! remember that the delta is in SECONDS

   CALL decimal_to_date(current_time%time, current_time%date_string, current_time%time_string)
   
   CALL output(status_flag)

   IF(write_restart_file)THEN
      CALL write_restart(status_flag)
   END IF

END DO
! end time loop
!***************************************************************************


WRITE(*,*)'completion with status=',status_flag

! transport only mode
!     - define transport_only flag
!     - read list of restart file names
!     - allocate time_1 and time_2 arrays to hold read-in U,V,W,D, 
!     - linear interpolation to compute U,V,W,D arrays between time_1, time_2
!     - watch out on source terms for perils of euler intergration with
!       large time steps



CLOSE(cfg_iounit)
CLOSE(output_iounit)
CALL plot_file_close()
CALL gage_file_close()
CALL diag_plot_file_close()
CALL mass_file_close()
CALL time_series_module_done()
CLOSE(utility_error_iounit)
CLOSE(utility_status_iounit)



END PROGRAM mass2







