MODULE io_routines_module
IMPLICIT NONE

CONTAINS
!####################################################################################
SUBROUTINE output_2d_array(io_unit,local_title,istart,imax,jstart,jmax,array)

USE misc_vars, ONLY: i_index_min, j_index_min
IMPLICIT NONE

INTEGER :: io_unit, imax, jmax, istart,jstart, i, j
CHARACTER*75 :: local_title
DOUBLE PRECISION :: array(i_index_min:,j_index_min:)

1000 FORMAT(i5,2x,60(f12.4,2x))
1010 FORMAT(a75)
1020 FORMAT(80('-'))
1030 FORMAT(60(4x,i6))
1035 FORMAT(10x,i5,4x)

WRITE(io_unit,1020)
WRITE(io_unit,1010)local_title
WRITE(io_unit,1020)

WRITE(io_unit,1035, advance='no')jstart
DO j=jstart+1,jmax
 WRITE(io_unit,1030, advance='no')j
END DO
WRITE(io_unit,*)

DO i=istart,imax
   WRITE(io_unit,1000)i, array(i,jstart:jmax)
END DO
WRITE(io_unit,*)


END SUBROUTINE output_2d_array

!#####################################################################################
SUBROUTINE plot_2d_array
IMPLICIT NONE

END SUBROUTINE plot_2d_array

END MODULE io_routines_module
