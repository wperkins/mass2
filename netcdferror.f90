! ----------------------------------------------------------------
! SUBROUTINE netcdferror
! used to report NetCDF errors for the specified file
! ----------------------------------------------------------------
SUBROUTINE netcdferror(file, errnum)
  
  IMPLICIT NONE
  
  CHARACTER*(*) file
  INTEGER :: errnum
  
  INCLUDE 'netcdf.inc'
  
  WRITE (*, *) 'NetCDF error: ', TRIM(file), ': ', NF_STRERROR(errnum)
  
END SUBROUTINE netcdferror

