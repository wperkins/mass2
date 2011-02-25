! ----------------------------------------------------------------
! LOGICAL FUNCTION realequal
! ----------------------------------------------------------------
LOGICAL FUNCTION realequal(x1, x2)

  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: x1, x2
  
  realequal = (ABS(x1-x2) .LE. EPSILON(x1))

END FUNCTION realequal


