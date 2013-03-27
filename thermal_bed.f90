! ----------------------------------------------------------------
! file: thermal_bed.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 27, 2013 by William A. Perkins
! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE thermal_bed
! ----------------------------------------------------------------
MODULE thermal_bed

  USE constants
  USE diffusive_bed

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE thermal_bed_rec
     TYPE (diffusive_bed_rec) :: dbed
     DOUBLE PRECISION :: density
     DOUBLE PRECISION :: specific_heat
     DOUBLE PRECISION :: conductivity
  END type thermal_bed_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION thermal_bed_diffusivity
  !
  ! computes thermal diffusivity, ft^2/s
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION thermal_bed_diffusivity(tbed)

    IMPLICIT NONE

    TYPE (thermal_bed_rec), INTENT(IN) :: tbed

    thermal_bed_diffusivity = tbed%conductivity/tbed%density/tbed%specific_heat ! m^2/s
    thermal_bed_diffusivity = thermal_bed_diffusivity/0.3048/0.3048             ! ft^2/s

  END FUNCTION thermal_bed_diffusivity


  ! ----------------------------------------------------------------
  ! SUBROUTINE thermal_bed_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE thermal_bed_initialize(tbed, depth, layers, k_bed, cp_bed, spec_grav_bed, t_inf, t_o)

    IMPLICIT NONE

    TYPE (thermal_bed_rec), INTENT(INOUT) :: tbed
    DOUBLE PRECISION, INTENT(IN) :: depth           ! bed depth, ft
    DOUBLE PRECISION, INTENT(IN) :: k_bed           ! bed conductivity, W/m/C
    DOUBLE PRECISION, INTENT(IN) :: cp_bed          ! bed specific heat, J/kg/C
    DOUBLE PRECISION, INTENT(IN) :: spec_grav_bed   ! bed specific grav
    DOUBLE PRECISION, INTENT(IN) :: t_inf, t_o
    INTEGER, INTENT(IN) :: layers

    DOUBLE PRECISION :: diffusivity                 ! thermal diffusivity, ft^2/s
    
    tbed%density = spec_grav_bed*metric_density
    tbed%specific_heat = cp_bed
    tbed%conductivity = k_bed

    diffusivity = thermal_bed_diffusivity(tbed)

    CALL diffusive_bed_initialize(tbed%dbed, depth, layers, diffusivity, t_inf, t_o)

  END SUBROUTINE thermal_bed_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE thermal_bed_release
  ! ----------------------------------------------------------------
  SUBROUTINE thermal_bed_release(tbed)

    IMPLICIT NONE

    TYPE (thermal_bed_rec), INTENT(INOUT) :: tbed
    
    CALL diffusive_bed_release(tbed%dbed)

  END SUBROUTINE thermal_bed_release

  ! ----------------------------------------------------------------
  ! SUBROUTINE thermal_bed_solve
  ! ----------------------------------------------------------------
  SUBROUTINE thermal_bed_solve(tbed, deltat, t_w)

    IMPLICIT NONE

    TYPE (thermal_bed_rec), INTENT(INOUT) :: tbed
    DOUBLE PRECISION, INTENT(IN) :: deltat, t_w

    tbed%dbed%kdiff = thermal_bed_diffusivity(tbed)

    CALL diffusive_bed_solve(tbed%dbed, deltat, t_w)

  END SUBROUTINE thermal_bed_solve

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION thermal_bed_flux
  !
  ! calculate thermal flux, W/m^2
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION thermal_bed_flux(tbed)

    IMPLICIT NONE

    TYPE (thermal_bed_rec), INTENT(IN) :: tbed

    thermal_bed_flux = diffusive_bed_flux(tbed%dbed) ! C ft/s
    thermal_bed_flux = thermal_bed_flux/0.3048       ! C m/s
    thermal_bed_flux = thermal_bed_flux*tbed%density*tbed%specific_heat ! W/m^2
    
  END FUNCTION thermal_bed_flux



END MODULE thermal_bed
  
