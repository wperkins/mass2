! ----------------------------------------------------------------
! file: total_conc.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 28, 2001 by William A. Perkins
! Last Change: Thu Mar  1 08:11:44 2001 by William A. Perkins <perk@dora.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION total_conc
! 
! This function computes the total contaminant concentration in the
! water column as the sum of dissolved and all particulate
! phases. This is needed by the biota source module.  Declaring it
! this way avoids a circular module dependancy.
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION total_conc(idiss, iblk, i, j)

  USE scalars
  USE scalars_source
  IMPLICIT NONE

  INTEGER :: idiss, iblk, i, j, ipart

  total_conc = species(idiss)%scalar(iblk)%conc(i,j)              
  DO ipart = 1, max_species
     SELECT CASE (scalar_source(ipart)%srctype)
     CASE (PART)
        IF (scalar_source(ipart)%part_param%disidx .EQ. idiss)&
             &total_conc = total_conc +&
             &species(ipart)%scalar(iblk)%conc(i,j)
     END SELECT
  END DO

END FUNCTION total_conc
