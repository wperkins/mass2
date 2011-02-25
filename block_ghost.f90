! ----------------------------------------------------------------
! file: block_ghost.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 24, 2011 by William A. Perkins
! Last Change: Thu Feb 24 14:56:50 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE block_ghost
! ----------------------------------------------------------------
MODULE block_ghost

  USE block_hydro_bc
  USE block_grid

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_build_one_ghost
  ! ----------------------------------------------------------------
  SUBROUTINE block_build_one_ghost(blk, bc, cblk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    TYPE (block_struct), INTENT(INOUT) :: cblk
    TYPE (bc_spec_struct), INTENT(IN) :: bc

    INTEGER :: k, cells, con_cells, ifcells, jfcells
    INTEGER :: i, ibeg, iend, coni, conibeg, coniend, icoff
    INTEGER :: j, jbeg, jend, conj, conjbeg, conjend, jcoff
    DOUBLE PRECISION :: fract

    DO k = 1, bc%num_cell_pairs

       SELECT CASE(bc%bc_loc)
       CASE("US")
          ibeg = 0 - (nghost - 1)
          iend = ibeg + (nghost - 1)
          coniend = cblk%xmax - 1
          conibeg = coniend - (nghost - 1)
          jbeg = bc%start_cell(k)
          jend = bc%end_cell(k) + 1
          conjbeg = bc%con_start_cell(k)
          conjend = bc%con_end_cell(k) + 1
          cells = jend - jbeg
          con_cells = conjend - conjbeg
          icoff = 0
          jcoff = 1
       CASE ("DS")
          ibeg = blk%xmax + 1
          iend = ibeg + (nghost - 1)
          conibeg = 2
          coniend = conibeg + (nghost - 1)
          jbeg = bc%start_cell(k)
          jend = bc%end_cell(k) + 1
          conjbeg = bc%con_start_cell(k)
          conjend = bc%con_end_cell(k) + 1
          cells = jend - jbeg
          con_cells = conjend - conjbeg
          icoff = 0
          jcoff = 1
       CASE ("RB") 
          jbeg = 0 - (nghost - 1)
          jend = jbeg + (nghost - 1)
          conjend = cblk%ymax - 1
          conjbeg = conjend - (nghost - 1)
          ibeg = bc%start_cell(k)
          iend = bc%end_cell(k) + 1
          conibeg = bc%con_start_cell(k)
          coniend = bc%con_end_cell(k) + 1
          cells = iend - ibeg
          con_cells = coniend - conibeg
          icoff = 1
          jcoff = 0
       CASE ("LB") 
          jbeg = blk%ymax + 1
          jend = jbeg + (nghost - 1)
          conjbeg = 2
          conjend = conjbeg + (nghost - 1)
          ibeg = bc%start_cell(k)
          iend = bc%end_cell(k) + 1
          conibeg = bc%con_start_cell(k)
          coniend = bc%con_end_cell(k) + 1
          cells = iend - ibeg 
          con_cells = coniend - conibeg
          icoff = 1
          jcoff = 0
       END SELECT

       ! fine cells per coarse cell

       SELECT CASE(bc%bc_loc)
       CASE ("US", "DS")
          ifcells = 1
          IF (cells .GE. con_cells) THEN
             jfcells = cells/con_cells
          ELSE 
             jfcells = con_cells/cells
          END IF
       CASE ("LB", "RB")
          IF (cells .GE. con_cells) THEN
             ifcells = cells/con_cells
          ELSE 
             ifcells = con_cells/cells
          END IF
          jfcells = 1
       END SELECT

       IF (cells .EQ. con_cells) THEN

          ! if the number of cells is equal on both sides, we
          ! just copy the ghost cell corners from the connecting
          ! block
          
          CALL block_var_get_some(cblk%bv_x_grid, &
               &conibeg, coniend, conjbeg, conjend, &
               &cblk%buffer(conibeg:coniend, conjbeg:conjend))

          coni = conibeg
          DO i = ibeg, iend
             conj = conjbeg
             DO j = jbeg, jend
                IF (block_owns(blk, i, j)) THEN
                   blk%x_grid(i,j) = cblk%buffer(coni,conj)
                END IF
                conj = conj + 1
             END DO
             coni = coni + 1
          END DO
          CALL block_var_put(blk%bv_x_grid)

          CALL block_var_get_some(cblk%bv_y_grid, &
               &conibeg, coniend, conjbeg, conjend, &
               &cblk%buffer(conibeg:coniend, conjbeg:conjend))

          coni = conibeg
          DO i = ibeg, iend
             conj = conjbeg
             DO j = jbeg, jend
                IF (block_owns(blk, i, j)) THEN
                   blk%y_grid(i,j) = cblk%buffer(coni,conj)
                END IF
                conj = conj + 1
             END DO
             coni = coni + 1
          END DO
          CALL block_var_put(blk%bv_y_grid)

          CALL block_var_get_some(cblk%bv_zbot_grid, &
               &conibeg, coniend, conjbeg, conjend, &
               &cblk%buffer(conibeg:coniend, conjbeg:conjend))

          coni = conibeg
          DO i = ibeg, iend
             conj = conjbeg
             DO j = jbeg, jend
                IF (block_owns(blk, i, j)) THEN
                   blk%zbot_grid(i,j) = cblk%buffer(coni,conj)
                END IF
                conj = conj + 1
             END DO
             coni = coni + 1
          END DO
          CALL block_var_put(blk%bv_zbot_grid)

      ELSE IF (cells .GT. con_cells) THEN

          ! if this is the fine block, we need to interpolate
          ! ghost cell corners from the coarse cell corners

          CALL error_message("block_build_ghost: 1-to-many connection not allowed", &
               &fatal=.TRUE.)

!!$                DO i = ibeg, iend
!!$                   coni = conibeg + (i - ibeg)/ifcells
!!$                   DO j = jbeg, jend
!!$                      conj = conjbeg + (j - jbeg)/jfcells
!!$
!!$                      IF (ifcells .EQ. 1) THEN
!!$                         fract = DBLE(MOD(j - jbeg, jfcells))
!!$                         fract = fract/DBLE(jfcells)
!!$                         icoff = 0
!!$                         jcoff = 1
!!$                      ELSE 
!!$                         fract = DBLE(MOD(i - ibeg, ifcells))
!!$                         fract = fract/DBLE(ifcells)
!!$                         icoff = 1
!!$                         jcoff = 0
!!$                      END IF
!!$
!!$                      CALL interpolate_point(fract,&
!!$                           &cblk%x_grid(coni,conj),&
!!$                           &cblk%y_grid(coni,conj),&
!!$                           &cblk%zbot_grid(coni,conj),&
!!$                           &cblk%x_grid(coni+icoff,conj+jcoff),&
!!$                           &cblk%y_grid(coni+icoff,conj+jcoff),&
!!$                           &cblk%zbot_grid(coni+icoff,conj+jcoff),&
!!$                           &blk%x_grid(i,j), &
!!$                           &blk%y_grid(i,j), &
!!$                           &blk%zbot_grid(i,j))
!!$
!!$                   END DO
!!$                END DO

       ELSE IF (cells .LT. con_cells) THEN

          ! if this is the coarse block, we copy selected fine
          ! block corners for the ghost cells

          CALL error_message("block_build_ghost: 1-to-many connection not allowed", &
               &fatal=.TRUE.)
!!$                DO i = ibeg, iend
!!$                   coni = conibeg + (i - ibeg)*ifcells
!!$                   DO j = jbeg, jend
!!$
!!$                      conj = conjbeg + (j - jbeg)*jfcells
!!$                      blk%x_grid(i,j) = cblk%x_grid(coni,conj)
!!$                      blk%y_grid(i,j) = cblk%y_grid(coni,conj)
!!$                      blk%zbot_grid(i,j) = cblk%zbot_grid(coni,conj)
!!$
!!$                   END DO
!!$                END DO
       END IF
    END DO

    CALL ga_sync()
    CALL block_var_get(blk%bv_x_grid)
    CALL block_var_get(blk%bv_y_grid)
    CALL block_var_get(blk%bv_zbot_grid)


  END SUBROUTINE block_build_one_ghost



  ! ----------------------------------------------------------------
  ! SUBROUTINE block_build_ghost
  ! ----------------------------------------------------------------
  SUBROUTINE block_build_ghost(iblk)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblk
    INTEGER :: num_bc, cblk

                                ! copy ghost cell coordinates for those
                                ! cells connecting with another block

    DO num_bc = 1, block_bc(iblk)%num_bc
       IF (block_bc(iblk)%bc_spec(num_bc)%bc_type .EQ. "BLOCK") THEN
          cblk = block_bc(iblk)%bc_spec(num_bc)%con_block
          CALL block_build_one_ghost(block(iblk),  &
               &block_bc(iblk)%bc_spec(num_bc), block(cblk))
       END IF
    END DO

  END SUBROUTINE block_build_ghost

END MODULE block_ghost
