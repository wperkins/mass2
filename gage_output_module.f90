!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	gage_output
!
! VERSION and DATE: MASS2 v0.241 9/28/98
!
! PURPOSE: manages time-series files for gage locations
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY: fixed-non F90 derived type initialization; mcr 9/28/98
!
!
!***************************************************************
!

MODULE gage_output

TYPE gage_specs_struct
	!CHARACTER (LEN=80) :: filename = ''  ! fails for non F95 compilers
	CHARACTER (LEN=80) :: filename
	INTEGER :: block
	INTEGER :: i_cell, j_cell
END TYPE gage_specs_struct

INTEGER :: num_gages

TYPE(gage_specs_struct), ALLOCATABLE :: gage_specs(:)

CONTAINS

!##################################################################################
SUBROUTINE gage_file_setup(error_iounit, status_iounit)

IMPLICIT NONE


INTEGER :: i, dum, error_iounit, status_iounit, alloc_stat
INTEGER :: len1,len2,len3,spot1,spot2,spot3
CHARACTER*20 string1,string2,string3
CHARACTER*80 stringall

! count up the number of gages and allocate the structure        
  num_gages = 0    
	OPEN(50,file="gage_control.dat")
	DO WHILE(.TRUE.)
		READ(50,*,END=100)dum
		num_gages = num_gages + 1	
	END DO
100	CLOSE(50)

	ALLOCATE(gage_specs(num_gages), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of gage specs '
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of gage specs'
		gage_specs%filename = ''
	ENDIF

	OPEN(50,file="gage_control.dat")
	DO i=1,num_gages
		READ(50,*)gage_specs(i)%block,gage_specs(i)%i_cell,gage_specs(i)%j_cell

			
	END DO
	CLOSE(50)

! construct the generic file name for the gage output files
! file = "gage_block=1_icell=11_jcell=2.out"
DO i=1,num_gages
        stringall = ''
	! gage_specs(i)%filename(1:11) = "gage_block="
        stringall(1:11) = "gage_block="
	WRITE(string1,*)gage_specs(i)%block
	WRITE(string2,*)gage_specs(i)%i_cell
	WRITE(string3,*)gage_specs(i)%j_cell
	!READ(gage_point(i),*)string2
	string1 = ADJUSTL(string1)
	string2 = ADJUSTL(string2)
	string3 = ADJUSTL(string3)
	len1 = LEN_TRIM(string1)
	len2 = LEN_TRIM(string2)
	len3 = LEN_TRIM(string3)
	spot1 =12 + len1 - 1
	! gage_specs(i)%filename(12:spot1) = string1(1:len1)
        stringall(12:spot1) = string1(1:len1)
	spot1 = spot1 + 1
	spot2 = spot1 + LEN_TRIM("_icell=") - 1
	! gage_specs(i)%filename(spot1:spot2) = "_icell="
        stringall(spot1:spot2) = "_icell="
	spot1 = spot2 + 1
	spot2 = spot1 + len2 - 1
	! gage_specs(i)%filename(spot1:spot2) = string2(1:len2)
        stringall(spot1:spot2) = string2(1:len2)
	spot1 = spot2 + 1
	spot2 = spot1 + LEN_TRIM("_jcell=") - 1
	! gage_specs(i)%filename(spot1:spot2) = "_jcell="
        stringall(spot1:spot2) = "_jcell="
	spot1 = spot2 + 1
	spot2 = spot1 + len3 - 1
	! gage_specs(i)%filename(spot1:spot2) = string3(1:len3)
        stringall(spot1:spot2) = string3(1:len3)
	spot1 = spot2 + 1
	spot2 = spot1 + LEN_TRIM(".out")
	! gage_specs(i)%filename(spot1:spot2) = '.out'
        stringall(spot1:spot2) = '.out'
        gage_specs(i)%filename = stringall
END DO

! open the files and write out the header info
DO i=1,num_gages
	OPEN(50,file=gage_specs(i)%filename)
	WRITE(50,1015)gage_specs(i)%block, gage_specs(i)%i_cell, gage_specs(i)%j_cell
	WRITE(50,1020)
	WRITE(50,1005)
1005 FORMAT('#date',8x,'time',5x,'hours from start',5x,'water elev',2x,'depth',5x,'vel mag',5x,'u-cart vel',4x,'v-cart vel', &
     7x,'Block Mass Source',2x,'species 1 - TDG (mg/l) ',2x,'species 2 - Temp (C)', &
		 2x,'Total Diss. Gas Press. (mmHg)',2x,'TDG deltaP (mmHg)',2x,'TDG %Sat')
	WRITE(50,1020)
	CLOSE(50)

END DO
1020 FORMAT('#',80('-'))
1015 FORMAT('#gage output for Block ',i5,' i cell =',i5,' j cell=',i5)
END SUBROUTINE gage_file_setup

!###################################################################################
!SUBROUTINE gage_print
!END SUBROUTINE gage_print

END MODULE gage_output
