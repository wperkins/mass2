!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 module file
!
! VERSION and DATE: 0.22 4-17-98
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS: sets up the bc's for each block. note that block connections are also included here.
!
! MOD HISTORY: 4-1-98 allocatable arrays, pointers
!
!
!***************************************************************
!
MODULE date_time

IMPLICIT NONE

TYPE datetime_struct

	CHARACTER (LEN=10) :: date_string
	CHARACTER (LEN=12)	 :: time_string
	DOUBLE PRECISION :: time

END TYPE datetime_struct



!#########################################################################
CONTAINS


!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	date_to_decimal
!
! VERSION and DATE: MASS2 
!
! PURPOSE: coverts a date/time string to decimal julian days
!
! RETURNS: decimal date
!
! REQUIRED:	IMSL routine NDAYS
!
! LOCAL VARIABLES:
!
! COMMENTS:	some luking precision problems that round things off
!	to seconds if a non-rational time increment is used.
!	eg. 1.333333. may be related to interaction with C++B
!
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

DOUBLE PRECISION FUNCTION date_to_decimal(date_string, time_string)

USE JULIAN

CHARACTER (LEN=*) :: date_string
CHARACTER (LEN=*)	 :: time_string

INTEGER :: mon,dd,yr,hh,mm
DOUBLE PRECISION :: ss

READ(date_string(1:2),'(i2)')mon
READ(date_string(4:5),'(i2)')dd
READ(date_string(7:10),'(i4)')yr
! WRITE(*,*) time_string
READ(time_string(1:2),'(i2)')hh
READ(time_string(4:5),'(i2)')mm
READ(time_string(7:),*)ss


date_to_decimal = juldays(mon, dd, yr, hh, mm, DBLE(ss))


END FUNCTION date_to_decimal


!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	decimal_to_date
!
! VERSION and DATE: MASS2 
!
! PURPOSE: coverts a decimal date to a date/time string
!
! RETURNS: time_string & time_string
!
! REQUIRED:	Uses IMSL routine NDYIN
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE decimal_to_date(decimal_date, date_string, time_string)

  USE JULIAN
  IMPLICIT NONE

  DOUBLE PRECISION :: decimal_date
  CHARACTER (LEN=*) :: date_string
  CHARACTER (LEN=*) :: time_string

  DOUBLE PRECISION :: sec
  INTEGER :: mon, day, yr, hr, min

  CALL CALCDATE(decimal_date,mon,day,yr,hr,min,sec)
  
  WRITE(date_string(7:10),'(i4.4)')yr
  WRITE(date_string(4:5),'(i2.2)')day
  WRITE(date_string(1:2),'(i2.2)')mon

  WRITE(time_string(1:2),'(i2.2)')hr
  WRITE(time_string(4:5),'(i2.2)')min
  WRITE(time_string(7:8),'(i2.2)') INT(sec)
  ! WRITE(*,*) time_string

END SUBROUTINE decimal_to_date

END MODULE date_time
