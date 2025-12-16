! Copyright 2025 ClassicalFEA and Dr. Brian Esp
! Original program developed by Dr. Bill Case
! ##################################################################################################################################
 
      SUBROUTINE WRT_DATE_TIME_INFILE ( PROG_NAME, UNT, INFILE, OUTFIL )
 
! Returns date info using Fortran DATE_AND_TIME intrinsic procedure
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: INFILE            ! Input file to the progrem that called this subr.
      CHARACTER(LEN=*), INTENT(IN)    :: PROG_NAME         ! Name of the program that called this subr.
      CHARACTER(LEN=*), INTENT(IN)    :: OUTFIL            ! File data was written to by the calling program.

      INTEGER                         :: YEAR              ! The year
      INTEGER                         :: MONTH             ! The month
      INTEGER                         :: DAY               ! The day

      INTEGER                         :: HOUR              ! The hour
      INTEGER                         :: MINUTE            ! The minute
      INTEGER                         :: SEC               ! The second
      INTEGER                         :: SFRAC             ! The sec frac

      INTEGER, INTENT(IN)             :: UNT               ! Unit to write outputs to (usually BDF)
      INTEGER                         :: VALS(8)           ! Contains year, month, day from intrinsic function DATE_AND_TIME
 
      INTRINSIC                       :: DATE_AND_TIME
 
! **********************************************************************************************************************************
      CALL DATE_AND_TIME( values=VALS)

      YEAR   = VALS(1)
      MONTH  = VALS(2)
      DAY    = VALS(3)

      HOUR   = VALS(5)
      MINUTE = VALS(6)
      SEC    = VALS(7)
      SFRAC  = VALS(8)

      WRITE(UNT,150) PROG_NAME, MONTH, DAY, YEAR, HOUR, MINUTE, SEC, SFRAC, TRIM(INFILE), TRIM(OUTFIL)

      RETURN

! **********************************************************************************************************************************
  150 FORMAT('$ INP2DAT Version 1.1 (12/16/2025) by Classical FEA and ESP Compositse - Copyright 2025', /,                              &
			 '$ Convert Abaqus/CalculiX INP file (nodes and elements) to a Nastran/MYSTRAN DAT/BDF deck', /,                                                   &
             '$ ', /,                                                   &
			 '$ ', A,' on: ',I2,'/',I2,'/',I4,' at ',I2,':',I2,':',I2,'.',I3, /,                                                   &
             '$ The input file was: "', A, '". The output file was: "', A, '"')

      END SUBROUTINE WRT_DATE_TIME_INFILE
