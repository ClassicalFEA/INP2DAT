! Copyright 2025 ClassicalFEA and Dr. Brian Esp
! Original program developed by Dr. Bill Case
 
! Prints error messages when IOSTAT is not zero on a file read. 
 
      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: LAST_REC          ! Last record read from ABQ_MODEL_FILE
 
      INTEGER, INTENT(IN)             :: FILE_UNIT         ! Unit number for file FILNAM
      INTEGER, INTENT(IN)             :: IOCHK             ! IOSTAT error number when reading a file

! **********************************************************************************************************************************
! IOCHK < 0 is due to EOF/EOR during read. IOCHK > 0 is due to error during read.
 
      IF (IOCHK < 0) THEN

         WRITE(*,902) IOCHK, TRIM(FILNAM)
         WRITE(*,998) LAST_REC

      ELSE

         WRITE(*,903) IOCHK, TRIM(FILNAM)
         WRITE(*,999) TRIM(LAST_REC)

      ENDIF

! **********************************************************************************************************************************
  902 FORMAT(' IOSTAT = ',I8,' READING FILE: "', A, '"', ' INDICATES WE HAVE REACHED THE END OF THE FILE')

  903 FORMAT(' ERROR ENCOUNTERED WITH IOSTAT = ',I8,' READING FILE: "', A, '"')

  998 fORMAT(' THE LAST RECORD READ PRIOR TO THE THE READ ERROR WAS: "', A, '"',/)

  999 FORMAT(' THE LAST RECORD READ PRIOR TO THE THE EOF WAS: "', A, '"',/)

! **********************************************************************************************************************************
 
      END SUBROUTINE READERR
