! Copyright 2025 ClassicalFEA and Dr. Brian Esp
! Original program developed by Dr. Bill Case
! ##################################################################################################################################

      SUBROUTINE OPNERR ( IOCHK, FILNAM )
 
! Prints error messages when IOSTAT is not zero on a file OPEN. 
 
      IMPLICIT NONE

      LOGICAL                         :: FILE_EXIST        ! True if FILNAM exists
      LOGICAL                         :: FILE_OPENED       ! True if FILNAM is open

      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
 
      INTEGER, INTENT(IN)             :: IOCHK             ! IOSTAT error number when opening/reading a file
 
! **********************************************************************************************************************************
! IOCHK < 0 is due to EOF/EOR during open. IOCHK > 0 is due to error during open.
 
      INQUIRE (FILE=FILNAM,OPENED=FILE_OPENED)
      INQUIRE (FILE=FILNAM, EXIST=FILE_EXIST)

      IF (IOCHK < 0) THEN

         WRITE(*,902) IOCHK, TRIM(FILNAM)

      ELSE

         IF (.NOT.FILE_EXIST) THEN

            WRITE(*,903) IOCHK, TRIM(FILNAM)
            WRITE(*,9222)

         ELSE IF (FILE_OPENED) THEN

            WRITE(*,903) IOCHK, TRIM(FILNAM)
            WRITE(*,9232)

         ELSE

            WRITE(*,903) IOCHK, TRIM(FILNAM)
            WRITE(*,9242)

         ENDIF               

      ENDIF

! **********************************************************************************************************************************
  902 FORMAT(' EOF/EOR ENCOUNTERED WITH IOSTAT = ',I8,' OPENING FILE: "', A, '"')

  903 FORMAT(' ERROR ENCOUNTERED WITH IOSTAT = ',I8,' OPENING FILE: "', A, '"')

 9222 FORMAT(' THE FILE DOES NOT EXIST.',/)

 9232 FORMAT(' IT MAY BE OPEN IN ANOTHER PROGRAM. IF SO, CLOSE IT & START AGAIN.',/)

 9242 FORMAT(' THE FILE EXISTS AND IS NOT OPENED.  THIS IS A PROGRAMMING ERROR.',/)

! **********************************************************************************************************************************
 
      END SUBROUTINE OPNERR
