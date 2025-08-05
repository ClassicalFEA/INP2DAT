! Copyright 2025 ClassicalFEA and Dr. Brian Esp
! Original program developed by Dr. Bill Case
! ##################################################################################################################################
! This program reads NODE and ELEMENT data from an ABAQUS inp file and translates it to MYSTRAN GRID and element connection entries.

! (1) On the MYSTRAN GRID entries no info is written regarding coordinate systems or permanent constraints (since that data is not
!     part of the inp file so these fields are left blank on GRID bdf entries.
! (2) For the element data see remarks in subr GET_ELEM_CONNECT_DATA.f90

      PROGRAM INP2DAT

      CHARACTER(LEN=*), PARAMETER     :: PROG_NAME = 'INP2DAT'
      INTEGER, PARAMETER              :: REC_LEN = 80      ! Length of records read from ABQ_MODEL_FILE

      CHARACTER(LEN=REC_LEN)          :: ABQ_MODEL_FILE    ! The name of the file that has the ABAQUS model file
      CHARACTER(LEN=REC_LEN)          :: MYSTRAN_BDF_FILE  ! The name of the file that has the MYSTRAN bdf file
      CHARACTER(LEN=REC_LEN)          :: INPUT_LINE        ! A line of input from the ABAQUS model file
      CHARACTER(LEN=REC_LEN)          :: LAST_REC          ! Prior record read from ABQ_MODEL_FILE

      INTEGER                         :: ABQ = 1           ! File number for ABAQUS model file
      INTEGER                         :: BDF = 2           ! File number for MYSTRAN output bdf file
      INTEGER                         :: IERR              ! Error count
      INTEGER                         :: INDEX_DECIMAL_PT  ! Location in a string where the last decimal point is found
      INTEGER                         :: GN                ! Grid (node) number
      INTEGER                         :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER                         :: NELEM             ! Count of the NUMBER OF element entries read from ABQFFIL and converted
      INTEGER                         :: NGRID             ! Count of the NUMBER OF node entries read from ABQFFIL and converted

      REAL                            :: GX, GY, GZ        ! Grid coords

      INTRINSIC INDEX

! **********************************************************************************************************************************
! Initialize

      IERR  = 0
      NGRID = 0
      NELEM = 0

! ++++++++++++++++++++++++++++++++++++++++++
! Execute the program with the command line argument of the ABAQUS model file name
      ARG_COUNT = COMMAND_ARGUMENT_COUNT()
      IF (ARG_COUNT < 1) THEN
          WRITE(*,*) 'ERROR: Usage: inp2dat <input_file>'
          STOP
      ENDIF
      CALL GET_COMMAND_ARGUMENT(1, ABQ_MODEL_FILE)

! +++ AUTO-APPEND .inp IF MISSING EXTENSION
      IF (INDEX(ABQ_MODEL_FILE, '.', .TRUE.) == 0) THEN
          ABQ_MODEL_FILE = TRIM(ABQ_MODEL_FILE) // '.inp'
      ENDIF
! +++++++++++++++++++++++++++++++++++++++++

      OPEN(ABQ, FILE=ABQ_MODEL_FILE, STATUS='OLD', ACTION='READ', IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN                                 ! If there were errors opening the file check to find out why
         CALL OPNERR ( IOCHK, ABQ_MODEL_FILE )
         IERR = IERR + 1
      ENDIF

! The MYSTRAN bdf will be put into a file with the same name as the ABAQUS file but with an extension of bdf. Open it:

      INDEX_DECIMAL_PT = INDEX(ABQ_MODEL_FILE, '.', .true. )
      MYSTRAN_BDF_FILE = ABQ_MODEL_FILE(1:INDEX_DECIMAL_PT) // 'bdf'
      OPEN(BDF, FILE=MYSTRAN_BDF_FILE, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN                                 ! If there were errors opening the file check to find out why
         CALL OPNERR ( IOCHK, MYSTRAN_BDF_FILE )
         IERR = IERR + 1
      ENDIF

! Write date, time and ABQ input file name to the BDF file

      CALL WRT_DATE_TIME_INFILE ( PROG_NAME, BDF, ABQ_MODEL_FILE, MYSTRAN_BDF_FILE )

      IF (IERR == 0) THEN                                  ! Process the data in ABQFIL if no errors opening files

         WRITE(BDF,1)                                      ! Write SOL 101, CEND and BEGIN BULK to the bdf file

         ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! No more reading the first line outside the loop
         ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
main:    DO 
            READ(ABQ,'(A)', IOSTAT=IOCHK) INPUT_LINE
            IF (IOCHK /= 0) THEN
                  CALL READERR ( IOCHK, ABQ_MODEL_FILE, ABQ, LAST_REC )
                  IERR = IERR + 1
                  EXIT main
            ENDIF

            LAST_REC = INPUT_LINE
            ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            ! Transform the input line to upper case for case insensitive comparison
            CALL TO_UPPER(INPUT_LINE)
            ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
            
            ! Check if it is a '*NODE' category
            IF (INPUT_LINE == '*NODE') THEN                ! Process NODE data to MYSTRAN GRID entries
               DO
                  READ(ABQ,'(A)') INPUT_LINE
                  IF (INPUT_LINE(1:1) == '*') EXIT
                  READ(INPUT_LINE,*) GN, GX, GY, GZ
                  NGRID = NGRID + 1
                  WRITE(BDF,1001) GN, GX, GY, GZ
               ENDDO
               WRITE(BDF,1000)
            ENDIF

            ! Check if it is a '*ELEMENT' category
            IF (INPUT_LINE(1:8) == '*ELEMENT') THEN        ! Process elem data to MYSTRAN elem connection entries
               CALL GET_ELEM_CONNECT_DATA ( INPUT_LINE, BDF, ABQ, NELEM )
            END IF 
            ! We don't care about the rest of the lines in the file so we just don't do anything with them
            ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         ENDDO main

         WRITE(BDF,9999)                                   ! Write ENDDATA to the bdf file
         CLOSE(BDF,STATUS='KEEP') 

         WRITE(*,9001) NGRID, NELEM, TRIM(ABQ_MODEL_FILE)
         WRITE(*,9002) TRIM(ABQ_MODEL_FILE), TRIM(MYSTRAN_BDF_FILE)

         IF (IERR > 0) STOP                                ! There was an error reading a record or we reached the end of the file

      ELSE                                                 ! IERR > 0 so  write messag and stop 

         WRITE(*,2) IERR
         STOP

      ENDIF

! **********************************************************************************************************************************
    1 FORMAT('SOL 101', /, 'CEND', /, 'BEGIN BULK', /, '$')

    2 FORMAT(' Processing stopped due to above listed ', I4, ' error(s)')

 1000 FORMAT('$')

 1001 FORMAT('GRID, ', I8, ', ,', 3(1ES14.6, ','), ',')

 9001 FORMAT(' This program processed ',I8,' node entries and ',I8,' element entries from "',A, '"')
 
 9002 FORMAT(' Input ABAQUS and output MYSTRAN model file names are: "', A, '" and "', A, '"')
 
 9999 FORMAT('$', /, 'ENDDATA')

! **********************************************************************************************************************************

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CONTAINS

      ! +++ ADDED HELPER SUBROUTINE FOR CASE INSENSITIVITY
      SUBROUTINE TO_UPPER(STR)
          CHARACTER(LEN=*), INTENT(INOUT) :: STR
          INTEGER                         :: I
          DO I = 1, LEN(STR)
              IF (STR(I:I) >= 'a' .AND. STR(I:I) <= 'z') THEN
                  STR(I:I) = CHAR(ICHAR(STR(I:I)) - 32)
              ENDIF
          ENDDO
      END SUBROUTINE TO_UPPER
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      END PROGRAM INP2DAT
