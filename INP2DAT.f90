! Copyright 2025 ClassicalFEA and Dr. Brian Esp
! Original program developed by Dr. Bill Case
! ##################################################################################################################################
! This program reads NODE and ELEMENT data from an ABAQUS inp file and translates it to MYSTRAN GRID and element connection entries.

! (1) On the MYSTRAN GRID entries no info is written regarding coordinate systems or permanent constraints (since that data is not
!     part of the inp file so these fields are left blank on GRID bdf entries.
! (2) For the element data see remarks in subr GET_ELEM_CONNECT_DATA.f90

PROGRAM INP2DAT

  CHARACTER(LEN=*), PARAMETER :: PROG_NAME = 'INP2DAT'
  INTEGER, PARAMETER :: REC_LEN = 80      ! Length of records read from ABQ_MODEL_FILE

  CHARACTER(LEN=REC_LEN) :: ABQ_MODEL_FILE    ! The name of the file that has the ABAQUS model file
  CHARACTER(LEN=REC_LEN) :: MYSTRAN_BDF_FILE  ! The name of the file that has the MYSTRAN bdf file
  CHARACTER(LEN=REC_LEN) :: INPUT_LINE        ! A line of input from the ABAQUS model file
  CHARACTER(LEN=REC_LEN) :: LAST_REC          ! Prior record read from ABQ_MODEL_FILE

  INTEGER :: ABQ = 1           ! File number for ABAQUS model file
  INTEGER :: BDF = 2           ! File number for MYSTRAN output bdf file
  INTEGER :: IERR              ! Error count
  INTEGER :: INDEX_DECIMAL_PT  ! Location in a string where the last decimal point is found
  INTEGER :: GN                ! Grid (node) number
  INTEGER :: IOCHK             ! IOSTAT error number when opening/reading a file
  INTEGER :: NELEM             ! Count of the NUMBER OF element entries read from ABQFFIL and converted
  INTEGER :: NGRID             ! Count of the NUMBER OF node entries read from ABQFFIL and converted

  REAL :: GX, GY, GZ        ! Grid coords

  ! +++ CHANGE: Variable to store EOF flag from subroutine
  LOGICAL :: EOF_FLAG          ! Flag to indicate end of file reached

  INTEGER :: ARG_COUNT         ! Number of command-line arguments
  ! +++ CHANGE - v3: Variables for flag support
  INTEGER :: FORMAT_OPTION     ! Which format to use: 1=basic, 2=prop, 3=propmat (default)
  CHARACTER(LEN=REC_LEN) :: ARG_STR  ! For reading command-line arguments
  ! +++ END CHANGE

  INTRINSIC INDEX

  ! *******************************************************************************************************
  ! Initialize

  IERR = 0
  NGRID = 0
  NELEM = 0

  ! Get the number of arguments
  ARG_COUNT = COMMAND_ARGUMENT_COUNT()

  ! +++ Change v3: Command arguments may contain flags
  ! If < 1, no file given. Output error
  IF (ARG_COUNT .lt. 1) THEN
    WRITE (*, *) 'ERROR: Usage: inp2dat [-basic|-prop|-propmat] <input_file>'
    STOP
  END IF

  ! Parse command-line arguments
  IF (ARG_COUNT .eq. 1) THEN
    ! Only filename provided, use default format
    CALL GET_COMMAND_ARGUMENT(1, ABQ_MODEL_FILE)
    FORMAT_OPTION = 3            ! Default to propmat format
    ! To change the default format, change this number
    ! Each number is linked to a format with if statements in lines 135
  ELSE IF (ARG_COUNT .eq. 2) THEN
    ! Flag and filename provided
    CALL GET_COMMAND_ARGUMENT(1, ARG_STR)
    CALL TO_UPPER(ARG_STR)
    IF (ARG_STR .eq. '-BASIC') THEN
      FORMAT_OPTION = 1
    ELSE IF (ARG_STR .eq. '-PROP') THEN
      FORMAT_OPTION = 2
    ELSE IF (ARG_STR .eq. '-PROPMAT') THEN
      FORMAT_OPTION = 3
      ! To add more flags:
      ! Add another ELSE IF block with the same structure
      ! ELSE IF (ARG_STR .eq. '-yourflag') THEN
      !   FORMAT_OPTION = `NEXT_NUMBER`
      ! Change accordingly in lines ~135
    ELSE
      WRITE (*, *) 'ERROR: Invalid flag. Use -basic, -prop, or -propmat'
      STOP
    END IF
    CALL GET_COMMAND_ARGUMENT(2, ABQ_MODEL_FILE)
  ELSE
    WRITE (*, *) 'ERROR: Too many arguments. Usage: inp2dat [-basic|-prop|-propmat] <input_file>'
    STOP
  END IF
  ! End Change v3

  ! To revert: Remove everything between comments with "Change v3"
  ! and uncomment the code below
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! If < 1, no file given. Output error
!   IF (ARG_COUNT .lt. 1) THEN
!     WRITE (*, *) 'ERROR: Usage: inp2dat <input_file>'
!     STOP
!   END IF
!   CALL GET_COMMAND_ARGUMENT(1, ABQ_MODEL_FILE)
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ! +++ AUTO-APPEND .inp IF MISSING EXTENSION
  IF (INDEX(ABQ_MODEL_FILE, '.', .TRUE.) .eq. 0) THEN
    ABQ_MODEL_FILE = TRIM(ABQ_MODEL_FILE)//'.inp'
  END IF
  ! +++

  OPEN (ABQ, FILE=ABQ_MODEL_FILE, STATUS='OLD', ACTION='READ', IOSTAT=IOCHK)
  IF (IOCHK .ne. 0) THEN                                 ! If there were errors opening the file check to find out why
    CALL OPNERR(IOCHK, ABQ_MODEL_FILE)
    IERR = IERR + 1
  END IF

  ! The MYSTRAN bdf will be put into a file with the same name as the ABAQUS file but with an extension of dat. Open it:

  INDEX_DECIMAL_PT = INDEX(ABQ_MODEL_FILE, '.', .true.)
  ! +++ CHANGE: Output file extension changed to .dat
  ! To revert, just change 'dat' back to 'bdf' below
  MYSTRAN_BDF_FILE = ABQ_MODEL_FILE(1:INDEX_DECIMAL_PT)//'dat'
  ! +++ END CHANGE
  OPEN (BDF, FILE=MYSTRAN_BDF_FILE, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IOCHK)
  IF (IOCHK .ne. 0) THEN                                 ! If there were errors opening the file check to find out why
    CALL OPNERR(IOCHK, MYSTRAN_BDF_FILE)
    IERR = IERR + 1
  END IF

  ! Write date, time and ABQ input file name to the BDF file
  CALL WRT_DATE_TIME_INFILE(PROG_NAME, BDF, ABQ_MODEL_FILE, MYSTRAN_BDF_FILE)

  IF (IERR .eq. 0) THEN                                  ! Process the data in ABQFIL if no errors opening files

    ! +++ Change - v3: Check the format option and write the appropriate header
    ! Write the appropriate format based on the selected option
    IF (FORMAT_OPTION .eq. 1) THEN
      WRITE (BDF, 101)                                   ! Basic format
    ELSE IF (FORMAT_OPTION .eq. 2) THEN
      WRITE (BDF, 102)                                   ! Prop format
    ELSE IF (FORMAT_OPTION .eq. 3) THEN
      WRITE (BDF, 103)                                   ! Propmat format (default)
      ! If more flags were added, add another ELSE IF statement with structure
      ! ELSE IF (FORMAT_OPTION .eq. `NEXT_NUMBER`) THEN
      ! WRITE (BDF, FORMAT_LABEL)
    END IF
    ! +++ End Change v3
    ! To Revert:
    ! Remove code above and
    ! Uncomment line below
    ! WRITE(BDF, 103)

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! No more reading the first line outside the loop
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    main: DO
      READ (ABQ, '(A)', IOSTAT=IOCHK) INPUT_LINE
      ! +++ CHANGE: Added EOF handling to distinguish between normal file end and read errors
      IF (IOCHK .ne. 0) THEN
        IF (IOCHK .lt. 0) THEN                      ! EOF is expected and not an error
          EXIT main                           ! Exit cleanly without incrementing IERR
        ELSE                                      ! Actual read error
          CALL READERR(IOCHK, ABQ_MODEL_FILE, ABQ, LAST_REC)
          IERR = IERR + 1
          EXIT main
        END IF
      END IF
      ! +++ END CHANGE

      ! +++ OLD CODE:
      ! IF (IOCHK /= 0) THEN
      !       CALL READERR ( IOCHK, ABQ_MODEL_FILE, ABQ, LAST_REC )
      !       IERR = IERR + 1
      !       EXIT main
      ! ENDIF
      ! +++ END OLD CODE

      LAST_REC = INPUT_LINE
      ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Transform the input line to upper case for case insensitive comparison
      CALL TO_UPPER(INPUT_LINE)
      ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ! Check if it is a '*NODE' category
      IF (INPUT_LINE .eq. '*NODE') THEN                ! Process NODE data to MYSTRAN GRID entries
        DO
          READ (ABQ, '(A)') INPUT_LINE
          IF (INPUT_LINE(1:1) .eq. '*') EXIT
          READ (INPUT_LINE, *) GN, GX, GY, GZ
          NGRID = NGRID + 1
          WRITE (BDF, 1001) GN, GX, GY, GZ
        END DO
        WRITE (BDF, 1000)
      END IF

      ! Check if it is a '*ELEMENT' category
      IF (INPUT_LINE(1:8) .eq. '*ELEMENT') THEN        ! Process elem data to MYSTRAN elem connection entries
        ! +++ CHANGE: Added EOF handling inside subroutine call
        CALL GET_ELEM_CONNECT_DATA(INPUT_LINE, BDF, ABQ, NELEM, EOF_FLAG)
        IF (EOF_FLAG) EXIT main ! Exit if EOF reached inside subroutine
      END IF
      ! We don't care about the rest of the lines in the file so we just don't do anything with them
      ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    END DO main

    WRITE (BDF, 9999)                                   ! Write ENDDATA to the bdf file
    CLOSE (BDF, STATUS='KEEP')

    WRITE (*, 9001) NGRID, NELEM, TRIM(ABQ_MODEL_FILE)
    WRITE (*, 9002) TRIM(ABQ_MODEL_FILE), TRIM(MYSTRAN_BDF_FILE)

    IF (IERR .gt. 0) STOP                                ! There was an error reading a record or we reached the end of the file

  ELSE                                                 ! IERR > 0 so  write messag and stop

    WRITE (*, 2) IERR
    STOP

  END IF

! ************************************************************************************

  ! 1 FORMAT('SOL 101', /, 'CEND', /, 'BEGIN BULK', /, '$')    'If a default SOL 101 is desired

  ! Option 1: No PSOLID OR MAT1 entries. Use flag "-basic".

101 FORMAT('BEGIN BULK', /, '$')  !No PSOLID or MAT1 entries

  ! Option 2: PSOLID only. Use flag "-prop".

102 FORMAT('$', /, 'BEGIN BULK', /, '$', /, &
         '$The property cards are for the 4-node tet (ID=3104) and 10-node test (ID=3110). ', /, &
         '$These were not part of the original INP file.', /, &
         '$--1---><--2---><--3---><--4---><--5---><--6---><--7---><--8---><--9---><--10-->', /, &
         'PSOLID      3104      99       0', /, &
         'PSOLID      3110      99       0', /, &
         '$', /, &
         '$')

  !   Option 3: PSOLID and MAT1. Use flag "-propmat" or if no flag is present.
  !NOTE: This is the default.
  !   The following code creates default PSOLID and MAT1 entries
103 FORMAT('$', /, 'BEGIN BULK', /, '$', /, &
         '$The following material is a dummy material and should be changed accordingly.', /, &
         '$--1---><--2---><--3---><--4---><--5---><--6---><--7---><--8---><--9---><--10-->', /, &
         'MAT1          99   1.0+7             0.3', /, &
         '$', /, &
         '$The property cards are for the 4-node tet (ID=3104) and 10-node test (ID=3110). ', /, &
         '$These were not part of the original INP file.', /, &
         '$--1---><--2---><--3---><--4---><--5---><--6---><--7---><--8---><--9---><--10-->', /, &
         'PSOLID      3104      99       0', /, &
         'PSOLID      3110      99       0', /, &
         '$', /, &
         '$')

  ! For more flags, just expand the formats:
  ! 10X FORMAT([CONTENTS])
  ! I recommend the format labels to be 101, 102, 103, 104, etc. and match the last 2 digits to the FORMAT_OPTION variable for consistency and readability:
  ! label 104 --> FORMAT_OPTION = 4
  ! label 105 --> FORMAT_OPTION = 5 ...
2 FORMAT(' Processing stopped due to above listed ', I4, ' error(s)')

1000 FORMAT('$')

1001 FORMAT('GRID, ', I8, ', ,', 3(1ES14.6, ','), ',')

9001 FORMAT(' This program processed ', I8, ' node entries and ', I8, ' element entries from "', A, '"')

9002 FORMAT(' Input ABAQUS and output MYSTRAN model file names are: "', A, '" and "', A, '"')

9999 FORMAT('$', /, 'ENDDATA')

! *******************************************************************************************

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CONTAINS

  ! +++ ADDED HELPER SUBROUTINE FOR CASE INSENSITIVITY
  SUBROUTINE TO_UPPER(STR)
    CHARACTER(LEN=*), INTENT(INOUT) :: STR
    INTEGER :: I
    DO I = 1, LEN(STR)
      IF (STR(I:I) .ge. 'a' .AND. STR(I:I) .le. 'z') THEN
        STR(I:I) = CHAR(ICHAR(STR(I:I)) - 32)
      END IF
    END DO
  END SUBROUTINE TO_UPPER
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END PROGRAM INP2DAT
