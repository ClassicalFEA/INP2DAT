! Copyright 2025 ClassicalFEA and Dr. Brian Esp
! Original program developed by Dr. Bill Case
! ##################################################################################################################################
! This subr is CALL'd by main program INP2DAT to write element connection entries for MYSTRAN based on ABAQUS inp file
! data on *ELEMENT entries

!                                         /---------------------------------------------\
!                                         |   ABAQUS elements coded into MYSTRAN input  |
!                                         |   ----------------------------------------  |
!                                         |                                             |
!                                         |     ABAQUS Element      MYSTRAN Element     |
!                                         |     --------------      ---------------     |
!                                         |       SPRINGA        ->  2 node CELAS1      |
!                                         |                                             |
!                                         |       T2D2           ->  2 node CROD        |
!                                         |                                             |
!                                         |       B31            ->  2 node CBEAM       |
!                                         |                                             |
!                                         |       S3                 3 node CTRIA3      |
!                                         |       S4             ->  4 node CQUAD4      |
!                                         |                                             |
!                                         |       C3D4, C3D104   ->  4, 10 node CTETRA  |
!                                         |       C3D6, C2D156   ->  6, 15 node CPENTA  |
!                                         |       C3D8, C3D20    ->  8, 20 node CHEXA   |
!                                         |                                             |
!                                         \---------------------------------------------/

! (1) On the MYSTRAN element connection entries written here, as a result of data not being available in the ABAQUS inp file,
!     (a) Elem prop ID's (PID's) had to be made up (see below for values chosen). Every elem of a particular type has the same PID
!     (b) Springs are output as MYSTRAN CELAS1 elements. The components (1-6) for G1 and G2 are left blank
!     (c) Beams are output as MYSTRAN CBEAM elements. The v vector info is left blank


      SUBROUTINE GET_ELEM_CONNECT_DATA ( CHAR_LINE, BDF, ABQ, NELEM )

      INTEGER, PARAMETER              :: NCHARS  =  7      ! the number of characters in array  ELTYPE
      INTEGER, PARAMETER              :: NEDATA  = 21      ! The dimensioned size of array EDATA

      CHARACTER(LEN=*), INTENT(INOUT) :: CHAR_LINE         ! Input character string foer *ELEMENT ABAQUS input file entry
      !++++
      ! This line appears to be unused in the code
      !character(len=17)               :: cedata1 
      !++++
      CHARACTER(LEN=LEN(CHAR_LINE))   :: STRING1, STRING2  ! Portion of CHAR_LINE 
      CHARACTER(LEN=NCHARS)           :: ELTYPE            ! ABAQUS element type (C3D8, etc)

      INTEGER, INTENT(IN)             :: ABQ               ! File number for writing the MYSTRAN bdf data
      INTEGER, INTENT(IN)             :: BDF               ! File number for MYSTRAN output bdf file
      INTEGER, INTENT(INOUT)          :: NELEM             ! Count of the NUMBER OF element entries read from ABQFFIL and converted
      INTEGER                         :: UNT               ! Unit to write outputs to (usually BDF but can be 6 for testing code)
      INTEGER                         :: EDATA(NEDATA)     ! Int data read from elem conn entrIES after *ELEMENT line in input file
      INTEGER                         :: I                 ! Loop index
      INTEGER                         :: I1, I2            ! Positions in CHAR_LINE as determined by INTRINSIC fcn INDEX
      INTEGER                         :: IERR              ! Error count
                                                           ! line in ABAQUS input file
      INTRINSIC INDEX

! **********************************************************************************************************************************
      IERR = 0
      UNT  = BDF                                           ! BDF is where the output goes. However, if we want to debug we can set
!                                                            UNT = 6 and output will go to stdout (the console).

      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Transform the input string to upper case for case insensitive comparison
      CALL TO_UPPER(CHAR_LINE)
      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Some file end with 'END STEP' and some with '*END STEP'. This way we catch both cases
eltyp:DO WHILE ((CHAR_LINE(1:9) /= '*END STEP') .OR. (CHAR_LINE(1:8) /= 'END STEP') )              ! If we get this it is the end of the ABAQUS inp file

         IF (CHAR_LINE(1:8) == '*ELEMENT') THEN
            I1 = INDEX(CHAR_LINE, '=')                     ! ERROR: must be "=" sign follwing keyword 
            IF (I1 == 0) THEN
               WRITE(*,'(A)') '*FATAL_ERR: NO EQUAL SIGN ON THE FOLLOWIG *ELEMENT ENTRY SO CAN NOT FIND ELEMENT TYPE:'
               WRITE(*,'(A)') TRIM(CHAR_LINE)
               IERR = IERR + 1 
            ELSE                                           ! No error on"=" sign so get string containing elem connection info
               ! ++++
               ! Fixed the starting index to be I1 + 1 to skip the "=" sign
               ! ++++
               I2 = INDEX(CHAR_LINE(I1+1:LEN(CHAR_LINE)), ',')
               IF (I2 == 0) THEN                           ! There are no other parameters on the *ELEMENT entry
                  STRING1 = CHAR_LINE(I1+1:LEN(CHAR_LINE))
               ELSE
                  ! ++++
                  ! Use the correct indices
                  ! I1 --> I1 + 1 to skip the "=" sign
                  ! I2 --> I1 + I2 - 1 to get the string up to the next coma
                  ! ++++
                  STRING1 = CHAR_LINE(I1+1:I1+I2-1)
               ENDIF
               ! ++++ Remove white spaces from the string
               STRING2 = TRIM(STRING1)
               ELTYPE = ADJUSTL(STRING2)
            ENDIF
      
         ELSE IF (CHAR_LINE(1:1) == '*') THEN              ! Not *ELEMENT. If we find * it means we are through w/ elem data

            EXIT eltyp

         ENDIF

         IF (IERR == 0) THEN                               ! No errors so we are good to go searching for element data

            DO I=1,NEDATA                                  ! Initialize EDATA. It is the array which will be read for every elem
               EDATA(I) = 0
            ENDDO

            READ(ABQ,'(A)') CHAR_LINE                      ! Begin searching for the element type to proccess
            IF (CHAR_LINE(1:1) == '*') CYCLE eltyp

            IF (ELTYPE(1:NCHARS) == 'SPRINGA') THEN        ! 1D  2 node ELAS1 elem. Use 1102 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,3)
               WRITE(UNT,1102) (EDATA(I), I=1,3)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'T2D2   ') THEN   ! 1D  2 node ROD   elem. Use 1202 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,3)
               WRITE(UNT,1202) (EDATA(I), I=1,3)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'B31    ' ) THEN  ! 1D  2 node BEAM  elem. Use 1302 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,3)
               WRITE(UNT,1302) (EDATA(I), I=1,3)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'S3     ') THEN   ! 2D  3 node TRIA3 elem. Use 2103 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,4)
               WRITE(UNT,2103) (EDATA(I), I=1,4)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'S4     ') THEN   ! 2D  4 node QUAD4 elem. Use 2204 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,5)
               WRITE(UNT,2204) (EDATA(I), I=1,5)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'C3D4   ') THEN   ! 3D 4  node TETRA elem. Use 3104 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,5)
               WRITE(UNT,3104) (EDATA(I), I=1,5)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'C3D6   ') THEN   ! 3D  6 node PENTA elem. Use 3206 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,7)
               WRITE(UNT,3206) (EDATA(I), I=1,7)
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'C3D8   ') THEN   ! 3D  8 node HEXA  elem. Use 3308 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,9)
               CALL WRT_EDATA_WITH_CONT ( 'CHEXA   ',  9 )
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'C3D10  ') THEN   ! 3D 10 node TETRA elem. Use 3110 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,11)
               CALL WRT_EDATA_WITH_CONT ( 'CTETRA  ', 11 )
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'C3D15  ') THEN   ! 3D 15 node PENTA elem. Use 3215 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,16)
               CALL WRT_EDATA_WITH_CONT ( 'CPENTA  ', 16 )
               NELEM = NELEM + 1

            ELSE IF (ELTYPE(1:NCHARS) == 'C3D20  ') THEN   ! 3D 20 node HEXA  elem. Use 3320 as prop ID 
               READ(CHAR_LINE,*) (EDATA(I), I=1,21)
               CALL WRT_EDATA_WITH_CONT ( 'CHEXA   ', 21 )
               NELEM = NELEM + 1

            ELSE                                           ! Elem type not recognized so write msg and cycle to find another elem

               WRITE(*, 1) ELTYPE
               CYCLE eltyp   

            ENDIF

         ELSE                                              ! IERR > 0 so there were errors and we return to calling program

            RETURN

         ENDIF


      ENDDO eltyp

! **********************************************************************************************************************************
    1 FORMAT(' ABAQUS ELEMENT TYPE "', A7, '" IS NOT RECOGNIZED AND IS IGNORED')

 1102 FORMAT('CELAS1  ', I8, '    1102',  I8, '        ', I8 )

 1202 FORMAT('CROD    ', I8, '    1202',  2I8)

 1302 FORMAT('CBEAM   ', I8, '    1302',  2I8)

 2103 FORMAT('CTRIA3  ', I8, '    2103',  3I8)

 2204 FORMAT('CQUAD4  ', I8, '    2204',  4I8)

 3104 FORMAT('CTETRA  ', I8, '    3104',  4I8)

 3206 FORMAT('CPENTA  ', I8, '    3206',  6I8)


! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE WRT_EDATA_WITH_CONT ( ELEM_CNAME, N )

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: ELEM_CNAME        ! Element connection entry name (CHEXA, etc)

      INTEGER, INTENT(IN)             :: N                 ! Number of fields of element connection data to write to the bdf file

! **********************************************************************************************************************************

      IF (N >= 9) THEN

         IF (N == 9) THEN

            WRITE(UNT,9901) ELEM_CNAME, EDATA(1), '3308', (EDATA(I), I=2,7), '+'
            WRITE(UNT,9902) '+', (EDATA(I), I=8,9)

         ELSE IF (N == 11) THEN

            WRITE(UNT,9901) ELEM_CNAME, EDATA(1), '3110', (EDATA(I), I=2,7), '+'
            WRITE(UNT,9903) '+', (EDATA(I), I=8,11)


         ELSE IF (N == 16) THEN

            WRITE(UNT,9901) ELEM_CNAME, EDATA(1), '3215', (EDATA(I), I=2,7), '+'
            WRITE(UNT,9904) '+', (EDATA(I), I=8,15), ' +'

            WRITE(UNT,9902) '+', EDATA(16)

         ELSE IF (N == 21) THEN

            WRITE(UNT,9901) ELEM_CNAME, EDATA(1), '3320', (EDATA(I), I=2,7), '+'
            WRITE(UNT,9905) '+ ', (EDATA(I), I=8,15), ' +'

            WRITE(UNT,9906) '+ ', (EDATA(I1), I=16,21)

         ENDIF

      ELSE

         WRITE(*,9991) N, ELEM_CNAME

      ENDIF     

      RETURN         

! **********************************************************************************************************************************
 9901 FORMAT(A, I8, A8, 6I8, A1)

 9902 FORMAT(A1, 7X, 2I8)

 9903 FORMAT(A1, 7X, 4I8)

 9904 FORMAT(A1, 7X, 8I8, A1)

 9905 FORMAT(A1, 7X, 8I8, A1)

 9906 FORMAT(A1, 7X, 6I8)

 9991 FORMAT(' Subr WRT_EDATA_WITH_CONT is only used for MYSTRAN elem. conn. info that need cont. entryies (i.e. ones with', /,&
             ' more than 8 data fields). This subr did not proccess the ', A, ' connection entry passed to it')

      END SUBROUTINE WRT_EDATA_WITH_CONT

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

! **********************************************************************************************************************************

      END SUBROUTINE GET_ELEM_CONNECT_DATA
