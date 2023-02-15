      PROGRAM DistAutoCorrelation
!     Declare variables

      implicit none

!     Declare variables
!     PARAMETER (N=2666)
!     PARAMETER (M=2666)
!     PARAMETER (L=1000)
      REAL, DIMENSION(10000) :: W1, W2
      INTEGER, DIMENSION(10000) :: P1, P2
      REAL, DIMENSION(2000) :: OCCURENCE
      INTEGER :: I, J, IMAX, N, M, L, DIST, STATUS, NC, MC
      CHARACTER(len=50) :: FILENAME1, FILENAME2, FILENAME3
      INTEGER :: FILE_UNIT1, FILE_UNIT2, FILE_UNIT3, ierr

      FILE_UNIT1=10
      FILE_UNIT2=20
      FILE_UNIT3=30

      N=10000
      M=10000
      L=1200

!     set NC and MC counters for counting lines in file1 and file2
      NC=0
      MC=0

!     set IMAX = L; L is the number of lines for output
      IMAX=L

!     Prompt the user to enter the input filename1
      WRITE(*,*) 'Enter the filename1: '
      READ(*,*) FILENAME1

!     Prompt the user to enter the input filename2
      WRITE(*,*) 'Enter the filename2: '
      READ(*,*) FILENAME2

!     Prompt the user to enter the output filename
      WRITE(*,*) 'Enter the output filename: '
      READ(*,*) FILENAME3

!CCCCCCCCCCCCCCCCCCCCCCCCCCC 
!     Open the file1
      OPEN(FILE_UNIT1, FILE=FILENAME1, STATUS='OLD', IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
        WRITE(*,*) 'Error: could not open file'
        STOP
      END IF

!     Skip header line
      READ(FILE_UNIT1,*)
!     Read the data from the file into the array
      DO I=1,N
!     set NC to count lines read
        NC=NC+1
!       READ(FILE_UNIT1,*,IOSTAT=STATUS) X(I,1), X(I,2)
        READ(FILE_UNIT1,*,IOSTAT=STATUS) P1(I), W1(I)
        IF (STATUS.NE.0) EXIT
      END DO
!     Close the file
      CLOSE(FILE_UNIT1)
!CCCCCCCCCCCCCCCCCCCCCCCCCCC 
!     Open the file2
      OPEN(FILE_UNIT2, FILE=FILENAME2, STATUS='OLD', IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
        WRITE(*,*) 'Error: could not open file'
        STOP
      END IF

!     Skip header line
      READ(FILE_UNIT2,*)
!     Read the data from the file into the array
      DO I=1,M
!     set MC to count lines read
        MC=MC+1
!       READ(FILE_UNIT2,*,IOSTAT=STATUS) Y(I,1), Y(I,2)
        READ(FILE_UNIT2,*,IOSTAT=STATUS) P2(I), W2(I)
        IF (STATUS.NE.0) EXIT
      END DO
!     Close the file
      CLOSE(FILE_UNIT2)
!CCCCCCCCCCCCCCCCCCCCCCCCCCC 

!     Print the contents of the array
      WRITE(*,*) 'The contents of the array are:'
      DO I=1,MC-1
!       WRITE(*,*) X(I,1), X(I,2), Y(I,1), Y(I,2)
        WRITE(*,*) P1(I), W1(I), P2(I), W2(I)
      END DO

      WRITE(*,*) NC-1, MC-1

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C   Calculate Distance and Occurence              CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! Initialize Occurence = 0
      DO I=1,IMAX

         OCCURENCE(I)=0.0

      END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C         Calculate Distance                      CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO I=1,NC-1
         DO J=1,MC-1
         
!           DIST = Y(J,1) - X(I,1) + 1
            DIST = P2(J) - P1(I) + 1

!           WRITE(*,*) DIST
      
            IF ((DIST .GE. 0) .AND. (DIST .LE. IMAX)) THEN
!                OCCURENCE(DIST) = OCCURENCE(DIST) + X(I,2) * Y(J,2)
                 OCCURENCE(DIST) = OCCURENCE(DIST) + W1(I) * W2(J)
            END IF

         END DO
      END DO  

!C   
!C Write output array
!C
      OPEN(FILE_UNIT3, FILE=FILENAME3, status='replace', action='write')
      WRITE(FILE_UNIT3,*) "Dist      DAC"
      DO I=1,IMAX
        WRITE(FILE_UNIT3,'(I5, F9.3)') I, OCCURENCE(I)
!       WRITE(FILE_UNIT3,*) I, OCCURENCE(I)
      END DO

      END PROGRAM DistAutoCorrelation
