      MODULE SORT_I
!
!     This module provides the merge, quick, and insertion sort
!     routines.
!
!-----------------------------------------------------------------------
!     USE the module wherever you would like to use a sorting
!     procedure.
!-----------------------------------------------------------------------
!
      USE DATA_OBJ_M
      USE KINDS_M
!       USE OMP_M
      IMPLICIT NONE
!
      INTERFACE ABS_SORT
        MODULE PROCEDURE ABS_SORT_FLT,                                  &
     &                   ABS_SORT_INT
      END INTERFACE ABS_SORT
!
      INTERFACE ISORT
        MODULE PROCEDURE DATA_OBJ_ISORT,                                &
     &                   FLT_ISORT,                                     &
     &                   INT_ISORT
      END INTERFACE ISORT
!
      INTERFACE MSORT
        MODULE PROCEDURE DATA_OBJ_MSORT,                                &
     &                   FLT_MSORT,                                     &
     &                   INT_MSORT
      END INTERFACE MSORT
!
      INTERFACE QSORT
        MODULE PROCEDURE DATA_OBJ_QSORT,                                &
     &                   FLT_QSORT,                                     &
     &                   INT_QSORT
      END INTERFACE QSORT
!
      PRIVATE :: ABS_SORT_FLT,                                          &
     &           ABS_SORT_INT,                                          &
     &           DATA_OBJ_ISORT,                                        &
     &           DATA_OBJ_MERGESORT,                                    &
     &           DATA_OBJ_MERGE,                                        &
     &           DATA_OBJ_MSORT,                                        &
     &           DATA_OBJ_QPART,                                        &
     &           DATA_OBJ_QSORT,                                        &
     &           DATA_OBJ_QUICKSORT,                                    &
     &           FLT_ISORT,                                             &
     &           FLT_MERGESORT,                                         &
     &           FLT_MERGE,                                             &
     &           FLT_MSORT,                                             &
     &           FLT_QPART,                                             &
     &           FLT_QSORT,                                             &
     &           FLT_QUICKSORT,                                         &
     &           INT_ISORT,                                             &
     &           INT_MERGESORT,                                         &
     &           INT_MERGE,                                             &
     &           INT_MSORT,                                             &
     &           INT_QPART,                                             &
     &           INT_QSORT,                                             &
     &           INT_QUICKSORT
      PUBLIC :: ABS_SORT,                                               &
     &          ISORT,                                                  &
     &          MSORT,                                                  &
     &          QSORT
!
!=======================================================================
      CONTAINS
!=======================================================================
!
      SUBROUTINE ABS_SORT_FLT (LIST)
!
!       Takes the absolute value of LIST and sorts the result, with the
!       assumption that the input LIST was sorted before the absolute
!       value was taken.  Duplicates produced by taking the absolute
!       value are removed.
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:) Sorted signed 1D array to have the absolute
!                            value taken and be resorted.
!
!       output: LIST REAL(:) Sorted unsigned 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, ALLOCATABLE, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I, J, K
        REAL, ALLOCATABLE :: ABS_LIST(:)
!
!       End of declarations.
!
!=======================================================================
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IF (LIST(1) .GT. 0.0) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          RETURN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ELSE IF (LIST(SIZE(LIST)) .LT. 0.0) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          LIST = ABS(LIST(SIZE(LIST):1:-1))
!
          RETURN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!-----------------------------------------------------------------------
!
        ALLOCATE (ABS_LIST(SIZE(LIST)))
!
        I = 1
        J = SIZE(LIST)
        K = SIZE(LIST) + 1
!
        DO WHILE ((LIST(I) .LT. 0.0) .AND. (LIST(J) .GT. 0.0))
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF (-LIST(I) .GT. LIST(J)) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            K = K - 1
!
            ABS_LIST(K) = -LIST(I)
!
            I = I + 1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF (LIST(J) .GT. -LIST(I)) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            K = K - 1
!
            ABS_LIST(K) = LIST(J)
!
            J = J - 1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            I = I + 1
!
            J = J - 1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        END DO
!
        DO WHILE (LIST(I) .LT. 0)
          K = K - 1
!
          ABS_LIST(K) = -LIST(I)
!
          I = I + 1
        END DO
!
        DO WHILE (LIST(J) .GT. 0)
          K = K - 1
!
          ABS_LIST(K) = LIST(J)
!
          J = J - 1
        END DO
!
        LIST = ABS_LIST(K:)
!
        DEALLOCATE (ABS_LIST)
!
        RETURN
      END SUBROUTINE ABS_SORT_FLT
!
!=======================================================================
!
      SUBROUTINE ABS_SORT_INT (LIST)
!
!       Finds takes the absolute value of LIST and sorts the result,
!       with the assumption that the input LIST was already sorted
!       before the absolute value was taken.  Duplicates (after the
!       absolute value is taken) are removed.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) Sorted signed 1D array to have the
!                               absolute value taken and be resorted.
!
!       output: LIST INTEGER(:) Sorted unsigned 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, ALLOCATABLE, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I, J, K
        INTEGER, ALLOCATABLE :: ABS_LIST(:)
!
!       End of declarations.
!
!=======================================================================
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IF (LIST(1) .GT. 0) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          RETURN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ELSE IF (LIST(SIZE(LIST)) .LT. 0) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          LIST = ABS(LIST(SIZE(LIST):1:-1))
!
          RETURN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!-----------------------------------------------------------------------
!
        ALLOCATE (ABS_LIST(SIZE(LIST)))
!
        I = 1
        J = SIZE(LIST)
        K = SIZE(LIST) + 1
!
        DO WHILE ((LIST(I) .LT. 0) .AND. (LIST(J) .GT. 0))
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF (-LIST(I) .GT. LIST(J)) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            K = K - 1
!
            ABS_LIST(K) = -LIST(I)
!
            I = I + 1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE IF (LIST(J) .GT. -LIST(I)) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            K = K - 1
!
            ABS_LIST(K) = LIST(J)
!
            J = J - 1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            I = I + 1
!
            J = J - 1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        END DO
!
        DO WHILE (LIST(I) .LT. 0)
          K = K - 1
!
          ABS_LIST(K) = -LIST(I)
!
          I = I + 1
        END DO
!
        DO WHILE (LIST(J) .GT. 0)
          K = K - 1
!
          ABS_LIST(K) = LIST(J)
!
          J = J - 1
        END DO
!
        LIST = ABS_LIST(K:)
!
        DEALLOCATE (ABS_LIST)
!
        RETURN
      END SUBROUTINE ABS_SORT_INT
!
!=======================================================================
!
      SUBROUTINE DATA_OBJ_ISORT (LIST, LBND, UBND)
!
!       This routine performs an insertion sort (ascending) on LIST.
!
!       [unhack] - DATA_OBJ_ISORT
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) 1D array to be sorted.
!               LBND INTEGER     Lower bound of LIST to be considered.
!               UBND INTEGER     Upper bound of LIST to be considered.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (IN) :: LBND, UBND
!
!       Declare local variables.
!
        INTEGER :: I, J
        CLASS (DATA_OBJ), ALLOCATABLE :: SWP
!
!       End of declarations.
!
!=======================================================================
!
        ALLOCATE (SWP, MOLD = LIST(LBND))
!
        DO I = (LBND + 1), UBND
          SWP = LIST(I)
!
!         Find the first value smaller than the Ith value of the array
!         and swap their positions.
!
          DO J = (I - 1), LBND, -1
            IF (LIST(J) .LT. SWP) EXIT
            LIST(J+1) = LIST(J)
          END DO
          LIST(J+1) = SWP
        END DO
!
        RETURN
      END SUBROUTINE DATA_OBJ_ISORT
!
!=======================================================================
!
      SUBROUTINE DATA_OBJ_MERGE (LIST, MLST, LBND, UBND, PRT)
!
!       This routine performs a merge of two sorted (ascending) sections
!       of LIST.
!
!       [unhack] - DATA_OBJ_MERGE
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) 1D array to be sorted.
!               LBND INTEGER     Lower bound of LIST to be considered.
!               UBND INTEGER     Upper bound of LIST to be considered.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:), MLST(:)
        INTEGER, INTENT (IN) :: LBND, UBND, PRT
!
!       Declare local variables.
!
        INTEGER ::  I, J, K, N
!
!       End of declarations.
!
!=======================================================================
!
        N = UBND
!
        I = LBND
        J = PRT + 1
        K = LBND
        DO WHILE ((PRT .GE. I) .AND. (N .GE. J))
          IF (MLST(I) .LT. MLST(J)) THEN
            LIST(K) = MLST(I)
            I = I + 1
          ELSE
            LIST(K) = MLST(J)
            J = J + 1
          END IF
          K = K + 1
        END DO
!
!-----------------------------------------------------------------------
        IF (PRT .GE. I) THEN
!-----------------------------------------------------------------------
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     PARALLEL DO IF ((PRT - I) .GT. MIN_OMP_LOOP_LEN)              &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (J)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          DO J = 0, (PRT - I)
            LIST(K+J) = MLST(I+J)
          END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!-----------------------------------------------------------------------
        ELSE IF (N .GE. J) THEN
!-----------------------------------------------------------------------
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     PARALLEL DO IF ((N - J) .GT. MIN_OMP_LOOP_LEN)                &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (I)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          DO I = 0, (N - J)
            LIST(K+I) = MLST(J+I)
          END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!-----------------------------------------------------------------------
        END IF
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE DATA_OBJ_MERGE
!
!=======================================================================
!
      RECURSIVE SUBROUTINE DATA_OBJ_MERGESORT (LIST, MLST, LBND, UBND)
!
!       This routine performs a merge sort (ascending) on LIST.
!
!       [unhack] - DATA_OBJ_MERGESORT
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) 1D array to be sorted.
!               LBND INTEGER     Lower bound of LIST to be considered.
!               UBND INTEGER     Upper bound of LIST to be considered.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:), MLST(:)
        INTEGER, INTENT (IN) :: LBND, UBND
!
!       Declare local variables.
!
        INTEGER :: N, PRT
!
!       End of declarations.
!
!=======================================================================
!
!       If the list isn't small then continue to perform merge sorts,
!       but if it is small then perform an insertion sort.
!
        N = UBND - LBND + 1
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (7:)
!-----------------------------------------------------------------------
          PRT = LBND + N / 2
!
          CALL DATA_OBJ_MERGESORT (MLST, LIST, LBND, PRT)
          CALL DATA_OBJ_MERGESORT (MLST, LIST, (PRT + 1), UBND)
!
          CALL DATA_OBJ_MERGE (LIST, MLST, LBND, UBND, PRT)
!-----------------------------------------------------------------------
        CASE (2:6)
!-----------------------------------------------------------------------
          CALL DATA_OBJ_ISORT (LIST, LBND, UBND)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE DATA_OBJ_MERGESORT
!
!=======================================================================
!
      SUBROUTINE DATA_OBJ_MSORT (LIST, LBND, UBND, PRTS)
!
!       This routine initializes additional memory, then starts merge
!       sorting (ascending) LIST.  If PRTS is provided, then merges will
!       be performed between the N + 1 presorted sections of LIST given
!       by:
!
!         LIST(:PRTS(1)), LIST(PRTS(1)+1:PRTS(2)), ... , LIST(PRTS(N):)
!
!       [unhack] - DATA_OBJ_MSORT
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) 1D array to be sorted.
!               LBND INTEGER     Lower bound of LIST to be considered.
!               UBND INTEGER     Upper bound of LIST to be considered.
!               PRTS INTEGER(:)  Partitions that specify the already
!                                sorted sections of LIST.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (IN) :: LBND, UBND
        INTEGER, CONTIGUOUS, OPTIONAL, INTENT (IN) :: PRTS(:)
!
!       Declare local variables.
!
        INTEGER :: I, N
        CLASS (DATA_OBJ), ALLOCATABLE :: MLST(:)
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
        LOGICAL :: PRESENT_SIZE_GE_1
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!       End of declarations.
!
!=======================================================================
!
        N = UBND - LBND + 1
!
!       Determine if LIST is large enough to warrant merge sorting.  If
!       so, initialize and begin merge sorting; if not, perform an
!       insertion sort.
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (7:)
!-----------------------------------------------------------------------
          ALLOCATE (MLST(SIZE(LIST)), MOLD = LIST)
!
! -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
!$OMP     PARALLEL DO IF (N .GT. MIN_OMP_LOOP_LEN)                      &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (I)
! -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
          DO I = LBND, UBND
            MLST(I) = LIST(I)
          END DO
! -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
!$OMP     END PARALLEL DO
! -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
!
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!         [unhack] - DATA_OBJ_MSORT
!
!         [unhack] - The PRESENT_SIZE_GE function does not seem to
!         [unhack] - properly respect the PRESENT property of PRTS as of
!         [unhack] - gcc 4.9.0 20131222.  The function is recreated
!         [unhack] - inline here instead.
!
          IF (PRESENT(PRTS)) THEN
            PRESENT_SIZE_GE_1 = SIZE(PRTS) .GE. 1
          ELSE
            PRESENT_SIZE_GE_1 = .FALSE.
          END IF
!
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF (PRESENT_SIZE_GE_1) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!           Perform merges of sorted sub-lists with the given
!           partitions.
!
            I = 2
!
            IF (MOD(SIZE(PRTS), 2) .EQ. 0) THEN
              CALL DATA_OBJ_MERGE (MLST, LIST, LBND, PRTS(I), PRTS(I-1))
!
              I = I + 1
            END IF
!
            DO WHILE (I .LE. SIZE(PRTS))
              CALL DATA_OBJ_MERGE (LIST, MLST, LBND, PRTS(I), PRTS(I-1))
!
              I = I + 1
!
              CALL DATA_OBJ_MERGE (MLST, LIST, LBND, PRTS(I), PRTS(I-1))
!
              I = I + 1
            END DO
!
            CALL DATA_OBJ_MERGE (LIST, MLST, LBND, UBND, PRTS(I-1))
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            CALL DATA_OBJ_MERGESORT (LIST, MLST, LBND, UBND)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          DEALLOCATE (MLST)
!-----------------------------------------------------------------------
        CASE (2:6)
!-----------------------------------------------------------------------
          CALL DATA_OBJ_ISORT (LIST, LBND, UBND)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE DATA_OBJ_MSORT
!
!=======================================================================
!
      FUNCTION DATA_OBJ_QPART(LIST, LBND, UBND) RESULT (PRT_INDX)
!
!       This routine performs a partition on LIST for the quick sort
!       algorithm and returns the partition index.
!
!       [unhack] - DATA_OBJ_QPART
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST      DATA_OBJ(:) 1D array to be sorted.
!               LBND      INTEGER     Lower bound of LIST to be
!                                     considered.
!               UBND      INTEGER     Upper bound of LIST to be
!                                     considered.
!
!       output: LIST      DATA_OBJ(:) Partitioned 1D array.
!               PRT_INDX  INTEGER     Index along which LIST is split
!                                     into two partitions.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (IN) :: LBND, UBND
        INTEGER :: PRT_INDX
!
!       Declare local variables.
!
        INTEGER :: I, J, N
        REAL :: RAND
        CLASS (DATA_OBJ), ALLOCATABLE :: PRT, SWP
!
!       End of declarations.
!
!=======================================================================
!
        ALLOCATE (PRT, MOLD = LIST(1))
        ALLOCATE (SWP, MOLD = LIST(1))
!
        N = (UBND - LBND + 1)
!
!       Choose a value inside the list randomly.
!
        CALL RANDOM_NUMBER (RAND)
        J = LBND + INT(RAND * N) + 1
        PRT = LIST(J)
!
        LIST(J) = LIST(UBND)
!
!       Split the array in two sections (one with values less than the
!       randomly chosen value and the other with values greater than).
!
        J = 1
        DO I = LBND, (UBND - 1)
          IF (LIST(I) .LT. PRT) THEN
            SWP = LIST(J)
            LIST(J) = LIST(I)
            LIST(I) = SWP
            J = J + 1
          END IF
        END DO
!
        LIST(N) = LIST(J)
        LIST(J) = PRT
!
        PRT_INDX = J
!
        RETURN
      END FUNCTION DATA_OBJ_QPART
!
!=======================================================================
!
      SUBROUTINE DATA_OBJ_QSORT (LIST, LBND, UBND)
!
!       This routine initializes the random number generator, then starts
!       quick sorting (ascending) LIST.
!
!       [unhack] - DATA_OBJ_QSORT
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) 1D array to be sorted.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (IN) :: LBND, UBND
!
!       Declare local variables.
!
        INTEGER :: N
        INTEGER, ALLOCATABLE :: SEED(:)
        INTEGER :: I
!
!       End of declarations.
!
!=======================================================================
!
!       Determine if LIST is large enough to warrant quick sorting.  If
!       so, initialize and begin quick sorting; if not, perform an
!       insertion sort.
!
        N = (UBND - LBND + 1)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (8:)
!-----------------------------------------------------------------------
!
!         This call assigns N to be the minimum size of the array used
!         with for the PUT/GET arguments to RANDOM_SEED, it does not use
!         N as input.
!
          CALL RANDOM_SEED (SIZE = N)
!
          ALLOCATE (SEED(N))
!
          DO I = 1, N
            CALL SYSTEM_CLOCK (COUNT = SEED(I))
          END DO
!
          CALL RANDOM_SEED (PUT = SEED)
!
          CALL DATA_OBJ_QUICKSORT (LIST, LBND, UBND)
!
          DEALLOCATE (SEED)
!-----------------------------------------------------------------------
        CASE (2:7)
!-----------------------------------------------------------------------
          CALL DATA_OBJ_ISORT (LIST, LBND, UBND)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE DATA_OBJ_QSORT
!
!=======================================================================
!
      RECURSIVE SUBROUTINE DATA_OBJ_QUICKSORT (LIST, LBND, UBND)
!
!       This routine performs a quick sort (ascending) on LIST.
!
!       [unhack] - DATA_OBJ_QUICKSORT
!
!       [unhack] -                 !!Warning!!
!       [unhack] - Do not input array slices to this procedure!
!
!       [unhack] - Remove the bounds inputs (use array slices instead)
!       [unhack] - once GCC bug 56691 is fixed.
!
!       [unhack] - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56691
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) 1D array to be sorted.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (IN) :: LBND, UBND
!
!       Declare local variables.
!
        INTEGER :: N, PRT
!
!       End of declarations.
!
!=======================================================================
!
!       If the list isn't small then continue to perform quick sorts,
!       but if it is small then perform an insertion sort.
!
        N = (UBND - LBND + 1)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (8:)
!-----------------------------------------------------------------------
          PRT = DATA_OBJ_QPART(LIST, LBND, UBND)
          IF (PRT .NE. LBND) THEN
            CALL DATA_OBJ_QUICKSORT (LIST, LBND, (PRT - 1))
          END IF
          IF (PRT .NE. UBND) THEN
            CALL DATA_OBJ_QUICKSORT (LIST, (PRT + 1), UBND)
          END IF
!-----------------------------------------------------------------------
        CASE (2:7)
!-----------------------------------------------------------------------
          CALL DATA_OBJ_ISORT (LIST, LBND, UBND)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE DATA_OBJ_QUICKSORT
!
!=======================================================================
!
      SUBROUTINE FLT_ISORT (LIST)
!
!       This routine performs an insertion sort (ascending) on LIST.
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:) 1D array to be sorted.
!
!       output: LIST REAL(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I, J
        REAL :: SWP
!
!       End of declarations.
!
!=======================================================================
!
        DO I = 2, SIZE(LIST)
          SWP = LIST(I)
!
!         Find the first value smaller than the Ith value of the array
!         and swap their positions.
!
          DO J = (I - 1), 1, -1
            IF (LIST(J) .LT. SWP) EXIT
            LIST(J+1) = LIST(J)
          END DO
          LIST(J+1) = SWP
        END DO
!
        RETURN
      END SUBROUTINE FLT_ISORT
!
!=======================================================================
!
      SUBROUTINE FLT_MERGE (LIST, MLST, PRT)
!
!       This routine performs a merge of two sorted (ascending) sections
!       of LIST.
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:) 1D array to be sorted.
!
!       output: LIST REAL(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:), MLST(:)
        INTEGER, INTENT (IN) :: PRT
!
!       Declare local variables.
!
        INTEGER ::  I, J, K, N
!
!       End of declarations.
!
!=======================================================================
!
        N = SIZE(LIST)
!
        I = 1
        J = PRT + 1
        K = 1
        DO WHILE ((PRT .GE. I) .AND. (N .GE. J))
          IF (MLST(I) .LT. MLST(J)) THEN
            LIST(K) = MLST(I)
            I = I + 1
          ELSE
            LIST(K) = MLST(J)
            J = J + 1
          END IF
          K = K + 1
        END DO
!
!-----------------------------------------------------------------------
        IF (PRT .GE. I) THEN
!-----------------------------------------------------------------------
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     PARALLEL DO !IF ((PRT - I) .GT. MIN_OMP_LOOP_LEN)             &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (J)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          DO J = 0, (PRT - I)
            LIST(K+J) = MLST(I+J)
          END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!-----------------------------------------------------------------------
        ELSE IF (N .GE. J) THEN
!-----------------------------------------------------------------------
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     PARALLEL DO !IF ((N - J) .GT. MIN_OMP_LOOP_LEN)               &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (I)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          DO I = 0, (N - J)
            LIST(K+I) = MLST(J+I)
          END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!-----------------------------------------------------------------------
        END IF
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE FLT_MERGE
!
!=======================================================================
!
      RECURSIVE SUBROUTINE FLT_MERGESORT (LIST, MLST)
!
!       This routine performs a merge sort (ascending) on LIST.
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:) 1D array to be sorted.
!
!       output: LIST REAL(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:), MLST(:)
!
!       Declare local variables.
!
        INTEGER :: N, PRT
!
!       End of declarations.
!
!=======================================================================
!
!       If the list isn't small then continue to perform merge sorts,
!       but if it is small then perform an insertion sort.
!
        N = SIZE(LIST)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (7:)
!-----------------------------------------------------------------------
          PRT = N / 2
!
          CALL FLT_MERGESORT (MLST(:PRT), LIST(:PRT))
          CALL FLT_MERGESORT (MLST(PRT+1:), LIST(PRT+1:))
!
          CALL FLT_MERGE (LIST, MLST, PRT)
!-----------------------------------------------------------------------
        CASE (2:6)
!-----------------------------------------------------------------------
          CALL FLT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE FLT_MERGESORT
!
!=======================================================================
!
      SUBROUTINE FLT_MSORT (LIST, PRTS)
!
!       This routine initializes additional memory, then starts merge
!       sorting (ascending) LIST.  If PRTS is provided, then merges will
!       be performed between the N + 1 presorted sections of LIST given
!       by:
!
!         LIST(:PRTS(1)), LIST(PRTS(1)+1:PRTS(2)), ... , LIST(PRTS(N):)
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:)    1D array to be sorted.
!               PRTS INTEGER(:) Partitions that specify the already
!                               sorted sections of LIST.
!
!       output: LIST REAL(:)    Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, CONTIGUOUS, OPTIONAL, INTENT (IN) :: PRTS(:)
!
!       Declare local variables.
!
        INTEGER :: I, N
        REAL, ALLOCATABLE :: MLST(:)
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
        LOGICAL :: PRESENT_SIZE_GE_1
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!       End of declarations.
!
!=======================================================================
!
        N = SIZE(LIST)
!
!       Determine if LIST is large enough to warrant merge sorting.  If
!       so, initialize and begin merge sorting; if not, perform an
!       insertion sort.
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (7:)
!-----------------------------------------------------------------------
          ALLOCATE (MLST(SIZE(LIST)), SOURCE = LIST)
!
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!         [unhack] - FLT_MSORT
!
!         [unhack] - The PRESENT_SIZE_GE function does not seem to
!         [unhack] - properly respect the PRESENT property of PRTS as of
!         [unhack] - gcc 4.9.0 20131222.  The function is recreated
!         [unhack] - inline here instead.
!
          IF (PRESENT(PRTS)) THEN
            PRESENT_SIZE_GE_1 = SIZE(PRTS) .GE. 1
          ELSE
            PRESENT_SIZE_GE_1 = .FALSE.
          END IF
!
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF (PRESENT_SIZE_GE_1) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!           Perform merges of sorted sub-lists with the given
!           partitions.
!
            I = 2
!
            IF (MOD(SIZE(PRTS), 2) .EQ. 0) THEN
              CALL FLT_MERGE (MLST(:PRTS(I)), LIST(:PRTS(I)), PRTS(I-1))
!
              I = I + 1
            END IF
!
            DO WHILE (I .LE. SIZE(PRTS))
              CALL FLT_MERGE (LIST(:PRTS(I)), MLST(:PRTS(I)), PRTS(I-1))
!
              I = I + 1
!
              CALL FLT_MERGE (MLST(:PRTS(I)), LIST(:PRTS(I)), PRTS(I-1))
!
              I = I + 1
            END DO
!
            CALL FLT_MERGE (LIST, MLST, PRTS(I-1))
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            CALL FLT_MERGESORT (LIST, MLST)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          DEALLOCATE (MLST)
!-----------------------------------------------------------------------
        CASE (2:6)
!-----------------------------------------------------------------------
          CALL FLT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE FLT_MSORT
!
!=======================================================================
!
      FUNCTION FLT_QPART(LIST) RESULT (PRT_INDX)
!
!       This routine performs a partition on LIST for the quick sort
!       algorithm and returns the partition index.
!
!-----------------------------------------------------------------------
!        input: LIST      REAL(:) 1D array to be sorted.
!
!       output: LIST      REAL(:) Partitioned 1D array.
!               PRT_INDX  INTEGER Index along which LIST is split into
!                                 two partitions.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER :: PRT_INDX
!
!       Declare local variables.
!
        INTEGER :: I, J, N
        REAL :: RAND
        REAL :: PRT, SWP
!
!       End of declarations.
!
!=======================================================================
!
        N = SIZE(LIST)
!
!       Choose a value inside the list randomly.
!
        CALL RANDOM_NUMBER (RAND)
        J = INT(RAND * N) + 1
        PRT = LIST(J)
!
        LIST(J) = LIST(N)
!
!       Split the array in two sections (one with values less than the
!       randomly chosen value and the other with values greater than).
!
        J = 1
        DO I = 1, (N - 1)
          IF (LIST(I) .LT. PRT) THEN
            SWP = LIST(J)
            LIST(J) = LIST(I)
            LIST(I) = SWP
            J = J + 1
          END IF
        END DO
!
        LIST(N) = LIST(J)
        LIST(J) = PRT
!
        PRT_INDX = J
!
        RETURN
      END FUNCTION FLT_QPART
!
!=======================================================================
!
      SUBROUTINE FLT_QSORT (LIST)
!
!       This routine initializes the random number generator, then starts
!       quick sorting (ascending) LIST.
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:) 1D array to be sorted.
!
!       output: LIST REAL(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: N
        INTEGER, ALLOCATABLE :: SEED(:)
        INTEGER :: I
!
!       End of declarations.
!
!=======================================================================
!
!       Determine if LIST is large enough to warrant quick sorting.  If
!       so, initialize and begin quick sorting; if not, perform an
!       insertion sort.
!
        N = SIZE(LIST)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (8:)
!-----------------------------------------------------------------------
!
!         This call assigns N to be the minimum size of the array used
!         with for the PUT/GET arguments to RANDOM_SEED, it does not use
!         N as input.
!
          CALL RANDOM_SEED (SIZE = N)
!
          ALLOCATE (SEED(N))
!
          DO I = 1, N
            CALL SYSTEM_CLOCK (COUNT = SEED(I))
          END DO
!
          CALL RANDOM_SEED (PUT = SEED)
!
          CALL FLT_QUICKSORT (LIST)
!
          DEALLOCATE (SEED)
!-----------------------------------------------------------------------
        CASE (2:7)
!-----------------------------------------------------------------------
          CALL FLT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE FLT_QSORT
!
!=======================================================================
!
      RECURSIVE SUBROUTINE FLT_QUICKSORT (LIST)
!
!       This routine performs a quick sort (ascending) on LIST.
!
!-----------------------------------------------------------------------
!        input: LIST REAL(:) 1D array to be sorted.
!
!       output: LIST REAL(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        REAL, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: N, PRT
!
!       End of declarations.
!
!=======================================================================
!
!       If the list isn't small then continue to perform quick sorts,
!       but if it is small then perform an insertion sort.
!
        N = SIZE(LIST)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (8:)
!-----------------------------------------------------------------------
          PRT = FLT_QPART(LIST)
          IF (PRT .NE. 1) THEN
            CALL FLT_QUICKSORT (LIST(:PRT-1))
          END IF
          IF (PRT .NE. N) CALL FLT_QUICKSORT (LIST(PRT+1:))
!-----------------------------------------------------------------------
        CASE (2:7)
!-----------------------------------------------------------------------
          CALL FLT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE FLT_QUICKSORT
!
!=======================================================================
!
      SUBROUTINE INT_ISORT (LIST)
!
!       This routine performs an insertion sort (ascending) on LIST.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) 1D array to be sorted.
!
!       output: LIST INTEGER(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I, J
        INTEGER :: SWP
!
!       End of declarations.
!
!=======================================================================
!
        DO I = 2, SIZE(LIST)
          SWP = LIST(I)
!
!         Find the first value smaller than the Ith value of the array
!         and swap their positions.
!
          DO J = (I - 1), 1, -1
            IF (LIST(J) .LT. SWP) EXIT
            LIST(J+1) = LIST(J)
          END DO
          LIST(J+1) = SWP
        END DO
!
        RETURN
      END SUBROUTINE INT_ISORT
!
!=======================================================================
!
      SUBROUTINE INT_MERGE (LIST, MLST, PRT)
!
!       This routine performs a merge of two sorted (ascending) sections
!       of LIST.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) 1D array to be sorted.
!
!       output: LIST INTEGER(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:), MLST(:)
        INTEGER, INTENT (IN) :: PRT
!
!       Declare local variables.
!
        INTEGER ::  I, J, K, N
!
!       End of declarations.
!
!=======================================================================
!
        N = SIZE(LIST)
!
        I = 1
        J = PRT + 1
        K = 1
        DO WHILE ((PRT .GE. I) .AND. (N .GE. J))
          IF (MLST(I) .LT. MLST(J)) THEN
            LIST(K) = MLST(I)
            I = I + 1
          ELSE
            LIST(K) = MLST(J)
            J = J + 1
          END IF
          K = K + 1
        END DO
!
!-----------------------------------------------------------------------
        IF (PRT .GE. I) THEN
!-----------------------------------------------------------------------
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     PARALLEL DO !IF ((PRT - I) .GT. MIN_OMP_LOOP_LEN)             &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (J)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          DO J = 0, (PRT - I)
            LIST(K+J) = MLST(I+J)
          END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!-----------------------------------------------------------------------
        ELSE IF (N .GE. J) THEN
!-----------------------------------------------------------------------
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     PARALLEL DO !IF ((N - J) .GT. MIN_OMP_LOOP_LEN)               &
!$OMP&      DEFAULT (SHARED)                                            &
!$OMP&      PRIVATE (I)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          DO I = 0, (N - J)
            LIST(K+I) = MLST(J+I)
          END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP     END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!-----------------------------------------------------------------------
        END IF
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE INT_MERGE
!
!=======================================================================
!
      RECURSIVE SUBROUTINE INT_MERGESORT (LIST, MLST)
!
!       This routine performs a merge sort (ascending) on LIST.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) 1D array to be sorted.
!
!       output: LIST INTEGER(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:), MLST(:)
!
!       Declare local variables.
!
        INTEGER :: N, PRT
!
!       End of declarations.
!
!=======================================================================
!
!       If the list isn't small then continue to perform merge sorts,
!       but if it is small then perform an insertion sort.
!
        N = SIZE(LIST)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (7:)
!-----------------------------------------------------------------------
          PRT = N / 2
!
          CALL INT_MERGESORT (MLST(:PRT), LIST(:PRT))
          CALL INT_MERGESORT (MLST(PRT+1:), LIST(PRT+1:))
!
          CALL INT_MERGE (LIST, MLST, PRT)
!-----------------------------------------------------------------------
        CASE (2:6)
!-----------------------------------------------------------------------
          CALL INT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE INT_MERGESORT
!
!=======================================================================
!
      SUBROUTINE INT_MSORT (LIST, PRTS)
!
!       This routine initializes additional memory, then starts merge
!       sorting (ascending) LIST.  If PRTS is provided, then merges will
!       be performed between the N + 1 presorted sections of LIST given
!       by:
!
!         LIST(:PRTS(1)), LIST(PRTS(1)+1:PRTS(2)), ... , LIST(PRTS(N):)
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) 1D array to be sorted.
!               PRTS INTEGER(:) Partitions that specify the already
!                               sorted sections of LIST.
!
!       output: LIST INTEGER(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, CONTIGUOUS, OPTIONAL, INTENT (IN) :: PRTS(:)
!
!       Declare local variables.
!
        INTEGER :: I, N
        INTEGER, ALLOCATABLE :: MLST(:)
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
        LOGICAL :: PRESENT_SIZE_GE_1
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!       End of declarations.
!
!=======================================================================
!
        N = SIZE(LIST)
!
!       Determine if LIST is large enough to warrant merge sorting.  If
!       so, initialize and begin merge sorting; if not, perform an
!       insertion sort.
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (7:)
!-----------------------------------------------------------------------
          ALLOCATE (MLST(SIZE(LIST)), SOURCE = LIST)
!
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!         [unhack] - INT_MSORT
!
!         [unhack] - The PRESENT_SIZE_GE function does not seem to
!         [unhack] - properly respect the PRESENT property of PRTS as of
!         [unhack] - gcc 4.9.0 20131222.  The function is recreated
!         [unhack] - inline here instead.
!
          IF (PRESENT(PRTS)) THEN
            PRESENT_SIZE_GE_1 = SIZE(PRTS) .GE. 1
          ELSE
            PRESENT_SIZE_GE_1 = .FALSE.
          END IF
!
!% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF (PRESENT_SIZE_GE_1) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!           Perform merges of sorted sub-lists with the given
!           partitions.
!
            I = 2
!
            IF (MOD(SIZE(PRTS), 2) .EQ. 0) THEN
              CALL INT_MERGE (MLST(:PRTS(I)), LIST(:PRTS(I)), PRTS(I-1))
!
              I = I + 1
            END IF
!
            DO WHILE (I .LE. SIZE(PRTS))
              CALL INT_MERGE (LIST(:PRTS(I)), MLST(:PRTS(I)), PRTS(I-1))
!
              I = I + 1
!
              CALL INT_MERGE (MLST(:PRTS(I)), LIST(:PRTS(I)), PRTS(I-1))
!
              I = I + 1
            END DO
!
            CALL INT_MERGE (LIST, MLST, PRTS(I-1))
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            CALL INT_MERGESORT (LIST, MLST)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          END IF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          DEALLOCATE (MLST)
!-----------------------------------------------------------------------
        CASE (2:6)
!-----------------------------------------------------------------------
          CALL INT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE INT_MSORT
!
!=======================================================================
!
      FUNCTION INT_QPART(LIST) RESULT (PRT_INDX)
!
!       This routine performs a partition on LIST for the quick sort
!       algorithm and returns the partition index.
!
!-----------------------------------------------------------------------
!        input: LIST      INTEGER(:) 1D array to be sorted.
!
!       output: LIST      INTEGER(:) Partitioned 1D array.
!               PRT_INDX  INTEGER    Index along which LIST is split
!                                    into two partitions.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER :: PRT_INDX
!
!       Declare local variables.
!
        INTEGER :: I, J, N
        REAL :: RAND
        INTEGER :: PRT, SWP
!
!       End of declarations.
!
!=======================================================================
!
        N = SIZE(LIST)
!
!       Choose a value inside the list randomly.
!
        CALL RANDOM_NUMBER (RAND)
        J = INT(RAND * N) + 1
        PRT = LIST(J)
!
        LIST(J) = LIST(N)
!
!       Split the array in two sections (one with values less than the
!       randomly chosen value and the other with values greater than).
!
        J = 1
        DO I = 1, (N - 1)
          IF (LIST(I) .LT. PRT) THEN
            SWP = LIST(J)
            LIST(J) = LIST(I)
            LIST(I) = SWP
            J = J + 1
          END IF
        END DO
!
        LIST(N) = LIST(J)
        LIST(J) = PRT
!
        PRT_INDX = J
!
        RETURN
      END FUNCTION INT_QPART
!
!=======================================================================
!
      SUBROUTINE INT_QSORT (LIST)
!
!       This routine initializes the random number generator, then starts
!       quick sorting (ascending) LIST.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) 1D array to be sorted.
!
!       output: LIST INTEGER(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: N
        INTEGER, ALLOCATABLE :: SEED(:)
        INTEGER :: I
!
!       End of declarations.
!
!=======================================================================
!
!       Determine if LIST is large enough to warrant quick sorting.  If
!       so, initialize and begin quick sorting; if not, perform an
!       insertion sort.
!
        N = SIZE(LIST)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (8:)
!-----------------------------------------------------------------------
!
!         This call assigns N to be the minimum size of the array used
!         with for the PUT/GET arguments to RANDOM_SEED, it does not use
!         N as input.
!
          CALL RANDOM_SEED (SIZE = N)
!
          ALLOCATE (SEED(N))
!
          DO I = 1, N
            CALL SYSTEM_CLOCK (COUNT = SEED(I))
          END DO
!
          CALL RANDOM_SEED (PUT = SEED)
!
          CALL INT_QUICKSORT (LIST)
!
          DEALLOCATE (SEED)
!-----------------------------------------------------------------------
        CASE (2:7)
!-----------------------------------------------------------------------
          CALL INT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE INT_QSORT
!
!=======================================================================
!
      RECURSIVE SUBROUTINE INT_QUICKSORT (LIST)
!
!       This routine performs a quick sort (ascending) on LIST.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) 1D array to be sorted.
!
!       output: LIST INTEGER(:) Sorted 1D array.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: N, PRT
!
!       End of declarations.
!
!=======================================================================
!
!       If the list isn't small then continue to perform quick sorts,
!       but if it is small then perform an insertion sort.
!
        N = SIZE(LIST)
!
        SELECT CASE (N)
!-----------------------------------------------------------------------
        CASE (8:)
!-----------------------------------------------------------------------
          PRT = INT_QPART(LIST)
          IF (PRT .NE. 1) THEN
            CALL INT_QUICKSORT (LIST(:PRT-1))
          END IF
          IF (PRT .NE. N) CALL INT_QUICKSORT (LIST(PRT+1:))
!-----------------------------------------------------------------------
        CASE (2:7)
!-----------------------------------------------------------------------
          CALL INT_ISORT (LIST)
!-----------------------------------------------------------------------
        END SELECT
!-----------------------------------------------------------------------
!
        RETURN
      END SUBROUTINE INT_QUICKSORT
!
!=======================================================================
!
      END MODULE SORT_I
