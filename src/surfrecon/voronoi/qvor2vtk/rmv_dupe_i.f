      MODULE RMV_DUPE_I
!
!     This module provides routines to remove duplicate entries from
!     sorted 1D arrays.
!
!-----------------------------------------------------------------------
!     USE the module wherever you would like to remove duplicate entries
!     from sorted 1D arrays.
!-----------------------------------------------------------------------
!
      USE DATA_OBJ_M
      USE KINDS_M
      IMPLICIT NONE
!
      INTERFACE RMV_DUPE
        MODULE PROCEDURE DATA_OBJ_RMV_DUPE,                             &
     &                   DATA_OBJ_RMV_DUPE_RESIZE,                      &
     &                   INT_RMV_DUPE,                                  &
     &                   INT_RMV_DUPE_RESIZE
      END INTERFACE RMV_DUPE
!
      INTERFACE RMV_DUPE_IDS
        MODULE PROCEDURE ID_RMV_DUPE_RESIZE,                            &
     &                   INT_RMV_DUPE
      END INTERFACE RMV_DUPE_IDS
!
      PRIVATE :: ID_RMV_DUPE_RESIZE,                                    &
     &           DATA_OBJ_RMV_DUPE,                                     &
     &           DATA_OBJ_RMV_DUPE_RESIZE,                              &
     &           INT_RMV_DUPE,                                          &
     &           INT_RMV_DUPE_RESIZE
      PUBLIC :: RMV_DUPE,                                               &
     &          RMV_DUPE_IDS
!
!=======================================================================
      CONTAINS
!=======================================================================
!
      SUBROUTINE DATA_OBJ_RMV_DUPE (LIST, N)
!
!       This routine removes all duplicate entries in a sorted 1D array.
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) Sorted 1D array to have duplicate
!                                entries removed from.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array to with duplicate
!                                entries removed.
!               N    INTEGER     Size of 1D array after entries are
!                                removed.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (OUT) :: N
!
!       Declare local variables.
!
        INTEGER :: I
!
!       End of declarations.
!
!=======================================================================
!
        IF (SIZE(LIST) .EQ. 0) THEN
          N = 0
!
          RETURN
        END IF
!
        N = 1
!
        DO I = 2, SIZE(LIST)
          IF (LIST(I) .NE. LIST(N)) THEN
            N = N + 1
!
            LIST(N) = LIST(I)
          END IF
        END DO
!
        RETURN
      END SUBROUTINE DATA_OBJ_RMV_DUPE
!
!=======================================================================
!
      SUBROUTINE DATA_OBJ_RMV_DUPE_RESIZE (LIST)
!
!       This routine removes all duplicate entries in a sorted 1D array.
!
!-----------------------------------------------------------------------
!        input: LIST DATA_OBJ(:) Sorted 1D array to have duplicate
!                                entries removed from.
!
!       output: LIST DATA_OBJ(:) Sorted 1D array with duplicate entries
!                                removed (resized to only include unique
!                                entries).
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), ALLOCATABLE, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I, N
!
!       End of declarations.
!
!=======================================================================
!
        IF (SIZE(LIST) .EQ. 0) RETURN
!
        N = 1
!
        DO I = 2, SIZE(LIST)
          IF (LIST(I) .NE. LIST(N)) THEN
            N = N + 1
!
            LIST(N) = LIST(I)
          END IF
        END DO
!
        CALL REALLOC_ASSGN_DAT (LIST, LIST(:N))
!         LIST = LIST(:N)
!
        RETURN
      END SUBROUTINE DATA_OBJ_RMV_DUPE_RESIZE
!
!=======================================================================
!
      SUBROUTINE ID_RMV_DUPE_RESIZE (LIST)
!
!       This routine removes all duplicate entries in a sorted 1D array
!       of positive integers.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) Sorted 1D array of positive integers to
!                               have duplicate entries removed from.
!
!       output: LIST INTEGER(:) Sorted 1D array with duplicate entries
!                               removed (resized to only include unique
!                               entries).
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, ALLOCATABLE, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I
!
!       End of declarations.
!
!=======================================================================
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP   PARALLEL DO !IF (SIZE(LIST) .GT. MIN_OMP_LOOP_LEN)              &
!$OMP&    DEFAULT (SHARED)                                              &
!$OMP&    PRIVATE (I)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        DO I = 2, SIZE(LIST)
          IF (ABS(LIST(I)) .EQ. ABS(LIST(I-1))) LIST(I-1) = -LIST(I-1)
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP   END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
        LIST = PACK(LIST, LIST .GT. 0)
!
        RETURN
      END SUBROUTINE ID_RMV_DUPE_RESIZE
!
!=======================================================================
!
      SUBROUTINE INT_RMV_DUPE (LIST, N)
!
!       This routine removes all duplicate entries in a sorted 1D array.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) Sorted 1D array to have duplicate
!                               entries removed from.
!
!       output: LIST INTEGER(:) Sorted 1D array to with duplicate
!                               entries removed.
!               N    INTEGER    Size of 1D array after entries are
!                               removed.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, CONTIGUOUS, INTENT (INOUT) :: LIST(:)
        INTEGER, INTENT (OUT) :: N
!
!       Declare local variables.
!
        INTEGER :: I
!
!       End of declarations.
!
!=======================================================================
!
        IF (SIZE(LIST) .EQ. 0) THEN
          N = 0
!
          RETURN
        END IF
!
        N = 1
!
        DO I = 2, SIZE(LIST)
          IF (LIST(I) .NE. LIST(N)) THEN
            N = N + 1
!
            LIST(N) = LIST(I)
          END IF
        END DO
!
        RETURN
      END SUBROUTINE INT_RMV_DUPE
!
!=======================================================================
!
      SUBROUTINE INT_RMV_DUPE_RESIZE (LIST)
!
!       This routine removes all duplicate entries in a sorted 1D array.
!
!-----------------------------------------------------------------------
!        input: LIST INTEGER(:) Sorted 1D array to have duplicate
!                               entries removed from.
!
!       output: LIST INTEGER(:) Sorted 1D array with duplicate entries
!                               removed (resized to only include unique
!                               entries).
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        INTEGER, ALLOCATABLE, INTENT (INOUT) :: LIST(:)
!
!       Declare local variables.
!
        INTEGER :: I, N
!
!       End of declarations.
!
!=======================================================================
!
        IF (SIZE(LIST) .EQ. 0) RETURN
!
        N = 1
!
        DO I = 2, SIZE(LIST)
          IF (LIST(I) .NE. LIST(N)) THEN
            N = N + 1
!
            LIST(N) = LIST(I)
          END IF
        END DO
!
        LIST = LIST(:N)
!
        RETURN
      END SUBROUTINE INT_RMV_DUPE_RESIZE
!
!=======================================================================
!
      END MODULE RMV_DUPE_I
