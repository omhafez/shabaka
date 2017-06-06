      MODULE DATA_OBJ_M
!
!     This module defines the derived type DATA_OBJ that can be used
!     with the data structures, sorting functions, binary searches, etc.
!     defined for Celeris.
!
!-----------------------------------------------------------------------
!     Suggested use is through a derived type that extends DATA_OBJ.
!-----------------------------------------------------------------------
!
      USE KINDS_M
!       USE OMP_M
!       USE TYP_ERR_M
      IMPLICIT NONE
!       PRIVATE :: ERR_HNDLR
!
      TYPE, ABSTRACT :: ABS_DATA_OBJ
        CONTAINS
!
        GENERIC :: ASSIGNMENT (=) => ASSGN_DAT
        GENERIC :: OPERATOR (.EQ.) => EQUAL_DAT
        GENERIC :: OPERATOR (.GT.) => GRTRT_DAT
        GENERIC :: OPERATOR (.LT.) => LESST_DAT
!
        PROCEDURE (ABS_INT_DEL), DEFERRED :: DEL
!
        PROCEDURE (ABS_INT_ASN), PRIVATE, DEFERRED :: ASSGN_DAT
        PROCEDURE (ABS_INT_REL), PRIVATE, DEFERRED :: EQUAL_DAT
        PROCEDURE (ABS_INT_REL), PRIVATE, DEFERRED :: GRTRT_DAT
        PROCEDURE (ABS_INT_REL), PRIVATE, DEFERRED :: LESST_DAT
      END TYPE ABS_DATA_OBJ
!
!-----------------------------------------------------------------------
!
      TYPE, ABSTRACT, EXTENDS (ABS_DATA_OBJ) :: DATA_OBJ
        CONTAINS
!
        GENERIC :: OPERATOR (.GE.) => GRTEQ_DAT
        GENERIC :: OPERATOR (.LE.) => LSSEQ_DAT
        GENERIC :: OPERATOR (.NE.) => NOTEQ_DAT
!
        PROCEDURE, PRIVATE :: EQUAL_DAT => NO_DAT_COMP
        PROCEDURE, PRIVATE :: GRTEQ_DAT
        PROCEDURE, PRIVATE :: GRTRT_DAT => NO_DAT_COMP
        PROCEDURE, PRIVATE :: LSSEQ_DAT
        PROCEDURE, PRIVATE :: LESST_DAT => NO_DAT_COMP
        PROCEDURE, PRIVATE :: NOTEQ_DAT
      END TYPE DATA_OBJ
!
      INTERFACE ASSIGNMENT (=)
        MODULE PROCEDURE REALLOC_ASSGN_DAT
      END INTERFACE ASSIGNMENT (=)
!
      PRIVATE :: ABS_INT_ASN,                                           &
     &           ABS_INT_DEL,                                           &
     &           ABS_INT_REL,                                           &
     &           GRTEQ_DAT,                                             &
     &           NO_DAT_COMP,                                           &
     &           LSSEQ_DAT,                                             &
     &           NOTEQ_DAT!,                                             &
!      &           REALLOC_ASSGN_DAT
!
!=======================================================================
!
!     This is the abstract interface upon which an overloaded
!     assignment operator for each extended type will be built.
!
!     [review] - ABS_INT_ASN
!
!     [review] - There is no apparent way to make the COPY_DLL or
!     [review] - COPY_TREE procedures PURE.  If we want to overload
!     [review] - assignment for derived types with BINARY_SEARCH_TREE or
!     [review] - DBLY_LNK_LST components we will need to remove the
!     [review] - ELEMENTAL property from ABS_INT_ASN.
!
      ABSTRACT INTERFACE
        ELEMENTAL SUBROUTINE ABS_INT_ASN (A, B)
          IMPORT :: ABS_DATA_OBJ, DATA_OBJ
!
          CLASS (ABS_DATA_OBJ), INTENT (INOUT) :: A
          CLASS (DATA_OBJ), INTENT (IN) :: B
        END SUBROUTINE ABS_INT_ASN
      END INTERFACE
!
!=======================================================================
!
!     This is the abstract interface upon which a deletion function for
!     each extended type will be built.  The logical flag CODE indicates
!     whether data being pointed at (if any is contained by the DATA_OBJ
!     extended type) should be deallocated.
!
      ABSTRACT INTERFACE
        SUBROUTINE ABS_INT_DEL (A, CODE)
          IMPORT :: ABS_DATA_OBJ
!
          CLASS (ABS_DATA_OBJ), INTENT (INOUT) :: A
          LOGICAL, OPTIONAL, INTENT (IN) :: CODE
        END SUBROUTINE ABS_INT_DEL
      END INTERFACE
!
!=======================================================================
!
!     This is the abstract interface upon which overloaded relational
!     operators (.EQ., .GT., and .LT.) for each extended type will be
!     built.
!
      ABSTRACT INTERFACE
        FUNCTION ABS_INT_REL(A, B)
          IMPORT :: ABS_DATA_OBJ, DATA_OBJ
!
          CLASS (ABS_DATA_OBJ), INTENT (IN) :: A
          CLASS (DATA_OBJ), INTENT (IN) :: B
          LOGICAL :: ABS_INT_REL
        END FUNCTION ABS_INT_REL
      END INTERFACE
!
!=======================================================================
      CONTAINS
!=======================================================================
!
      FUNCTION NO_DAT_COMP(A, B)
!
!       No Comparison:
!
!       A placeholder function to be used when no comparison is intended
!       for an extension of DATA_OBJ.  This function should never be
!       called.
!
!-----------------------------------------------------------------------
!        input: A           DATA_OBJ
!               B           DATA_OBJ
!
!       output: NO_DAT_COMP LOGICAL
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), INTENT (IN) :: A
        CLASS (DATA_OBJ), INTENT (IN) :: B
        LOGICAL :: NO_DAT_COMP
!
!       Declare local variables.
!
!       NONE
!
!       End of declarations.
!
!=======================================================================
!
!         CALL ERR_HNDLR ('NO_DAT_COMP', 3_KIERRH, ERR_LOG_ABORT_SEV,     &
!      &                  STR = 'DATA_OBJ_M')
!
        RETURN
      END FUNCTION NO_DAT_COMP
!
!=======================================================================
!
      FUNCTION GRTEQ_DAT(A, B)
!
!       This function overloads the greater-than-or-equal operator,
!       returning .TRUE. if A is greater than or equal to B.
!
!-----------------------------------------------------------------------
!        input: A         DATA_OBJ
!               B         DATA_OBJ
!
!       output: GRTEQ_DAT LOGICAL  Wheter DATA_OBJ A is greater than or
!                                  equal to DATA_OBJ B.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), INTENT (IN) :: A, B
        LOGICAL :: GRTEQ_DAT
!
!       Declare local variables.
!
!       NONE
!
!       End of declarations.
!
!=======================================================================
!
        GRTEQ_DAT = .NOT. (A .LT. B)
!
        RETURN
      END FUNCTION GRTEQ_DAT
!
!=======================================================================
!
      FUNCTION LSSEQ_DAT(A, B)
!
!       This function overloads the less-than-or-equal operator,
!       returning .TRUE. if A is less than or equal to B.
!
!-----------------------------------------------------------------------
!        input: A         DATA_OBJ
!               B         DATA_OBJ
!
!       output: LSSEQ_DAT LOGICAL  Wheter DATA_OBJ A is less than or
!                                  equal to DATA_OBJ B.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), INTENT (IN) :: A, B
        LOGICAL :: LSSEQ_DAT
!
!       Declare local variables.
!
!       NONE
!
!       End of declarations.
!
!=======================================================================
!
        LSSEQ_DAT = .NOT. (A .GT. B)
!
        RETURN
      END FUNCTION LSSEQ_DAT
!
!=======================================================================
!
      FUNCTION NOTEQ_DAT(A, B)
!
!       This function overloads the not-equality operator, returning
!       .FALSE. if A and B are equal.
!
!-----------------------------------------------------------------------
!        input: A         DATA_OBJ
!               B         DATA_OBJ
!
!       output: NOTEQ_DAT LOGICAL  Not-equality of DATA_OBJ A and
!                                  DATA_OBJ B.
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), INTENT (IN) :: A, B
        LOGICAL :: NOTEQ_DAT
!
!       Declare local variables.
!
!       NONE
!
!       End of declarations.
!
!=======================================================================
!
        NOTEQ_DAT = .NOT. (A .EQ. B)
!
        RETURN
      END FUNCTION NOTEQ_DAT
!
!=======================================================================
!
      SUBROUTINE REALLOC_ASSGN_DAT (A, B)
!
!       Reallocate on Assignment Data Object:
!
!       For use when reallocation on assignment is intended to be used
!       with defined/overloaded assignment.
!
!-----------------------------------------------------------------------
!        input: A DATA_OBJ(:)
!               B DATA_OBJ(:)
!
!       output:
!-----------------------------------------------------------------------
!
!       Declare dummy variables.
!
        CLASS (DATA_OBJ), ALLOCATABLE, INTENT (INOUT) :: A(:)
        CLASS (DATA_OBJ), INTENT (IN) :: B(:)
!
!       Declare local variables.
!
        INTEGER :: I
        CLASS (DATA_OBJ), ALLOCATABLE :: TMP(:)
!
!       End of declarations.
!
!=======================================================================
!
        ALLOCATE (TMP(SIZE(B)), MOLD = B)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP   PARALLEL DO IF (SIZE(B) .GT. MIN_OMP_LOOP_LEN)                  &
!$OMP&    DEFAULT (SHARED)                                              &
!$OMP&    PRIVATE (I)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        DO I = 1, SIZE(B)
          TMP(I) = B(I)
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!$OMP   END PARALLEL DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
        CALL MOVE_ALLOC (TMP, A)
!
        RETURN
      END SUBROUTINE REALLOC_ASSGN_DAT
!
!=======================================================================
!
      END MODULE DATA_OBJ_M
