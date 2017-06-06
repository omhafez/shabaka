      MODULE MATRIX_M
!
!     This module provides functions for matrix operations on 
!     double-precision real matrices.
!
!=======================================================================
!
      USE KINDS_M
      IMPLICIT NONE
!
!=======================================================================
      CONTAINS
!=======================================================================
!
      FUNCTION M_M(A, B)
!
        REAL (DBL), INTENT (IN) :: A(:,:), B(:,:)
        REAL (DBL) :: M_M(SIZE(A,1),SIZE(B,2))
!
        INTEGER :: I, J
!
!-----------------------------------------------------------------------
!
        FORALL (I = 1 : SIZE(A,1), J = 1 : SIZE(B,2))
          M_M(I,J) = SUM(A(I,:) * B(:,J))
        END FORALL
!
        RETURN
      END FUNCTION M_M
!
!=======================================================================
!
      FUNCTION MT_M(A, B)
!
        REAL (DBL), INTENT (IN) :: A(:,:), B(:,:)
        REAL (DBL) :: MT_M(SIZE(A,2),SIZE(B,2))
!
        INTEGER :: I, J
!
!-----------------------------------------------------------------------
!
        FORALL (I = 1 : SIZE(A,2), J = 1 : SIZE(B,2))
          MT_M(I,J) = SUM(A(:,I) * B(:,J))
        END FORALL
!
        RETURN
      END FUNCTION MT_M
!
!=======================================================================
!
      FUNCTION M_MT(A, B)
!
        REAL (DBL), INTENT (IN) :: A(:,:), B(:,:)
        REAL (DBL) :: M_MT(SIZE(A,1),SIZE(B,1))
!
        INTEGER :: I, J
!
!-----------------------------------------------------------------------
!
        FORALL (I = 1 : SIZE(A,1), J = 1 : SIZE(B,1))
          M_MT(I,J) = SUM(A(I,:) * B(J,:))
        END FORALL
!
        RETURN
      END FUNCTION M_MT
!
!=======================================================================
!
      FUNCTION M_V(A, B)
!
        REAL (DBL), INTENT (IN) :: A(:,:), B(:)
        REAL (DBL) :: M_V(SIZE(A,1))
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        FORALL (I = 1 : SIZE(A,1)) M_V(I) = SUM(A(I,:) * B(:))
!
        RETURN
      END FUNCTION M_V
!
!=======================================================================
!
      FUNCTION MT_V(A, B)
!
        REAL (DBL), INTENT (IN) :: A(:,:), B(:)
        REAL (DBL) :: MT_V(SIZE(A,2))
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        FORALL (I = 1 : SIZE(A,2)) MT_V(I) = SUM(A(:,I) * B(:))
!
        RETURN
      END FUNCTION MT_V
!
!=======================================================================
!
      FUNCTION DETM(A, N)
!
!     This function returns the determinant of A, where A is a 3 x 3
!     matrix.  Note that, if N is given as 2, then it is assumed that
!     A(I,J) = 0.D0 for IJ = 13, 23, 31, 32; and A(3,3) = 1.D0.
! 
!-----------------------------------------------------------------------
!
      REAL (DBL), INTENT (IN) :: A(3,3)
      REAL (DBL) :: DETM
      INTEGER :: N
!
!     declare dummy variables
!
      REAL (DBL) :: D1, D2, D3
!
!-----------------------------------------------------------------------
!
      IF (N .EQ. 2) THEN
        DETM = A(1,1) * A(2,2) - A(1,2) * A(2,1)
      ELSE IF (N .EQ. 3) THEN
        D1 = A(2,2) * A(3,3) - A(2,3) * A(3,2)
        D2 = A(2,1) * A(3,3) - A(2,3) * A(3,1)
        D3 = A(2,1) * A(3,2) - A(2,2) * A(3,1)
        DETM = A(1,1) * D1 - A(1,2) * D2 + A(1,3) * D3
      ELSE
        DETM = 0.D0
      END IF
!
      RETURN
      END FUNCTION DETM
!
!=======================================================================
!
      SUBROUTINE INV3 (F, DETI, N, FI)
!
!     This routine returns the inverse of a 3 x 3 matrix.  Note that,
!     if N is given as 2, then it is assumed that A(I,J) = 0.D0 for
!     IJ = 13, 23, 31, 32, and A(3,3) = 1.D0.
!
!-----------------------------------------------------------------------
!
      REAL (DBL), INTENT (IN) :: F(3,3), DETI
      INTEGER, INTENT (IN) :: N
      REAL (DBL), INTENT (OUT) :: FI(3,3)
!
!     declare dummy variables - none
!
!-----------------------------------------------------------------------
!
      IF (N .EQ. 2) THEN
        FI(1,1) = F(2,2) * DETI
        FI(2,2) = F(1,1) * DETI
        FI(1,2) = -F(1,2) * DETI
        FI(2,1) = -F(2,1) * DETI
      ELSE IF (N .EQ. 3) THEN
        FI(1,1) = (F(2,2) * F(3,3) - F(2,3) * F(3,2)) * DETI
        FI(1,2) = (F(3,2) * F(1,3) - F(1,2) * F(3,3)) * DETI
        FI(1,3) = (F(1,2) * F(2,3) - F(2,2) * F(1,3)) * DETI
        FI(2,1) = (F(2,3) * F(3,1) - F(2,1) * F(3,3)) * DETI
        FI(2,2) = (F(1,1) * F(3,3) - F(1,3) * F(3,1)) * DETI
        FI(2,3) = (F(1,3) * F(2,1) - F(1,1) * F(2,3)) * DETI
        FI(3,1) = (F(2,1) * F(3,2) - F(3,1) * F(2,2)) * DETI
        FI(3,2) = (F(1,2) * F(3,1) - F(1,1) * F(3,2)) * DETI
        FI(3,3) = (F(1,1) * F(2,2) - F(1,2) * F(2,1)) * DETI
      END IF
!
      RETURN
      END SUBROUTINE INV3
!
!=======================================================================
!
      SUBROUTINE INVERSE (A, AI)
!
!     This routine computes the inverse of square matrix A via simple
!     Gauss elimination.  A is returned unmodified.
!
!--------------------------------------------------------------------
!
      REAL (DBL), INTENT (IN) :: A(:,:)
      REAL (DBL), INTENT (OUT) :: AI(:,:)
!
!     declare dummy variables
!
      REAL (DBL), ALLOCATABLE :: AS(:,:)
      INTEGER, ALLOCATABLE :: ICOL(:), IBLK(:)
      REAL (DBL) :: AMAX, ASM, DIV, FAC
      INTEGER :: N, I, J, K, JMAX
!
!--------------------------------------------------------------------
!
!     store the input matrix in scratch space
!
      N = SIZE(A, 1)
      ALLOCATE (AS(N,N), ICOL(N), IBLK(N))
      IF (SIZE(AI, 1) .NE. N .OR. SIZE(AI, 2) .NE. N) RETURN
      AI = A
!
!     fill the inverse with ones on the diagonal
!
      IBLK = 0
      AS = 0.D0
      FORALL (I = 1 : N) AS(I,I) = 1.D0
!
!     loop over columns; the result of each pass through the loop
!     is a column with zeros everywhere except for a single one
!
      DO I = 1, N
!
!       find the largest element in the current column; the 
!       corresponding row cannot have been used as a pivot row 
!       previously
!
        JMAX = 1
        AMAX = -1.D0
        DO J = 1, N
          IF (IBLK(J) .EQ. 0) THEN
            ASM = ABS(AI(J,I))
            IF (ASM .GE. AMAX) THEN
              AMAX = ASM
              JMAX = J
            END IF
          END IF
        END DO
        DIV = 1.D0 / AI(JMAX,I)
        ICOL(I) = JMAX
        IBLK(JMAX) = 1
!
!       eliminate all nonzero entries in the current column using
!       the JMAX element as pivot
!
        DO J = 1, N
          IF (J .NE. JMAX) THEN
            FAC = AI(J,I) * DIV
            DO K = 1, N
              AS(J,K) = AS(J,K) - FAC * AS(JMAX,K)
              AI(J,K) = AI(J,K) - FAC * AI(JMAX,K)
            END DO
            AI(J,I) = 0.D0
          END IF
        END DO
        AS(JMAX,1:N) = DIV * AS(JMAX,1:N)
        AI(JMAX,1:N) = DIV * AI(JMAX,1:N)
      END DO
!
!     swap rows
!
      FORALL (I = 1 : N) AI(I,1:N) = AS(ICOL(I),1:N)
!
      DEALLOCATE (AS, ICOL, IBLK)
!
      RETURN
      END SUBROUTINE INVERSE
!
!=======================================================================
!
      FUNCTION CROSS_PR(U, V)
!
!     This function returns to 3D cross-product of 2 vectors.
! 
!-----------------------------------------------------------------------
!
      REAL (DBL), INTENT (IN) :: U(3), V(3)
      REAL (DBL) :: CROSS_PR(3)
!
!-----------------------------------------------------------------------
!
      CROSS_PR(1) = U(2) * V(3) - U(3) * V(2)
      CROSS_PR(2) = U(3) * V(1) - U(1) * V(3)
      CROSS_PR(3) = U(1) * V(2) - U(2) * V(1)
!
      RETURN
      END FUNCTION CROSS_PR
!
!=======================================================================
!
      FUNCTION MAT_DOT(A, B, SWAP)
!
!       Returns the matrix inner product A_ij * B_ij.  If SWAP is 
!       present and true, then A_ij * B_ji is returned instead.
!
        REAL (DBL), INTENT (IN) :: A(3,3), B(3,3)
        LOGICAL, OPTIONAL, INTENT (IN) :: SWAP
        REAL (DBL) :: MAT_DOT
        LOGICAL :: SW
!
!-----------------------------------------------------------------------
!
        SW = .FALSE.
        IF (PRESENT(SWAP)) THEN
          IF (SWAP) SW = .TRUE.
        END IF
        IF (SW) THEN
          MAT_DOT =                                                     &
     &            A(1,1) * B(1,1) + A(1,2) * B(2,1) + A(1,3) * B(3,1) + &
     &            A(2,1) * B(1,2) + A(2,2) * B(2,2) + A(2,3) * B(3,2) + &
     &            A(3,1) * B(1,3) + A(3,2) * B(2,3) + A(3,3) * B(3,3)
        ELSE
          MAT_DOT =                                                     &
     &            A(1,1) * B(1,1) + A(1,2) * B(1,2) + A(1,3) * B(1,3) + &
     &            A(2,1) * B(2,1) + A(2,2) * B(2,2) + A(2,3) * B(2,3) + &
     &            A(3,1) * B(3,1) + A(3,2) * B(3,2) + A(3,3) * B(3,3)
        END IF
!
        RETURN
      END FUNCTION MAT_DOT
!
!=======================================================================
!
      END MODULE MATRIX_M
