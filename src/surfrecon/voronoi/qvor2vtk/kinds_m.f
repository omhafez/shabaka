      MODULE KINDS_M
!
!     This module defines kind type parameters for use throughout the
!     code.
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(15, 307)
      INTEGER, PARAMETER :: MIN_OMP_LOOP_LEN  = 2048      
!
!-----------------------------------------------------------------------
!
      END MODULE KINDS_M
