! -----------------------------------------------------------------------------
! 
! Shabaka is the proprietary property of The Regents of the University of California.
!
! Copyright (C) 2017 The Regents of the University of California, Davis campus. All Rights Reserved.
!
! This software may be patent pending.
!
! The software program and documentation are supplied "as is", without any accompanying services from The Regents, for purposes of confidential discussions only. The Regents does not warrant that the operation of the program will be uninterrupted or error-free. The end-user understands that the program was developed for research purposes and is advised not to rely exclusively on the program for any reason.
!
! IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. THE REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE REGENTS HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
! 
! -----------------------------------------------------------------------------

      PROGRAM PLY2SMFE
!       
!     interface program that takes as input the results of qvoronoi and 
!     outputs the necessary information for Celeris
!
!=======================================================================
!
      USE KINDS_M
      USE SORT_I
      IMPLICIT NONE
!       
!-----------------------------------------------------------------------
!     local variables
!-----------------------------------------------------------------------
! 
      TYPE :: FACET
        INTEGER :: NUM_VERTS = 0
        INTEGER, ALLOCATABLE :: VERT_NUMS(:)
      END TYPE FACET
      
      INTEGER :: LN, NL, NF, STAT, IOS, R_UNIT, W_UNIT
      INTEGER :: I, J, K, N_VERTS, N_FACETS, TOTBREPVERTS, NDIM
      INTEGER :: NUMUNIQUE, NUMUNIQBREPVERTS, IND, N_CELLS
      INTEGER, ALLOCATABLE :: TEMP(:), BREP_VERT_NUMS(:)
      REAL (DBL), ALLOCATABLE :: VERTS(:,:)
      TYPE (FACET), ALLOCATABLE :: FACETS(:)
      CHARACTER (LEN = 50) :: BASE_NAME
      CHARACTER (40) :: STRING_VAR
! 
!-----------------------------------------------------------------------
! 
!     get base name for output files
! 
      CALL GET_COMMAND_ARGUMENT (1, BASE_NAME, STATUS = STAT)
!
      IF (STAT .NE. 0) THEN
        PRINT *, 'error reading root filename from command line'
        PRINT *, 'aborting'
        STOP
      END IF
        
      NF = 0
      NL = 0
      DO I = 1, 50
        IF (NF .EQ. 0) THEN
          IF (BASE_NAME(I:I) .NE. ' ') NF = I
        ELSE
          IF (BASE_NAME(I:I) .EQ. ' ') THEN
            NL = I - 1
            EXIT
          END IF
        END IF
      END DO
      LN = NL - NF + 1
      BASE_NAME(1:LN) = BASE_NAME(NF:NL)  
! 
!     open .ply  file for reading
! 
      R_UNIT = 7
      
      OPEN (UNIT = R_UNIT, ACCESS = 'SEQUENTIAL',                       &
     &      FILE = BASE_NAME(1:LN), FORM = 'FORMATTED',                 &
     &      STATUS = 'OLD', IOSTAT = IOS)      
      IF (IOS .NE. 0) THEN
        PRINT *, 'error opening file ', BASE_NAME(1:LN)
        PRINT *, 'aborting'
        STOP
      END IF     
      
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR    
      READ (R_UNIT, *) STRING_VAR, STRING_VAR, N_VERTS
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR, STRING_VAR, N_FACETS      
      READ (R_UNIT, *) STRING_VAR
      READ (R_UNIT, *) STRING_VAR
      
      ALLOCATE(VERTS(3,N_VERTS),FACETS(N_FACETS))
      
      DO I = 1, N_VERTS
        READ (R_UNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)
      END DO
      
      TOTBREPVERTS = 0      
      DO I = 1, N_FACETS
        READ (R_UNIT, *) FACETS(I)%NUM_VERTS
        ALLOCATE (FACETS(I)%VERT_NUMS(FACETS(I)%NUM_VERTS))
        TOTBREPVERTS = TOTBREPVERTS + FACETS(I)%NUM_VERTS        
        FACETS(I)%VERT_NUMS = 0
        BACKSPACE (R_UNIT)
        READ (R_UNIT, *) FACETS(I)%NUM_VERTS,                           &
     &              (FACETS(I)%VERT_NUMS(J), J = 1, FACETS(I)%NUM_VERTS)
        FACETS(I)%VERT_NUMS = FACETS(I)%VERT_NUMS + 1
      END DO
!  
!     EXPORT BREP AS ONE POLYHEDRON
! 
      W_UNIT = 8
! 
!     open files for writing
! 
      OPEN (UNIT = W_UNIT, ACCESS = 'SEQUENTIAL',                       &
     &      FILE = BASE_NAME(1:LN-4)//'.m', FORM = 'UNFORMATTED',       &
     &      STATUS = 'REPLACE')
!       
!     print to .m file
!       
      NDIM = 3
      N_CELLS = 1
      WRITE (W_UNIT) NDIM, N_VERTS, N_CELLS, 0, 1, 0          
      DO I = 1, N_VERTS
        WRITE (W_UNIT) I, NDIM, VERTS(1,I), VERTS(2,I), VERTS(3,I)          
      END DO
!       

!       come up with ordered list of brep verts

        ALLOCATE (TEMP(TOTBREPVERTS))
!       
        IND = 0
        DO I = 1, N_FACETS
          DO K = 1, FACETS(I)%NUM_VERTS
            IND = IND + 1
            TEMP(IND) = FACETS(I)%VERT_NUMS(K)
          END DO
        END DO
!         
!       sort the list 
! 
        CALL ISORT (TEMP)
!         
!       count unique vertices
! 
        NUMUNIQUE = 1
        DO J = 2, TOTBREPVERTS
          IF(TEMP(J) /= TEMP(J - 1)) NUMUNIQUE = NUMUNIQUE + 1
        END DO
! 
!       copy non-duplicates into cell list 
! 
        ALLOCATE(BREP_VERT_NUMS(NUMUNIQUE))
        BREP_VERT_NUMS(1) = TEMP(1)
        NUMUNIQUE = 1
        DO J = 2, TOTBREPVERTS
          IF(TEMP(J) /= TEMP(J - 1)) THEN
            NUMUNIQUE = NUMUNIQUE + 1
            BREP_VERT_NUMS(NUMUNIQUE) = TEMP(J)
          END IF
        END DO
!         
        NUMUNIQBREPVERTS = NUMUNIQUE
!         
        DEALLOCATE (TEMP)
        
!         
!     fiddle with facet vertex nums for SMFE format sake
! 
      DO I = 1, N_FACETS
        FACETS(I)%VERT_NUMS(1) = -FACETS(I)%VERT_NUMS(1)
      END DO
! 
      DO I = 1, N_CELLS
!       
!       print to file with all material types
!     
        WRITE (W_UNIT) I, NDIM, 0, 0,                                   &
     &            NUMUNIQBREPVERTS + TOTBREPVERTS,                      &
     &            (BREP_VERT_NUMS(J), J = 1, NUMUNIQBREPVERTS),         &
     &            ((FACETS(J)%VERT_NUMS(K), K = 1, FACETS(J)%NUM_VERTS),&
     &            J = 1, N_FACETS)
     
       END DO    
       CLOSE (W_UNIT)
      
      STOP
!
!=======================================================================
!       
      END PROGRAM PLY2SMFE      
