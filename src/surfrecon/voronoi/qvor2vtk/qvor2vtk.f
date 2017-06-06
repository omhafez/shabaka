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

      PROGRAM QVOR2VTK
!       
!     interface program that takes as input the results of qvoronoi and 
!     outputs the necessary information for Celeris
!
!=======================================================================
!
      USE KINDS_M
      USE SORT_I
      USE RMV_DUPE_I
      USE MATRIX_M
      USE DATA_TYPES_M
      USE OMP_LIB
      IMPLICIT NONE
!       
!-----------------------------------------------------------------------
!     local variables
!-----------------------------------------------------------------------
!      
      INTEGER :: I, J, K, L, LN, LN2, NEWSIZE, MAX_THREADS
      INTEGER :: IND, INDX, JNDX, NV, OO, NN, NF, MM, CC, VA, VB, VNUMA, VNUMB
      INTEGER :: NL, NT, NS, PP, NFCT, NFACE, NB, NIA, NIB, VJ, VK, NVV, VNUM, NPNTS
      INTEGER :: SEGIND, SSNUM, FNUM, SITES(2)
      INTEGER :: NCELLS, NVERTS, NFACETS, NSEGS, NCYCLESEGS, NSNAPSEGS
      INTEGER :: NBREPFCTS, NGOOD, NBAD, NNEWFCTCOUNT, NNEWFCTS
      INTEGER :: NSTACK, NTRIFCTS, NBADVERTS, NSQUEEZE
      INTEGER :: NBADKEEP, NTRIBFCTS, NORIGVERTS, NSSVERTS, NCENVERTS, NSUPERSEGS
      INTEGER :: NTRGOODFCTS, NCYCPRINT, NSNAPPRINT, NMYSSVERTS
      INTEGER :: NG3, NGP, NB3, NBP
      INTEGER :: IOS, RUNIT, WUNIT, STAT, DUMMY, DUMMY1, DUMMY2, DUMMY3
      INTEGER, ALLOCATABLE :: BVNI(:), BVINDICES(:), MYSSVERTS(:), BADKEEP(:)
      INTEGER, ALLOCATABLE :: POSTR(:), POSBK(:), FLIST(:)
      INTEGER, ALLOCATABLE :: TEMPSQUEEZE(:), TRGOODFCTS(:), SQUEEZE(:)
      INTEGER, ALLOCATABLE :: TEMP(:), IA(:), IB(:), POS(:), STACK(:), G3(:), GP(:)
      INTEGER, ALLOCATABLE :: BREPFCTS(:), GBREPFCTS(:), BBREPFCTS(:), B3(:), BP(:)
      INTEGER, ALLOCATABLE :: MARKS(:), GOODFCTS(:), BADFCTS(:), BREPMARKS(:)      
      INTEGER, ALLOCATABLE :: CYCLESEGS(:),  CYCPRINT(:), SNAPSEGS(:), SNAPPRINT(:)
      REAL (DBL) :: ZERO, ONE, VEC1(3), VEC2(3), NRML(3), AVG(3), DIFF(3), DP
      REAL (DBL), ALLOCATABLE :: VERTS(:,:), SSVERTS(:,:), CENVERTS(:,:), TEMPR(:,:)
      TYPE (VERT), ALLOCATABLE :: VDAT(:)
      TYPE (SEG), ALLOCATABLE :: SEGS(:), BADSEGS(:)
      TYPE (SPSEG), ALLOCATABLE :: SUPERSEGS(:)      
      TYPE (FACET), ALLOCATABLE :: FACETS(:), FCTS(:), NEWFCTS(:)      
      TYPE (CELL), ALLOCATABLE :: CELLS(:)
      LOGICAL :: FLAG, ENC, DEBUG
      LOGICAL, ALLOCATABLE :: VISITED(:), SVISITED(:), CYCLES(:), TEMPBOOL(:), PLANAR(:)
      CHARACTER (LEN = 50) :: BASENAME, BASENAME2
! 
!-----------------------------------------------------------------------
! 
!     set constants
! 
      ZERO = 0.0_DBL
      ONE = 1.0_DBL
      MAX_THREADS = OMP_GET_MAX_THREADS()
      CALL OMP_SET_NUM_THREADS(MAX_THREADS)
!       CALL OMP_SET_NESTED(.TRUE.)      
! 
!     get base name for output files
! 
      CALL GET_COMMAND_ARGUMENT (1, BASENAME, STATUS = STAT)
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
          IF (BASENAME(I:I) .NE. ' ') NF = I
        ELSE
          IF (BASENAME(I:I) .EQ. ' ') THEN
            NL = I - 1
            EXIT
          END IF
        END IF
      END DO
      LN = NL - NF + 1
      BASENAME(1:LN) = BASENAME(NF:NL)
!       
!     see if export call made
! 
      DEBUG = .FALSE.
      CALL GET_COMMAND_ARGUMENT (2, BASENAME2, STATUS = STAT)
      IF (STAT .EQ. 0) THEN
        NF = 0
        NL = 0
        DO I = 1, 50
          IF (NF .EQ. 0) THEN
            IF (BASENAME2(I:I) .NE. ' ') NF = I
          ELSE
            IF (BASENAME2(I:I) .EQ. ' ') THEN
              NL = I - 1
              EXIT
            END IF
          END IF
        END DO
        LN2 = NL - NF + 1
        BASENAME2(1:LN2) = BASENAME2(NF:NL)
               
        IF (BASENAME2(1:LN2) == 'debug') THEN
          DEBUG = .TRUE.
        ELSE
          PRINT *, 'error reading debug command'
          PRINT *, 'aborting'
          STOP          
        END IF
      END IF   
      
      PRINT *, 'READ QVORONOI DATA'      
! 
!     open qsites.node file for reading
! 
      RUNIT = 7
      
      OPEN (UNIT = RUNIT, ACCESS = 'SEQUENTIAL',                        &
     &      FILE = BASENAME(1:LN)//'.xyz', FORM = 'FORMATTED',          &
     &      STATUS = 'OLD', IOSTAT = IOS)      
      IF (IOS .NE. 0) THEN
        PRINT *, 'error opening file ', BASENAME(1:LN)//'.xyz'
        PRINT *, 'aborting'
        STOP
      END IF     
!
      READ (RUNIT, *) DUMMY, NCELLS
      ALLOCATE(CELLS(NCELLS))
      
      DO I = 1, NCELLS/2
        READ (RUNIT, *) CELLS(I)%SITE(1), CELLS(I)%SITE(2), CELLS(I)%SITE(3)
        CELLS(I)%MATNUM = 1
        ALLOCATE (CELLS(I)%FACETNUMS(20))        
      END DO
      DO I = NCELLS/2 + 1, NCELLS
        READ (RUNIT, *) CELLS(I)%SITE(1), CELLS(I)%SITE(2), CELLS(I)%SITE(3)
        CELLS(I)%MATNUM = 2
        ALLOCATE (CELLS(I)%FACETNUMS(20))        
      END DO
      CLOSE (RUNIT)
! 
!     open qsites.out file for reading
! 
      RUNIT = 7
      
      OPEN (UNIT = RUNIT, ACCESS = 'SEQUENTIAL',                        &
     &      FILE = BASENAME(1:LN)//'.vor', FORM = 'FORMATTED',          &
     &      STATUS = 'OLD', IOSTAT = IOS)      
      IF (IOS .NE. 0) THEN
        PRINT *, 'error opening file ', BASENAME(1:LN)//'.vor'
        PRINT *, 'aborting'
        STOP
      END IF     
!
      READ (RUNIT, *) DUMMY
      READ (RUNIT, *) NVERTS
      NVERTS = NVERTS + 1
      ALLOCATE(VERTS(3,NVERTS))
      VERTS(:,1) = 0.0_DBL
      DO I = 2, NVERTS
        READ (RUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)
      END DO
      ALLOCATE(VDAT(NVERTS))      
      
      READ (RUNIT, *) NFACETS
      ALLOCATE(FACETS(NFACETS), BREPFCTS(NFACETS))
      ALLOCATE(GBREPFCTS(NFACETS), BBREPFCTS(NFACETS),BREPMARKS(NFACETS))
     
      DO I = 1, NFACETS
        READ (RUNIT, *) DUMMY1, DUMMY2, DUMMY3
        FACETS(I)%NVERTS = DUMMY1 - 2
        FACETS(I)%NSEGS = FACETS(I)%NVERTS
        ALLOCATE(FACETS(I)%VERTS(FACETS(I)%NVERTS))
        ALLOCATE(FACETS(I)%SEGS(FACETS(I)%NSEGS))
        ALLOCATE(TEMP(FACETS(I)%NVERTS))
        FACETS(I)%SITES(1) = DUMMY2 + 1
        FACETS(I)%SITES(2) = DUMMY3 + 1
        BACKSPACE (RUNIT)
        
        READ (RUNIT, *) DUMMY1, DUMMY2, DUMMY3,                         &
     &                  (TEMP(J), J = 1, FACETS(I)%NVERTS)
        
        FACETS(I)%VERTS = TEMP + 1
        
        DEALLOCATE(TEMP)
      END DO
      CLOSE (RUNIT)
!       
! **************************************************************************
! **************************************************************************    
! 
!     CATEGORIZE FACET DATA
      PRINT *, 'CATEGORIZE FACET DATA'
!       
      NBREPFCTS = 0
     
      DO I = 1, NFACETS   
!      
!       add facet to cells associated with site1 and site2 
! 
        SITES = FACETS(I)%SITES
        
        DO J = 1, 2
!         
!         ensure there is space in list of facet numbers
! 
          IF (SITES(J) <= NCELLS) THEN
            IF (CELLS(SITES(J))%NFACETS ==                              &
     &          SIZE(CELLS(SITES(J))%FACETNUMS)) THEN
!      
              ALLOCATE (TEMP(SIZE(CELLS(SITES(J))%FACETNUMS)))
              TEMP = CELLS(SITES(J))%FACETNUMS              
              DEALLOCATE (CELLS(SITES(J))%FACETNUMS)
!               
              NEWSIZE = CELLS(SITES(J))%NFACETS + 20
              ALLOCATE (CELLS(SITES(J))%FACETNUMS(NEWSIZE))
!               
              IF (ALLOCATED(TEMP)) THEN
                CELLS(SITES(J))%FACETNUMS(1:SIZE(TEMP,1)) = TEMP
                DEALLOCATE (TEMP)
              END IF
            END IF
!           
!           add facet to list
! 
            CELLS(SITES(J))%NFACETS = CELLS(SITES(J))%NFACETS + 1
            CELLS(SITES(J))%FACETNUMS(CELLS(SITES(J))%NFACETS) = I
!             
          END IF
!           
        END DO
        
        IF (CELLS(SITES(1))%MATNUM .NE. CELLS(SITES(2))%MATNUM) THEN
            NBREPFCTS = NBREPFCTS + 1        
            BREPFCTS(NBREPFCTS) = I
!             
            IF(SITES(2) .EQ. (SITES(1) + NCELLS/2)) THEN
              BREPMARKS(NBREPFCTS) = 1
            ELSE
              BREPMARKS(NBREPFCTS) = 2
            END IF
        END IF

      END DO
!       
! **************************************************************************
! **************************************************************************    
!        
!     ORIENT FACETS CORRECTLY  
      PRINT *, 'REORIENT FACETS'
!       
!     loop through facets for each cell, identify node ordering 
!     based on RHR, where normal point outward from convex polyhedron
!            
      DO I = 1, NCELLS     
        NFACE = CELLS(I)%NFACETS
        ALLOCATE(CELLS(I)%FACETS(NFACE))
        IF (CELLS(I)%MATNUM .EQ. 1) THEN
          DO J = 1, NFACE
!           
!           assign facet to cell
! 
            FNUM = CELLS(I)%FACETNUMS(J)
            CELLS(I)%FACETS(J) = FACETS(FNUM)    
            IF (FACETS(FNUM)%NVERTS .GT. 0) THEN
!            
!           compute avg location of facet
! 
            AVG(1) = SUM(VERTS(1,FACETS(FNUM)%VERTS)) / FACETS(FNUM)%NVERTS
            AVG(2) = SUM(VERTS(2,FACETS(FNUM)%VERTS)) / FACETS(FNUM)%NVERTS
            AVG(3) = SUM(VERTS(3,FACETS(FNUM)%VERTS)) / FACETS(FNUM)%NVERTS     
!             
!           determine normal
! 
            VNUMA = FACETS(FNUM)%VERTS(1)
            VNUMB = FACETS(FNUM)%VERTS(2)
            
            VEC1 = (/ VERTS(1,VNUMA) - AVG(1),                          &
     &              VERTS(2,VNUMA) - AVG(2),                            &
     &              VERTS(3,VNUMA) - AVG(3)  /)   
            VEC2 = (/ VERTS(1,VNUMB) - AVG(1),                          &
     &              VERTS(2,VNUMB) - AVG(2),                            &
     &              VERTS(3,VNUMB) - AVG(3)  /)
            NRML = CROSS_PR(VEC1, VEC2)
            
            NRML = NRML / NORM2(NRML)
            DIFF = VERTS(:,VNUMA) - CELLS(I)%SITE(:)
            DIFF = DIFF / NORM2(DIFF)
! 
!           determine correct direction of normal
! 
            DP = DOT_PRODUCT(NRML, DIFF)        
            IF (DP .GE. ZERO) THEN
!             
!             normal is correct, ordering of vertices already follows RHR
!      
              CELLS(I)%FACETS(J)%NRML = NRML
              CELLS(I)%FACETS(J)%VERTS(1) = -CELLS(I)%FACETS(J)%VERTS(1)
!             
            ELSE
!             
!             normal is in wrong direction, need to flip order of vertices
!                
              CELLS(I)%FACETS(J)%NRML = -NRML
              NV = FACETS(FNUM)%NVERTS
              CELLS(I)%FACETS(J)%VERTS = FACETS(FNUM)%VERTS(NV:1:-1)
              FACETS(FNUM)%VERTS = FACETS(FNUM)%VERTS(NV:1:-1)
              CELLS(I)%FACETS(J)%VERTS(1) = -CELLS(I)%FACETS(J)%VERTS(1)     
            END IF
          
            FACETS(CELLS(I)%FACETNUMS(J))%NRML = CELLS(I)%FACETS(J)%NRML

            END IF
          END DO
        END IF
      END DO
!       
! **************************************************************************
! **************************************************************************
! 
!     RENAME BREPFCTS TO FCTS MOVING FORWARD
! 
      ALLOCATE (FCTS(NBREPFCTS))
      DO I = 1, NBREPFCTS
        FCTS(I) = FACETS(BREPFCTS(I))
      END DO
      NFCT = NBREPFCTS
      
      ALLOCATE (MARKS(NFCT))
      DO I = 1, NFCT
        MARKS(I) = BREPMARKS(I)
      END DO
!       
! 
!     GENERATE VERTEX TO FACET LOOKUP
      PRINT *, 'GENERATE VERTEX TO FACET LOOKUP'
!       
      DO I = 1, NFCT
        FNUM = I
        DO J = 1, FCTS(FNUM)%NVERTS
          VNUM = FCTS(FNUM)%VERTS(J)
          VDAT(VNUM)%NFACETS = VDAT(VNUM)%NFACETS + 1
          VDAT(VNUM)%FACETS(VDAT(VNUM)%NFACETS) = FNUM
          VDAT(VNUM)%FINDX(VDAT(VNUM)%NFACETS) = J
        END DO
      END DO      
!       
! **************************************************************************
! **************************************************************************
!  
!     DETERMINE BREP FACETS 
! 
      PRINT *, 'DETERMINE BREP FACETS'
!       
!     FIRST HANDLE PLANAR CASE
! 
      DO I = 1, NFCT
!         
!       LOOP THROUGH VERTS OF THIS FACET AND POPULATE LIST OF NEIGHBORING FACETS
!  
        CC = 0
        DO J = 1, FCTS(I)%NVERTS
          VNUM = FCTS(I)%VERTS(J)
          CC = CC + VDAT(VNUM)%NFACETS
        END DO
        
        ALLOCATE(FLIST(CC))
        CC = 1
        DO J = 1, FCTS(I)%NVERTS
          VNUM = FCTS(I)%VERTS(J)
          DO K = 1, VDAT(VNUM)%NFACETS
            FNUM = VDAT(VNUM)%FACETS(K)
!             
!           ADD FNUM TO LIST
!           
            FLIST(CC) = FNUM
            CC = CC + 1
!             
          END DO
        END DO
!         
!       SORT AND REMOVE DUPES OF LIST OF FACETS 
! 
        CALL QSORT (FLIST)
        CALL RMV_DUPE(FLIST, CC)
!         
!       COMPARE NORMAL OF THIS FACET WITH NORMALS OF LIST
! 
        ALLOCATE(PLANAR(CC))
        PLANAR = .FALSE.
        DO J = 1, CC
          FNUM = FLIST(J)
          DP = DOT_PRODUCT(FCTS(I)%NRML, FCTS(FNUM)%NRML)
!           
          IF(ABS(DP) .GT. 0.999) THEN
            PLANAR(J) = .TRUE.
          END IF
        END DO
! 
!       IF NORMALS ARE SIMILAR ENOUGH, THIS FACET IS A BAD FACET
!       THIS WILL GENERATE A VAST SUPERSEG, AND NONE OF THOSE FACETS WILL BE
!       SNAPPED OUT
! 
        IF ((COUNT(PLANAR .EQV. .TRUE.) .GT. 4)) THEN
          MARKS(I) = 2
        END IF
        
        DEALLOCATE(FLIST, PLANAR)
      END DO
!     
      NGOOD = COUNT(MARKS .EQ. 1)
      NBAD = COUNT(MARKS .EQ. 2)
!         
      ALLOCATE (POS(NFCT), GOODFCTS(NGOOD), BADFCTS(NBAD))
        
      DO I = 1, NFCT
        POS(I) = I
      END DO
        
      GOODFCTS(1:NGOOD) = PACK(POS, MARKS .EQ. 1)
      BADFCTS(1:NBAD) = PACK(POS, MARKS .EQ. 2)   
      
      DEALLOCATE (POS)
!       
! **************************************************************************
! **************************************************************************
! 
!     GENERATE VERTEX TO GOOD FACET LOOKUP
      PRINT *, 'GENERATE VERTEX TO GOOD FACET LOOKUP'
!       
      DO I = 1, NGOOD
        FNUM = GOODFCTS(I)
        DO J = 1, FCTS(FNUM)%NVERTS
          VNUM = FCTS(FNUM)%VERTS(J)
          VDAT(VNUM)%NGFACETS = VDAT(VNUM)%NGFACETS + 1
          VDAT(VNUM)%GFACETS(VDAT(VNUM)%NGFACETS) = FNUM
          VDAT(VNUM)%GFINDX(VDAT(VNUM)%NGFACETS) = J
        END DO
      END DO     
! 
! **************************************************************************
! **************************************************************************
! 
!     EXPORT BREP TO VTK
      PRINT *, 'EXPORT BREP'
!       
      WUNIT = 8
      OPEN (UNIT = WUNIT, ACCESS = 'SEQUENTIAL',                        &
     &        FILE = BASENAME(1:LN)//'-init.vtk',                       &
     &        FORM = 'FORMATTED', STATUS = 'REPLACE')
     
      WRITE(WUNIT, 900)
  900 FORMAT('# vtk DataFile Version 2.0')
      WRITE(WUNIT, 901)
  901 FORMAT('vtk output')
      WRITE(WUNIT, 902)
  902 FORMAT('ASCII')
      WRITE(WUNIT, 903)
  903 FORMAT('DATASET POLYDATA')
      WRITE(WUNIT, 904) NVERTS
  904 FORMAT('POINTS', I20, ' float')
      DO I = 1, NVERTS
        WRITE(WUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)     
      END DO
      WRITE(WUNIT, 905)
  905 FORMAT(' ')
!   
      CC = NFCT
      DO I = 1, NFCT
        CC = CC + FCTS(I)%NVERTS
      END DO  
!   
      WRITE(WUNIT, 906) NFCT, CC
  906 FORMAT('POLYGONS', I10, ' ', I10)

      DO I = 1, NFCT
        WRITE(WUNIT, *) FCTS(I)%NVERTS,                                 &
     &                  (FCTS(I)%VERTS(J)-1, J = 1, FCTS(I)%NVERTS)
      END DO
    
      CLOSE (WUNIT)
      
      IF (DEBUG .EQV. .TRUE.) THEN
!       
!     EXPORT GOOD FACETS TO VTK
      PRINT *, 'EXPORT GOOD FACETS'
!       
      WUNIT = 8
      OPEN (UNIT = WUNIT, ACCESS = 'SEQUENTIAL',                        &
     &        FILE = BASENAME(1:LN)//'-goodfcts.vtk',                   &
     &        FORM = 'FORMATTED', STATUS = 'REPLACE')
     
      WRITE(WUNIT, 500)
  500 FORMAT('# vtk DataFile Version 2.0')
      WRITE(WUNIT, 501)
  501 FORMAT('vtk output')
      WRITE(WUNIT, 502)
  502 FORMAT('ASCII')
      WRITE(WUNIT, 503)
  503 FORMAT('DATASET POLYDATA')
      WRITE(WUNIT, 504) NVERTS
  504 FORMAT('POINTS', I20, ' float')
      DO I = 1, NVERTS
        WRITE(WUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)     
      END DO
      WRITE(WUNIT, 505)
  505 FORMAT(' ')
! 
      CC = NGOOD
      DO I = 1, NGOOD
        CC = CC + FCTS(GOODFCTS(I))%NVERTS
      END DO
! 
      WRITE(WUNIT, 506) NGOOD, CC
  506 FORMAT('POLYGONS', I10, ' ', I10)

      DO I = 1, NGOOD
        WRITE(WUNIT, *) FCTS(GOODFCTS(I))%NVERTS,                       &
     &                   (FCTS(GOODFCTS(I))%VERTS(J)-1,                 &
     &                   J = 1, FCTS(GOODFCTS(I))%NVERTS)
      END DO
     
      CLOSE(WUNIT)
!       
!     EXPORT BAD FACETS TO VTK
      PRINT *, 'EXPORT BAD FACETS'
!       
      WUNIT = 8
      OPEN (UNIT = WUNIT, ACCESS = 'SEQUENTIAL',                        &
     &        FILE = BASENAME(1:LN)//'-badfcts.vtk',                    &
     &        FORM = 'FORMATTED', STATUS = 'REPLACE')
     
      WRITE(WUNIT, 600)
  600 FORMAT('# vtk DataFile Version 2.0')
      WRITE(WUNIT, 601)
  601 FORMAT('vtk output')
      WRITE(WUNIT, 602)
  602 FORMAT('ASCII')
      WRITE(WUNIT, 603)
  603 FORMAT('DATASET POLYDATA')
      WRITE(WUNIT, 604) NVERTS
  604 FORMAT('POINTS', I20, ' float')
      DO I = 1, NVERTS
        WRITE(WUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)     
      END DO
      WRITE(WUNIT, 605)
  605 FORMAT(' ')
! 
      CC = NBAD
      DO I = 1, NBAD
        CC = CC + FCTS(BADFCTS(I))%NVERTS
      END DO      
! 
      WRITE(WUNIT, 606) NBAD, CC
  606 FORMAT('POLYGONS', I10, ' ', I10)

      DO I = 1, NBAD
        WRITE(WUNIT, *) FCTS(BADFCTS(I))%NVERTS,                        &
     &                   (FCTS(BADFCTS(I))%VERTS(J)-1,                  &
     &                   J = 1, FCTS(BADFCTS(I))%NVERTS)
      END DO
    
      CLOSE(WUNIT)   
      
      END IF
! 
! **************************************************************************
! **************************************************************************
!       
!     POPULATE SEGS
      PRINT *, 'POPULATE SEGS'
! 
      NSEGS = 0
      DO I = 1, NFCT
        NSEGS = NSEGS + FCTS(I)%NVERTS
      END DO
      NSEGS = NSEGS / 2
      
      ALLOCATE(TEMPSQUEEZE(NSEGS))
      NSQUEEZE = 0
      
      ALLOCATE (SEGS(NSEGS), BVNI(NVERTS))
      BVNI = 0
      
      SEGIND = 0
           
      DO I = 1, NFCT  
!         
        DO J = 1, FCTS(I)%NVERTS - 1
          VA = MIN(FCTS(I)%VERTS(J), FCTS(I)%VERTS(J + 1))
          VB = MAX(FCTS(I)%VERTS(J), FCTS(I)%VERTS(J + 1))
         
          FLAG = .FALSE.
          
          DO K = 1, VDAT(VA)%NNEIGHBS
            IF (VDAT(VA)%NEIGHBS(K) .EQ. VB) THEN
              INDX = VDAT(VA)%SEGS(K)
              SEGS(INDX)%FACETS(2) = I
              FCTS(I)%SEGCOUNT = FCTS(I)%SEGCOUNT + 1
              FCTS(I)%SEGS(FCTS(I)%SEGCOUNT) = INDX                   
              FLAG = .TRUE.
!               
              IF(MARKS(SEGS(INDX)%FACETS(1)) .EQ. 2 .AND.               &
     &          MARKS(SEGS(INDX)%FACETS(2)) .EQ. 2) THEN
                NSQUEEZE = NSQUEEZE + 1
                TEMPSQUEEZE(NSQUEEZE) = INDX
                BVNI(VA) = VA
                BVNI(VB) = VB
!               
                VDAT(VA)%NBNEIGHBS = VDAT(VA)%NBNEIGHBS + 1
                VDAT(VA)%BNEIGHBS(VDAT(VA)%NBNEIGHBS) = VB
                VDAT(VA)%BSEGS(VDAT(VA)%NBNEIGHBS) = INDX
                VDAT(VB)%NBNEIGHBS = VDAT(VB)%NBNEIGHBS + 1
                VDAT(VB)%BNEIGHBS(VDAT(VB)%NBNEIGHBS) = VA
                VDAT(VB)%BSEGS(VDAT(VB)%NBNEIGHBS) = INDX
              END IF              
              
              EXIT
            END IF
          END DO
          
          IF (FLAG .EQV. .FALSE.) THEN
            SEGIND = SEGIND + 1
            SEGS(SEGIND)%FACETS(1) = I
            FCTS(I)%SEGCOUNT = FCTS(I)%SEGCOUNT + 1
            FCTS(I)%SEGS(FCTS(I)%SEGCOUNT) = SEGIND            
            SEGS(SEGIND)%VERTS(1) = VA
            SEGS(SEGIND)%VERTS(2) = VB
            VDAT(VA)%NNEIGHBS = VDAT(VA)%NNEIGHBS + 1
            VDAT(VA)%NEIGHBS(VDAT(VA)%NNEIGHBS) = VB
            VDAT(VA)%SEGS(VDAT(VA)%NNEIGHBS) = SEGIND
            VDAT(VB)%NNEIGHBS = VDAT(VB)%NNEIGHBS + 1
            VDAT(VB)%NEIGHBS(VDAT(VB)%NNEIGHBS) = VA
            VDAT(VB)%SEGS(VDAT(VB)%NNEIGHBS) = SEGIND            
          END IF          
        END DO
!         
!       DO AGAIN FOR LAST VERTEX OF FACET
! 
        J = FCTS(I)%NVERTS
        VA = MIN(FCTS(I)%VERTS(J), FCTS(I)%VERTS(1))
        VB = MAX(FCTS(I)%VERTS(J), FCTS(I)%VERTS(1))
         
        FLAG = .FALSE.          
          
        DO K = 1, VDAT(VA)%NNEIGHBS
          IF (VDAT(VA)%NEIGHBS(K) .EQ. VB) THEN
            INDX = VDAT(VA)%SEGS(K)          
            SEGS(INDX)%FACETS(2) = I
            FCTS(I)%SEGCOUNT = FCTS(I)%SEGCOUNT + 1
            FCTS(I)%SEGS(FCTS(I)%SEGCOUNT) = INDX                
            FLAG = .TRUE.
            
            IF(MARKS(SEGS(INDX)%FACETS(1)) .EQ. 2 .AND.                 &
     &        MARKS(SEGS(INDX)%FACETS(2)) .EQ. 2) THEN
              NSQUEEZE = NSQUEEZE + 1
              TEMPSQUEEZE(NSQUEEZE) = INDX
              BVNI(VA) = VA
              BVNI(VB) = VB
              
              VDAT(VA)%NBNEIGHBS = VDAT(VA)%NBNEIGHBS + 1
              VDAT(VA)%BNEIGHBS(VDAT(VA)%NBNEIGHBS) = VB
              VDAT(VA)%BSEGS(VDAT(VA)%NBNEIGHBS) = INDX
              VDAT(VB)%NBNEIGHBS = VDAT(VB)%NBNEIGHBS + 1
              VDAT(VB)%BNEIGHBS(VDAT(VB)%NBNEIGHBS) = VA
              VDAT(VB)%BSEGS(VDAT(VB)%NBNEIGHBS) = INDX   
            END IF
            
            EXIT
          END IF
        END DO
!           
        IF (FLAG .EQV. .FALSE.) THEN
          SEGIND = SEGIND + 1
          SEGS(SEGIND)%FACETS(1) = I
          FCTS(I)%SEGCOUNT = FCTS(I)%SEGCOUNT + 1
          FCTS(I)%SEGS(FCTS(I)%SEGCOUNT) = SEGIND
          SEGS(SEGIND)%VERTS(1) = VA
          SEGS(SEGIND)%VERTS(2) = VB
          VDAT(VA)%NNEIGHBS = VDAT(VA)%NNEIGHBS + 1
          VDAT(VA)%NEIGHBS(VDAT(VA)%NNEIGHBS) = VB
          VDAT(VA)%SEGS(VDAT(VA)%NNEIGHBS) = SEGIND
          VDAT(VB)%NNEIGHBS = VDAT(VB)%NNEIGHBS + 1
          VDAT(VB)%NEIGHBS(VDAT(VB)%NNEIGHBS) = VA
          VDAT(VB)%SEGS(VDAT(VB)%NNEIGHBS) = SEGIND          
        END IF
      END DO       
!       
!     identify bad segs
!  
      ALLOCATE (SQUEEZE(NSQUEEZE))
      SQUEEZE = TEMPSQUEEZE(:NSQUEEZE)
      DEALLOCATE (TEMPSQUEEZE)
      
      ALLOCATE(BADSEGS(NSQUEEZE))
 
      DO I = 1, NSQUEEZE
        BADSEGS(I) = SEGS(SQUEEZE(I))
      END DO      
!       
! *****************************************************************************
! *****************************************************************************     
!       
!     CREATE SUPERSEGS
      PRINT *, 'GENERATE SUPERSEGS'
!     
      NBADVERTS = COUNT(BVNI .NE. 0)
      ALLOCATE(BVINDICES(NBADVERTS), POS(NVERTS))
      DO I = 1, NVERTS
        POS(I) = I
      END DO
      BVINDICES(1:NBADVERTS) = PACK(POS, BVNI .NE. 0)
      DEALLOCATE(POS)
      
      ALLOCATE(VISITED(NVERTS), SVISITED(NSEGS), SUPERSEGS(NSQUEEZE))     
      VISITED = .FALSE.
      SVISITED = .FALSE.  
      NSUPERSEGS = 0
      NSTACK = 0
      
      ALLOCATE(CYCLES(NSQUEEZE))
      CYCLES = .FALSE.       
!           
      DO INDX = 1, NBADVERTS
        I = BVINDICES(INDX)
        IF (VISITED(I) .EQV. .FALSE.) THEN
          ALLOCATE (STACK(100))   
          STACK = 0          
          NSTACK = 1
          STACK(NSTACK) = I
          NSUPERSEGS = NSUPERSEGS + 1
          VISITED(I) = .TRUE.
          
          ALLOCATE (SUPERSEGS(NSUPERSEGS)%VERTS(100),                   &
     &              SUPERSEGS(NSUPERSEGS)%BADFCTS(100),                 &
     &              SUPERSEGS(NSUPERSEGS)%TERMINALINDX(100))
          SUPERSEGS(NSUPERSEGS)%VERTS = 0
          NV = SUPERSEGS(NSUPERSEGS)%NVERTS
          NV = NV + 1
          SUPERSEGS(NSUPERSEGS)%NVERTS = NV
          SUPERSEGS(NSUPERSEGS)%VERTS(NV) = I
!           
          DO WHILE (NSTACK .GT. 0)     
!           
!           first check to ensure no stack vertices are neighbors, i.e., make
!           sure no loops. still finish the superseg, but we will throw it away
 
            IF (CYCLES(NSUPERSEGS) .EQV. .FALSE.) THEN
              DO MM = 1, NSTACK
                K = STACK(MM)
                DO NN = 1, VDAT(K)%NBNEIGHBS
                  DO OO = MM + 1, NSTACK 
                    IF (VDAT(K)%BNEIGHBS(NN) .EQ. STACK(OO)) THEN
                      CYCLES(NSUPERSEGS) = .TRUE.
                      GOTO 987
                    END IF
                  END DO
                END DO
987           END DO
            END IF
!             
!           pop off stack
! 
            L = STACK(NSTACK)
            NSTACK = NSTACK - 1
!           
!           loop through neighbors of stack value and add to superseg
! 
            DO JNDX = 1, VDAT(L)%NBNEIGHBS
              J = VDAT(L)%BNEIGHBS(JNDX)
              PP = VDAT(L)%BSEGS(JNDX)              
              IF (SVISITED(PP) .EQV. .FALSE.) THEN
                SVISITED(PP) = .TRUE.              
              
                IF (NSTACK == SIZE(STACK)) THEN
                  ALLOCATE (TEMP(SIZE(STACK)))
                  TEMP = STACK              
                  DEALLOCATE (STACK)
                  NEWSIZE = NSTACK + 100
                  ALLOCATE (STACK(NEWSIZE))
                  IF (ALLOCATED(TEMP)) THEN
                    STACK(1:SIZE(TEMP,1)) = TEMP
                    DEALLOCATE (TEMP)
                  END IF
                END IF
                NSTACK = NSTACK + 1
                STACK(NSTACK) = J
                VISITED(J) = .TRUE.
                
                NV = SUPERSEGS(NSUPERSEGS)%NVERTS                
                IF (NV == SIZE(SUPERSEGS(NSUPERSEGS)%VERTS)) THEN
                  ALLOCATE (TEMP(NV))
                  TEMP = SUPERSEGS(NSUPERSEGS)%VERTS              
                  DEALLOCATE (SUPERSEGS(NSUPERSEGS)%VERTS)
                  NEWSIZE = NV + 100
                  ALLOCATE (SUPERSEGS(NSUPERSEGS)%VERTS(NEWSIZE))
                  IF (ALLOCATED(TEMP)) THEN
                    SUPERSEGS(NSUPERSEGS)%VERTS(1:SIZE(TEMP,1)) = TEMP
                    DEALLOCATE (TEMP)
                  END IF
                END IF
                NV = NV + 1
                SUPERSEGS(NSUPERSEGS)%NVERTS = NV
                SUPERSEGS(NSUPERSEGS)%VERTS(NV) = J
                
                NB = SUPERSEGS(NSUPERSEGS)%NBADFCTS
                IF (NB == SIZE(SUPERSEGS(NSUPERSEGS)%BADFCTS)) THEN
                  ALLOCATE (TEMP(NB))
                  TEMP = SUPERSEGS(NSUPERSEGS)%BADFCTS
                  DEALLOCATE (SUPERSEGS(NSUPERSEGS)%BADFCTS)
                  NEWSIZE = NB + 100
                  ALLOCATE (SUPERSEGS(NSUPERSEGS)%BADFCTS(NEWSIZE))
                  IF (ALLOCATED(TEMP)) THEN
                    SUPERSEGS(NSUPERSEGS)%BADFCTS(1:SIZE(TEMP,1)) = TEMP
                    DEALLOCATE (TEMP)
                  END IF
                END IF
                NB = NB + 2
                SUPERSEGS(NSUPERSEGS)%NBADFCTS = NB
                SUPERSEGS(NSUPERSEGS)%BADFCTS(NB-1) = SEGS(PP)%FACETS(1)
                SUPERSEGS(NSUPERSEGS)%BADFCTS(NB) = SEGS(PP)%FACETS(2)
              END IF
            END DO
!             
!           add to terminal vertices
! 
            IF (VDAT(L)%NBNEIGHBS .EQ. 1) THEN
              NT = SUPERSEGS(NSUPERSEGS)%NTERMINAL
!               
              IF (NT == SIZE(SUPERSEGS(NSUPERSEGS)%TERMINALINDX)) THEN
                ALLOCATE (TEMP(NT))
                TEMP = SUPERSEGS(NSUPERSEGS)%TERMINALINDX
                DEALLOCATE (SUPERSEGS(NSUPERSEGS)%TERMINALINDX)
!               
                NEWSIZE = NT + 100
                ALLOCATE (SUPERSEGS(NSUPERSEGS)%TERMINALINDX(NEWSIZE))
!               
                IF (ALLOCATED(TEMP)) THEN
                  SUPERSEGS(NSUPERSEGS)%TERMINALINDX(1:SIZE(TEMP,1)) = TEMP
                  DEALLOCATE (TEMP)
                END IF
              END IF              
!               
              NT = NT + 1
              SUPERSEGS(NSUPERSEGS)%NTERMINAL = NT
              SUPERSEGS(NSUPERSEGS)%TERMINALINDX(NT) = L
            END IF            
!
          END DO
          DEALLOCATE (STACK)
        END IF
      END DO
!       
!     GENERATE ORDERED, NON-DUPLICATED LIST OF SEGS FOR EACH SUPERSEG 
! 
      DO I = 1, NSUPERSEGS
        ALLOCATE(SUPERSEGS(I)%SEGS(5*SUPERSEGS(I)%NVERTS))
        CC = 0
        DO J = 1, SUPERSEGS(I)%NVERTS
          VA = SUPERSEGS(I)%VERTS(J)
          NV = VDAT(VA)%NBNEIGHBS
          SUPERSEGS(I)%SEGS(CC+1:CC+NV) = VDAT(VA)%BSEGS(:NV)
          CC = CC + NV
        END DO
!         
        CALL QSORT (SUPERSEGS(I)%SEGS(:CC))
        CALL RMV_DUPE(SUPERSEGS(I)%SEGS(:CC), SUPERSEGS(I)%NSEGS)
!         
        ALLOCATE (TEMP(SUPERSEGS(I)%NSEGS))
        TEMP = SUPERSEGS(I)%SEGS(:SUPERSEGS(I)%NSEGS)
        DEALLOCATE(SUPERSEGS(I)%SEGS)
        ALLOCATE(SUPERSEGS(I)%SEGS(SUPERSEGS(I)%NSEGS))
        SUPERSEGS(I)%SEGS = TEMP
        DEALLOCATE(TEMP)
      END DO
! 
! **************************************************************************
! **************************************************************************
! 
!     GENERATE FACET2SUPERSEG LOOKUP TO HANDLE PROTRUDING CASE
      PRINT *, 'GENERATE FACET TO SUPERSEG LOOKUP'
! 
      DO I = 1, NSUPERSEGS 
        DO L = 1, SUPERSEGS(I)%NVERTS
          VNUM = SUPERSEGS(I)%VERTS(L)
          DO J = 1, VDAT(VNUM)%NFACETS
            FNUM = VDAT(VNUM)%FACETS(J)
            FCTS(FNUM)%NSSEGS = FCTS(FNUM)%NSSEGS + 1
            FCTS(FNUM)%SSEGS(FCTS(FNUM)%NSSEGS) = I
          END DO
        END DO
      END DO
      
      DO I = 1, NFCT
        FNUM = I
        CALL QSORT (FCTS(FNUM)%SSEGS(:FCTS(I)%NSSEGS))
        CALL RMV_DUPE(FCTS(FNUM)%SSEGS(:FCTS(I)%NSSEGS), FCTS(FNUM)%NSSEGS)
      END DO      
!       
! **************************************************************************
! **************************************************************************      
! 
!     HANDLE FRINGE CASES
      PRINT *, 'HANDLE FRINGE CASES'
!       
! --------------------------------------------------------------------------
!     DISCARD SUPERSEGS THAT HAVE A CYCLE
! --------------------------------------------------------------------------
!       
!       
      DO I = 1, NFCT
        FNUM = I
        IF(FCTS(FNUM)%NSSEGS .EQ. 1) THEN
          IF(CYCLES(FCTS(FNUM)%SSEGS(1)) .EQV. .FALSE.) THEN
            SSNUM = FCTS(FNUM)%SSEGS(1)
            NS = SUPERSEGS(SSNUM)%NSEGS
!           
!           SET INTERSECTION FROM STACK OVERFLOW 
!           http://stackoverflow.com/questions/8868297/fortran-set-operations

            ALLOCATE (IA(NS), IB(FCTS(FNUM)%NSEGS))
            IA = SUPERSEGS(SSNUM)%SEGS(:NS)
            IB = FCTS(FNUM)%SEGS(:FCTS(FNUM)%NSEGS)         
!           
            CALL QSORT (IA)
            CALL RMV_DUPE(IA, NIA)
            CALL QSORT (IB)
            CALL RMV_DUPE(IB, NIB)                        
!           
            ENC = .TRUE.
            MM = 1
            DO WHILE (MM <= NIB)
              IF (ANY(IA(:NIA) == IB(MM))) THEN
                MM = MM + 1
              ELSE
                ENC = .FALSE.
                EXIT
              END IF
            END DO               
!          
            IF (ENC) THEN
              CYCLES(FCTS(FNUM)%SSEGS(1)) = .TRUE.
            END IF
!           
            DEALLOCATE (IA, IB)
          END IF
        END IF        
      END DO
!       
      DO I = 1, NSUPERSEGS
        IF (SUPERSEGS(I)%NSEGS .GT. 12) THEN
          CYCLES(I) = .TRUE.
        END IF
      END DO
!       
!     POPULATE CYCLESEGS AND SNAPSEGS
!     
      NSNAPSEGS = COUNT(CYCLES(:NSUPERSEGS) .EQV. .FALSE.)
      NCYCLESEGS = COUNT(CYCLES(:NSUPERSEGS) .EQV. .TRUE.)
      
      ALLOCATE(SNAPSEGS(NSNAPSEGS),CYCLESEGS(NCYCLESEGS))
      ALLOCATE(POS(NSUPERSEGS))
        
      DO I = 1, NSUPERSEGS
        POS(I) = I
      END DO
        
      SNAPSEGS(1:NSNAPSEGS) = PACK(POS, CYCLES(:NSUPERSEGS) .EQV. .FALSE.)
      CYCLESEGS(1:NCYCLESEGS) = PACK(POS, CYCLES(:NSUPERSEGS) .EQV. .TRUE.)
      DEALLOCATE(POS)
!       
! --------------------------------------------------------------------------
!     DEAL WITH CASE WHERE SUPERSEG SNAPS OUT N-1 VERTS AND UPDATE SNAPSEGS
! --------------------------------------------------------------------------
!       
      DO IND = 1, NSNAPSEGS
        I = SNAPSEGS(IND)    
        DO L = 1, SUPERSEGS(I)%NVERTS
          VNUM = SUPERSEGS(I)%VERTS(L)
          DO J = 1, VDAT(VNUM)%NFACETS
            FNUM = VDAT(VNUM)%FACETS(J)
            FCTS(FNUM)%NSNAPSSEGS = FCTS(FNUM)%NSNAPSSEGS + 1
            FCTS(FNUM)%SNAPSSEGS(FCTS(FNUM)%NSNAPSSEGS) = I
          END DO
        END DO
      END DO
      
      DO IND = 1, NGOOD
        I = GOODFCTS(IND)
        FNUM = I
        CALL QSORT (FCTS(FNUM)%SNAPSSEGS(:FCTS(I)%NSNAPSSEGS))
        CALL RMV_DUPE(FCTS(FNUM)%SNAPSSEGS(:FCTS(I)%NSNAPSSEGS),        &
     &                FCTS(FNUM)%NSNAPSSEGS)        
      END DO
!       
      DO IND = 1, NGOOD
        I = GOODFCTS(IND)
        FNUM = I
!         
        IF(FCTS(FNUM)%NSNAPSSEGS .LE. 2) THEN
          NMYSSVERTS = 0
          DO J = 1, FCTS(FNUM)%NSNAPSSEGS
            SSNUM = FCTS(FNUM)%SNAPSSEGS(J)
            NMYSSVERTS = NMYSSVERTS + SUPERSEGS(SSNUM)%NVERTS
          END DO
          ALLOCATE(MYSSVERTS(NMYSSVERTS))
          
          CC = 0
          DO J = 1, FCTS(FNUM)%NSNAPSSEGS
            SSNUM = FCTS(FNUM)%SNAPSSEGS(J)
            NV = SUPERSEGS(SSNUM)%NVERTS
            MYSSVERTS(CC+1:CC + NV) = SUPERSEGS(SSNUM)%VERTS(:NV)
            CC = CC + NV
          END DO
!           
!         SET INTERSECTION FROM STACK OVERFLOW 
!         http://stackoverflow.com/questions/8868297/fortran-set-operations

          ALLOCATE (IA(NMYSSVERTS), IB(FCTS(FNUM)%NVERTS))
          IA = MYSSVERTS(:NMYSSVERTS)
          IB = FCTS(FNUM)%VERTS          
!           
          CALL QSORT (IA)
          CALL RMV_DUPE(IA, NIA)
          CALL QSORT (IB)
          CALL RMV_DUPE(IB, NIB)      
! 
          IF (NIB > NIA) THEN
            ALLOCATE(TEMP(NIB))
            TEMP = IB(:NIB)
            DEALLOCATE(IB)
            NIB = NIA
            ALLOCATE(IB(NIB))
            IB = IA(:NIA)
            DEALLOCATE(IA)
            NIA = SIZE(TEMP,1)
            IA = TEMP(:NIA)
            DEALLOCATE(TEMP)
          END IF
!           
          ALLOCATE(TEMPBOOL(NIA))    
!           
          TEMPBOOL = .FALSE.
          DO J = 1, NIB
            TEMPBOOL(J) = ANY(IB(J) == IA)
          END DO
          NPNTS = COUNT(TEMPBOOL .EQV. .TRUE.)
!           
          DEALLOCATE (IA, IB, TEMPBOOL)
          DEALLOCATE (MYSSVERTS)
! 
          IF (NPNTS == FCTS(FNUM)%NVERTS - 1) THEN
            DO J = 1, FCTS(FNUM)%NSNAPSSEGS
              SSNUM = FCTS(FNUM)%SNAPSSEGS(J)
              CYCLES(SSNUM) = .TRUE.
            END DO
          END IF
        END IF        
      END DO
      
      DEALLOCATE (SNAPSEGS, CYCLESEGS)
      DO I = 1, NFCT
        FCTS(I)%NSNAPSSEGS = 0
        FCTS(I)%SNAPSSEGS = 0
      END DO
!       
!     REPOPULATE CYCLESEGS AND SNAPSEGS
!     
      NSNAPSEGS = COUNT(CYCLES(:NSUPERSEGS) .EQV. .FALSE.)
      NCYCLESEGS = COUNT(CYCLES(:NSUPERSEGS) .EQV. .TRUE.)
      
      ALLOCATE(SNAPSEGS(NSNAPSEGS),CYCLESEGS(NCYCLESEGS))
      ALLOCATE(POS(NSUPERSEGS))
        
      DO I = 1, NSUPERSEGS
        POS(I) = I
      END DO
        
      SNAPSEGS(1:NSNAPSEGS) = PACK(POS, CYCLES(:NSUPERSEGS) .EQV. .FALSE.)
      CYCLESEGS(1:NCYCLESEGS) = PACK(POS, CYCLES(:NSUPERSEGS) .EQV. .TRUE.)
      DEALLOCATE(POS)      
!       
! --------------------------------------------------------------------------
!     ADD ADDITIONAL VERTS TO DEAL WITH NON-MANIFOLD CASES
! --------------------------------------------------------------------------
! 
!     algorithm: find the intersection of all terminal vertex neighbors,
!     add to list of superseg verts
! 
      DO IND = 1, NSNAPSEGS
        I = SNAPSEGS(IND)
        IF (SUPERSEGS(I)%NTERMINAL > 2) THEN
          DO J = 1, SUPERSEGS(I)%NTERMINAL
            VJ = SUPERSEGS(I)%TERMINALINDX(J)          
            DO K = J + 1, SUPERSEGS(I)%NTERMINAL
              VK = SUPERSEGS(I)%TERMINALINDX(K)
!               
!             INTERSECT VERTNEIGHBS OF VJ AND VK
!             
              IF (VDAT(VJ)%NNEIGHBS > VDAT(VK)%NNEIGHBS) THEN
                ALLOCATE (IA(VDAT(VJ)%NNEIGHBS), IB(VDAT(VK)%NNEIGHBS))
                IA = VDAT(VJ)%NEIGHBS(:VDAT(VJ)%NNEIGHBS)
                IB = VDAT(VK)%NEIGHBS(:VDAT(VK)%NNEIGHBS)
              ELSE
                ALLOCATE (IA(VDAT(VK)%NNEIGHBS), IB(VDAT(VJ)%NNEIGHBS))         
                IA = VDAT(VK)%NEIGHBS(:VDAT(VK)%NNEIGHBS)
                IB = VDAT(VJ)%NEIGHBS(:VDAT(VJ)%NNEIGHBS)     
              END IF
!               
              CALL QSORT (IA)
              CALL RMV_DUPE(IA, NIA)
              CALL QSORT (IB)
              CALL RMV_DUPE(IB, NIB)
!           
              ALLOCATE(TEMP(NIA))
              TEMP = 0
! 
              CC = 0
              DO MM = 1, NIB
                IF (ANY(IA(:NIA) == IB(MM))) THEN
                  CC = CC + 1
                  TEMP(CC) = IB(MM)
                END IF               
              END DO
!               
              DEALLOCATE (IA, IB)
              
              IF (CC > 0) THEN
                NV = SUPERSEGS(I)%NVERTS
                SUPERSEGS(I)%NVERTS = SUPERSEGS(I)%NVERTS + CC
                SUPERSEGS(I)%VERTS(NV+1:SUPERSEGS(I)%NVERTS)=TEMP(:CC)   
                NVV = SUPERSEGS(I)%NVERTS                
                CALL QSORT (SUPERSEGS(I)%VERTS(:NVV))
                CALL RMV_DUPE (SUPERSEGS(I)%VERTS(:NVV),                &
     &                         SUPERSEGS(I)%NVERTS)
              END IF
              
              DEALLOCATE(TEMP)
            END DO
          END DO
        END IF
      END DO
! 
! --------------------------------------------------------------------------
!     DEAL WITH PROTRUDING CASE
! --------------------------------------------------------------------------
!           
      ALLOCATE (TRGOODFCTS(NGOOD))
      TRGOODFCTS = 0
      NTRGOODFCTS = 0      
!       
      DO IND = 1, NSNAPSEGS
        I = SNAPSEGS(IND)    
        DO L = 1, SUPERSEGS(I)%NVERTS
          VNUM = SUPERSEGS(I)%VERTS(L)
          DO J = 1, VDAT(VNUM)%NFACETS
            FNUM = VDAT(VNUM)%FACETS(J)
            FCTS(FNUM)%NSNAPSSEGS = FCTS(FNUM)%NSNAPSSEGS + 1
            FCTS(FNUM)%SNAPSSEGS(FCTS(FNUM)%NSNAPSSEGS) = I
          END DO
        END DO
      END DO
      
      DO IND = 1, NGOOD
        I = GOODFCTS(IND)
        FNUM = I
        CALL QSORT (FCTS(FNUM)%SNAPSSEGS(:FCTS(I)%NSNAPSSEGS))
        CALL RMV_DUPE(FCTS(FNUM)%SNAPSSEGS(:FCTS(I)%NSNAPSSEGS),        &
     &                FCTS(FNUM)%NSNAPSSEGS)

          NMYSSVERTS = 0
          DO J = 1, FCTS(FNUM)%NSNAPSSEGS
            SSNUM = FCTS(FNUM)%SNAPSSEGS(J)
            NMYSSVERTS = NMYSSVERTS + SUPERSEGS(SSNUM)%NVERTS
          END DO
          ALLOCATE(MYSSVERTS(NMYSSVERTS))
          
          CC = 0
          DO J = 1, FCTS(FNUM)%NSNAPSSEGS
            SSNUM = FCTS(FNUM)%SNAPSSEGS(J)
            NV = SUPERSEGS(SSNUM)%NVERTS
            MYSSVERTS(CC+1:CC + NV) = SUPERSEGS(SSNUM)%VERTS(:NV)
            CC = CC + NV
          END DO
!           
!         SET INTERSECTION FROM STACK OVERFLOW 
!         http://stackoverflow.com/questions/8868297/fortran-set-operations

          ALLOCATE (IA(NMYSSVERTS), IB(FCTS(FNUM)%NVERTS))
          IA = MYSSVERTS(:NMYSSVERTS)
          IB = FCTS(FNUM)%VERTS          
!           
          CALL QSORT (IA)
          CALL RMV_DUPE(IA, NIA)
          CALL QSORT (IB)
          CALL RMV_DUPE(IB, NIB)      
! 
          IF (NIB > NIA) THEN
            ALLOCATE(TEMP(NIB))
            TEMP = IB(:NIB)
            DEALLOCATE(IB)
            NIB = NIA
            ALLOCATE(IB(NIB))
            IB = IA(:NIA)
            DEALLOCATE(IA)
            NIA = SIZE(TEMP,1)
            IA = TEMP(:NIA)
            DEALLOCATE(TEMP)
          END IF
!           
          ALLOCATE(TEMPBOOL(NIA))       
!        
          TEMPBOOL = .FALSE.
          DO J = 1, NIB
            TEMPBOOL(J) = ANY(IB(J) == IA)
          END DO
          NPNTS = COUNT(TEMPBOOL .EQV. .TRUE.)
          FCTS(FNUM)%NSNAPPNTS = NPNTS
          DEALLOCATE (IA, IB, TEMPBOOL)
          DEALLOCATE (MYSSVERTS)   
      END DO
      
      DO IND = 1, NGOOD
        I = GOODFCTS(IND)
        FNUM = I
        IF(FCTS(FNUM)%NSNAPSSEGS .GT. 2) THEN
          NTRGOODFCTS = NTRGOODFCTS + 1
          TRGOODFCTS(NTRGOODFCTS) = FNUM
        ELSE
          IF (FCTS(FNUM)%NSNAPPNTS <= FCTS(FNUM)%NVERTS - 1) THEN
            NTRGOODFCTS = NTRGOODFCTS + 1
            TRGOODFCTS(NTRGOODFCTS) = FNUM
          END IF          
        END IF
      END DO
! 
! **************************************************************************
! **************************************************************************      
!       
!     ADD AVERAGE LOCATION VERTICES FOR EACH SUPERSEG
      PRINT *, 'GENERATE SNAP-TO SUPERSEG LOCATIONS'
!       
      ALLOCATE(SSVERTS(3,NSNAPSEGS))
      DO IND = 1, NSNAPSEGS
        I = SNAPSEGS(IND)   
        NV = SUPERSEGS(I)%NVERTS
!       COMPUTE AVERAGE VERTEX LOCATION OF EACH SUPERSEGS
        SSVERTS(1,IND) = SUM(VERTS(1, SUPERSEGS(I)%VERTS(1:NV))/NV)
        SSVERTS(2,IND) = SUM(VERTS(2, SUPERSEGS(I)%VERTS(1:NV))/NV)  
        SSVERTS(3,IND) = SUM(VERTS(3, SUPERSEGS(I)%VERTS(1:NV))/NV)             
      END DO      
      
      NORIGVERTS = NVERTS
      NSSVERTS = NSNAPSEGS
      NVERTS = NVERTS + NSSVERTS
      ALLOCATE(TEMPR(3,NORIGVERTS))
      TEMPR = VERTS
      DEALLOCATE(VERTS)
      ALLOCATE(VERTS(3,NVERTS))
      VERTS(:,:NORIGVERTS) = TEMPR
      VERTS(:,NORIGVERTS+1:NVERTS) = SSVERTS
      DEALLOCATE(TEMPR)
!       
! **************************************************************************
! **************************************************************************      
! 
      IF (DEBUG .EQV. .TRUE.) THEN
      
!     EXPORT BAD SEGS
      PRINT *, 'EXPORT BAD SEGS'
!       
      NCYCPRINT = 0
      DO I = 1, NCYCLESEGS
        NCYCPRINT = NCYCPRINT + SUPERSEGS(CYCLESEGS(I))%NSEGS
      END DO
      ALLOCATE(CYCPRINT(NCYCPRINT))
      CC = 0
      DO I = 1, NCYCLESEGS
        NS = SUPERSEGS(CYCLESEGS(I))%NSEGS
        CYCPRINT(CC + 1 : CC + NS) = SUPERSEGS(CYCLESEGS(I))%SEGS(:NS)
        CC = CC + NS
      END DO
!    
      WUNIT = 8
      OPEN (UNIT = WUNIT, ACCESS = 'SEQUENTIAL',                        &
     &        FILE = BASENAME(1:LN)//'-cyclesegs.vtk',                  &
     &        FORM = 'FORMATTED', STATUS = 'REPLACE')
     
      WRITE(WUNIT, 510)
  510 FORMAT('# vtk DataFile Version 3.0')
      WRITE(WUNIT, 511)
  511 FORMAT('vtk output')
      WRITE(WUNIT, 512)
  512 FORMAT('ASCII')
      WRITE(WUNIT, 513)
  513 FORMAT('DATASET POLYDATA')
      WRITE(WUNIT, 514) NORIGVERTS
  514 FORMAT('POINTS', I20, ' float')
      DO I = 1, NORIGVERTS
        WRITE(WUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)
      END DO
      WRITE(WUNIT, 515)
  515 FORMAT(' ')
      WRITE(WUNIT, 516) NCYCPRINT, 3 * NCYCPRINT
  516 FORMAT('LINES', I10, ' ', I10)
     
      DO I = 1, NCYCPRINT
        WRITE(WUNIT, *) 2, (SEGS(CYCPRINT(I))%VERTS(J)-1, J = 1, 2)
      END DO     
    
      CLOSE(WUNIT)  
! 
      NSNAPPRINT = 0
      DO I = 1, NSNAPSEGS
        NSNAPPRINT = NSNAPPRINT + SUPERSEGS(SNAPSEGS(I))%NSEGS
      END DO
      ALLOCATE(SNAPPRINT(NSNAPPRINT))
      CC = 0
      DO I = 1, NSNAPSEGS
        NS = SUPERSEGS(SNAPSEGS(I))%NSEGS
        SNAPPRINT(CC + 1 : CC + NS) = SUPERSEGS(SNAPSEGS(I))%SEGS(:NS)
        CC = CC + NS
      END DO
! 
      PRINT *, 'EXPORT SUPERSEGS' 
      WUNIT = 8
      OPEN (UNIT = WUNIT, ACCESS = 'SEQUENTIAL',                        &
     &        FILE = BASENAME(1:LN)//'-snapsegs.vtk',                   &
     &        FORM = 'FORMATTED', STATUS = 'REPLACE')
     
      WRITE(WUNIT, 520)
  520 FORMAT('# vtk DataFile Version 3.0')
      WRITE(WUNIT, 521)
  521 FORMAT('vtk output')
      WRITE(WUNIT, 522)
  522 FORMAT('ASCII')
      WRITE(WUNIT, 523)
  523 FORMAT('DATASET POLYDATA')
      WRITE(WUNIT, 524) NORIGVERTS
  524 FORMAT('POINTS', I20, ' float')
      DO I = 1, NORIGVERTS
        WRITE(WUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)
      END DO
      WRITE(WUNIT, 525)
  525 FORMAT(' ')
      WRITE(WUNIT, 526) NSNAPPRINT, 3 * NSNAPPRINT
  526 FORMAT('LINES', I10, ' ', I10)
     
      DO I = 1, NSNAPPRINT
        WRITE(WUNIT, *) 2, (SEGS(SNAPPRINT(I))%VERTS(J)-1, J = 1, 2)
      END DO    
    
      CLOSE(WUNIT) 
      
      END IF
!       
! **************************************************************************
! **************************************************************************
!       
!     identify bad facets that are associated with supersegs that will be squeezed out
      PRINT *, 'ID BAD FACETS TO KEEP'
!      
      NBADKEEP = SUM(SUPERSEGS(CYCLESEGS(:NCYCLESEGS))%NBADFCTS)
      ALLOCATE(TEMP(NBADKEEP))
      CC = 0
     
      DO IND = 1, NCYCLESEGS
        I = CYCLESEGS(IND)  
!      
        NBADKEEP = CC + SUPERSEGS(I)%NBADFCTS
        TEMP(CC+1:NBADKEEP) = SUPERSEGS(I)%BADFCTS(:SUPERSEGS(I)%NBADFCTS)
        CC = NBADKEEP        
      END DO
!       
      ALLOCATE(BADKEEP(NBADKEEP))
      BADKEEP = TEMP
      CALL QSORT(BADKEEP)      
      CALL RMV_DUPE(BADKEEP, NBADKEEP)
      DEALLOCATE(TEMP)
!       
! **************************************************************************
! **************************************************************************
! 
!     MAKE COPY OF VERTS FOR EACH FACET
! 
      DO I = 1, NFCT
        FCTS(I)%ONVERTS = FCTS(I)%NVERTS
        ALLOCATE(FCTS(I)%OVERTS(FCTS(I)%ONVERTS))
        FCTS(I)%OVERTS(:FCTS(I)%ONVERTS) = FCTS(I)%VERTS(:FCTS(I)%NVERTS)
      END DO
!       
! **************************************************************************
! **************************************************************************
! 
!     CREATE CLEANED BREP
      PRINT *, 'CREATE CLEANED BREP'
! 
!     replace vertex nums to snap out bad segs
!     need a vertex2facet lookup
!     for each superseg, loop through all points of superseg and 
!     find if there are any matches with any good AND BAD facets and replace vnum
! 
      DO IND = 1, NSNAPSEGS
        I = SNAPSEGS(IND)   
        DO L = 1, SUPERSEGS(I)%NVERTS
          VNUM = SUPERSEGS(I)%VERTS(L)
          DO J = 1, VDAT(VNUM)%NFACETS
            FNUM = VDAT(VNUM)%FACETS(J)
              K = VDAT(VNUM)%FINDX(J)
              FCTS(FNUM)%VERTS(K) = NORIGVERTS + IND
          END DO
        END DO
      END DO
!       
!     remove duplicated vertices in all facets
!     modified version from the Rosetta Code
!     (https://www.rosettacode.org/wiki/Remove_duplicate_elements#Fortran)
! 
      DO L = 1, NFCT
        K = 1
        ALLOCATE (TEMP(FCTS(L)%NVERTS))
        TEMP(1) = FCTS(L)%VERTS(1)
        OUTER: DO I = 2, FCTS(L)%NVERTS
          DO J = 1, K
            IF (TEMP(J) == FCTS(L)%VERTS(I)) THEN
             ! Found a match so start looking again
             CYCLE OUTER
            END IF
          END DO
          ! No match found so add it to the output
          K = K + 1
          TEMP(K) = FCTS(L)%VERTS(I)
        END DO OUTER
        FCTS(L)%NVERTS = K
        FCTS(L)%VERTS(:K) = TEMP(:K)
        DEALLOCATE(TEMP)
      END DO
!       
!     remove all good facets with two or fewer vertices
! 
      ALLOCATE(TEMP(NFCT))
      TEMP = 0
      CC = 0
      DO IND = 1, NTRGOODFCTS
        I = TRGOODFCTS(IND)
        IF (FCTS(I)%NVERTS .GT. 2) THEN
          CC = CC + 1
          TEMP(CC) = I
        END IF
      END DO
      NTRGOODFCTS = CC
      TRGOODFCTS = 0
      TRGOODFCTS(:NTRGOODFCTS) = TEMP(:NTRGOODFCTS)
      DEALLOCATE(TEMP)
!       
! **************************************************************************
! **************************************************************************
! 
!     COMPUTE AVG COORDS OF ORIGINAL VERTS
!     COMPUTE AVERAGE, ACTUALLY MAKES FOR BETTER-SHAPED TRIANGLES!
      PRINT *, 'COMPUTE AVG COORDS'
! 
!     DETERMINE FCT INDICES OF GOOD FCTS THAT ARE TRIANGLES VS NOT
!     SAME FOR BAD FCTS
! 
      NFCT = SIZE(FCTS,1)
      NG3 = COUNT(FCTS(TRGOODFCTS(:NTRGOODFCTS))%ONVERTS .EQ. 3)
      NGP = COUNT(FCTS(TRGOODFCTS(:NTRGOODFCTS))%ONVERTS .GT. 3)
      NB3 = COUNT(FCTS(BADKEEP(:NBADKEEP))%ONVERTS .EQ. 3)
      NBP = COUNT(FCTS(BADKEEP(:NBADKEEP))%ONVERTS .GT. 3)
      ALLOCATE(POSTR(NTRGOODFCTS), POSBK(NBADKEEP), G3(NG3), GP(NGP), B3(NB3), BP(NBP))
      DO I = 1, NTRGOODFCTS
        POSTR(I) = TRGOODFCTS(I)
      END DO
      DO I = 1, NBADKEEP
        POSBK(I) = BADKEEP(I)
      END DO
      G3(1:NG3) = PACK(POSTR, FCTS(TRGOODFCTS(:NTRGOODFCTS))%ONVERTS .EQ. 3)
      GP(1:NGP) = PACK(POSTR, FCTS(TRGOODFCTS(:NTRGOODFCTS))%ONVERTS .GT. 3)
      B3(1:NB3) = PACK(POSBK, FCTS(BADKEEP(:NBADKEEP))%ONVERTS .EQ. 3)
      BP(1:NBP) = PACK(POSBK, FCTS(BADKEEP(:NBADKEEP))%ONVERTS .GT. 3)       
      DEALLOCATE (POSTR, POSBK)
!       
!     GENERATE CENTER POINTS 
! 
      NCENVERTS = NGP + NBP
      ALLOCATE (CENVERTS(3,NCENVERTS))
!       
      DO I = 1, NGP
        CENVERTS(1, I) = SUM(VERTS(1, FCTS(GP(I))%OVERTS)) / FCTS(GP(I))%ONVERTS
        CENVERTS(2, I) = SUM(VERTS(2, FCTS(GP(I))%OVERTS)) / FCTS(GP(I))%ONVERTS
        CENVERTS(3, I) = SUM(VERTS(3, FCTS(GP(I))%OVERTS)) / FCTS(GP(I))%ONVERTS      
      END DO
!       
      DO I = 1, NBP
        CENVERTS(1, I + NGP) = SUM(VERTS(1, FCTS(BP(I))%OVERTS)) / FCTS(BP(I))%ONVERTS
        CENVERTS(2, I + NGP) = SUM(VERTS(2, FCTS(BP(I))%OVERTS)) / FCTS(BP(I))%ONVERTS
        CENVERTS(3, I + NGP) = SUM(VERTS(3, FCTS(BP(I))%OVERTS)) / FCTS(BP(I))%ONVERTS
      END DO
!       
! **************************************************************************
! **************************************************************************      
!       
!     triangulate good facets
      PRINT *, 'TRIANGULATE'
!    
      NTRIFCTS = 0
      DO I = 1, NGP
        NTRIFCTS = NTRIFCTS + FCTS(GP(I))%NVERTS
      END DO
      NTRIBFCTS = 0
      DO I = 1, NBP
        NTRIBFCTS = NTRIBFCTS + FCTS(BP(I))%NVERTS
      END DO
      NNEWFCTS = NTRIFCTS + NTRIBFCTS
!       
      ALLOCATE(NEWFCTS(NNEWFCTS))
      NNEWFCTCOUNT = 0
      DO I = 1, NGP
        DO J = 1, FCTS(GP(I))%NVERTS - 1
          NNEWFCTCOUNT = NNEWFCTCOUNT + 1
          NEWFCTS(NNEWFCTCOUNT)%NVERTS = 3
          ALLOCATE(NEWFCTS(NNEWFCTCOUNT)%VERTS(3))
          NEWFCTS(NNEWFCTCOUNT)%VERTS(1) = NVERTS +I
          NEWFCTS(NNEWFCTCOUNT)%VERTS(2) = FCTS(GP(I))%VERTS(J)
          NEWFCTS(NNEWFCTCOUNT)%VERTS(3) = FCTS(GP(I))%VERTS(J+1)
          NEWFCTS(NNEWFCTCOUNT)%PARENT = GP(I)
        END DO
        NNEWFCTCOUNT = NNEWFCTCOUNT + 1
        NEWFCTS(NNEWFCTCOUNT)%NVERTS = 3
        ALLOCATE(NEWFCTS(NNEWFCTCOUNT)%VERTS(3))
        NEWFCTS(NNEWFCTCOUNT)%VERTS(1) = NVERTS + I
        NV = FCTS(GP(I))%NVERTS
        NEWFCTS(NNEWFCTCOUNT)%VERTS(2) = FCTS(GP(I))%VERTS(NV)
        NEWFCTS(NNEWFCTCOUNT)%VERTS(3) = FCTS(GP(I))%VERTS(1)
        NEWFCTS(NNEWFCTCOUNT)%PARENT = GP(I)   
       
      END DO
!       
!     triangulate bad facets that we'll keep too
! 
      DO IND = 1, NBP
        I = IND + NGP
        DO J = 1, FCTS(BP(IND))%NVERTS - 1
          NNEWFCTCOUNT = NNEWFCTCOUNT + 1
          NEWFCTS(NNEWFCTCOUNT)%NVERTS = 3
          ALLOCATE(NEWFCTS(NNEWFCTCOUNT)%VERTS(3))
          NEWFCTS(NNEWFCTCOUNT)%VERTS(1) = NVERTS + I
          NEWFCTS(NNEWFCTCOUNT)%VERTS(2) = FCTS(BP(IND))%VERTS(J)
          NEWFCTS(NNEWFCTCOUNT)%VERTS(3) = FCTS(BP(IND))%VERTS(J+1)
          NEWFCTS(NNEWFCTCOUNT)%PARENT = BP(IND)
        END DO
        NNEWFCTCOUNT = NNEWFCTCOUNT + 1
        NEWFCTS(NNEWFCTCOUNT)%NVERTS = 3
        ALLOCATE(NEWFCTS(NNEWFCTCOUNT)%VERTS(3))
        NEWFCTS(NNEWFCTCOUNT)%VERTS(1) = NVERTS + I
        NV = FCTS(BP(IND))%NVERTS
        NEWFCTS(NNEWFCTCOUNT)%VERTS(2) = FCTS(BP(IND))%VERTS(NV)
        NEWFCTS(NNEWFCTCOUNT)%VERTS(3) = FCTS(BP(IND))%VERTS(1)
        NEWFCTS(NNEWFCTCOUNT)%PARENT = BP(IND)
      END DO
! 
! **************************************************************************
! **************************************************************************
! 
!     EXPORT TRIANGULATED BREP
      PRINT *, 'EXPORT TRIANGULATED BREP'
! 
      NVERTS = NVERTS + NCENVERTS
      ALLOCATE(TEMPR(3,NORIGVERTS + NSSVERTS))
      TEMPR = VERTS
      DEALLOCATE(VERTS)
      ALLOCATE(VERTS(3,NVERTS))
      VERTS(:,:(NORIGVERTS + NSSVERTS)) = TEMPR
      VERTS(:,(NORIGVERTS + NSSVERTS)+1:NVERTS) = CENVERTS
      DEALLOCATE(TEMPR)      
!       
!     write out triangulated facets to vtk 
! 
      WUNIT = 8
      OPEN (UNIT = WUNIT, ACCESS = 'SEQUENTIAL',                        &
     &        FILE = BASENAME(1:LN)//'-fine.vtk',                       &
     &        FORM = 'FORMATTED', STATUS = 'REPLACE')
     
      WRITE(WUNIT, 570)
  570 FORMAT('# vtk DataFile Version 2.0')
      WRITE(WUNIT, 571)
  571 FORMAT('vtk output')
      WRITE(WUNIT, 572)
  572 FORMAT('ASCII')
      WRITE(WUNIT, 573)
  573 FORMAT('DATASET POLYDATA')
      WRITE(WUNIT, 574) NVERTS
  574 FORMAT('POINTS', I20, ' float')
      DO I = 1, NVERTS
        WRITE(WUNIT, *) VERTS(1,I), VERTS(2,I), VERTS(3,I)
      END DO
      WRITE(WUNIT, 575)
  575 FORMAT(' ')
!   
      WRITE(WUNIT, 576) NG3 + NB3 + NNEWFCTS, (NG3 + NB3 + NNEWFCTS) * 4
  576 FORMAT('POLYGONS', I10, ' ', I10)
      DO I = 1, NG3
        WRITE(WUNIT, *) 3, (FCTS(G3(I))%VERTS(J)-1,                     &
     &                   J = 1, FCTS(G3(I))%NVERTS)
      END DO  
      DO I = 1, NB3
        WRITE(WUNIT, *) 3, (FCTS(B3(I))%VERTS(J)-1,                     &
     &                  J = 1, FCTS(B3(I))%NVERTS)    
      END DO   
      DO I = 1, NNEWFCTS
        WRITE(WUNIT, *) 3, (NEWFCTS(I)%VERTS(J)-1,                      &
     &                     J = 1, NEWFCTS(I)%NVERTS)
      END DO  

      CLOSE(WUNIT)  
      
      DEALLOCATE (GOODFCTS, BADFCTS, TRGOODFCTS, SQUEEZE, VISITED,      &
     &            SUPERSEGS, CYCLESEGS, SNAPSEGS, MARKS, BVINDICES,     &
     &            SEGS, BVNI, FACETS, BREPFCTS, GBREPFCTS, BBREPFCTS,   &
     &            BREPMARKS, SSVERTS, VERTS, VDAT, CELLS, FCTS,         &
     &            BADKEEP, NEWFCTS, CENVERTS, BADSEGS)      

!=======================================================================
      STOP
!=======================================================================
! 
      END PROGRAM QVOR2VTK
