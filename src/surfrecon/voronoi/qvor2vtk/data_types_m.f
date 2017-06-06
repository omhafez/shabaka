      MODULE DATA_TYPES_M
! 
!     This module defines the various geometric types needed to define
!     a Voronoi partition
! 
!=======================================================================
! 
      USE KINDS_M
      IMPLICIT NONE
!-----------------------------------------------------------------------
! 
      TYPE :: VERT
        INTEGER :: LOC(3) = 0
        INTEGER :: NFACETS = 0
        INTEGER :: FACETS(50) = 0
        INTEGER :: FINDX(50) = 0
        INTEGER :: NGFACETS = 0
        INTEGER :: GFACETS(50) = 0
        INTEGER :: GFINDX(50) = 0        
        INTEGER :: NNEIGHBS = 0
        INTEGER :: NEIGHBS(50) = 0
        INTEGER :: SEGS(50) = 0
        INTEGER :: NBNEIGHBS = 0
        INTEGER :: BNEIGHBS(50) = 0
        INTEGER :: BSEGS(50) = 0   
      END TYPE VERT
!       
      TYPE :: SEG
        INTEGER :: FACETS(2) = 0
        INTEGER :: VERTS(2) = 0
      END TYPE SEG
!       
      TYPE :: SPSEG
        INTEGER :: NVERTS = 0
        INTEGER :: NTERMINAL = 0
        INTEGER :: NBADFCTS = 0
        INTEGER :: NSEGS = 0
        INTEGER, ALLOCATABLE :: VERTS(:)
        INTEGER, ALLOCATABLE :: TERMINALINDX(:)
        INTEGER, ALLOCATABLE :: BADFCTS(:)
        INTEGER, ALLOCATABLE :: SEGS(:)
      END TYPE SPSEG
! 
      TYPE :: FACET
        INTEGER :: SITES(2)
        REAL (DBL) :: AREA, NRML(3)
        INTEGER :: NVERTS = 0, ONVERTS = 0
        INTEGER :: NSEGS = 0, SEGCOUNT = 0
        INTEGER :: NSSEGS = 0, NSNAPSSEGS = 0
        INTEGER :: PARENT = 0, NSNAPPNTS = 0, MYVERT = 0
        INTEGER, ALLOCATABLE :: VERTS(:), OVERTS(:), SEGS(:)
        INTEGER :: SSEGS(50) = 0, SNAPSSEGS(50) = 0
      END TYPE FACET
! 
      TYPE :: CELL
        REAL (DBL) :: SITE(3)
        INTEGER :: MATNUM
        INTEGER :: NVERTS = 0
        INTEGER :: TOTVERTS = 0
        INTEGER :: NFACETS = 0
        INTEGER, ALLOCATABLE :: VERTS(:)
        INTEGER, ALLOCATABLE :: FACETNUMS(:)
        TYPE (FACET), ALLOCATABLE :: FACETS(:)
      END TYPE CELL
!         
!=======================================================================
! 
      END MODULE DATA_TYPES_M
      