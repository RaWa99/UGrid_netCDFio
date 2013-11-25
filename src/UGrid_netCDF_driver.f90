!******************************************************************************
!******************************************************************************
!  Driver to test GridnetCDFio module. This program reads a test data set
!  (dep.xye), generates a netcdf file, then reads the file and checks for
!  discrepancies.
!    np = number of nodes
!    ne = number of elements
!    ncn = number of element vertices
!    iXYcoord = 0 for spherical polar coordinates, otherwise Cartesian coordinates
!    iZcoord = 0 for depth positive down, otherwise depth positive up
!    ncode are node attribute identifiers
!    ecode are element attribute identifiers
!    nen(ncn,ne) is the element vertex list in CCW order
!******************************************************************************
!******************************************************************************
     
Program Test_UGRID_netCDF

!    Create netCDF file to contain grid binary output from PreMod 

USE UGrid_netCDFio
implicit none


! Local variables
integer j,k,name_len

! input file variables
integer ne,np,ncn,stat,ndummy,iXYcoord,iZcoord
integer ::  ncode(1000),ecode(1000)
integer ::  nen(3,1000), nen4(4,1000),nadj(20,1000)
integer :: nbp,nbnd,nbnd1,numadj,maxadj=20
integer bnode_index(1000),bnode_id(1000),bnodes(1000)
real*8 ::  x(1000),y(1000),depth(1000)
character(len=256) fname
character(len=80) outresfile
logical err

! *** first read binary file

! *** open output file
    write(*,*) ' Enter filename for input file'
    read(*,'(a)') fname
    open(22,file=fname,status='old',iostat=stat)
    if(stat.ne.0) then
      stop 'Unable to open file'
    endif

    name_len=len_trim(fname)
    OutResFile = 'grid_'//trim(fname(1:name_len-4))//'.nc'

! *** read indices
    read(22,*) np,ne,ncn,iXYcoord,iZcoord

    call Create_Grid_netCDF (np,ne,ncn,iXYcoord,iZcoord,outresfile,err)
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif

! *** read the coordinates      
    do j=1,np
      read(22,*) x(j), y(j), depth(j), ncode(j)
    enddo

! *** and the triangles
    if(ncn.eq.4) then
      do j=1,ne
        read(22,*) (NEN4(k,j),k=1,4),ecode(j)
      enddo
    elseif(ncn.eq.3) then
      do j=1,ne
        read(22,*) (NEN(k,j),k=1,3),ndummy,ecode(j)
      enddo
      nen4 = 0
      do j=1,ne
        do k=1,3
          nen4(k,j) = nen(k,j)  !code uses 4 node elements below
        enddo
      enddo
    else
      write(*,*) ' Invalid value for ncn:',ncn
    endif
    close(22)
   
    if(ncn.eq.4) then
      call Write_Grid_netCDF (np,ne,ncn,x,y,depth,ncode,ecode,nen4,err)
    elseif(ncn.eq.3) then
      call Write_Grid_netCDF (np,ne,ncn,x,y,depth,ncode,ecode,nen,err)
    endif
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif

! *** generate boundary data from element_face_connectivity and node_code

      call GenerateNL(np,maxadj,numadj,nadj,ne,nen4)
        
      call Boundary_list(np,nadj,maxadj,x,y,ncode,&
                                 nbp,nbnd,bnode_index,bnode_id,bnodes)

    
    call Write_Boundary_netCDF (nbp,nbnd,bnode_index,bnode_id,bnodes,err)
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif
    
    call Close_netCDF (err)
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif

! *** end writing, now zero variables and read back in to test

    np = 0
    ne = 0
    ncn = 0
    x = 0.
    y = 0.
    depth = 0.
    ncode = 0
    ne  = 0
    ecode = 0

    write(*,*) ' Reading file'
    call Read_GridSize_netCDF ( outresfile,np,ne,ncn,err )
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif
    write(*,*) ' np,ne,ncn=',np,ne,ncn

    if(ncn.eq.3) then
      call Read_Grid_netCDF ( np,ne,ncn,x,y,depth,ncode,ecode,nen,iXYcoord,iZcoord,err )
    elseif(ncn.eq.4) then
      call Read_Grid_netCDF ( np,ne,ncn,x,y,depth,ncode,ecode,nen4,iXYcoord,iZcoord,err )
    endif
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif
    write(*,*) ' xycoordinate=',iXYcoord
    write(*,*) ' zcoordinate=',iZcoord

    call Read_BoundarySize_netCDF (nbp,nbnd,nbnd1,err)
    
    call Read_Boundary_netCDF (nbp,nbnd,bnode_index,bnode_id,bnodes,err)
    
    call Close_netCDF (err)
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif

! *** write out a test comparison

    open(23,file='testout.dat',status='unknown')
    write(23,'(5i10)') np,ne,ncn,iXYcoord,iZcoord
    do j=1,np
      write(23,'(3(1PE15.7),i5)') x(j), y(j), depth(j), ncode(j)
    enddo
    
    if(ncn.eq.4) then
      do j=1,ne
        write(23,'(5i10)') (NEN4(k,j),k=1,4),ecode(j)
      enddo
    else
      do j=1,ne
        write(23,'(5i10)') (NEN(k,j),k=1,3),ndummy,ecode(j)
      enddo
    endif
    close(23)

! *** write out a test comparison for boundary data
! *** replace node_code with values from bnode_id. 
! *** the  file should be identical to the previous file.

    do j=1,nbnd
      do k=bnode_index(j),bnode_index(j+1)-1
        ncode(bnodes(k)) = bnode_id(j)
      enddo
    enddo

    open(23,file='testbndout.dat',status='unknown')
    write(23,'(5i10)') np,ne,ncn,iXYcoord,iZcoord
    do j=1,np
      write(23,'(3(1PE15.7),i5)') x(j), y(j), depth(j), ncode(j)
    enddo
    
    if(ncn.eq.4) then
      do j=1,ne
        write(23,'(5i10)') (NEN4(k,j),k=1,4),ecode(j)
      enddo
    else
      do j=1,ne
        write(23,'(5i10)') (NEN(k,j),k=1,3),ndummy,ecode(j)
      enddo
    endif
    close(23)

end

!********************************************************************************

      SUBROUTINE GenerateNL(np,maxngh,numngh,nbrs,ne,nen)

! ***********************************************************************
! This routine converts from triangle list (TRIANG 
! format) and node file (NODE format) to NEIGH format
! ******************************************************

      IMPLICIT NONE

! *** passed VARIABLES ***
!      integer nindx
      integer np,maxngh,numngh,nbrs(maxngh,np)
      integer ne,nen(4,ne)

! *** LOCAL VARIABLES ***
      integer ncn2, nonbrs(np)
!   CUVX - current vertex being considered
      INTEGER CUVX
!   CUNBR - current neighbour being considered
      INTEGER CUNBR
      INTEGER II,JJ,KK,LL,MM
      LOGICAL pass1, NEWNBR
      character cstr*80

!   Starting neighbor list

!   Set count of nbrs to zero for all nodes

      DO KK = 1, np
        NONBRS(KK) = 0
        DO JJ = 1, maxngh
          nbrs(JJ,KK) = 0
        enddo
      enddo

! *** Check each triangle and check that each vertex is in each of 
! *** the other two vertices' neighbour lists

      pass1 = .true.
      DO JJ = 1, ne
        ncn2 = 3 + min0(1,nen(4,JJ))
! *** Check each vertex in current triangle
        DO II = 1, ncn2
! *** Choose current vertex
          CUVX = nen(II,JJ)
! *** Take other two vertices in turn
          DO LL = 1,ncn2
            IF(LL.eq.II) cycle
!            Choose current neighbour of chosen vertex 
            CUNBR = nen(LL,JJ)
!            Check if CUNBR is already in neighbour list of CUVX
            NEWNBR = .TRUE.
            IF(NONBRS(CUVX).ne.0) then
              DO MM = 1, NONBRS(CUVX)
                IF(CUNBR.eq.nbrs(MM,CUVX)) NEWNBR = .FALSE.
              enddo
            endif
!            If CUNBR is new neighbour of CUVX, add to list
            IF(NEWNBR) THEN     
              if(nonbrs(cuvx).ge.maxngh) then
                if(pass1) then
                  cstr =' Too many neighbor points - truncating:'
!                    call PigMessageOK(cstr, 'Error')
                  pass1 = .false.
                endif
              else
                NONBRS(CUVX) = NONBRS(CUVX) + 1
                nbrs(NONBRS(CUVX),CUVX) = CUNBR
              endif
            ENDIF
          enddo
        enddo
      enddo

!       Find max number of neighbours
      numngh = maxval(nonbrs)

      RETURN
      END

!********************************************************************************
        
      SUBROUTINE Boundary_list(itot,nl,nbtot,dxray,dyray,ncode,&
                                 nbp,nbnd,bnode_index, bnode_id,rnodes)

      IMPLICIT NONE

!       WARNING - Boundary nodes retrieved in RETRONODES are stored here in
!                 local array RNODES( ). The nodes for each boundary are in a
!                 consecutive block, but the order of the nodes within each 
!                 each block is the reverse of the order usual in NODE format
!                 files, i.e. in rnodes( ), the outer boundary is in clockwise 
!                 order and islands are in counterclockwise order. Also, the outer 
!                 boundary is not necessarily the first block & rnodes( ) does not
!                 have the interior nodes in it. If the RETRONODE function in
!                 GRIDGEN is supposed to produce a properly ordered set of nodes,
!                 equivalent to reading in a NODE format file, then 3 further 
!                 steps need to be implemented:
!                  1) Put outer boundary first
!                  2) Reverse order of nodes in each boundary  
!                  3) Add interior nodes to (retro) node array

    
!       Purpose: To form a boundary list for a grid.
!                - Some error detection capability.
!
!       Input variables
!       itot   = number of nodes in grid
!       nbtot   = number adjacent nodes


! *** Passed variables ***

      integer, intent(in) :: itot,nbtot,ncode(itot)
      integer, intent(inout) :: nl(nbtot,itot+1)
      INTEGER, intent(out) :: RNODES(2*itot),nbnd
      integer, intent(out) :: nbp, bnode_index(itot+1), bnode_id(itot)
      real*8, intent(in) :: dxray(itot), dyray(itot)


! *** Local variables ***

      INTEGER NextVer                     ! Integer functions
      integer ListTr(4)
      INTEGER IN, SNB(NBTOT+1)
      INTEGER numverts, ThisJ, NextCcwNbr, NextJ, NextVertex
      INTEGER I,J,K,M
!      LOGICAL ELIVE

!      ListTr(1, ) to ListTr( ,4) are nodes of element in ccw order

      INTEGER NUMN(itot)   ! no. of neighbours of each node

!---  variables added 25/9/98 for RETRONODE operation ------------------------

      integer, parameter :: maxnnb=10000
      integer, save :: NUMperBDY(MAXNNB)
      INTEGER Nfound,NUMofBDYS

!     PASSED variable added 25/9/98 for RETRONODE operation
      LOGICAL :: retrowanted=.true.

!    Local variables added 25/9/98 for RETRONODE operation
      INTEGER firstnode
      LOGICAL followboundary

!     Local variables added 25/9/98 for output of retrieved node file
      INTEGER  sumofnodes, numouter, presum
      REAL*8 minimumx

!---  end of extra variable section ------------------------------------------

! ----BEGIN------------------------------------------------------------

!     number of boundaries so far detected
      NUMofBDYS = 0
!     number of boundary nodes found so far
      Nfound = 0
      NUMperBDY = 0

!     Count sort (ccw from east) the neighbours of each node 
      do I = 1, itot
        numn(I) = 0
        call SORTNBRS(I,NUMN(I),SNB,itot,nbtot,nl,dxray,dyray)
        do J = 1, NUMN(I)
          NL(J,I) = SNB(J)
        enddo
        if(numn(i).lt.nbtot) then
          do j=numn(i)+1,nbtot
            nl(j,i) = 0
          enddo
        endif
      enddo

!      Neighbour lists now sorted ccw, counted and packed left
!      (no need to set unused neighbours spaces at right to zero)

!     Set element counts to zero
      ListTr = 0
 !     LISTERR = 0

! ----------------------------------------------------------------------
!     BEGIN SEARCH FOR ELEMENTS
!     For each node in grid
      DO IN = 1, itot

!       first look after degenerate elements, nodes with 0 or 1 neighbours 
        if (numn(IN).lt.2) then
!           case of node with no or one neighbour.
          cycle
        endif

!       - from here on, node IN assumed to have at least 2 neighbours
!      Begin element check around each node, starting from first neighbour
        j = 1
!        do 60 while (j.le.numn(IN))
        do while (j.le.numn(IN))
          ThisJ = j
!          ELIVE = .true.      ! element still valid

!      begin new element with line from node IN to its Jth neighbour
          ListTr(1) = IN        ! first node of fresh element is node IN
          ListTr(2) = NL(j,IN)
!         kill element if lower-numbered node is encountered      
          if(ListTr(2).lt.IN) then
            j = j + 1
            cycle !go to 60
          endif

          numverts = 2

!        Find  next neighbour of IN in ccw direction for later check
!                                                 of polygon closure

          call CwwNbr(IN,ThisJ,NextCcwNbr,NextJ,numn(in),nbtot,itot,nl)

!         Find consecutive vertices of polygon in ccw direction until
!         polygon closes or terminates for some reason
!         - maybe make NextVer return an error indicator
!         nextvertex = NextVer(IN,NB(IN,j))
          nextvertex = NextVer(IN,NL(j,IN),numn(NL(j,IN)),nl,nbtot,itot)
          ListTr(3) = nextvertex

!         kill element if lower-numbered node is encountered      
          if(ListTr(3).le.IN) then
            j = j + 1
            cycle !go to 60
          endif
          if (nextvertex.eq.NextCcwNbr) then
!         element is triangle
            ListTr(4) = 0
            j = j + 1
            cycle !go to 60
          else
!           ELEMENT IS POLYGON with more than 3 sides or is not closed
            nextvertex = NextVer(NL(j,IN),ListTr(3), &
                                numn(ListTr(3)),nl,nbtot,itot)
!           kill element if lower-numbered node is encountered      
            if(nextvertex.lt.IN) then
              j = j + 1
              cycle !go to 60
            endif
            ListTr(4) = nextvertex

            if (nextvertex.eq.NextCcwNbr) then
!           element is quadrilateral
              j = j + 1
              cycle !go to 60
            else
              if(retrowanted) then
                followboundary = .true.
!          element has 5 or more sides - assume it is a boundary, not an element
                NUMofBDYS = NUMofBDYS + 1
!      store these 4 nodes top down in array RNODE( ) - note that outer boundary
!      will thus be in clockwise order and islands in ccw order. Dirns. will have 
!      to be reversed on output
                do m = 1,4
                  rnodes(Nfound+m) = ListTr(m)
                end do
                Nfound = Nfound + 4
                NUMperBDY(NUMofBDYS) = 4
                firstnode = ListTr(1)

!           look for 5th, 6th, ... nodes on this boundary
                do while(followboundary)
                  nextvertex = NextVer(rnodes(Nfound-1),rnodes(Nfound), &
     &                        numn(rnodes(Nfound)),nl,nbtot,itot)
!             kill element if lower-numbered node is encountered      
                  if(nextvertex.lt.IN) then
                    Nfound = Nfound - NUMperBDY(NUMofBDYS)
                    NUMofBDYS = NUMofBDYS - 1
!                    elcount = elcount - 1
                    j = j + 1
                    cycle !go to 60
                  endif
                  if(nextvertex.ne.firstnode) then
!               boundary is not yet closed, add node to list of boundary nodes
                    Nfound = Nfound + 1
                    rnodes(Nfound) = nextvertex
                    NUMperBDY(NUMofBDYS) = NUMperBDY(NUMofBDYS) + 1

                  else
!             boundary now closed
                    followboundary = .false.               
                  endif
                enddo
              endif
!              elcount = elcount - 1
              j = j + 1
              cycle !go to 60
            endif
          endif

!60      enddo
        enddo

      enddo  !itot loop

!      rnodes( ) holds the various boundaries, each in reverse of required order.
!     
!            First boundary has NUMperBDY(1) nodes, which are in
!              rnodes(1),...,rnodes(NUMperBDY(1))
!            Second boundary has NUMperBDY(2) nodes, which are in
!              rnodes(NUMperBDY(1)+1),....,rnodes(NUMperBDY(1)+(NUMperBDY(2))
!            and so on.

!      First step is to determine which of the NUMofBDYS boundaries is the outer.
!      Any of the extreme values must lie on outer boundary
!        - find minimum x-coordinate over all boundary nodes
        minimumx = 1.E+20
        do k = 1, Nfound
          if(minimumx.lt.dxray(rnodes(k))) minimumx = dxray(rnodes(k))
        end do

!      now find which boundary has the node with minimum x
        sumofnodes = 0
        do k = 1, NUMofBDYS
          do m = sumofnodes+1,sumofnodes+ NUMperBDY(k)
            if(dxray(rnodes(m)).le.minimumx) then
              numouter = k
!           the numouter-th boundary is the outer one
              presum = sumofnodes
!           presum is number of nodes in rnodes( ) prior to start of outer boundaary
!           outer boundary (in ccw order) consists of nodes
!           rnodes(presum+NUMperBNDY(numouter)) to rnodes(presum+1) 
              go to 444
            endif
          end do
          sumofnodes = sumofnodes + NUMperBDY(k)
        end do
444     continue

        sumofnodes = 0
        nbnd = 0
        do k = 1, NUMofBDYS
          nbnd = nbnd + 1
          bnode_index(nbnd) = sumofnodes + 1
          bnode_id(nbnd) = ncode(rnodes(sumofnodes + 1))
          do m = sumofnodes+1,sumofnodes+ NUMperBDY(k)
            if(ncode(rnodes(m)).ne.bnode_id(nbnd)) then !new segment
              nbnd = nbnd + 1
              bnode_index(nbnd) = m
              bnode_id(nbnd) = ncode(rnodes(m))
            endif
          end do
          sumofnodes = sumofnodes + NUMperBDY(k)
        end do
        nbp = sumofnodes
        bnode_index(nbnd+1) = sumofnodes + 1
        
        return
        
! *** test boundary
      open(41,file='btestout.dat',status='unknown')
      write(41,*) sumofnodes
      write(41,*) NUMofBDYS
      m = 0
      do k=1,NUMofBDYS
        write(41,*) NUMperBDY(k)
        do j=1,NUMperBDY(k)
          m = m + 1
          write(41,*) dxray(rnodes(m)),dyray(rnodes(m)),ncode(rnodes(m))
        enddo
      enddo
      m = 0
      write(41,*) m
      close(41)

      RETURN
      END

!************************************************************************
      
      SUBROUTINE CwwNbr(INODE,ThisJ,NextCcwNbr,NextJ,numin,nbm,nm,nl)

!     Purpose: Given position ThisJ in neighbour list of node INODE
!              routine returns the next neighbour of INODE counter-
!              clockwise, i.e. NextCcwNbr, and its position, NextJ, in
!              neighbour list of INODE, allowing for cyclic nature of 
!              list and assuming neighbours of all nodes already ccw

      IMPLICIT NONE

! *** Passed variables ***
      INTEGER INODE, ThisJ, numin, nbm, nm
      integer nl(nbm,nm+1)

! *** Local variables
      INTEGER NextJ, NextCcwNbr

! ----BEGIN------------------------------------------------------------

      if (ThisJ.lt.numin) then !numn(INODE))then
        NextJ = ThisJ + 1
      else
        NextJ = 1
      endif

      NextCcwNbr = NL(NextJ,INODE)

      RETURN
      END
      
!************************************************************************
      
      INTEGER FUNCTION NextVer(NODE, NBR, numnbr, nl, nbm, nm)

!     Purpose: Given some neighbour, with index NBR, of node NODE -
!              routine returns the next neighbour of NBR clockwise 
!              from NODE (generally not same as next neighbour of NODE 
!              ccw from NBR).
!              Assumes neighbours of all nodes already sorted ccw
! 
!     Use: When finding the vertices of a polygon in ccw direction,
!          next node is NextVer (thisnode,lastnode)

      IMPLICIT NONE

! *** Passed variables ***
      INTEGER NODE, NBR, numnbr, nbm,nm
      integer nl(nbm,nm+1)

! *** Local variables
      INTEGER K

! ----BEGIN------------------------------------------------------------

!     Find where NODE sits in neighbour list of NBR, and take preceding
!                                  neighbour in clockwise direction
      NextVer = 0  ! kill compiler warning
      do k = 1, numnbr  !NUMN(NBR)
        if (NODE.eq.NL(k,NBR)) then
!         thus NODE is kth neighbour of NBR
          if (k.ne.1) then 
            NextVer = NL(k-1,NBR)
          else
            NextVer = NL(numnbr,nbr)  !NUMN(NBR),NBR)
          endif
        endif
      enddo

      RETURN
      END
      
!************************************************************************
      
      SUBROUTINE SORTNBRS(INODE,NBRS,SNB,nm,nbtot,nl,x,y)

!       Purpose: To sort neighbours of current node INODE into
!                counterclockwise order

      IMPLICIT NONE

! *** PASSED VARIABLES

      integer nm,nbtot,nl(nbtot,nm+1)
      integer INODE, NBRS, SNB(NBTOT+1)
      real*8 x(nm),y(nm)
!       INODE - index of current node
!       NBRS  - number of neighbours of current node
!       SNB( ) - sorted neighbours & SNB(NBRS+1) = SNB(1)

! *** LOCAL VARIABLES

      REAL*8 XDIFF,YDIFF,X1,Y1,X2,Y2
      integer I,J,N, TNb1, TNb2
      integer OCT,UNB(NBTOT),NUinOCT(8),NBinOCT(8,nbtot)
!       OCT - octant counter
!       UNB( ) - packed, unsorted neighbours  
!       NUinOCT(I) - No. of neighbours found in octant I
!       NBinOCT(I, ) - Indices of neighbours found in octant I
!       TNb1( ) - temporary first nbr in sort within octant
!       TNb2( ) - temporary second nbr in sort within octant

! ----BEGIN------------------------------------------------------------
!     Find total no. of nbrs of INODE and form packed unsorted list
      NBRS = 0
      unb = 0
      snb = 0
      DO I = 1, NBTOT
        IF(NL(I,INODE).GT.0) THEN
          NBRS = NBRS + 1
          UNB(NBRS) = NL(I,INODE)
        ENDIF
      enddo

!     Set counts of neighbours in octants to zero
!      DO I = 1,8
        NUinOCT = 0
        NBinOCT = 0
!      enddo

!     Sort nbrs into octants, update counts, store nbrs by octant
      DO I = 1, NBRS
        XDIFF = X(UNB(I)) - X(INODE)
        YDIFF = Y(UNB(I)) - Y(INODE)
        CALL FINDOCT(OCT,XDIFF,YDIFF) !INODE,UNB(I))
        NUinOCT(OCT) = NUinOCT(OCT) + 1
        NBinOCT(OCT,NUinOCT(OCT)) = UNB(I)
      enddo

!     If more than one nbr in octant, do ccw sort within octant

      N = 0

!         DO 30 OCT = 1,8
      OCT = 1
      DO 30 WHILE ((OCT .LE. 8) .AND. (N.LT.NBTOT)) 

        IF ( NUinOCT(OCT) .EQ. 1 ) THEN
          N = N + 1
          SNB(N) = NBinOCT(OCT,1)
        ELSEIF (NUinOCT(OCT) .gt. 1 ) THEN
          DO 20 I = 1, NUinOCT(OCT)          
            DO 20 J = 1, NUinOCT(OCT) - I     
              TNb1 = NBinOCT(OCT,J)
              TNb2 = NBinOCT(OCT,J+1)
              X1 = X(TNb1) - X(INODE)
              X2 = X(TNb2) - X(INODE)
              Y1 = Y(TNb1) - Y(INODE)
              Y2 = Y(TNb2) - Y(INODE)
!               Check cw v.ccw order by vector cross-product
              IF(X1*Y2-X2*Y1.LT.0)THEN 
                NBinOCT(OCT,J) = TNb2
                NBinOCT(OCT,J+1) = TNb1
              ENDIF
20        CONTINUE
          DO 25 I = 1, NUinOCT(OCT)
            N = N + 1
            IF (N.LE.NBTOT) THEN
              SNb(N) = NBinOCT(OCT,I)
            ENDIF
25        CONTINUE

        ENDIF

!         Following reset is necessary
        NUinOCT(OCT)=0
        OCT = OCT +1
30    CONTINUE

      SNb(N+1) = SNb(1)
!       
      RETURN
      END

! *********************************************************************
      
      SUBROUTINE FINDOCT(OCT, X, Y ) !P1, P2)

!     Purpose: Finds which octant (45 degree sector) node P2 lies in
!               relative to node P1, by first sorting into quadrants,
!               then rotating axes 45 degrees and sorting into new
!               quadrants. Octants numbered counterclockwise from ENE

      IMPLICIT NONE

! *** PASSED VARIABLES ***
      integer   OCT !, P1, P2
      REAL*8    X, Y

! *** LOCAL VARIABLES ***   
      LOGICAL Up, Rite, UpRite, LeftUP

! ----BEGIN------------------------------------------------------------

      Up = Y .GE. 0
      Rite = X .GE. 0
      UpRite = Y+X .GE. 0
      LeftUp = Y-X .GE. 0

      IF ( Up ) THEN
        IF ( Rite ) THEN
          IF (.NOT. LeftUp ) THEN
            OCT = 1
          ELSE
            OCT = 2
          ENDIF
        ELSE 
          IF ( UpRite ) THEN
            OCT = 3
          ELSE
            OCT = 4
          ENDIF
        ENDIF
      ELSE
        IF (.NOT. Rite ) THEN
          IF ( LeftUp ) THEN
            OCT = 5
          ELSE
            OCT = 6
          ENDIF
        ELSE
          IF (.NOT. UpRite ) THEN
            OCT = 7
          ELSE
            OCT = 8
          ENDIF
        ENDIF
      ENDIF

      end

!********************************************************************************
