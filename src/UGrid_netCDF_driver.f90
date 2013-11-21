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
     
Program ConvertBin2netCDF

!    Create netCDF file to contain grid binary output from PreMod 

USE UGrid_netCDFio
implicit none


! Local variables
integer status,j,k,name_len

! input file variables
integer ne,np,ncn,stat,ndummy,iXYcoord,iZcoord
integer, allocatable ::  nen(:,:),ncode(:),ecode(:)
real*8, allocatable ::  x(:),y(:),depth(:)
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

    call Create_Grid_netCDF (np,ne,ncn,iXYcoord,iZcoord,outresfile,fname,err)
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif

! *** allocate arrays
    ALLOCATE (x(np),y(np),depth(np),ncode(np),ecode(ne),nen(ncn,ne), STAT = status ) 
    if(status.ne.0) then
      write(*,*) 'FATAL ERROR: Cannot allocate main storage arrays'
      stop
    endif

! *** read the coordinates      
    do j=1,np
      read(22,*) x(j), y(j), depth(j), ncode(j)
    enddo

! *** and the triangles
    if(ncn.eq.4) then
      do j=1,ne
        read(22,*) (NEN(k,j),k=1,4),ecode(j)
      enddo
    elseif(ncn.eq.3) then
       do j=1,ne
        read(22,*) (NEN(k,j),k=1,3),ndummy,ecode(j)
      enddo
    else
      write(*,*) ' Invalid value for ncn:',ncn
    endif
    close(22)
   
    call Write_Grid_netCDF (np,ne,ncn,x,y,depth,ncode,ecode,nen,err)
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif
    
    write(*,*) ' Reading file'
    call Read_GridSize_netCDF ( outresfile,np,ne,ncn,err )
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif
    write(*,*) ' np,ne,ncn=',np,ne,ncn

    call Read_Grid_netCDF ( np,ne,ncn,x,y,depth,ncode,ecode,nen,iXYcoord,iZcoord,err )
    if(err) then
      write(*,*) ' Error encountered, aborting...'
      stop
    endif
    write(*,*) ' xycoordinate=',iXYcoord
    write(*,*) ' zcoordinate=',iZcoord

! write out a test comparison
    open(23,file='testout.dat',status='unknown')
    write(23,'(5i10)') np,ne,ncn,iXYcoord,iZcoord
    do j=1,np
      write(23,'(3(1PE15.7),i5)') x(j), y(j), depth(j), ncode(j)
    enddo
    
    if(ncn.eq.4) then
      do j=1,ne
        write(23,'(5i10)') (NEN(k,j),k=1,4),ecode(j)
      enddo
    else
      do j=1,ne
        write(23,'(5i10)') (NEN(k,j),k=1,3),ndummy,ecode(j)
      enddo
    endif
    close(23)

end

!********************************************************************************
