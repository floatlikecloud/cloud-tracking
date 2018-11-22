PROGRAM timeHistory
    
    ! Main Program of time tracking through labels
    !
    ! The Program output the connection web of cloud labels between nearby 
    ! time-steps, including forward and backward links.
    ! You will need to run the cloud_labeling.out first to have the labeling
    ! maps.
    !
    ! Last Modified by floatlikecloud Nov.22 2018
    ! Contact: floatlikecloud@gmail.com
    !
    ! ---------- USAGE --------
    ! Preparation:
    ! You will need to run the cloud_labeling.out first to have the labeling
    ! maps.
    ! Since the tracking is done under a Lagrangian view, the inputlist that
    ! used in the cloud_labeling.out is still needed to obtain the wind field
    ! at every time step.
    !
    ! Setting:
    ! Next, set the grid size and time step length (for calculating advection). 
    ! 
    ! -------------------------

    use cloudList_type
    use input_netcdf_module
    use useful_module
    implicit none

    integer                     :: check
    integer                     :: NC_START(4), NC_COUNT(4)
    integer                     :: ncid_in
    integer                     :: xid, yid, zid, tid
    integer                     :: varid_in
    integer                     :: nx, ny, nz, nt, nt_check, dum_scalar(1)

    CHARACTER(256)              :: case_name, cloud_def, inputlabel_name, output_name
    CHARACTER(256)              :: inputlist_name, inputfile_name

    integer, allocatable        :: labelCloud(:,:,:,:), sizeCloud(:,:), numCloud(:)

    ! For advection calculation
    REAL, allocatable           :: xstag_temp(:,:,:), ystag_temp(:,:,:) ! raw data
    REAL, allocatable           :: unstag_U(:,:,:,:), unstag_V(:,:,:,:) ! unstagged data
    REAL, allocatable           :: intertime_unstag_U(:,:,:,:), intertime_unstag_V(:,:,:,:) ! at t+0.5dt
    integer, allocatable        :: x_shift(:,:,:,:), y_shift(:,:,:,:) ! converted wind: grids/timestep
    
    type(cloudTimeLinkedlist)   :: cloudBackList, cloudForList ! The outputs

    real, parameter             :: xy_length   = 3000.0       ! meters
    real, parameter             :: time_length = 30.0*60.0    ! second

    integer :: tCtr, lCtr, pCtr

! ##################################################################
! ######################## MANUAL OPTIONS ##########################
! ##################################################################
    case_name = 'NTU-S3DVC'
    cloud_def = 'qci5em5'
    inputlist_name = 'inputList_NTU-S3DVC'

    inputlabel_name = 'out_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.nc'
! ##################################################################
! ##################################################################

! ------- Initialization -----
    CALL nf_open_check(inputlabel_name, nf_nowrite, ncid_in)
    
    CALL nf_inq_varid_check(ncid_in, 'nx', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'nx', (/1/), (/1/), dum_scalar)
    nx = dum_scalar(1)
    CALL nf_inq_varid_check(ncid_in, 'ny', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'ny', (/1/), (/1/), dum_scalar)
    ny = dum_scalar(1)
    CALL nf_inq_varid_check(ncid_in, 'nz', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'nz', (/1/), (/1/), dum_scalar)
    nz = dum_scalar(1)
    CALL nf_inq_varid_check(ncid_in, 'nt', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'nt', (/1/), (/1/), dum_scalar)
    nt = dum_scalar(1)

    allocate( labelCloud(nx,ny,nz,nt) )
    allocate( numCloud(nt) )
    allocate( xstag_temp(nx+1,ny,nz) )
    allocate( ystag_temp(nx,ny+1,nz) )
    allocate( unstag_U(nx,ny,nz,nt) )
    allocate( unstag_V(nx,ny,nz,nt) )
    allocate( intertime_unstag_U(nx,ny,nz,nt-1) )
    allocate( intertime_unstag_V(nx,ny,nz,nt-1) )
    allocate( x_shift(nx,ny,nz,nt-1) )
    allocate( y_shift(nx,ny,nz,nt-1) )
    NC_START = (/1,1,1,1/)
    NC_COUNT = (/nx,ny,nz,nt/)

    CALL NF_INQ_DIMID_check(ncid_in, 'west_east', xid)
    CALL NF_INQ_DIMID_check(ncid_in, 'south_north', yid)
    CALL NF_INQ_DIMID_check(ncid_in, 'bottom_top', zid)
    CALL NF_INQ_DIMID_check(ncid_in, 'Time', tid)
    
    CALL nf_inq_varid_check(ncid_in, 'labelCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'labelCloud', NC_START, NC_COUNT, labelCloud)
    
    CALL nf_inq_varid_check(ncid_in, 'numCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'numCloud', (/NC_START(4)/), (/NC_COUNT(4)/), numCloud)

    CALL nf_close_check(inputlabel_name, ncid_in)
    ! --------- 
    ! start reading nc-files to get winds
    OPEN(unit=101, file=inputlist_name, status='OLD', action='READ', iostat=check)
    CALL check_iostatus(check)
    READ(101,*) nt_check
    if (nt_check.ne.nt) then 
        STOP 'ERROR: Time size of inputlist_name does not consistent with nt.'
    end if
    do tCtr = 1, nt
        READ(101,'(A256)') inputfile_name
        WRITE(*,*)  'Inputting file: ', inputfile_name
        CALL nf_open_check(inputfile_name, nf_nowrite, ncid_in) 
        
        CALL NF_INQ_DIMID_check(ncid_in, 'west_east', xid)
        CALL NF_INQ_DIMID_check(ncid_in, 'south_north', yid)
        CALL NF_INQ_DIMID_check(ncid_in, 'bottom_top', zid)
        CALL NF_INQ_DIMID_check(ncid_in, 'Time', tid)
        
        ! get wind
        NC_COUNT = (/nx+1,ny,nz,1/)
        CALL nf_inq_varid_check(ncid_in, 'U', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'U', NC_START, NC_COUNT, xstag_temp(:,:,:))
        CALL unstag_onedim_real(xstag_temp, unstag_U(:,:,:,tCtr), nx+1, ny, nz, 1)
        NC_COUNT = (/nx,ny+1,nz,1/)
        CALL nf_inq_varid_check(ncid_in, 'V', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'V', NC_START, NC_COUNT, ystag_temp(:,:,:))
        CALL unstag_onedim_real(ystag_temp, unstag_V(:,:,:,tCtr), nx, ny+1, nz, 2)

        CALL nf_close_check(inputfile_name, ncid_in) 
    end do ! end do tCtr

    CALL unstag_time_real(unstag_U, intertime_unstag_U, nx, ny, nz, nt, 4)
    CALL unstag_time_real(unstag_V, intertime_unstag_V, nx, ny, nz, nt, 4)

    WRITE(*,*) 'Obtaining shift...'
    x_shift = NINT(intertime_unstag_U*(time_length/xy_length))
    y_shift = NINT(intertime_unstag_V*(time_length/xy_length))
    deallocate( unstag_U )
    deallocate( unstag_V )
    deallocate( intertime_unstag_U )
    deallocate( intertime_unstag_V )

    ! Time Tracking 
    WRITE(*,*) 'Building Linked List...'
    CALL cloudTimeLinkedlist_build2d(cloudBackList, nt, numCloud)
    CALL cloudTimeLinkedlist_build2d(cloudForList,  nt, numCloud)
    WRITE(*,*) 'Tracking BACKWARD:'
    CALL tracking(cloudBackList, labelCloud, numCloud, nx, ny, nz, nt, -1, x_shift, y_shift)
    WRITE(*,*) 'Tracking FORWARD:'
    CALL tracking(cloudForList, labelCloud, numCloud, nx, ny, nz, nt, 1, x_shift, y_shift)
    ! - end of tracking -

    ! output
    output_name = 'cloudBackList_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.txt'
    OPEN( unit=101, file=output_name, status='replace', action='write', iostat=check)
    CALL check_iostatus(check)
    output_name = 'cloudForList_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.txt'
    OPEN( unit=102, file=output_name, status='replace', action='write', iostat=check)
    CALL check_iostatus(check)

    CALL cloudTimeLinkedList_outputTXT(cloudBackList, 101)
    CALL cloudTimeLinkedList_outputTXT(cloudForList, 102)

CONTAINS

! Check if the cloud is already existed in the list
SUBROUTINE check_isNew( inList, n, tgt, isNew )
    integer, intent(in) :: inList(:), n, tgt
    logical, intent(out) :: isNew
    integer :: nCtr

    isNew = .True.
    do nCtr = 1, n
        if (inList(nCtr).eq.tgt) then
            isNew = .False.
            EXIT
        end if
    end do
END SUBROUTINE check_isNew

! cloud tracking
SUBROUTINE tracking(cloudList, labelCloud, numCloud, nx, ny, nz, nt, opt, in_x_shift, in_y_shift)
    ! opt: linked target time shift (e.g. opt=-1 is backward link, opt=1 is forward link)
    type(cloudTimeLinkedlist), intent(inout) :: cloudList
    integer, intent(in) :: labelCloud(:,:,:,:), numCloud(:)
    integer, intent(in) :: in_x_shift(:,:,:,:), in_y_shift(:,:,:,:)
    integer, intent(in) :: nx, ny, nz, nt, opt
    integer :: t_start, t_end, nt_track
    integer :: xCtr, yCtr, zCtr, tCtr, lCtr, xCtr_new, yCtr_new

    integer              :: x_shift(nx,ny,nz,nt-1), y_shift(nx,ny,nz,nt-1)
    integer, allocatable :: temp_linkedlist(:,:), np(:) ! pp, cc/ll & cc/ll
    integer              :: temp_max
    integer              :: stag_adj ! dealing with interpolated time

    integer              :: nowLabel, tgtLabel
    logical              :: isNew

    ! get the advection shift 
    if (opt.lt.0) then ! backward
        t_start = nt
        t_end   = 1 - opt
        x_shift = in_x_shift * (-1)
        y_shift = in_y_shift * (-1)
        stag_adj = -1
    elseif (opt.gt.0) then
        t_start = 1
        t_end   = nt - opt
        x_shift = in_x_shift
        y_shift = in_y_shift
        stag_adj = 0
    else
        STOP '(ERROR) tracking opt cannot be 0'
    end if
    nt_track = abs(t_end - t_start) + 1

    ! identifying time continuity
    temp_max = 0
    do tCtr = t_start, t_end, opt
        WRITE(*,*) 'Processing: ', tCtr
        temp_max = numCloud(tCtr)
        if (temp_max.eq.0) then 
            temp_max = 1
        end if
        allocate( temp_linkedlist( temp_max, temp_max ) )
        allocate( np( temp_max ) )
        np(:) = 0
        temp_linkedlist(:,:) = 0
        ! for labels on each grid
        do xCtr = 1, nx
        do yCtr = 1, ny
        do zCtr = 1, nz
            ! get the equal Lagrangian grid at the target time step
            ! only horizontal advection is considered
            xCtr_new = xCtr+x_shift(xCtr,yCtr,zCtr,tCtr+stag_adj)
            yCtr_new = yCtr+y_shift(xCtr,yCtr,zCtr,tCtr+stag_adj)
            ! check if Lagrangian grid is not out of boundary
            if (xCtr_new.le.nx.and.xCtr_new.ge.1.and.yCtr_new.le.ny.and.yCtr_new.ge.1) then
                nowLabel = labelCloud(xCtr,yCtr,zCtr,tCtr)
                tgtLabel = labelCloud(xCtr_new,yCtr_new,zCtr,tCtr+opt)
                ! check if the target label is new, if so, put it in the list.
                if (nowLabel.ne.0 .and. tgtLabel.ne.0) then
                    if (np(nowLabel).ne.0) then
                        CALL check_isNew( temp_linkedlist(:,nowLabel), temp_max, tgtLabel, isNew)
                    else
                        isNew = .True.
                    end if
                    
                    if (isNew) then
                        np(nowLabel) = np(nowLabel) + 1
                        temp_linkedlist( np(nowLabel), nowLabel ) = tgtLabel
                    end if 
                end if
            end if
        end do
        end do
        end do
        
        do lCtr = 1, numCloud(tCtr)
            CALL cloudTimeLinkedlist_buildAssignPtr(cloudList, tCtr, lCtr, np(lCtr), temp_linkedlist( :, lCtr))
        end do

        deallocate( temp_linkedlist )
        deallocate( np )

    end do

END SUBROUTINE tracking


END PROGRAM timeHistory

! error handling
SUBROUTINE checking(check, errMessage)
    INTEGER, intent(in) :: check
    CHARACTER(len=*), intent(in) :: errMessage
    if (check.ne.nf_noerr) then
        WRITE(*,*) errMessage, ':: check = ', check
        STOP
    end if
END SUBROUTINE checking
    

