PROGRAM cloud_labeling

    ! Main Program of 3D Cloud Labeling
    ! The Program output the label map of every time step (snapshots)
    ! If you want to obtain the time connections between clouds,
    ! please first run this program then run the timeHistory.out.
    !
    ! Last Modified by floatlikecloud Nov.22 2018
    ! Contact: floatlikecloud@gmail.com
    !
    ! ---------- USAGE --------
    ! Preparation:
    ! You will need a txt-list of filepath pointing to the target model output fields.
    ! The first line of the list should be a number, the number of files.
    ! From the second line, one file should be presented in a line.
    ! See inputList_TWPICE for example.
    !
    ! Setting:
    ! Next, set your deifinition of whether a grid box is cloudy.
    ! Currently there are only two thresholds of defining: W and Q.
    ! You can manually modify the threshold in the MANUAL OPTIONS below.
    ! Or you can define your own threshold by adding codes.
    ! 
    ! -------------------------
 
    use input_netcdf_module     ! input netcdf module 
    use sct1uft_3d_module       ! connected-labeling scan: step one
    use secondScan_3d_module    ! connected-labeling scan: step two
    use cloudList_type          ! data structure storing the cloud list
    implicit none

    REAL                        :: cpu_time_start, cpu_time_end

    CHARACTER(256)              :: inputlist_name, case_name, cloud_def
    CHARACTER(256)              :: inputfile_name, outputfile_name

    REAL, allocatable           :: Q_total(:,:,:)    ! for thresholds
    REAL, allocatable           :: Q_temp(:,:,:)     ! for thresholds
    REAL, allocatable           :: W(:,:,:)          ! for thresholds
    REAL, allocatable           :: W_temp(:,:,:)     ! for thresholds

    INTEGER                     :: check ! IO Status 
    INTEGER                     :: NC_START(4), NC_COUNT(4), DIMS(4)
    INTEGER                     :: ncid_in, ncid_out ! file IO
    INTEGER                     :: varid_in, varid_out(64) ! variable IO
    INTEGER                     :: xid, yid, zid, tid ! dimension IO

    LOGICAL                     :: enable_Q_thres   ! threshold on/off
    LOGICAL                     :: option_rainsnow  ! Q including rain/snow
    LOGICAL                     :: enable_W_thres   ! thershold on/off
    REAL                        :: Q_thres, W_thres ! threshold values

    INTEGER, allocatable        :: isCloud(:,:,:,:)     ! cloud binary 3D fields
    INTEGER, allocatable        :: labelCloud(:,:,:,:)  ! label 3D fields
    INTEGER                     :: nx, ny, nz, nt, nc 
    INTEGER                     :: xCtr, yCtr, zCtr, tCtr
    ! isCloud(:,:,:,tCtr)    -(sct1uft_3d)-> integer, allocatable, dimension(:,:,:)  :: image_3d 
    ! labelCloud(:,:,:,tCtr) -(sct1uft_3d)-> integer, allocatable, dimension(:,:,:)  :: label_3d
    INTEGER, allocatable        :: connectionTable(:,:)  ! tree of labeling (nLabelMax)
    INTEGER, allocatable        :: numCloud(:)

    REAL, allocatable           :: XLONG(:,:), XLAT(:,:) ! lat, lon
    REAL, allocatable           :: xstag_temp(:,:,:)     ! stagged field (e.g. u-wind)
    REAL, allocatable           :: ystag_temp(:,:,:)     ! e.g. v-wind
    REAL, allocatable           :: zstag_temp(:,:,:)     ! e.g. w-wind


! ##################################################################
! ######################## MANUAL OPTIONS ##########################
! ##################################################################
   
! -- File Options -- 
    inputlist_name = 'inputList_NTU-S3DVC'  ! filename of input filelist (txt)
    case_name      = 'NTU-S3DVC'         ! target case name
    cloud_def      = 'qci5em5'           ! name of cloud definition

! -- Cloud Options -- 
    enable_Q_thres   = .true.          ! enable the threshold of cloud water content
    option_rainsnow  = .false.         ! cloud water: including rain/snow/graupel or not
    enable_W_thres   = .false.         ! enable the threshold of vertical speed
    Q_thres        = 5.0D-5            ! Threshold of cloud water content: kg/kg
    !W_thres        = 1.0D0             ! Threshold of vertical speed: m/s

! -- Dimension Size -- 
    nx = 201
    ny = 201
    nz = 44

! ##################################################################
! ##################################################################


    CALL cpu_time(cpu_time_start)
    nc = nx*ny*nz / 2
                      ! in nc(inversed): time,bt,sn,we
    NC_START(1) = 1   ! west_east
    NC_START(2) = 1   ! south_north
    NC_START(3) = 1   ! bottom_top
   
    OPEN(unit=101, file=inputlist_name, status='OLD', action='READ', iostat=check)
    CALL check_iostatus(check)
    READ(101,*) nt
    WRITE(*,*) 'nt = ', nt
    NC_START(4) = 1    ! time

    if (enable_Q_thres) then  
        allocate( Q_temp(nx,ny,nz) )
        allocate( Q_total(nx,ny,nz) )
    end if
    if (enable_W_thres) then  
        allocate( W(nx,ny,nz) )
        allocate( W_temp(nx,ny,nz+1) )
    end if
    allocate( isCloud(nx,ny,nz,nt) )
    allocate( labelCloud(nx,ny,nz,nt) )
    allocate( connectionTable(nc, nt) )
    allocate( numCloud(nt) )
    allocate( XLONG(nx,ny) )
    allocate( XLAT(nx,ny) )
    allocate( xstag_temp(nx+1,ny,nz) )
    allocate( ystag_temp(nx,ny+1,nz) )
    allocate( zstag_temp(nx,ny,nz+1) )

    isCloud(:,:,:,:) = 0

    do tCtr = 1, nt
        READ(101,'(A256)') inputfile_name
        WRITE(*,*)  'Inputting file: ', inputfile_name
        CALL nf_open_check(inputfile_name, nf_nowrite, ncid_in) 
        
        CALL NF_INQ_DIMID_check(ncid_in, 'west_east', xid)
        CALL NF_INQ_DIMID_check(ncid_in, 'south_north', yid)
        CALL NF_INQ_DIMID_check(ncid_in, 'bottom_top', zid)
        CALL NF_INQ_DIMID_check(ncid_in, 'Time', tid)
        
        if (tCtr .eq. 1) then
            NC_COUNT(1) = nx
            NC_COUNT(2) = ny
            NC_COUNT(3) = 1 ! temporary use
            
            CALL nf_inq_varid_check(ncid_in, 'XLONG', varid_in)
            CALL nf_get_vara_real_check(ncid_in, varid_in, 'XLONG', NC_START(1:3), NC_COUNT(1:3), XLONG)
            CALL nf_inq_varid_check(ncid_in, 'XLAT', varid_in)
            CALL nf_get_vara_real_check(ncid_in, varid_in, 'XLAT', NC_START(1:3), NC_COUNT(1:3), XLAT)
        end if
        
        NC_COUNT(1) = nx
        NC_COUNT(2) = ny
        NC_COUNT(3) = nz
        NC_COUNT(4) = 1
        
        if (enable_Q_thres) then
            Q_total  = 0.d0
            
            CALL nf_inq_varid_check(ncid_in, 'QCLOUD', varid_in)
            CALL nf_get_vara_real_check(ncid_in, varid_in, 'QCLOUD', NC_START, NC_COUNT, Q_temp)
            Q_total = Q_total + Q_temp
            
            CALL nf_inq_varid_check(ncid_in, 'QICE', varid_in)
            CALL nf_get_vara_real_check(ncid_in, varid_in, 'QICE', NC_START, NC_COUNT, Q_temp)
            Q_total = Q_total + Q_temp

            if (option_rainsnow) then
                CALL nf_inq_varid_check(ncid_in, 'QSNOW', varid_in)
                CALL nf_get_vara_real_check(ncid_in, varid_in, 'QSNOW', NC_START, NC_COUNT, Q_temp)
                Q_total = Q_total + Q_temp
                
                CALL nf_inq_varid_check(ncid_in, 'QRAIN', varid_in)
                CALL nf_get_vara_real_check(ncid_in, varid_in, 'QRAIN', NC_START, NC_COUNT, Q_temp)
                Q_total = Q_total + Q_temp
                
                CALL nf_inq_varid_check(ncid_in, 'QGRAUP', varid_in)
                CALL nf_get_vara_real_check(ncid_in, varid_in, 'QGRAUP', NC_START, NC_COUNT, Q_temp)
                Q_total = Q_total + Q_temp
            end if        
 
        end if
        
        if (enable_W_thres) then
    
            W       = 0.d0
            NC_COUNT(3) = nz+1
        
            CALL nf_inq_varid_check(ncid_in, 'W', varid_in)
            CALL nf_get_vara_real_check(ncid_in, varid_in, 'W', NC_START, NC_COUNT, W_temp)
            CALL unstag_onedim_real(W_temp, W, nx, ny, nz+1, 3)
            
            NC_COUNT(3) = nz
    
        end if
       
        CALL nf_close_check(inputfile_name, ncid_in)

        do zCtr = 1, nz
        do yCtr = 1, ny
        do xCtr = 1, nx
            if (enable_Q_thres) then
                if (Q_total(xCtr,yCtr,zCtr).gt.Q_thres) then
                    isCloud(xCtr,yCtr,zCtr,tCtr) = 1
                end if
            end if
            if (enable_W_thres) then
                if (W(xCtr,yCtr,zCtr).gt.W_thres .and. isCloud(xCtr,yCtr,zCtr,tCtr).eq.1) then
                    isCloud(xCtr,yCtr,zCtr,tCtr) = 1
                else
                    isCloud(xCtr,yCtr,zCtr,tCtr) = 0
                end if
            end if
        end do
        end do
        end do
    
    end do ! end do tCtr

    if (enable_Q_thres) then
        deallocate(Q_total)
        deallocate(Q_temp)
    end if
    if (enable_W_thres) then
        deallocate(W_temp)
        deallocate(W)
    end if

    do tCtr = 1, nt
        WRITE(*,*) 'Processing: ', tCtr
        ! first_scan is to label all the connected components and build the labeling table
        CALL first_scan(isCloud(:,:,:,tCtr), labelCloud(:,:,:,tCtr), connectionTable(:,tCtr), nx, ny, nz)
        ! second scan relabel the map by using the labeling table
        ! output of cloud properties are done in second scan for efficiency
        CALL second_scan(labelCloud(:,:,:,tCtr), connectionTable(:,tCtr), nx, ny, nz, numCloud(tCtr))
        ! end of main process
    end do
    deallocate( isCloud )
    deallocate( connectionTable )

    WRITE(*,*) 'Outputting Labeling File...'
    outputfile_name = 'out_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.nc'
    CALL nf_create_check(outputfile_name, NF_64BIT_OFFSET, ncid_out)
    CALL nf_def_dim_check(ncid_out, 'west_east',   nx, xid)
    CALL nf_def_dim_check(ncid_out, 'south_north', ny, yid)
    CALL nf_def_dim_check(ncid_out, 'bottom_top',  nz, zid)
    CALL nf_def_dim_check(ncid_out, 'Time',        nt, tid)
    
    DIMS(1) = xid
    DIMS(2) = yid
    DIMS(3) = zid
    DIMS(4) = tid
    CALL nf_def_var_check(ncid_out, 'XLONG', NF_REAL, 2, DIMS(1:2), varid_out(1))
    CALL nf_def_var_check(ncid_out, 'XLAT', NF_REAL, 2, DIMS(1:2), varid_out(2))
    CALL nf_def_var_check(ncid_out, 'labelCloud', NF_INT, 4, DIMS, varid_out(3))   
    CALL nf_def_var_check(ncid_out, 'numCloud',  NF_INT, 1, (/tid/), varid_out(4))
    CALL nf_def_var_check(ncid_out, 'nx', NF_INT, 0, (/0/), varid_out(11))
    CALL nf_def_var_check(ncid_out, 'ny', NF_INT, 0, (/0/), varid_out(12))
    CALL nf_def_var_check(ncid_out, 'nz', NF_INT, 0, (/0/), varid_out(13))
    CALL nf_def_var_check(ncid_out, 'nt', NF_INT, 0, (/0/), varid_out(14))
    CALL nf_enddef_check(outputfile_name, ncid_out) 
    
    CALL NF_PUT_var_REAL_check(ncid_out, 'XLONG', varid_out(1), XLONG)
    CALL NF_PUT_var_REAL_check(ncid_out, 'XLAT', varid_out(2), XLAT)
    CALL NF_PUT_var_INT_check(ncid_out, 'labelCloud', varid_out(3), labelCloud)
    CALL NF_PUT_var_INT_check(ncid_out, 'numCloud', varid_out(4), numCloud)
    CALL NF_PUT_var_INT_check(ncid_out, 'nx', varid_out(11), (/nx/))
    CALL NF_PUT_var_INT_check(ncid_out, 'ny', varid_out(12), (/ny/))
    CALL NF_PUT_var_INT_check(ncid_out, 'nz', varid_out(13), (/nz/))
    CALL NF_PUT_var_INT_check(ncid_out, 'nt', varid_out(14), (/nt/))
    CALL nf_close_check(outputfile_name, ncid_out)
    
    deallocate( labelCloud )
    deallocate( numCloud )
    deallocate( XLONG )
    deallocate( XLAT )
    
    CALL cpu_time(cpu_time_end)
    print '("Time = ",f15.7," seconds.")', cpu_time_end-cpu_time_start

END PROGRAM cloud_labeling
