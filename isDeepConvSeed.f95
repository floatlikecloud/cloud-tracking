PROGRAM isDeepConvSeed
    
    ! Main Program of finding the deep convection seeds
    ! The Program output the clouds (labels) which are
    ! with deep convection seeds embedded.
    ! The list of clouds could be used as the seeds of
    ! cloud family searching (searchFamilyUnion.out).
    ! 
    ! Last Modified by floatlikecloud Nov.22 2018
    ! Contact: floatlikecloud@gmail.com
    !
    ! ---------- USAGE --------
    ! Preparation:
    ! You will need to run the cloud_labeling.out first to have the labeling
    ! maps.
    ! To know if a grid is deep convection seed, some properties are needed,
    ! thus we also need the inputList to get the original output fields.
    !
    ! Setting:
    ! Set your definition of deep convection seeds.
    ! It should be very strict that it could only be acheived
    ! somewhere and at some stage in a single deep convection system.
    !
    ! -------------------------

    use useful_module
    use input_netcdf_module
    use cloudList_type
    implicit none

    integer                     :: check
    integer                     :: NC_START(4), NC_COUNT(4)

    ! file IO
    integer                     :: ncid_in, ncid_out
    integer                     :: varid_in, varid_out
    integer                     :: xid, yid, zid, tid

    ! data properties
    integer                     :: nx, ny, nz, nt, dum_scalar(1), nt_check
    CHARACTER(256)              :: case_name, cloud_def, seed_def 
    CHARACTER(256)              :: inputlabel_name, inputlist_name
    CHARACTER(256)              :: inputfile_name, outputfile_name

    ! labeling fields
    integer, allocatable        :: labelCloud(:,:,:,:), numCloud(:)
    real, allocatable           :: q_field(:,:,:,:), w_field(:,:,:,:)
    real, allocatable           :: nostag_temp(:,:,:), zstag_temp(:,:,:)

    ! thresholds
    real                        :: q_thres, w_thres
    logical, allocatable        :: q_labels(:), w_labels(:), final_labels(:)

    ! output
    type(cloudFamilyList)       :: isDeepCoreList ! %time(tCtr)%ptr(pp)

    integer :: xCtr, yCtr, zCtr, tCtr, lCtr, max_cloud

! ##################################################################
! ######################## MANUAL OPTIONS ##########################
! ##################################################################
    case_name = 'NTU-S3DVC'
    cloud_def = 'qci5em5'
    seed_def  = 'q1em3w10'
    inputlist_name = 'inputList_NTU-S3DVC'

    inputlabel_name  = 'out_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.nc'
    outputfile_name  = 'isDCSeed_' // trim(case_name) // '_' // trim(seed_def) // '_CLD' // trim(cloud_def) // '.txt'
! ##################################################################
! ##################################################################
   
    WRITE(*,*)  'Input labeling file: ', inputlabel_name 
    CALL nf_open_check(inputlabel_name, nf_nowrite, ncid_in) 
        
    CALL NF_INQ_DIMID_check(ncid_in, 'west_east', xid)
    CALL NF_INQ_DIMID_check(ncid_in, 'south_north', yid)
    CALL NF_INQ_DIMID_check(ncid_in, 'bottom_top', zid)
    CALL NF_INQ_DIMID_check(ncid_in, 'Time', tid)

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
    
    q_thres = 1.0e-3
    w_thres = 10.0

    allocate( labelCloud(nx,ny,nz,nt) )
    allocate( numCloud(nt) )
    allocate( nostag_temp(nx,ny,nz) )
    allocate( zstag_temp(nx,ny,nz+1) )
    allocate( q_field(nx,ny,nz,nt) )
    allocate( w_field(nx,ny,nz,nt) )

    NC_START = (/1,1,1,1/)  
    NC_COUNT = (/nx,ny,nz,nt/)

    CALL nf_inq_varid_check(ncid_in, 'labelCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'labelCloud', NC_START, NC_COUNT, labelCloud)
    CALL nf_inq_varid_check(ncid_in, 'numCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'numCloud', (/NC_START(4)/), (/NC_COUNT(4)/), numCloud)
    CALL nf_close_check(inputlabel_name, ncid_in)   

    ! Get inputlist of nc-files
    OPEN(unit=101, file=inputlist_name, status='OLD', action='READ', iostat=check)
    CALL check_iostatus(check)
    READ(101,*) nt_check
    if (nt_check.ne.nt) then
        STOP 'ERROR: Time size of inputlist_name does not consistent with nt.'
    end if
 
    ! INPUT original fields
    ! start reading nc-files...
    q_field = 0.0
    w_field = 0.0
    NC_COUNT = (/nx,ny,nz,1/)
    do tCtr = 1, nt
        READ(101,'(A256)') inputfile_name
        WRITE(*,*)  'Inputting file: ', inputfile_name
        CALL nf_open_check(inputfile_name, nf_nowrite, ncid_in)

        CALL NF_INQ_DIMID_check(ncid_in, 'west_east', xid)
        CALL NF_INQ_DIMID_check(ncid_in, 'south_north', yid)
        CALL NF_INQ_DIMID_check(ncid_in, 'bottom_top', zid)

        CALL nf_inq_varid_check(ncid_in, 'QCLOUD', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QCLOUD', NC_START, NC_COUNT, nostag_temp)
        q_field(:,:,:,tCtr) = q_field(:,:,:,tCtr) + nostag_temp
        CALL nf_inq_varid_check(ncid_in, 'QICE', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QICE', NC_START, NC_COUNT, nostag_temp)
        q_field(:,:,:,tCtr) = q_field(:,:,:,tCtr) + nostag_temp
        CALL nf_inq_varid_check(ncid_in, 'QRAIN', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QRAIN', NC_START, NC_COUNT, nostag_temp)
        q_field(:,:,:,tCtr) = q_field(:,:,:,tCtr) + nostag_temp
        CALL nf_inq_varid_check(ncid_in, 'QSNOW', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QSNOW', NC_START, NC_COUNT, nostag_temp)
        q_field(:,:,:,tCtr) = q_field(:,:,:,tCtr) + nostag_temp
        CALL nf_inq_varid_check(ncid_in, 'QGRAUP', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QGRAUP', NC_START, NC_COUNT, nostag_temp)
        q_field(:,:,:,tCtr) = q_field(:,:,:,tCtr) + nostag_temp
       
        NC_COUNT(3) = nz+1
        CALL nf_inq_varid_check(ncid_in, 'W', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'W', NC_START, NC_COUNT, zstag_temp)
        CALL unstag_onedim_real(zstag_temp, w_field(:,:,:,tCtr), nx, ny, nz+1, 3)
        NC_COUNT(3) = nz

        CALL nf_close_check(inputfile_name, ncid_in)
    end do

    max_cloud = maxval(numCloud)
    allocate( q_labels(max_cloud) )
    allocate( w_labels(max_cloud) )
    allocate( final_labels(max_cloud) )

    ! --- ALLOCATE ---
    isDeepCoreList%n_time = nt
    allocate( isDeepCoreList%time(isDeepCoreList%n_time) )
    do tCtr = 1, nt
        isDeepCoreList%time(tCtr)%n_label = 0
        allocate( isDeepCoreList%time(tCtr)%label(numCloud(tCtr)) )
    end do

    ! obtain the grids that satisfy the threholds
    ! in fact this program is just a boring logical mask...
    do tCtr = 1,nt
        q_labels = .false.
        w_labels = .false.
        final_labels = .false.

        do xCtr = 1,nx
        do yCtr = 1,ny
        do zCtr = 1,nz
            if (q_field(xCtr,yCtr,zCtr,tCtr).ge.q_thres) then
                if (labelCloud(xCtr,yCtr,zCtr,tCtr).ne.0) then
                    q_labels(labelCloud(xCtr,yCtr,zCtr,tCtr)) = .true.
                end if
            end if
            if (w_field(xCtr,yCtr,zCtr,tCtr).ge.w_thres) then
                if (labelCloud(xCtr,yCtr,zCtr,tCtr).ne.0) then
                    w_labels(labelCloud(xCtr,yCtr,zCtr,tCtr)) = .true.
                end if
            end if
        end do
        end do
        end do

        final_labels = q_labels.and.w_labels
        do lCtr = 1, max_cloud
            if (final_labels(lCtr)) then
                isDeepCoreList%time(tCtr)%n_label = isDeepCoreList%time(tCtr)%n_label + 1
                isDeepCoreList%time(tCtr)%label(isDeepCoreList%time(tCtr)%n_label) = lCtr
            end if
        end do

    end do
    
    deallocate( q_labels )
    deallocate( w_labels )
    deallocate( final_labels )
    deallocate( labelCloud )
    deallocate( numCloud )
    deallocate( q_field )
    deallocate( w_field )
    
    ! output seed list
    OPEN( unit=101, file=outputfile_name, status='replace', action='write' )
    WRITE(101,*) isDeepCoreList%n_time
    do tCtr = 1, isDeepCoreList%n_time
        WRITE(101,*) isDeepCoreList%time(tCtr)%n_label
        do lCtr = 1, isDeepCoreList%time(tCtr)%n_label
            WRITE(101,*) isDeepCoreList%time(tCtr)%label(lCtr)
        end do
    end do
    CLOSE(101) 

CONTAINS

! check if the label is already in the seed list
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

! error handling
SUBROUTINE checking(check, errMessage)
    INTEGER, intent(in) :: check
    CHARACTER(len=*), intent(in) :: errMessage
    if (check.ne.nf_noerr) then
        WRITE(*,*) errMessage, ':: check = ', check
        STOP
    end if
END SUBROUTINE checking

END PROGRAM isDeepConvSeed
    

