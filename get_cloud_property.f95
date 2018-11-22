PROGRAM get_cloud_property
    
    ! Main Program of cloud properties obtaining 
    ! This program output the properties of each cloud
    ! at each time step in txt format.
    !
    ! Last Modified by floatlikecloud Nov.22 2018
    ! Contact: floatlikecloud@gmail.com
    !
    ! ---------- USAGE --------
    ! Preparation:
    ! You will need the labeling file and the inputList
    ! to obtain the properties of every cloud.
    !
    ! Setting:
    ! Switch on/off the Output Property Options to 
    ! pick your desired cloud properties.
    ! As seen, there are plenty of empty slots that
    ! you could manually add your desired properties to obtain.
    ! 
    !
    ! NOTE: Since the output format varies a lot between properties,
    ! one who add new cloud properties should go to OUTPUT PROPERTIES
    ! to see if you need further modification on output YOUR property.
    ! This cannot being set in the section of MANUAL OPTIONS.
    ! Currently there are txt output and nc output.
    !
    ! NOTE: When compiling, plenty of warning shows up that the vars
    ! may not be well-initialized. This is ignorable if the switches
    ! are well arranged.
    !
    ! -------------------------

    use useful_module
    use input_netcdf_module
    use cloudList_type
    implicit none

    ! Settings
    REAL                        :: cpu_time_start, cpu_time_end
    INTEGER                     :: nx, ny, nz, nt, dum_scalar(1), nt_check
    CHARACTER(256)              :: case_name, cloud_def
    CHARACTER(256)              :: inputlist_name, inputlabel_name
    CHARACTER(256)              :: inputfile_name, outputfile_name

    INTEGER                     :: check, ncid_in, ncid_out, varid_in, varid_out(64)
    INTEGER                     :: xid, yid, zid, tid
    INTEGER                     :: NC_START(4), NC_COUNT(4), DIMS(4)

    INTEGER, allocatable        :: labelCloud(:,:,:,:), numCloud(:)
    REAL, allocatable           :: nostag_temp(:,:,:), xstag_temp(:,:,:), ystag_temp(:,:,:), zstag_temp(:,:,:)
    INTEGER                     :: xCtr, yCtr, zCtr, tCtr, oCtr, lCtr

    ! switches of properties output
    type(cloudPropertyList)     :: cloudProperties(64)
    LOGICAL                     :: output_options(64) = .false.
    CHARACTER(256)              :: output_names(64)
    INTEGER                     :: output_fileid
    
! ##################################################################
! ######################## MANUAL OPTIONS ##########################
! ##################################################################

    ! useful buffer for cloud properties
    INTEGER                     :: nowLabel
    REAL                        :: nowValue ! buffer for temporary got value
    REAL                        :: prevProp ! buffer for current property
    ! specific buffer for cloud properties can be declared below...
    
        ! ------ property-based variables -------
        ! ----------------------------------------
        
        ! shared vars
        REAL, allocatable           :: gpt_height(:,:,:), thickness(:,:,:), volume(:,:,:)
        REAL, parameter             :: CONSTR4_g = 9.8        ! gravity acceleration ms-2
        REAL, parameter             :: xy_length = 3000.0     ! (d01,d02,d03,d04)=(27,9,3,1)*1000 m

        ! 2: QCLOUD(QC+QI) of clouds
        REAL, parameter         :: kappa = 0.2854, dry_R = 287.0, theta_0 = 300.0, p_0 = 1e6
        REAL, allocatable       :: temperature(:,:,:), pressure(:,:,:), air_density(:,:,:)
        REAL, allocatable       :: QCI(:,:,:) ! also: 7
        ! 3: QPRECIP(QR+QS+QG) of clouds
        REAL, allocatable       :: QPR(:,:,:)
        ! 4,5: cloud base/top
        REAL, allocatable       :: height(:,:,:)
        ! 6: max updraft velocity
        REAL, allocatable       :: W(:,:,:)
        ! 7: COGalt
        ! 10: convCore
        INTEGER, allocatable  :: isCore(:,:,:,:) 
        REAL                  :: convcore_w = 1.0 ! m/s
        ! ----------------------------------------
        
    ! ----------------------------------------
    ! end specific buffer for cloud properties

    
! -- File Options -- 
    inputlist_name = 'inputList_NTU-S3DVC'       ! filename of input filelist (txt)
    case_name      = 'NTU-S3DVC'       ! target case name
    cloud_def      = 'qci5em5'         ! name of cloud definition


! -- Output Property Options --
    output_options(1)  = .true.        ! size of clouds
    output_options(2)  = .true.        ! QCLOUD(QC+QI) mass of clouds
    output_options(3)  = .true.         ! QRAIN+QSNOW+QGRAUP mass of clouds *
    output_options(4)  = .true.         ! cloud base *
    output_options(5)  = .true.         ! cloud top *
    output_options(6)  = .true.         ! max updraft velocity *
    output_options(7)  = .true.         ! center of gravity altitude (QCLOUD) *
    output_options(8)  = .false.        ! preserved
    output_options(9)  = .false.        ! preserved
    output_options(10) = .true.          ! simple convection core definition (using w)
    output_options(11) = .true.       ! convcore volume (non-convcore volume can be derived by 1)
    output_options(12) = .true.       ! convcore weighted-averaged w
    output_options(13) = .true.       ! non_convcore weighted-averaged w
    output_options(14) = .true.       ! cloud air mass
    output_options(15) = .true.       ! cloud air mass (onlyCore)
    output_options(16) = .true.       ! cloud air mass (onlyNonCore)
! -- Output Names --    
    output_names(1)  = 'size'        ! size of clouds
    output_names(2)  = 'QCImass'        ! QCLOUD(QC+QI) of clouds
    output_names(3)  = 'QPRmass'        ! 
    output_names(4)  = 'cldBase'        ! 
    output_names(5)  = 'cldTop'        ! 
    output_names(6)  = 'maxW'        ! 
    output_names(7)  = 'COGalt'        ! 
!    output_names(8)  = 'ncolArea'        ! 
!    output_names(9)  = 'colArea'        ! 
    output_names(10) = 'convCore'        ! 
    output_names(11) = 'convCoreSize'        ! 
    output_names(12) = 'convCoreAvgW'        ! 
    output_names(13) = 'nonConvCoreAvgW'        ! 
    output_names(14) = 'cldAirMass'        ! 
    output_names(15) = 'cldAirMassCore'        ! 
    output_names(16) = 'cldAirMassNonCore'        ! 

! ##################################################################
! #################### MANUAL OPTIONS END ##########################
! ##################################################################


    CALL cpu_time(cpu_time_start)
   ! READ LABELs
    inputlabel_name = 'out_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.nc'
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
    
    allocate( numCloud(nt) )
    allocate( labelCloud(nx,ny,nz,nt) )
    allocate( nostag_temp(nx,ny,nz) )
    allocate( xstag_temp(nx+1,ny,nz) )
    allocate( ystag_temp(nx,ny+1,nz) )
    allocate( zstag_temp(nx,ny,nz+1) )
    ! allocate properties-obtaining vars
    allocate( gpt_height(nx,ny,nz+1) )
    allocate( thickness(nx,ny,nz) )
    allocate( volume(nx,ny,nz) )
    allocate( height(nx,ny,nz) )
        ! 2
        if (output_options(2)) then ! size
            allocate( QCI(nx,ny,nz) )
        end if
        ! 3
        if (output_options(3)) then
            allocate( QPR(nx,ny,nz) )
        end if
        ! 6
        if (output_options(6)) then
            allocate( W(nx,ny,nz) )
        end if
        ! 10
        if (output_options(10)) then
            allocate( isCore(nx,ny,nz,nt) )
            isCore = 0
        end if
        ! 12,13
        if (output_options(12).or.output_options(13)) then
            allocate( temperature(nx,ny,nz) )
            allocate( pressure(nx,ny,nz) )
            allocate( air_density(nx,ny,nz) )
        end if
    ! end allocate properties-obtaining vars
    NC_START = (/1,1,1,1/)
    NC_COUNT = (/nx,ny,nz,nt/)
    
    CALL nf_inq_varid_check(ncid_in, 'numCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'numCloud', (/NC_START(4)/), (/NC_COUNT(4)/), numCloud)
    CALL nf_inq_varid_check(ncid_in, 'labelCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'labelCloud', NC_START, NC_COUNT, labelCloud)
        
    CALL nf_close_check(inputlabel_name, ncid_in)
            
    ! Get inputlist of nc-files
    OPEN(unit=101, file=inputlist_name, status='OLD', action='READ', iostat=check)
    CALL check_iostatus(check)
    READ(101,*) nt_check
    if (nt_check.ne.nt) then 
        STOP 'ERROR: Time size of inputlist_name does not consistent with nt.'
    end if

    ! allocate space to cloud properties list, initialized to 0.0
    do oCtr = 1, 64
        if (output_options(oCtr)) then
            CALL cloudPropertyList_build(cloudProperties(oCtr),nt,numCloud)
        end if
    end do
    CALL cloudPropertyList_assign(cloudProperties(4), 20000.0)

    ! start reading nc-files...
    do tCtr = 1, nt
        READ(101,'(A256)') inputfile_name
        WRITE(*,*)  'Inputting file: ', inputfile_name
        CALL nf_open_check(inputfile_name, nf_nowrite, ncid_in) 
        
        CALL NF_INQ_DIMID_check(ncid_in, 'west_east', xid)
        CALL NF_INQ_DIMID_check(ncid_in, 'south_north', yid)
        CALL NF_INQ_DIMID_check(ncid_in, 'bottom_top', zid)
        CALL NF_INQ_DIMID_check(ncid_in, 'Time', tid)
       
        ! ----- SOME SHARED FIELDS ARE READ HERE FIRST ------

        NC_COUNT = (/nx,ny,nz+1,1/)
        ! get vertical thickness    
        CALL nf_inq_varid_check(ncid_in, 'PHB', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'PHB', NC_START, NC_COUNT, zstag_temp(:,:,:))
        gpt_height = zstag_temp
        CALL nf_inq_varid_check(ncid_in, 'PH', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'PH', NC_START, NC_COUNT, zstag_temp(:,:,:))
        gpt_height = gpt_height + zstag_temp
        gpt_height = gpt_height / CONSTR4_g
        CALL unstag_onedim_real(gpt_height, height, nx, ny, nz+1, 3)
        height = height / CONSTR4_g
        ! get volume
        do zCtr = 2, nz+1
            thickness(:,:,zCtr-1) = gpt_height(:,:,zCtr) - gpt_height(:,:,zCtr-1)
        end do
        volume = thickness * xy_length * xy_length

        ! START OBTAINING PROPERTIES...
        NC_COUNT = (/nx,ny,nz,1/)
        
        QCI = 0.0
        CALL nf_inq_varid_check(ncid_in, 'QCLOUD', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QCLOUD', NC_START, NC_COUNT, nostag_temp(:,:,:))
        QCI = nostag_temp        
        CALL nf_inq_varid_check(ncid_in, 'QICE', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QICE', NC_START, NC_COUNT, nostag_temp(:,:,:))
        QCI = QCI + nostag_temp
        
        QPR = 0.0
        CALL nf_inq_varid_check(ncid_in, 'QRAIN', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QRAIN', NC_START, NC_COUNT, nostag_temp(:,:,:))
        QPR = nostag_temp        
        CALL nf_inq_varid_check(ncid_in, 'QSNOW', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QSNOW', NC_START, NC_COUNT, nostag_temp(:,:,:))
        QPR = QPR + nostag_temp
        CALL nf_inq_varid_check(ncid_in, 'QGRAUP', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'QGRAUP', NC_START, NC_COUNT, nostag_temp(:,:,:))
        QPR = QPR + nostag_temp
        
        NC_COUNT = (/nx,ny,nz+1,1/)
        CALL nf_inq_varid_check(ncid_in, 'W', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'W', NC_START, NC_COUNT, zstag_temp(:,:,:))
        CALL unstag_onedim_real(zstag_temp, W, nx, ny, nz+1, 3)

        NC_COUNT = (/nx,ny,nz,1/)
        CALL nf_inq_varid_check(ncid_in, 'PB', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'PB', NC_START, NC_COUNT, nostag_temp(:,:,:))
        pressure = nostag_temp
        CALL nf_inq_varid_check(ncid_in, 'P', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'P', NC_START, NC_COUNT, nostag_temp(:,:,:))
        pressure = pressure + nostag_temp

        CALL nf_inq_varid_check(ncid_in, 'T', varid_in)
        CALL nf_get_vara_real_check(ncid_in, varid_in, 'T', NC_START, NC_COUNT, nostag_temp(:,:,:))
        temperature = (nostag_temp+300.0)* ((pressure/p_0)**kappa)
        air_density = pressure / dry_R / temperature

        ! ------ Actual Calculation of the properties of individual clouds are done
        ! here --------
        do zCtr = 1, nz
        do yCtr = 1, ny
        do xCtr = 1, nx
            nowLabel = labelCloud(xCtr,yCtr,zCtr,tCtr)    
            if (nowLabel.ne.0) then ! if isCloud
                if (output_options(1)) then ! size
                    nowValue = volume(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(1)%time(tCtr)%label(nowLabel)
                    cloudProperties(1)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                end if
                if (output_options(2)) then ! QCI
                    nowValue = QCI(xCtr,yCtr,zCtr) * air_density(xCtr,yCtr,zCtr) * volume(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(2)%time(tCtr)%label(nowLabel)
                    cloudProperties(2)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                end if
                if (output_options(3)) then ! QPR
                    nowValue = QPR(xCtr,yCtr,zCtr) * air_density(xCtr,yCtr,zCtr) * volume(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(3)%time(tCtr)%label(nowLabel)
                    cloudProperties(3)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                end if
                if (output_options(4)) then ! cloud_base
                    nowValue = height(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(4)%time(tCtr)%label(nowLabel)
                    if (nowValue.lt.prevProp) then
                        cloudProperties(4)%time(tCtr)%label(nowLabel) = nowValue
                    end if
                end if
                if (output_options(5)) then ! cloud_top
                    nowValue = height(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(5)%time(tCtr)%label(nowLabel)
                    if (nowValue.gt.prevProp) then
                        cloudProperties(5)%time(tCtr)%label(nowLabel) = nowValue
                    end if
                end if
                if (output_options(6)) then ! max_w
                    nowValue = W(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(6)%time(tCtr)%label(nowLabel)
                    if (nowValue.gt.prevProp) then
                        cloudProperties(6)%time(tCtr)%label(nowLabel) = nowValue
                    end if
                end if
                if (output_options(7)) then ! COG
                    nowValue = QCI(xCtr,yCtr,zCtr)* &
                             & air_density(xCtr,yCtr,zCtr)*volume(xCtr,yCtr,zCtr)*height(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(7)%time(tCtr)%label(nowLabel)
                    cloudProperties(7)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                end if
                if (output_options(10)) then ! convcore by w
                    nowValue = W(xCtr,yCtr,zCtr)
                    if (nowValue.ge.convcore_w) then
                        isCore(xCtr,yCtr,zCtr,tCtr) = 1
                    end if
                end if
                if (output_options(11)) then ! convcore size
                    nowValue = W(xCtr,yCtr,zCtr)
                    if (nowValue.ge.convcore_w) then
                        nowValue = volume(xCtr,yCtr,zCtr)
                        prevProp = cloudProperties(11)%time(tCtr)%label(nowLabel)
                        cloudProperties(11)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                    end if
                end if
                if (output_options(12)) then ! convcore avg w
                    nowValue = W(xCtr,yCtr,zCtr)
                    if (nowValue.ge.convcore_w) then
                        nowValue = W(xCtr,yCtr,zCtr)*air_density(xCtr,yCtr,zCtr)*volume(xCtr,yCtr,zCtr)
                        prevProp = cloudProperties(12)%time(tCtr)%label(nowLabel)
                        cloudProperties(12)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                    end if
                end if
                if (output_options(13)) then ! nonconvcore avg w
                    nowValue = W(xCtr,yCtr,zCtr)
                    if (nowValue.lt.convcore_w) then
                        nowValue = W(xCtr,yCtr,zCtr)*air_density(xCtr,yCtr,zCtr)*volume(xCtr,yCtr,zCtr)
                        prevProp = cloudProperties(13)%time(tCtr)%label(nowLabel)
                        cloudProperties(13)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                    end if
                end if
                if (output_options(14)) then ! cloud air mass
                    nowValue = air_density(xCtr,yCtr,zCtr) * volume(xCtr,yCtr,zCtr)
                    prevProp = cloudProperties(14)%time(tCtr)%label(nowLabel)
                    cloudProperties(14)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                end if
                if (output_options(15)) then ! cloud air mass
                    nowValue = W(xCtr,yCtr,zCtr)
                    if (nowValue.ge.convcore_w) then
                        nowValue = air_density(xCtr,yCtr,zCtr)*volume(xCtr,yCtr,zCtr)
                        prevProp = cloudProperties(15)%time(tCtr)%label(nowLabel)
                        cloudProperties(15)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                    end if
                end if
                if (output_options(16)) then ! cloud air mass
                    nowValue = W(xCtr,yCtr,zCtr)
                    if (nowValue.lt.convcore_w) then
                        nowValue = air_density(xCtr,yCtr,zCtr)*volume(xCtr,yCtr,zCtr)
                        prevProp = cloudProperties(16)%time(tCtr)%label(nowLabel)
                        cloudProperties(16)%time(tCtr)%label(nowLabel) = prevProp + nowValue
                    end if
                end if
            end if ! end if isCloud
        end do
        end do
        end do
        do lCtr = 1, cloudProperties(7)%time(tCtr)%n_label
            cloudProperties(7)%time(tCtr)%label(lCtr) = &
            & cloudProperties(7)%time(tCtr)%label(lCtr) / cloudProperties(2)%time(tCtr)%label(lCtr)
            cloudProperties(12)%time(tCtr)%label(lCtr) = &
            & cloudProperties(12)%time(tCtr)%label(lCtr) / cloudProperties(15)%time(tCtr)%label(lCtr)
            cloudProperties(13)%time(tCtr)%label(lCtr) = &
            & cloudProperties(13)%time(tCtr)%label(lCtr) / cloudProperties(16)%time(tCtr)%label(lCtr)
        end do


        ! END OBTAINING PROPERTIES...
        CALL nf_close_check(inputfile_name, ncid_in) 
    end do ! end do tCtr

    ! ------------- OUTPUT PROPERTIES --------------
    ! REMEMBER TO CHECK HERE WHEN YOU ADD NEW PROPERTIES
    WRITE(*,*) 'Start outputting cloud properties...(txt-file)'
    do oCtr = 1, 64
        if (output_options(oCtr)) then
            outputfile_name = 'prop_' // trim(case_name) // '_CLD' // trim(cloud_def) //&
                            & '_' // trim(output_names(oCtr)) // '.txt'
            output_fileid = 200 + oCtr
            WRITE(*,*) 'Outputting: ', outputfile_name
            OPEN(unit=output_fileid, file=outputfile_name, status='UNKNOWN', action='WRITE', iostat=check)
            CALL check_iostatus(check)
            CALL cloudPropertyList_outputTXT(cloudProperties(oCtr),output_fileid)
        end if
    end do
        
    WRITE(*,*) 'Start outputting cloud properties...(nc-file, if any)'
    if (output_options(10)) then
        outputfile_name = 'prop_' // trim(case_name) // '_CLD' // trim(cloud_def) //&
                        & '_' // trim(output_names(10)) // '.nc'
        CALL nf_create_check(outputfile_name, NF_64BIT_OFFSET, ncid_out)
        CALL nf_def_dim_check(ncid_out, 'west_east',   nx, xid)
        CALL nf_def_dim_check(ncid_out, 'south_north', ny, yid)
        CALL nf_def_dim_check(ncid_out, 'bottom_top',  nz, zid)
        CALL nf_def_dim_check(ncid_out, 'Time',        nt, tid)
       
        DIMS(1:4) = (/xid,yid,zid,tid/)
        CALL nf_def_var_check(ncid_out, 'isCore', NF_INT, 4, DIMS(1:4), varid_out(10))   
        CALL nf_enddef_check(outputfile_name, ncid_out) 
        CALL NF_PUT_var_INT_check(ncid_out, 'isCore', varid_out(10), isCore)
        CALL nf_close_check(outputfile_name, ncid_out) 
    end if
    ! END OUTPUT PROPERTIES

    CALL cpu_time(cpu_time_end)
    print '("Time = ",f15.7," seconds.")', cpu_time_end-cpu_time_start

END PROGRAM get_cloud_property
