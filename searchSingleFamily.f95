PROGRAM searchSingleFamily
    
    ! Main Program of time searching from a single seed
    !
    ! Given a seed,
    ! the program go through every link that start from the seed and
    ! record them down. The meaning of the list could be roughly
    ! intepreted as the time tracking of the seed cloud through out
    ! their life time and also through splitting/merging events.
    !
    ! Last Modified by floatlikecloud Nov.22 2018
    ! Contact: floatlikecloud@gmail.com
    !
    ! ---------- USAGE --------
    ! Preparation:
    ! You will need the labeling file (output from the cloud_labeling.out)
    ! just for obtaining the number of clouds to do space usage evaluation,
    ! a seed (a time and a label),
    ! and the time-connection list (two output from timeHistory.out).
    !
    ! Setting:
    ! N/A 
    ! 
    ! -------------------------

    use cloudList_type
    use input_netcdf_module
    use useful_module
    use family_search_module
    implicit none


    CHARACTER(256)            :: case_name, cloud_def, seed_def 
    character(256)            :: inputlabel_name, inputlinkedlist_name, inputcore_name, output_name
    integer                   :: tCtr, lCtr
    type(cloudTimeLinkedlist) :: cloudBackList, cloudForList
    type(cloudFamilyList)     :: isDeepConvCore
    integer                   :: nt

    integer :: input_time, input_label

    integer                     :: NC_START, NC_COUNT
    integer                     :: ncid_in, tid, varid_in
    integer, allocatable        :: numCloud(:)

    integer           :: tgt_t
    type(cloudFamilyList) :: tgt_cloudList
    integer           :: timePtr
    integer           :: searchListMax
  
! ##################################################################
! ######################## MANUAL OPTIONS ##########################
! ##################################################################
    case_name = 'TWPICE10m'
    cloud_def = 'qci3em4'
    seed_def  = 'q1em3w10'

    inputlabel_name = 'out_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.nc'
    
    input_time  = 194
    input_label = 540
! ##################################################################
! ##################################################################
    
    ! retrieve linked web
    inputlinkedlist_name = 'cloudBackList_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.txt'
    CALL READ_cloudTimeLinkedList(101, inputlinkedlist_name, cloudBackList)
    inputlinkedlist_name = 'cloudForList_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.txt'
    CALL READ_cloudTimeLinkedList(102, inputlinkedlist_name, cloudForList)
 

    nt = cloudBackList%n_time
    allocate( numCloud(nt) )    
    ! retrieve numCloud
    NC_START = 1
    NC_COUNT = nt
    CALL nf_open_check(inputlabel_name, nf_nowrite, ncid_in)
    CALL NF_INQ_DIMID_check(ncid_in, 'Time', tid)
    CALL nf_inq_varid_check(ncid_in, 'numCloud', varid_in)
    CALL nf_get_vara_int_check(ncid_in, varid_in, 'numCloud', (/NC_START/), (/NC_COUNT/), numCloud)
    CALL nf_close_check(inputlabel_name, ncid_in)

    ! Initialization
    tgt_cloudList%n_time = nt
    allocate(tgt_cloudList%time(nt))
    do tCtr = 1, tgt_cloudList%n_time
        allocate(tgt_cloudList%time(tCtr)%label(numCloud(tCtr)))
    end do
    tgt_cloudList%time(input_time)%n_label  = 1
    tgt_cloudList%time(input_time)%label(1) = input_label

    ! Search Start
    ! NEED num of possible labels!
    searchListMax = maxval(numCloud)
    
    ! set target cloud
    tgt_t = input_time
        timePtr = tgt_t
        if (timePtr.gt.1) then
        do while (timePtr.gt.1)
            CALL inputFamilyOneTimeStep(cloudBackList, tgt_cloudList, searchListMax, timePtr, -1)
            timePtr = timePtr - 1
        end do
        end if
        timePtr = tgt_t
        if (timePtr.lt.tgt_cloudList%n_time) then
        do while (timePtr.lt.tgt_cloudList%n_time)
            CALL inputFamilyOneTimeStep(cloudForList, tgt_cloudList, searchListMax, timePtr, 1)
            timePtr = timePtr + 1
        end do
        end if
    ! End Search

    output_name = 'bigSizeFamily01_' // trim(case_name) // '_' // trim(seed_def) // '_CLD' // trim(cloud_def) // '.txt'
    OPEN( unit=201, file=output_name, status='unknown', action='write')

    CALL cloudFamilyList_outputTXT(tgt_cloudList, 201)

END PROGRAM searchSingleFamily

