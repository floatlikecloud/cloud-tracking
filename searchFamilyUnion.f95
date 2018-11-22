PROGRAM searchFamilyUnion
    
    ! Main Program of time searching from multiple seeds 
    !
    ! Given a list of seeds (e.g. output from the isDeepConvSeed.out),
    ! the program go through every link that start from the seeds and
    ! record them down. The meaning of the list could be roughly
    ! intepreted as the time tracking of those seed clouds through out
    ! their life time and also through splitting/merging events.
    !
    ! Last Modified by floatlikecloud Nov.22 2018
    ! Contact: floatlikecloud@gmail.com
    !
    ! ---------- USAGE --------
    ! Preparation:
    ! You will need the labeling file (output from the cloud_labeling.out)
    ! just for obtaining the number of clouds to do space usage evaluation,
    ! a list of seeds (output from the isDeepConvSeed.out),
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

    ! nc-file input parameters
    integer                     :: check
    integer                     :: NC_START, NC_COUNT
    integer                     :: ncid_in, tid, varid_in
    integer, allocatable        :: numCloud(:)

    integer           :: tgt_t             ! now processing time (of seed)
    type(cloudFamilyList) :: tgt_cloudList ! output 
    integer           :: timePtr           ! now processing time (the searching from seed)
    integer           :: searchListMax     ! capacity of search list
   
! ##################################################################
! ######################## MANUAL OPTIONS ##########################
! ##################################################################
    case_name = 'NTU-S3DVC'
    cloud_def = 'qci5em5'
    seed_def  = 'q1em3w10'
! ##################################################################
! ##################################################################

    inputlabel_name = 'out_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.nc'
    
    ! retrieve linked web
    inputlinkedlist_name = 'cloudBackList_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.txt'
    CALL READ_cloudTimeLinkedList(101, inputlinkedlist_name, cloudBackList)
    inputlinkedlist_name = 'cloudForList_' // trim(case_name) // '_CLD' // trim(cloud_def) // '.txt'
    CALL READ_cloudTimeLinkedList(102, inputlinkedlist_name, cloudForList)
    ! retrieve target cloud labels
    inputcore_name = 'isDCSeed_' // trim(case_name) // '_' // trim(seed_def) // '_CLD' // trim(cloud_def) // '.txt'
    CALL READ_cloudFamilyList(103, inputcore_name, isDeepConvCore)
    ! end retrieval
    

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
        tgt_cloudList%time(tCtr)%n_label = isDeepConvCore%time(tCtr)%n_label
        if (isDeepConvCore%time(tCtr)%n_label.gt.0) then
            do lCtr = 1, isDeepConvCore%time(tCtr)%n_label
                tgt_cloudList%time(tCtr)%label(lCtr) = isDeepConvCore%time(tCtr)%label(lCtr)
            end do
        end if
    end do

    ! Search Start
    ! NEED num of possible labels!
    searchListMax = maxval(numCloud)
    
    ! set target cloud
    do tgt_t = 1, tgt_cloudList%n_time
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
    end do
    ! End Search

    output_name = 'dcFamily_' // trim(case_name) // '_' // trim(seed_def) // '_CLD' // trim(cloud_def) // '.txt'
    OPEN( unit=201, file=output_name, status='unknown', action='write')

    CALL cloudFamilyList_outputTXT(tgt_cloudList, 201)

END PROGRAM searchFamilyUnion

