module family_search_module

use cloudList_type

implicit none
CONTAINS

! given a cloud list, search their union links at the next step
SUBROUTINE inputFamilyOneTimeStep(cloudList, tgt_cloudList, searchListMax, timePtr, timeShift)    
    type(cloudTimeLinkedlist), intent(in) :: cloudList
    type(cloudFamilyList), intent(inout)  :: tgt_cloudList
    integer, intent(in)                   :: searchListMax, timePtr, timeShift
    
    integer              :: searchPtr, cCtr, lCtr, pCtr, nowCloud, inCloudTemp
    logical              :: isRepeat

    type(cloudLabelAxis) :: searchList
    
    searchList%n_label = 0 ! n_label here is size, not capacity
    allocate( searchList%label(searchListMax) )

    ! For every cloud of tgt_cloudList%time(timePtr)%label( ), find their child unrepeatly
    if (tgt_cloudList%time(timePtr)%n_label.ge.1) then
    do cCtr = 1, tgt_cloudList%time(timePtr)%n_label
        ! e.g.: say at timePtr=5, three clouds exist in tgt_cloudList, say #5,7,13
        ! We want to obtain the clouds that linked to #5,7,13 at the desired time step
        ! cCtr runs through every cloud (#5,7,13), under that, we have to run through every linkages.
        nowCloud = tgt_cloudList%time(timePtr)%label(cCtr)
        if (cloudList%time(timePtr)%label(nowCloud)%n_ptr.ge.1) then
        do pCtr = 1, cloudList%time(timePtr)%label(nowCloud)%n_ptr
            isRepeat    = .False.
            inCloudTemp = cloudList%time(timePtr)%label(nowCloud)%ptr(pCtr)
            ! First, see if label is repeated in current searchList
            if (searchList%n_label.ne.0) then
            do searchPtr = 1, searchList%n_label
                if (searchList%label(searchPtr).eq.inCloudTemp) then
                    isRepeat = .True.
                end if
            end do
            end if
            ! Next, see if label is repeated in previous searching (tgt_cloudList)
            do searchPtr = 1, tgt_cloudList%time(timePtr+timeShift)%n_label
                if (tgt_cloudList%time(timePtr+timeShift)%label(searchPtr).eq.inCloudTemp) then
                    isRepeat = .True.
                end if
            end do
            if (.not.isRepeat) then
                searchList%n_label = searchList%n_label+1
                searchList%label(searchList%n_label) = inCloudTemp
            end if  
        end do ! pCtr
        end if
    end do
    end if
   
    if (searchList%n_label.ge.1) then
        do cCtr = tgt_cloudList%time(timePtr+timeShift)%n_label+1, tgt_cloudList%time(timePtr+timeShift)%n_label+searchList%n_label
            tgt_cloudList%time(timePtr+timeShift)%label(cCtr) = searchList%label(cCtr-tgt_cloudList%time(timePtr+timeShift)%n_label)
        end do
        tgt_cloudList%time(timePtr+timeShift)%n_label = tgt_cloudList%time(timePtr+timeShift)%n_label+searchList%n_label
    end if

    deallocate(searchList%label)

END SUBROUTINE inputFamilyOneTimeStep

END MODULE family_search_module

