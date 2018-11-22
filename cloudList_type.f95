MODULE cloudList_type
    implicit none

    ! FOR STORING THE TIME CONNECTION BETWEEN CLOUD LABELS
    ! cloudTimeLinkedlist%time(t)%label(l)%ptr(p)
    ! -----------------------------------------------------------
    ! Associated Subroutines:
    ! SUBROUTINE cloudTimeLinkedlist_build2d(this, nt, nl) 
    ! SUBROUTINE cloudTimeLinkedlist_buildAssignPtr(this, tt, ll, np, ptr)
    ! SUBROUTINE cloudTimeLinkedList_outputTXT(this, file_id)
    ! SUBROUTINE READ_cloudTimeLinkedList(fileid, filename, inList)
    type cloudLabelPtrAxis
        integer :: n_ptr = 0
        integer, allocatable :: ptr(:)
    end type cloudLabelPtrAxis

    type cloudTimeAxis
        integer :: n_label = 0
        type(cloudLabelPtrAxis), allocatable :: label(:)
    end type cloudTimeAxis

    type cloudTimeLinkedlist
        ! this -> time -> label -> ptr
        integer :: n_time = 0
        type(cloudTimeAxis), allocatable :: time(:)
    end type cloudTimeLinkedlist
    ! -----------------------------------------------------------


    ! FOR STORING A FAMILY OF CLOUDS
    ! (obtained using searching from seeds)
    ! cloudFamilyList%time(t)%label(l)
    ! -----------------------------------------------------------
    ! Associated Subroutines:
    ! SUBROUTINE cloudFamilyList_build(this, nt, nl)
    ! SUBROUTINE cloudFamilyList_outputTXT(this, file_id)
    ! SUBROUTINE READ_cloudFamilyList(fileid, filename, inList)
    type cloudLabelAxis
        integer :: n_label = 0
        integer, allocatable :: label(:)
    end type cloudLabelAxis

    type cloudFamilyList
        ! this -> time -> label
        integer :: n_time = 0
        type(cloudLabelAxis), allocatable :: time(:)
    end type cloudFamilyList
    ! -----------------------------------------------------------
    

    ! FOR STORING PROPS OF EVERY CLOUDS (REAL)
    ! cloudPropertyList%time(t)%label(l)
    ! -----------------------------------------------------------
    ! Associated Subroutines:
    ! SUBROUTINE cloudPropertyList_build(this, nt, nl)
    ! SUBROUTINE cloudPropertyList_assign(this, val)
    ! SUBROUTINE cloudPropertyList_outputTXT(this, file_id)
    type cloudPropertyAxis
        integer :: n_label = 0
        real, allocatable :: label(:)
    end type cloudPropertyAxis
    
    type cloudPropertyList
        ! this -> time -> label
        integer :: n_time = 0
        type(cloudPropertyAxis), allocatable :: time(:)
    end type cloudPropertyList


CONTAINS

    ! Build the list by giving the allocating numbers
    ! But only allocates time and label (no cloud_pointers)
    SUBROUTINE cloudTimeLinkedlist_build2d(this, nt, nl) 
        type(cloudTimeLinkedlist), intent(inout) :: this
        integer, intent(in) :: nt, nl(:) ! nl(time)
        integer :: tt

        this%n_time = nt
        allocate( this%time(this%n_time) )
        do tt = 1, nt
            this%time(tt)%n_label = nl(tt)
            allocate( this%time(tt)%label(this%time(tt)%n_label) )
        end do
    END SUBROUTINE cloudTimeLinkedlist_build2d

    ! Continue by cloudTimeLinkedlist_build2d
    ! Allocates cloud_pointers 
    SUBROUTINE cloudTimeLinkedlist_buildAssignPtr(this, tt, ll, np, ptr)
        type(cloudTimeLinkedlist), intent(inout) :: this
        integer, intent(in) :: tt, ll, np, ptr(:)
        integer :: pp    

        this%time(tt)%label(ll)%n_ptr = np
        allocate( this%time(tt)%label(ll)%ptr(np) )
        do pp = 1, np
            this%time(tt)%label(ll)%ptr(pp) = ptr(pp)
        end do
    END SUBROUTINE cloudTimeLinkedlist_buildAssignPtr

    ! Build the list by giving the allocating numbers
    SUBROUTINE cloudFamilyList_build(this, nt, nl)
        type(cloudFamilyList), intent(inout) :: this
        integer, intent(in) :: nt, nl(:)
        integer :: tt

        this%n_time = nt
        allocate( this%time(this%n_time) )
        do tt = 1, nt
            this%time(tt)%n_label = nl(tt)
            allocate( this%time(tt)%label(this%time(tt)%n_label) )
        end do
    END SUBROUTINE cloudFamilyList_build
    
    ! Build the list by giving the allocating numbers
    ! Initialize the properties to zero.
    SUBROUTINE cloudPropertyList_build(this, nt, nl)
        type(cloudPropertyList), intent(inout) :: this
        integer, intent(in) :: nt, nl(:)
        integer :: tt, ll

        this%n_time = nt
        allocate( this%time(this%n_time) )
        do tt = 1, nt
            this%time(tt)%n_label = nl(tt)
            allocate( this%time(tt)%label(this%time(tt)%n_label) )
            do ll = 1, nl(tt)
                this%time(tt)%label(ll) = 0.0
            end do
        end do
    END SUBROUTINE cloudPropertyList_build

    ! Assign a single value to all the properties.    
    SUBROUTINE cloudPropertyList_assign(this, val)
        type(cloudPropertyList), intent(inout) :: this
        real, intent(in) :: val 
        integer :: nt, nl, tt, ll

        nt = this%n_time
        do tt = 1, nt
            nl = this%time(tt)%n_label
            do ll = 1, nl
                this%time(tt)%label(ll) = val
            end do
        end do
    END SUBROUTINE cloudPropertyList_assign

    ! Output the cloudTimeLinkedList (for debugging)
    SUBROUTINE cloudTimeLinkedList_outputTXT(this, file_id)
        type(cloudTimeLinkedList), intent(in) :: this
        integer, intent(in) :: file_id
        integer :: tt, ll, pp

        WRITE(file_id,*) this%n_time
        do tt = 1, this%n_time
            WRITE(file_id,*) this%time(tt)%n_label
            do ll = 1, this%time(tt)%n_label
                WRITE(file_id,*) this%time(tt)%label(ll)%n_ptr
                do pp = 1, this%time(tt)%label(ll)%n_ptr
                    WRITE(file_id,*) this%time(tt)%label(ll)%ptr(pp)
                end do
            end do
        end do

    END SUBROUTINE cloudTimeLinkedList_outputTXT

    ! Output the cloudFamilyList (for debugging)
    SUBROUTINE cloudFamilyList_outputTXT(this, file_id)
        type(cloudFamilyList), intent(in) :: this
        integer, intent(in) :: file_id
        integer :: tt, ll

        WRITE(file_id,*) this%n_time
        do tt = 1, this%n_time
            WRITE(file_id,*) this%time(tt)%n_label
            do ll = 1, this%time(tt)%n_label
                WRITE(file_id,*) this%time(tt)%label(ll)
            end do
        end do

    END SUBROUTINE cloudFamilyList_outputTXT
    
    ! Output the cloudPropertyList (for debugging)
    SUBROUTINE cloudPropertyList_outputTXT(this, file_id)
        type(cloudPropertyList), intent(in) :: this
        integer, intent(in) :: file_id
        integer :: tt, ll

        WRITE(file_id,*) this%n_time
        do tt = 1, this%n_time
            WRITE(file_id,*) this%time(tt)%n_label
            do ll = 1, this%time(tt)%n_label
                WRITE(file_id,*) this%time(tt)%label(ll)
            end do
        end do

    END SUBROUTINE cloudPropertyList_outputTXT

    ! READ in cloudTimeLinkedList (in the form of txt-file)
    ! The txt-file should be output by timeHistory.out
    SUBROUTINE READ_cloudTimeLinkedList(fileid, filename, inList)
        integer, intent(in)                    :: fileid
        character(256), intent(in)             :: filename
        type(cloudTimeLinkedlist), intent(out) :: inList

        integer :: check, tCtr, lCtr, pCtr

        OPEN(unit=fileid, file=filename, status='old', action='read', iostat=check)
        if (check.ne.0) STOP 'file reading error'
        READ(fileid,*) inList%n_time
        allocate( inList%time(inList%n_time) )
        do tCtr = 1, inList%n_time
            READ(fileid,*) inList%time(tCtr)%n_label
            if (inList%time(tCtr)%n_label.ne.0) then
                allocate( inList%time(tCtr)%label(inList%time(tCtr)%n_label) )
                do lCtr = 1, inList%time(tCtr)%n_label
                    READ(fileid,*) inList%time(tCtr)%label(lCtr)%n_ptr
                    if (inList%time(tCtr)%label(lCtr)%n_ptr.ne.0) then
                        allocate( inList%time(tCtr)%label(lCtr)%ptr(inList%time(tCtr)%label(lCtr)%n_ptr) )
                        do pCtr = 1, inList%time(tCtr)%label(lCtr)%n_ptr
                            READ(fileid,*) inList%time(tCtr)%label(lCtr)%ptr(pCtr)
                        end do
                    end if
                end do
            end if
        end do
        CLOSE(fileid)
    END SUBROUTINE READ_cloudTimeLinkedList

    ! READ in cloudFamilyList (in the form of txt-file)
    ! The txt-file should be output by searchFamilyUnion.out or searchSingleFamily.out 
    SUBROUTINE READ_cloudFamilyList(fileid, filename, inList)
        integer, intent(in)                    :: fileid
        character(256), intent(in)             :: filename
        type(cloudFamilyList), intent(out) :: inList

        integer :: check, tCtr, lCtr

        OPEN(unit=fileid, file=filename, status='old', action='read', iostat=check)
        if (check.ne.0) STOP 'file reading error'
        READ(fileid,*) inList%n_time
        allocate( inList%time(inList%n_time) )
        do tCtr = 1, inList%n_time
            READ(fileid,*) inList%time(tCtr)%n_label
            if (inList%time(tCtr)%n_label.ne.0) then
                allocate( inList%time(tCtr)%label(inList%time(tCtr)%n_label) )
                do lCtr = 1, inList%time(tCtr)%n_label
                    READ(fileid,*) inList%time(tCtr)%label(lCtr)
                end do
            end if
        end do
        CLOSE(fileid)
    END SUBROUTINE READ_cloudFamilyList

END MODULE cloudList_type
