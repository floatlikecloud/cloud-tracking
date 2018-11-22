MODULE useful_module
    
    ! --
    ! This module contains some subroutines shared among programs.
    ! Currently they are all for debugging and error handling.
    ! -- Hsiao, Wei-Ting wrote in Oct 2017
    
    implicit none
    CONTAINS

    SUBROUTINE check_iostatus(iostatus)
        implicit none
        integer, intent(in) :: iostatus
        if (iostatus<0) then
            STOP 'FILE I/O ERROR: beyond EOF'
        else if (iostatus>0) then
            STOP 'FILE I/O ERROR: file format error'
        end if
    END SUBROUTINE check_iostatus

    SUBROUTINE print_1d(vector, length)
        implicit none
        integer, intent(in) :: length
        integer, intent(in) :: vector(:)
        integer :: xCtr
    
        character(128) :: format_string
        WRITE(format_string, '(A1,I5,A3)') '(', length, 'I3)'
        WRITE(*,format_string) vector(:)

    END SUBROUTINE

    SUBROUTINE print_2d(image_2d, nx, ny)
        implicit none
        integer, intent(in) :: nx, ny
        integer, intent(in) :: image_2d(:,:)
        integer :: xCtr, yCtr
    
        character(128) :: format_string
        WRITE(format_string, '(A1,I5,A3)') '(', nx, 'I3)'

        do yCtr = 1, ny
            WRITE(*,format_string) image_2d(yCtr, :)
        end do

    END SUBROUTINE
END MODULE useful_module

