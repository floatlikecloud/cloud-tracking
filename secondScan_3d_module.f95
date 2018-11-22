MODULE secondScan_3d_module
    ! --
    ! This module contains the subroutines of 2D connected-component labeling (CCL)
    ! whose algorithm is introduced by Wu et. al (2009).
    ! 
    ! This method is adapted to 3-D usage according to He et. all (2011)
    !
    ! Only the subroutines related to the second_scan is in this file.
    !
    ! -- Hsiao, Wei-Ting (floatlikecloud) wrote in Oct 2017
    implicit none

    CONTAINS
    SUBROUTINE second_scan(label_3d, connectionTable, nx, ny, nz, numCloud)
        ! Unify the labels that could not be recognize to be the same in the first scan.
        ! Also rearrange the labels into consecutive numbers.
        integer, intent(in)    :: connectionTable(:), nx, ny, nz
        integer, intent(inout) :: label_3d(:,:,:)
        integer, intent(out)   :: numCloud
        integer :: xCtr, yCtr, zCtr
        integer :: nowLabel, rightLabel

        numCloud = 0

        do zCtr = 1, nz
        do yCtr = 1, ny
        do xCtr = 1, nx
            nowLabel = label_3d(xCtr,yCtr,zCtr)
            if (nowLabel.ne.0) then
                rightLabel = connectionTable(nowLabel)
                if (nowLabel.ne.rightLabel) then
                    label_3d(xCtr,yCtr,zCtr) = rightLabel
                end if
                if (numCloud.lt.label_3d(xCtr,yCtr,zCtr)) then
                    numCloud = label_3d(xCtr,yCtr,zCtr)
                end if
            end if
        end do
        end do
        end do
        
        
    END SUBROUTINE second_scan

END MODULE secondScan_3d_module
