MODULE sct1uft_3d_module
    
    ! --
    ! This module contains the subroutines of 2D connected-component labeling (CCL)
    ! of which the algorithm is introduced by Wu et. al (2009).
    !
    ! Then the method is adapted to 3-D usage according to He et. al (2011)
    ! for 6-connection in 3-D.
    !
    ! Only the subroutines related to the first_scan is in this file.
    ! Union-find-tree algorithm is used in the first_scan in order to build a
    ! label connection table.
    !
    ! -- Hsiao, Wei-Ting (floatlikecloud) wrote in Oct 2017
    
    use useful_module
    use unionFindTree_module
    implicit none
    CONTAINS
    SUBROUTINE first_scan(image_3d, label_3d, connectionTable, nx, ny, nz)
        ! This is the main subroutine of this module,
        ! that packed all the processes during the first_scan.
        implicit none
        integer, intent(in)     :: image_3d(:,:,:), ny, nx, nz
        integer, intent(inout)  :: label_3d(:,:,:), connectionTable(:)
        integer                 :: xCtr, yCtr, zCtr
        integer                 :: lCtr ! new labeling counter
        logical                 :: xStat, yStat, zStat
                                   ! if *Stat = .FALSE. , *axis is at lower boundary
                                   ! if *Stat = .TRUE.  , *axis is normal or at upper boundary
        xStat = .FALSE.
        yStat = .FALSE.
        zStat = .FALSE.
        lCtr = 1
        ! start scanning
        do zCtr = 1, nz
            yStat = .FALSE. ! reset y
            do yCtr = 1, ny
                xStat = .FALSE. ! reset x
                do xCtr = 1, nx
                    if (image_3d(xCtr,yCtr,zCtr).eq.0) then
                        label_3d(xCtr,yCtr,zCtr) = 0
                    else
                        ! label if image(currentPixel) = 1
                        CALL decision_tree(label_3d, connectionTable, xCtr, yCtr, zCtr, xStat, yStat, zStat, lCtr)
                    end if
                    xStat = .TRUE.
                end do
                yStat = .TRUE.
            end do
            zStat = .TRUE.
        end do
        ! end scanning

        ! relabel the labels in the table into consecutive integers
        if (lCtr.gt.nx*ny*nz/2) STOP 'lCtr boom'
        CALL flattenL(connectionTable, lCtr)
    END SUBROUTINE

    SUBROUTINE decision_tree(label_3d, connectionTable, xCtr, yCtr, zCtr, xStat, yStat, zStat, lCtr)
        ! Decide the label by searching the pixels in the mask.
        ! Using "forward mask" + "6-connection"
        !
        ! currentPixel: n
        ! z=zCtr-1        z=zCtr        z=zCtr+1
        ! |-|-|-|        |-|k|-|        |-|-|-|
        ! |-|e|-|        |m|n|-|        |-|-|-|
        ! |-|-|-|        |-|-|-|        |-|-|-|
        !
        implicit none
        integer, intent(in)     :: xCtr, yCtr, zCtr
        logical, intent(in)        :: xStat, yStat, zStat
        integer, intent(inout)    :: label_3d(:,:,:), connectionTable(:), lCtr
        integer :: mYES, kYES, eYES, decision
        
        mYES = 0
        kYES = 0
        eYES = 0
        if (xStat) then
            if (label_3d(xCtr-1,yCtr,zCtr).ne.0) then 
                mYES = 1
            end if
        end if
        if (yStat) then
            if (label_3d(xCtr,yCtr-1,zCtr).ne.0) then
                kYES = 1
            end if
        end if
        if (zStat) then
            if (label_3d(xCtr,yCtr,zCtr-1).ne.0) then 
                eYES = 1
            end if
        end if
        !write(*,'(A11,I1,A1,I1,A1,I1,A1)') '(x,y,z) = (',xCtr,',',yCtr,',',zCtr,')'
        !write(*,'(A11,I1,A1,I1,A1,I1,A1)') '(m,k,e) = (',mYES,',',kYES,',',eYES,')'
        decision = mYES + kYES*2 + eYES*4

        SELECT CASE(decision)
            
            CASE(0) ! (m,k,e) = (0,0,0)
                CALL new_label(label_3d, connectionTable, xCtr, yCtr, zCtr, lCtr)
            CASE(1) ! (m,k,e) = (1,0,0)
                CALL copy_one(label_3d, connectionTable, xCtr, yCtr, zCtr, -1, 0, 0)
            CASE(2) ! (m,k,e) = (0,1,0)
                CALL copy_one(label_3d, connectionTable, xCtr, yCtr, zCtr, 0, -1, 0)
            CASE(3) ! (m,k,e) = (1,1,0)
                CALL copy_two(label_3d, connectionTable, xCtr, yCtr, zCtr, -1, 0, 0, 0, -1, 0)
            CASE(4) ! (m,k,e) = (0,0,1)
                CALL copy_one(label_3d, connectionTable, xCtr, yCtr, zCtr, 0, 0, -1)
            CASE(5) ! (m,k,e) = (1,0,1)
                CALL copy_two(label_3d, connectionTable, xCtr, yCtr, zCtr, -1, 0, 0, 0, 0, -1)
            CASE(6) ! (m,k,e) = (0,1,1)
                CALL copy_two(label_3d, connectionTable, xCtr, yCtr, zCtr, 0, -1, 0, 0, 0, -1)
            CASE DEFAULT ! (m,k,e) = (1,1,1)
                CALL copy_three(label_3d, connectionTable, xCtr, yCtr, zCtr, -1, 0, 0, 0, -1, 0, 0, 0, -1)

        END SELECT
    
        CONTAINS
        SUBROUTINE copy_one(label_3d, connectionTable, xCtr, yCtr, zCtr, xShift, yShift, zShift)
            implicit none
            integer, intent(inout) :: label_3d(:,:,:), connectionTable(:)
            integer, intent(in)    :: xCtr, yCtr, zCtr, xShift, yShift, zShift

            label_3d(xCtr,yCtr,zCtr) = label_3d(xCtr+xShift, yCtr+yShift, zCtr+zShift)
        END SUBROUTINE copy_one
        
        SUBROUTINE copy_two(label_3d, connectionTable, xCtr, yCtr, zCtr, &
                            x1Shift, y1Shift, z1Shift, x2Shift, y2Shift, z2Shift)
            implicit none
            integer, intent(inout) :: label_3d(:,:,:), connectionTable(:)
            integer, intent(in)    :: xCtr, yCtr, zCtr, x1Shift, y1Shift, z1Shift, x2Shift, y2Shift, z2Shift

            CALL union(connectionTable, label_3d(xCtr+x1Shift, yCtr+y1Shift, zCtr+z1Shift), &
                       label_3d(xCtr+x2Shift, yCtr+y2Shift, zCtr+z2Shift), label_3d(xCtr,yCtr,zCtr))
        END SUBROUTINE copy_two
        
        SUBROUTINE copy_three(label_3d, connectionTable, xCtr, yCtr, zCtr, x1Shift, y1Shift, z1Shift, &
                            x2Shift, y2Shift, z2Shift, x3Shift, y3Shift, z3Shift)
            implicit none
            integer, intent(inout) :: label_3d(:,:,:), connectionTable(:)
            integer, intent(in)    :: xCtr, yCtr, zCtr, x1Shift, y1Shift, z1Shift, &
                                        x2Shift, y2Shift, z2Shift, x3Shift, y3Shift, z3Shift
            integer :: label_1, root_1, label_2, root_2

            ! First, arbitrary union two tree
            if (connectionTable(label_3d(xCtr+x1Shift, yCtr+y1Shift, zCtr+z1Shift)).ge.&
            connectionTable(label_3d(xCtr+x2Shift, yCtr+y2Shift, zCtr+z2Shift))) then
                label_1 = label_3d(xCtr+x1Shift, yCtr+y1Shift, zCtr+z1Shift)
                root_1 = connectionTable(label_3d(xCtr+x1Shift, yCtr+y1Shift, zCtr+z1Shift))
            else
                label_1 = label_3d(xCtr+x2Shift, yCtr+y2Shift, zCtr+z2Shift)
                root_1 = connectionTable(label_3d(xCtr+x2Shift, yCtr+y2Shift, zCtr+z2Shift))
            end if
            CALL union(connectionTable, label_3d(xCtr+x1Shift, yCtr+y1Shift, zCtr+z1Shift), &
                       label_3d(xCtr+x2Shift, yCtr+y2Shift, zCtr+z2Shift), label_3d(xCtr,yCtr,zCtr))
            
            if (root_1.ge.connectionTable(label_3d(xCtr+x3Shift, yCtr+y3Shift, zCtr+z3Shift))) then
                label_2 = label_1
                root_2  = root_1
            else
                label_2 = label_3d(xCtr+x3Shift, yCtr+y3Shift, zCtr+z3Shift)
                root_2  = connectionTable(label_3d(xCtr+x3Shift, yCtr+y3Shift, zCtr+z3Shift))
            end if
            CALL union(connectionTable, label_1, label_3d(xCtr+x3Shift, yCtr+y3Shift, zCtr+z3Shift), label_3d(xCtr,yCtr,zCtr))
        END SUBROUTINE copy_three

        SUBROUTINE new_label(label_3d, connectionTable, xCtr, yCtr, zCtr, lCtr)
            implicit none
            integer, intent(inout) :: label_3d(:,:,:), connectionTable(:), lCtr
            integer, intent(in)    :: xCtr, yCtr, zCtr

            label_3d(xCtr,yCtr,zCtr) = lCtr
            connectionTable(lCtr) = lCtr
            lCtr = lCtr + 1
        END SUBROUTINE new_label

    END SUBROUTINE decision_tree

END MODULE sct1uft_3d_module

