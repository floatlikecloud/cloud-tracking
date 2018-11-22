MODULE unionFindTree_module
    implicit none
    ! --
    ! This module contains the subroutines of 2D connected-component labeling (CCL)
    ! whose algorithm is introduced by Wu et. al (2009).
    ! 
    ! Only the subroutines related to the unionFindTree that is 
    ! used in the first_scan is in this file.
    !
    ! -- Hsiao, Wei-Ting (floatlikecloud) wrote in Oct 2017
    
    CONTAINS
    SUBROUTINE flattenL(tree, n)
        ! flatten the tree in to height 1 in consecutive labels
        ! used in the final stage of first_scan
        implicit none
        integer, intent(in)    :: n
        integer, intent(inout) :: tree(:)
        integer :: i, k
        
        k = 1
        do i = 1, n-1
            if (tree(i).lt.i) then
                tree(i) = tree(tree(i))
            else
                tree(i) = k
                k = k + 1
            end if
        end do
    END SUBROUTINE flattenL

    SUBROUTINE find_root_compress(tree, nowNode, root)
        ! find the root of the tree and compress the path at the same time
        implicit none
        integer, intent(inout) :: tree(:), nowNode
        integer, intent(out)   :: root

        CALL find_root(tree, nowNode, root)
        CALL set_root(tree, nowNode, root)
    END SUBROUTINE find_root_compress

    SUBROUTINE union(tree, node_i, node_j, root)
        ! unify two trees
        implicit none
        integer, intent(inout) :: tree(:), node_i, node_j
        integer, intent(out)   :: root
        integer :: root_j

        CALL find_root_compress(tree, node_i, root)
        if (node_i.ne.node_j) then
            CALL find_root_compress(tree, node_j, root_j)
            if (root.gt.root_j) root = root_j
            CALL set_root(tree, node_j, root)
        end if
        CALL set_root(tree, node_i, root)
    END SUBROUTINE union

    SUBROUTINE find_root(tree, nowNode, root)
        ! finding root
        implicit none
        integer, intent(in)  :: tree(:), nowNode
        integer, intent(out) :: root

        root = nowNode
        do while (tree(root).lt.root)
            root = tree(root)
        end do
    END SUBROUTINE find_root

    SUBROUTINE set_root(tree, nowNode, root)
        ! setting of the nodes in the tree to connected directly onto the root
        implicit none
        integer, intent(in)    :: root
        integer, intent(inout) :: tree(:), nowNode
        integer :: nowNode2

        do while (tree(root).lt.nowNode)
            nowNode2 = tree(nowNode)
            tree(nowNode) = root
            nowNode = nowNode2
        end do
        tree(nowNode) = root
    END SUBROUTINE set_root

END MODULE unionFindTree_module
