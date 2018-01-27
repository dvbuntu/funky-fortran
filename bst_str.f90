module bst_str
    implicit none

    ! For Array of strings of arbitrary length
    type string
        character(len=:), allocatable :: str
    end type string

    type :: node
        integer, allocatable :: value
        type(string), allocatable :: str
        type(node), pointer :: left => null()
        type(node), pointer :: right => null()
        type(node), pointer :: parent => null()
        logical :: initialized = .false.
    end type node

contains

type(string) function make_str(chars)
    character(len=*), intent(in) :: chars
    type(string) :: new_val
    allocate(character(len=len(chars)) :: new_val%str)
    new_val%str = chars
    make_str = new_val
end function make_str

subroutine bst_init(self,init_val, init_str)
    ! Create a new linked list with the base value
    type(node), pointer :: self
    integer, intent(in) :: init_val
    character(len=*), intent(in) :: init_str
    type(string) :: new_str

    new_str = make_str(init_str)


    allocate(self)
    nullify(self%left)
    nullify(self%right)
    nullify(self%parent)

    self%value = init_val
    self%str = new_str
    self%initialized = .true.

end subroutine bst_init

subroutine bst_add(self, val, str)
    ! Put a new node on the front
    type(node), pointer :: self
    type(node), pointer :: pos
    type(node), pointer :: tmp
    integer, intent(in) :: val
    character(len=*), intent(in) :: str
    type(string) :: new_str

    new_str = make_str(str)

    if ( .not. associated(self)  ) then
        call bst_init(self, val, str)
    else if (.not. self%initialized) then
        call bst_init(self, val, str)
    else
        ! Chase down position
        tmp => self
        recurse: do while ( associated(tmp) )
            if ( val < tmp%value ) then
                if (associated(tmp%left)) then
                    tmp => tmp%left
                else
                    pos => tmp
                    ! New memory
                    allocate(tmp%left)
                    tmp => tmp%left
                    pos%left => tmp
                    exit recurse
                end if
            else if ( val .eq. tmp%value ) then
                nullify(pos)
                exit recurse
            else if ( val > tmp%value ) then
                if (associated(tmp%right)) then
                    tmp => tmp%right
                else
                    pos => tmp
                    ! New memory
                    allocate(tmp%right)
                    tmp => tmp%right
                    pos%right => tmp
                    exit recurse
                end if
            end if
        end do recurse

        if (associated(pos)) then
            ! Assign value
            tmp%value = val
            tmp%str = new_str
            ! Point to parent
            tmp%parent => pos
            ! Empty left and right
            tmp%left => null()
            tmp%right => null()
            ! Initialize this one too
            tmp%initialized = .true.
        end if
    end if

end subroutine bst_add

!integer function list_pop(self)
!    ! Return top value and set head to next
!    integer :: val
!    type(node), pointer :: self, tmp
!    ! Grab the value
!    val = self%value
!    ! Save to dealloc later
!    tmp => self
!    ! Advance head
!    self => self%next
!    ! Free memory
!    deallocate(tmp)
!
!    list_pop = val ! This is stupid and confusing
!    return
!end function list_pop
!
recursive subroutine bst_print(self, depth)
    ! Print all values
    type(node), pointer :: self, tmp
    integer,intent(in),optional :: depth
    integer :: d
    tmp => self

    if (present(depth)) then
        d = depth
    else
        d = 0
    end if

    if (associated(tmp)) then
        if (associated(tmp%parent)) then
            write(*,*) "Depth", d, ":", tmp%parent%value, ":", tmp%parent%str%str, "->", tmp%value, ":", tmp%str%str
        else
            write(*,*) "Depth", d, ":", "   Root", "->", tmp%value, tmp%str%str
        end if
        ! Depth first print... not great
        call bst_print(tmp%left, d+1)
        call bst_print(tmp%right, d+1)
    else if (.not. associated(tmp) .and. d == 0) then
        write(*,*) "Empty!"
    end if
end subroutine bst_print

!integer function list_len(self)
!    ! Compute list length
!    integer :: l
!    type(node), pointer :: self, tmp
!    tmp => self
!
!    l = 0
!
!    do while ( associated(tmp))
!        l = l + 1
!        tmp => tmp%next
!    end do
!
!    list_len = l
!end function list_len
!
recursive function bst_find(self, val) result(ans)
    ! Find node containing value
    ! If not found, return node with null
    integer, intent(in) :: val
    type(node), target :: self
    type(node), pointer :: tmp, ans
    tmp => self
    ans => null()

    if (associated(tmp)) then
        if (tmp%value == val) then
            ans => tmp
            return
        else if (associated(tmp%left) .and. tmp%value > val) then
            ans => bst_find(tmp%left, val)
            if (associated(ans)) return
        else if (associated(tmp%right) .and. tmp%value < val) then
            ans => bst_find(tmp%right, val)
            if (associated(ans)) return
        end if
    end if
end function bst_find

recursive function bst_str_find(self, str) result(ans)
    ! Find node containing str
    ! Just need to look everywhere.
    ! If not found, return node with null
    type(node), target :: self
    character(len=*), intent(in) :: str
    type(node), pointer :: tmp, ans
    tmp => self
    ans => null()


    if (associated(tmp)) then
        if (tmp%str%str == str) then
            ans => tmp
            return
        end if
        if (associated(tmp%left)) then
            ans => bst_str_find(tmp%left, str)
            if (associated(ans)) return
        end if
        if (associated(tmp%right)) then
            ans => bst_str_find(tmp%right, str)
            if (associated(ans)) return
        end if
    end if
end function bst_str_find


subroutine bst_remove(self, val)
    ! Remove the occurrence of val from the bst
    integer, intent(in) :: val
    type(node), pointer :: self
    type(node), pointer :: tmp, loc, parent
    tmp => self

    loc => bst_find(tmp, val)

    if (associated(loc)) then
        ! Three options, leaf, 1 child, 2 children
        ! Leaf
        if ( .not. associated(loc%left) .and. .not. associated(loc%right) ) then
            ! Remove link from parent
            parent => loc%parent
            if ( parent%left%value == val) then
                parent%left => null()
            else if ( parent%right%value == val ) then
                parent%right => null()
            end if
            ! Free memory
            deallocate(loc%str)
            deallocate(loc)
        ! 1 child
        else if ( associated(loc%left) .neqv. associated(loc%right) )  then
            ! Replace current with child
            if ( associated(loc%left)) then
                loc%value = loc%left%value
                loc%str = loc%left%str
                deallocate(loc%left%str)
                deallocate(loc%left)
                loc%left => null()
            else if ( associated(loc%right)) then
                loc%value = loc%right%value
                loc%str = loc%right%str
                deallocate(loc%right%str)
                deallocate(loc%right)
                loc%right => null()
            end if
        ! 2 children, chase down first right, then left
        else
            tmp => loc%right
            do while (associated(tmp%left))
                tmp => tmp%left
            end do
            loc%value = tmp%value
            loc%str = tmp%str
            tmp%parent%left => null()
            deallocate(tmp%str)
            deallocate(tmp)
        end if
    end if
end subroutine bst_remove

subroutine bst_str_remove(self, str)
    ! Remove the occurrence of val from the bst
    character(len=*), intent(in) :: str
    type(node), pointer :: self
    type(node), pointer :: tmp, loc, parent
    tmp => self

    loc => bst_str_find(tmp, str)

    if (associated(loc)) then
        ! Three options, leaf, 1 child, 2 children
        ! Leaf
        if ( .not. associated(loc%left) .and. .not. associated(loc%right) ) then
            ! Remove link from parent
            parent => loc%parent
            if ( parent%left%str%str == str) then
                parent%left => null()
            else if ( parent%right%str%str == str ) then
                parent%right => null()
            end if
            ! Free memory
            deallocate(loc%str)
            deallocate(loc)
        ! 1 child
        else if ( associated(loc%left) .neqv. associated(loc%right) )  then
            ! Replace current with child
            if ( associated(loc%left)) then
                loc%value = loc%left%value
                deallocate(loc%left%str)
                deallocate(loc%left)
                loc%left => null()
            else if ( associated(loc%right)) then
                loc%value = loc%right%value
                deallocate(loc%right%str)
                deallocate(loc%right)
                loc%right => null()
            end if
        ! 2 children, chase down first right, then left
        else
            tmp => loc%right
            do while (associated(tmp%left))
                tmp => tmp%left
            end do
            loc%value = tmp%value
            tmp%parent%left => null()
            deallocate(tmp%str)
            deallocate(tmp)
        end if
    end if
end subroutine bst_str_remove

recursive subroutine bst_free(self)
    ! Safely demolish this list
    type(node), pointer :: self, tmp
    
    if (associated(self%left)) call bst_free(self%left)
    if (associated(self%right)) call bst_free(self%right)
    deallocate(self%str)
    deallocate(self)
end subroutine bst_free

end module bst_str


program treetest
! Make a string-storing binary search tree
    use bst_str
    type(node), pointer :: head
    integer :: ii, val

    write(*,*) "BST"
    call bst_init(head, 1, "FOO1")
    call bst_add(head, 0, "FOO0")
    call bst_add(head, 7, "FOO7")
    call bst_add(head, 4, "FOO4")
    call bst_add(head, 3, "FOO3")
    call bst_add(head, 4, "FOO4")
    call bst_add(head, 2, "FOO2")
    call bst_add(head, 6, "FOO6")
    call bst_add(head, 8, "FOO8")
    call bst_add(head, 5, "FOO5")
    call bst_print(head)
    write(*,*) "Locate 3"
    call bst_print(bst_find(head,3))
    write(*,*) "Locate FOO3"
    call bst_print(bst_str_find(head,"FOO3"))
    write(*,*) "Remove 8"
    call bst_remove(head,8)
    call bst_print(head)
    write(*,*) "Remove FOO3"
    call bst_str_remove(head,"FOO3")
    call bst_print(head)
    write(*,*) "Remove 4"
    call bst_remove(head,4)
    call bst_print(head)
!    write(*,*) "Len:", list_len(head)
!    val = list_pop(head)
!    write(*,*) "Popped:", val
!    call list_print(head)
!    write(*,*) "Len:", list_len(head)
!    write(*,*) "Where's 3?:", list_find(head,3)
!    write(*,*) "Where's 22?:", list_find(head,22)
    call bst_free(head)
    call bst_print(head)
!    nullify(head)
!    write(*,*) "Len:", list_len(head)

end program treetest
