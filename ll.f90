module ll
    implicit none


    type :: node
        integer, allocatable :: value
        type(node), pointer :: next => null()
        logical :: initialized = .false.
    end type node

contains


subroutine list_init(self,init_val)
    ! Create a new linked list with the base value
    type(node), pointer :: self
    integer, intent(in) :: init_val

    allocate(self)
    nullify(self%next)

    self%value = init_val
    self%initialized = .true.

end subroutine list_init

subroutine list_push(self, val)
    ! Put a new node on the front
    type(node), pointer :: self
    type(node), pointer :: tmp
    integer, intent(in) :: val

    if ( .not. associated(self)  ) then
        call list_init(self, val)
    else if (.not. self%initialized) then
        call list_init(self, val)
    else
        ! Get new memory
        allocate(tmp)
        ! Assign value
        tmp%value = val
        ! Point to previous
        tmp%next => self
        ! Initialize this one too
        tmp%initialized = .true.
        ! Move head pointer
        self => tmp
    end if

end subroutine list_push

integer function list_pop(self)
    ! Return top value and set head to next
    integer :: val
    type(node), pointer :: self, tmp
    ! Grab the value
    val = self%value
    ! Save to dealloc later
    tmp => self
    ! Advance head
    self => self%next
    ! Free memory
    deallocate(tmp)

    list_pop = val ! This is stupid and confusing
    return
end function list_pop

subroutine list_print(self)
    ! Print all values
    type(node), pointer :: self, tmp
    tmp => self

    do while ( associated(tmp))
        write(*,*) tmp%value
        tmp => tmp%next
    end do
end subroutine list_print

integer function list_len(self)
    ! Compute list length
    integer :: l
    type(node), pointer :: self, tmp
    tmp => self

    l = 0

    do while ( associated(tmp))
        l = l + 1
        tmp => tmp%next
    end do

    list_len = l
end function list_len

integer function list_find(self, val)
    ! Find index of value in list
    ! If not found, return -1
    integer, intent(in) :: val
    integer :: idx, ans
    type(node), target :: self
    type(node), pointer :: tmp
    tmp => self
    ans = -1
    idx = 0

    find: do while ( associated(tmp))
        if (tmp%value == val) then
            ans = idx  
        end if
        idx = idx + 1
        tmp => tmp%next
    end do find

    list_find = ans
end function list_find

subroutine list_remove(self, val)
    ! Remove the first occurrence of val from the list
    integer, intent(in) :: val
    type(node), pointer :: self
    type(node), pointer :: tmp, prev, next
    tmp => self

    find: do while ( associated(tmp))
        if (tmp%value == val) then
            next => tmp%next
            ! Do special if first element
            if (.not. associated(self)) then
                self%next => next
            else if (associated(next) ) then
                prev%next => next
            else
                prev%next => null()
            end if
            deallocate(tmp)
            nullify(tmp)
            exit find
        end if
        prev => tmp
        tmp => tmp%next
    end do find

end subroutine list_remove

subroutine list_free(self)
    ! Safely demolish this list
    type(node), pointer :: self, tmp
    
    do while ( associated(self))
        tmp => self
        self => self%next
        deallocate(tmp)
        nullify(tmp)
    end do
end subroutine list_free

end module ll


!program linker
!! Make a simple linked list with pointers
!    use ll
!    type(node), pointer :: head
!    integer :: ii, val
!
!    write(*,*) "Linked List"
!    call list_init(head, 1)
!    do ii = 2, 10
!        call list_push(head, ii)
!    end do
!    call list_print(head)
!    write(*,*) "Len:", list_len(head)
!    val = list_pop(head)
!    write(*,*) "Popped:", val
!    call list_print(head)
!    write(*,*) "Len:", list_len(head)
!    write(*,*) "Where's 3?:", list_find(head,3)
!    write(*,*) "Where's 22?:", list_find(head,22)
!    call list_free(head)
!    nullify(head)
!    write(*,*) "Len:", list_len(head)
!
!end program linker
