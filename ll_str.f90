module ll_str
    implicit none

    ! For Array of strings of arbitrary length
    type string
        character(len=:), allocatable :: str
    end type string

    type :: node
        type(string), allocatable :: value
        type(node), pointer :: next => null()
        logical :: initialized = .false.
    end type node

contains


subroutine list_init(self,init_val)
    ! Create a new linked list with the base value
    type(node), pointer :: self
    type(string), intent(in) :: init_val

    allocate(self)
    nullify(self%next)

    self%value = init_val
    self%initialized = .true.

end subroutine list_init

subroutine list_push(self, val)
    ! Put a new node on the front
    type(node), pointer :: self
    type(node), pointer :: tmp
    type(string), intent(in) :: val
    type(string), allocatable :: newstring
    integer :: i

    ! Make memory for and copy string
    allocate(newstring)
    
    newstring%str = val%str(1:len(val%str))

    if ( .not. associated(self)  ) then
        call list_init(self, newstring)
    else if (.not. self%initialized) then
        call list_init(self, newstring)
    else
        ! Get new memory
        allocate(tmp)
        ! Assign value
        tmp%value = newstring
        ! Point to previous
        tmp%next => self
        ! Initialize this one too
        tmp%initialized = .true.
        ! Move head pointer
        self => tmp
    end if

end subroutine list_push

type(string) function list_pop(self)
    ! Return top value and set head to next
    type(string) :: val
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

    if (self%initialized) then
        do while ( associated(tmp))
            write(*,*) tmp%value%str
            tmp => tmp%next
        end do
    end if
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
    type(string), intent(in) :: val
    integer :: idx, ans
    type(node), target :: self
    type(node), pointer :: tmp
    tmp => self
    ans = -1
    idx = 0

    find: do while ( associated(tmp))
        if (tmp%value%str == val%str) then
            ans = idx  
        end if
        idx = idx + 1
        tmp => tmp%next
    end do find

    list_find = ans
end function list_find

subroutine list_remove(self, val)
    ! Remove the first occurrence of val from the list
    type(string), intent(in) :: val
    type(node), pointer :: self
    type(node), pointer :: tmp, prev, next
    tmp => self
    nullify(prev)

    find: do while ( associated(tmp))
        if (tmp%value%str == val%str) then
            next => tmp%next
            ! Do special if first element
            if (.not. associated(self)) then
                self%next => next
            else if (associated(next) ) then
                prev%next => next
            else if (associated(prev) ) then
                prev%next => null()
            else
                ! Only had one element, so shut down
                self%initialized = .false.
            end if
            deallocate(tmp%value)
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
        deallocate(tmp%value)
        deallocate(tmp)
        nullify(tmp)
    end do
end subroutine list_free

end module ll_str


!program linker
!! Make a simple linked list with pointers
!    use ll_str
!    implicit none
!    type(node), pointer :: head
!    integer :: ii 
!    type(string) :: foo, val
!    type(string) :: strs(0:9)
!
!    foo%str = "FOO"
!
!    do ii = 0, 9
!        allocate(character(len=10) :: strs(ii)%str)
!        write(strs(ii)%str,'(i10)') ii
!        !strs(ii)%str = char(i+30)
!        write(*,*) strs(ii)%str
!        strs(ii)%str = adjustl(strs(ii)%str)
!    end do
!
!    write(*,*) "Linked List"
!    call list_init(head, strs(0))
!    do ii = 1,9
!        call list_push(head, strs(ii))
!    end do
!    call list_print(head)
!    write(*,*) "Len:", list_len(head)
!    val = list_pop(head)
!    write(*,*) "Popped:", val%str
!    call list_print(head)
!    write(*,*) "Len:", list_len(head)
!    val%str = 'val'
!    write(*,*) "Where's 'val'?:", list_find(head,val)
!    val%str = '3'
!    write(*,*) "Where's '3'?:", list_find(head,val)
!    call list_remove(head,val)
!    call list_print(head)
!    call list_free(head)
!    nullify(head)
!    write(*,*) "Len:", list_len(head)
!
!end program linker
