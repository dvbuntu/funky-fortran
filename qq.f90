module qq
    implicit none


    type :: qnode
        integer, allocatable :: value
        type(qnode), pointer :: next => null()
        type(qnode), pointer :: prev => null()
        logical :: initialized = .false.
    end type qnode

    type :: queue
        type(qnode), pointer :: back => null()
        type(qnode), pointer :: front => null()
    end type queue

contains


subroutine queue_init(selfq,init_val)
    ! Create a new linked queue with the base value
    type(qnode), pointer :: self
    type(queue), pointer :: selfq
    integer, intent(in) :: init_val

    allocate(selfq)
    allocate(selfq%front)
    self => selfq%front
    nullify(self%next)
    nullify(self%prev)

    self%value = init_val
    selfq%back => selfq%front
    self%initialized = .true.

end subroutine queue_init

subroutine queue_push(selfq, val)
    ! Put a new qnode on the front
    type(queue), pointer :: selfq
    type(qnode), pointer :: self
    type(qnode), pointer :: tmp
    integer, intent(in) :: val
    self => selfq%back

    if ( .not. associated(selfq)  ) then
        call queue_init(selfq, val)
    else if (.not. self%initialized) then
        call queue_init(selfq, val)
    else
        ! Get new memory
        allocate(tmp)
        ! Assign value
        tmp%value = val
        ! Point to previous and back
        tmp%next => self
        self%prev => tmp
        ! Initialize this one too
        tmp%initialized = .true.
        ! Move head pointer
        self => tmp
        selfq%back => self
    end if

end subroutine queue_push

integer function queue_pop(selfq)
    ! Return bot value and set head to next
    integer :: val
    type(queue), pointer :: selfq
    type(qnode), pointer :: self, tmp

    self => selfq%front
    ! Grab the value
    val = self%value
    ! Save to dealloc later
    tmp => self
    ! Advance head
    self => self%prev
    self%next => null()
    selfq%front => self
    ! Free memory
    deallocate(tmp)

    queue_pop = val ! This is stupid and confusing
    return
end function queue_pop

subroutine queue_print(selfq)
    ! Print all values
    type(queue), pointer :: selfq
    type(qnode), pointer :: self, tmp
    self => selfq%back
    tmp => self

    do while ( associated(tmp))
        write(*,*) tmp%value
        tmp => tmp%next
    end do
end subroutine queue_print

integer function queue_len(selfq)
    ! Compute queue length
    integer :: l
    type(queue), pointer :: selfq
    type(qnode), pointer :: self, tmp

    if (associated(selfq)) then
        self => selfq%back
        tmp => self

        l = 0

        do while ( associated(tmp))
            l = l + 1
            tmp => tmp%next
        end do

        queue_len = l
    else
        queue_len = 0
    end if
end function queue_len

integer function queue_find(selfq, val)
    ! Find index of value in queue
    ! If not found, return -1
    integer, intent(in) :: val
    integer :: idx, ans
    type(queue), pointer :: selfq
    type(qnode), pointer :: self
    type(qnode), pointer :: tmp
    self => selfq%back
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

    queue_find = ans
end function queue_find

subroutine queue_remove(selfq, val)
    ! Remove the first occurrence of val from the queue
    integer, intent(in) :: val
    type(queue), pointer :: selfq
    type(qnode), pointer :: self
    type(qnode), pointer :: tmp, prev, next
    self => selfq%back
    tmp => self

    ! For some reason this isn't null at start?
    prev => null()

    ! This needs special care
    find: do while ( associated(tmp))
        if (tmp%value == val) then
            next => tmp%next
            ! Do special if first element
            ! This shouldn't happen?
            if (.not. associated(self)) then
                self%next => next
            else if (associated(next) ) then
                if (associated(prev)) then
                    prev%next => next
                else
                    ! Just update back
                    selfq%back => next
                end if
            else
                prev%next => null()
            end if
            ! Check if this is front
            if (associated(selfq%front,tmp)) then
                selfq%front => prev
            end if
            deallocate(tmp)
            nullify(tmp)
            exit find
        end if
        prev => tmp
        tmp => tmp%next
    end do find

end subroutine queue_remove

subroutine queue_free(selfq)
    ! Safely demolish this queue
    type(queue), pointer :: selfq
    type(qnode), pointer :: self, tmp
    self => selfq%back
    
    do while ( associated(self))
        tmp => self
        self => self%next
        deallocate(tmp)
        nullify(tmp)
    end do
    deallocate(selfq)
end subroutine queue_free

end module qq


program queuer
! Make a simple linked queue with pointers
    use qq
    type(queue), pointer :: head
    integer :: ii, val

    write(*,*) "Queue"
    call queue_init(head, 1)
    do ii = 2, 10
        call queue_push(head, ii)
    end do
    call queue_print(head)
    write(*,*) "Len:", queue_len(head)
    val = queue_pop(head)
    write(*,*) "Popped:", val
    call queue_print(head)
    write(*,*) "Len:", queue_len(head)
    write(*,*) "Where's 3?:", queue_find(head,3)
    write(*,*) "Where's 22?:", queue_find(head,22)
    write(*,*) "Remove 4"
    call queue_remove(head,4)
    call queue_print(head)
    write(*,*) "Remove 2 (at front)"
    call queue_remove(head,2)
    call queue_print(head)
    write(*,*) "Remove 10 (at back)"
    call queue_remove(head,10)
    call queue_print(head)
    call queue_free(head)
    nullify(head)
    write(*,*) "Len:", queue_len(head)

end program queuer
