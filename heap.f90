module heap
    ! Min heap implemenation with integers
    implicit none


    type :: node
        integer, allocatable :: value
        type(node), pointer :: left => null()
        type(node), pointer :: right => null()
        type(node), pointer :: parent => null()
        logical :: initialized = .false.
        integer :: size = 0
    end type node

contains


subroutine heap_init(self,init_val)
    ! Create a new heap with the base value
    type(node), pointer :: self
    integer, intent(in) :: init_val

    allocate(self)
    nullify(self%left)
    nullify(self%right)
    nullify(self%parent)

    self%value = init_val
    self%initialized = .true.
    self%size = 1

end subroutine heap_init

subroutine heap_pos(self,idx,tmp,pos, bit)
    ! find the location of the most recently added node
    ! tmp will hold that node, and pos its parent
    ! Seems to allocate new nodes...
    type(node), pointer :: self
    type(node), pointer, intent(out) :: pos
    type(node), pointer, intent(out) :: tmp
    integer :: idx, nsteps, step, bit, lead

    tmp => self

    ! Chase down position and insert new node
    ! stupidly enough, this modify's self%size itself!
    ! idx = self%size +1
    ! So replace all mentions of with self%size +1...dumb
    lead = leadz(self%size + 1)
    nsteps = bit_size(self%size + 1) - lead - 1


    do step = 1, nsteps
        !bit = ibits(self%size + 1,lead + step,1)
        ! Counting starts from first 1...not from left
        ! And it's zero up...
        bit = ibits(self%size + 1,nsteps-step,1)
        if (bit == 0 .and. step < nsteps) then
            tmp => tmp%left
        else if (bit == 0 .and. step == nsteps) then
            ! allocate new node here
            pos => tmp
!            allocate(tmp%left)
!            tmp => tmp%left
!            pos%left => tmp
            exit
        else if (bit == 1 .and. step < nsteps) then
            tmp => tmp%right
        else if (bit == 1 .and. step == nsteps) then
            ! allocate new node here
            pos => tmp
!            allocate(tmp%right)
!            tmp => tmp%right
!            pos%right => tmp
            exit
        else
            write(*,*) "Shouldn't be here!"
        end if
    end do



end subroutine heap_pos

subroutine heap_add(self, val)
    ! Add to lowest, rightmost position
    type(node), pointer :: self
    type(node), pointer :: pos
    type(node), pointer :: tmp
    integer, intent(in) :: val
    integer :: idx, nsteps, step, bit, lead, tmpval
    logical :: assoc

    if ( .not. associated(self)  ) then
        call heap_init(self, val)
    else if (.not. self%initialized) then
        call heap_init(self, val)
    else
!        tmp => self

!        ! Chase down position and insert new node
!        idx = self%size + 1
!        lead = leadz(idx)
!        nsteps = bit_size(idx) - lead - 1
!
!
!        do step = 1, nsteps
!            !bit = ibits(idx,lead + step,1)
!            ! Counting starts from first 1...not from left
!            ! And it's zero up...
!            bit = ibits(idx,nsteps-step,1)
!            if (bit == 0 .and. associated(tmp%left)) then
!                tmp => tmp%left
!            else if (bit == 0 .and. .not. associated(tmp%left)) then
!                ! allocate new node here
!                pos => tmp
!                allocate(tmp%left)
!                tmp => tmp%left
!                pos%left => tmp
!            else if (bit == 1 .and. associated(tmp%right)) then
!                tmp => tmp%right
!            else if (bit == 1 .and. .not. associated(tmp%right)) then
!                ! allocate new node here
!                pos => tmp
!                allocate(tmp%right)
!                tmp => tmp%right
!                pos%right => tmp
!            else
!                write(*,*) "Shouldn't be here!"
!            end if
!        end do

        bit = 0
        call heap_pos(self, self%size, tmp, pos, bit)

        ! Allocate new memory and rejigger pointers
        if (bit == 0) then
            allocate(tmp%left)
            tmp => tmp%left
            pos%left => tmp
        else if (bit == 1) then
            allocate(tmp%right)
            tmp => tmp%right
            pos%right => tmp
        end if

        ! Assign the value in tmp
        if (associated(pos)) then
            ! Assign value
            tmp%value = val
            ! Point to parent
            tmp%parent => pos
            ! Empty left and right
            tmp%left => null()
            tmp%right => null()
            ! Initialize this one too
            tmp%initialized = .true.
            ! Heap size is a little bit larger
            self%size = self%size + 1
        end if


        ! Now bubble up the value
        do while(associated(pos))
            if (pos%value > tmp%value) then
                ! We already have an integer variable, why not use?
                ! HERE HERE HERE, something broken
                tmpval = tmp%value 
                tmp%value = pos%value
                pos%value = tmpval
                pos => pos%parent
                tmp => tmp%parent
            else
                exit
            end if
        end do

    end if

end subroutine heap_add

integer function heap_pop(self)
    ! Return root node and update the heap
    ! Need to "bubble down" highest val when removing
    integer :: val, bit, tmpval
    type(node), pointer :: self, tmp, pos

    ! Check if we're empty
    if (.not. associated(self)) then
        write(*,*) "Nothing Left!"
        heap_pop = -1 ! Maybe should just error...
        return
    else if (self%size == 0) then
        write(*,*) "Nothing Left!"
        heap_pop = -1 ! Maybe should just error...
        return
    else if (self%size == 1) then
        heap_pop = self%value
        call heap_free(self)
        return
    end if

    ! Grab the value
    val = self%value

    ! Replace with most recently added
    ! And nuke the most recent position, tmp
    bit = 0
    ! Could be dangerous
    self%size = self%size - 1
    call heap_pos(self, self%size, tmp, pos, bit)
!    write(*,*) "FOO", bit
!    call heap_print(pos)
!    write(*,*) "BAR", pos%value
!    call heap_print(tmp)
!    write(*,*) "GIT"
    ! This seems to swap the values...
    if (bit == 0) then
        self%value = tmp%left%value
        tmp => pos%left
        pos%left => null()
    else
        self%value = tmp%right%value
        tmp => pos%right
        pos%right => null()
    end if
    
    deallocate(tmp)

    ! Start winding down the tree
    tmp => self

    ! Swap parent with smaller child and continue down
    ! Only swap if smaller than both
    ! Some problem HERE HERE HERE
    do while(associated(tmp%left) .or. associated(tmp%right))
        ! Both leaves present, check both, use smaller
        if (associated(tmp%left) .and. associated(tmp%right)) then
            if (tmp%left%value < tmp%right%value .and. tmp%left%value < tmp%value) then
                tmpval = tmp%value
                tmp%value = tmp%left%value
                tmp => tmp%left
                tmp%value = tmpval
                cycle
            else if (tmp%right%value < tmp%value) then
                tmpval = tmp%value
                tmp%value = tmp%right%value
                tmp => tmp%right
                tmp%value = tmpval
                cycle 
            else
                exit
            end if
        ! Left leaf only
        else if (associated(tmp%left)) then
            if (tmp%left%value < tmp%value) then
                tmpval = tmp%value
                tmp%value = tmp%left%value
                tmp => tmp%left
                tmp%value = tmpval
                cycle 
            else
                exit
            end if
        ! Right leaf only
        else if (associated(tmp%right)) then
            ! We should never be here
            if (tmp%right%value < tmp%value) then
                tmpval = tmp%value
                tmp%value = tmp%right%value
                tmp => tmp%right
                tmp%value = tmpval
                cycle 
            else
                exit
            end if
        end if
    end do

    ! Double check we're at a leaf
!    if (associated(tmp%left) .or. associated(tmp%right)) then
!        write(*,*) "Shouldn't have any children!"
!    end if

    ! Fix parent, could be dangerous if equal value
!    if (tmp%parent%left%value == tmp%value) then
!        tmp%parent%left => null()
!    else if (tmp%parent%right%value == tmp%value) then
!        tmp%parent%right => null()
!    end if


    ! Free memory
!    if (associated(tmp)) then
!        deallocate(tmp)
!        nullify(tmp)
!    end if


    heap_pop = val ! This is stupid and confusing
    return
end function heap_pop

recursive subroutine heap_print(self, depth)
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
            write(*,*) "Depth", d, ":", tmp%parent%value, tmp%value
        else
            write(*,*) "Depth", d, ":", "   Root", tmp%value
        end if
        ! Depth first print... not great
        call heap_print(tmp%left, d+1)
        call heap_print(tmp%right, d+1)
    else if (.not. associated(tmp) .and. d == 0) then
        write(*,*) "Empty!"
    end if
end subroutine heap_print

recursive subroutine heap_free(self)
    ! Safely demolish this heap
    type(node), pointer :: self, tmp
    
    ! Depth-first traversal to nuke
    if (associated(self)) then
        if (associated(self%left)) call heap_free(self%left)
        if (associated(self%right)) call heap_free(self%right)
        deallocate(self)
    end if 
end subroutine heap_free

end module heap


program heaptest
! Make a simple linked list with pointers
    use heap
    type(node), pointer :: head
    integer :: ii, val

!    ! binary goofing
!    integer :: n
!
!    do n = 0, 16
!        print "(B32,3(' ',I2))", n, leadz(n), ibits(n,leadz(n)+1,1), bit_size(n) - leadz(n)
!    end do
!    ! Seems counting is zero up
!    ! ibits seems...broken
!    do n = 0, 32
!        print "(B32,3(' ',I2))", 12, 12, ibits(12,n,1)
!    end do
!    do n = 0, 31
!        print "(B32,3(' ',I2))", 3, 3, ibits(3,n,1), n
!        !bit = ibits(idx,nsteps-step,1)
!    end do
!    ! Ah, the position starts from the right!

    write(*,*) "Min Heap"
    call heap_init(head, 1)
    write(*,*) "SIZE", head%size
    call heap_add(head, 0)
    write(*,*) "SIZE", head%size
    call heap_add(head, 7)
    write(*,*) "SIZE", head%size
    call heap_add(head, 4)
    call heap_add(head, 3)
    call heap_add(head, 4)
    call heap_add(head, 2)
    call heap_add(head, 6)
    call heap_add(head, 8)
    call heap_add(head, 5)
    call heap_print(head)
    write(*,*) "SIZE", head%size
    write(*,*) "Pop root"
    write(*,*) heap_pop(head)
    write(*,*) "SIZE", head%size
    call heap_print(head)
    write(*,*) "Pop root"
    write(*,*) heap_pop(head)
    call heap_print(head)
    write(*,*) "Nuke list"
    call heap_free(head)
    call heap_print(head)

    nullify(head)

    ! New heap
    write(*,*) "New list"
    call heap_init(head, 1)
    call heap_add(head, 0)
    call heap_add(head, 7)
    call heap_add(head, 4)
    call heap_add(head, 3)
    call heap_print(head)
    do i=1,10
        write(*,*) "Pop root", i
        if (associated(head)) then
            write(*,*) "size", head%size
        end if
        call heap_print(head)
        write(*,*) heap_pop(head)
    end do

    write(*,*) "Nuke list"
    call heap_free(head)
    call heap_print(head)
!!    write(*,*) "Len:", list_len(head)

end program heaptest
