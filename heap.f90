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

subroutine heap_add(self, val)
    ! Add to lowest, rightmost position
    type(node), pointer :: self
    type(node), pointer :: pos
    type(node), pointer :: tmp
    integer, intent(in) :: val
    integer :: idx, nsteps, step, bit, lead

    if ( .not. associated(self)  ) then
        call heap_init(self, val)
    else if (.not. self%initialized) then
        call heap_init(self, val)
    else
        tmp => self

        ! Chase down position and insert new node
        idx = self%size + 1
        lead = leadz(idx)
        nsteps = bit_size(idx) - lead - 1


        do step = 1, nsteps
            !bit = ibits(idx,lead + step,1)
            ! Counting starts from first 1...not from left
            ! And it's zero up...
            bit = ibits(idx,nsteps-step,1)
            if (bit == 0 .and. associated(tmp%left)) then
                tmp => tmp%left
            else if (bit == 0 .and. .not. associated(tmp%left)) then
                ! allocate new node here
                pos => tmp
                allocate(tmp%left)
                tmp => tmp%left
                pos%left => tmp
            else if (bit == 1 .and. associated(tmp%right)) then
                tmp => tmp%right
            else if (bit == 1 .and. .not. associated(tmp%right)) then
                ! allocate new node here
                pos => tmp
                allocate(tmp%right)
                tmp => tmp%right
                pos%right => tmp
            else
                write(*,*) "Shouldn't be here!"
            end if
        end do

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
        do while(associated(pos) .and. pos%value > tmp%value)
            ! We already have an integer variable, why not use?
            idx = tmp%value 
            tmp%value = pos%value
            pos%value = idx
            pos => pos%parent
            tmp => tmp%parent
        end do

    end if

end subroutine heap_add

integer function heap_pop(self)
    ! Return root node and update the heap
    integer :: val
    type(node), pointer :: self, tmp
    ! Grab the value
    val = self%value
    ! Start winding down the tree
    tmp => self

    ! Swap parent with smaller child and continue down
    do while(associated(tmp%left) .or. associated(tmp%right))
        if (associated(tmp%left) .and. associated(tmp%right)) then
            if (tmp%left%value > tmp%right%value) then
                tmp%value = tmp%right%value
                tmp => tmp%right
                cycle 
            else if (tmp%left%value < tmp%right%value) then
                tmp%value = tmp%left%value
                tmp => tmp%left
                cycle 
            end if
        else if (associated(tmp%left)) then
                tmp%value = tmp%left%value
                tmp => tmp%left
                cycle 
        else if (associated(tmp%right)) then
                tmp%value = tmp%right%value
                tmp => tmp%right
                cycle 
        end if
    end do

    ! Double check we're at a leaf
    if (associated(tmp%left) .or. associated(tmp%right)) then
        write(*,*) "Shouldn't have any children!"
    end if

    ! Fix parent
    if (tmp%parent%left%value == tmp%value) then
        tmp%parent%left => null()
    else if (tmp%parent%right%value == tmp%value) then
        tmp%parent%right => null()
    end if

    ! Free memory
    if (associated(tmp)) deallocate(tmp)


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

!!integer function list_len(self)
!!    ! Compute list length
!!    integer :: l
!!    type(node), pointer :: self, tmp
!!    tmp => self
!!
!!    l = 0
!!
!!    do while ( associated(tmp))
!!        l = l + 1
!!        tmp => tmp%next
!!    end do
!!
!!    list_len = l
!!end function list_len
!!
!recursive function heap_find(self, val) result(ans)
!    ! Find node containing value
!    ! If not found, return node with null
!    integer, intent(in) :: val
!    type(node), target :: self
!    type(node), pointer :: tmp, ans
!    tmp => self
!    ans => null()
!
!    if (associated(tmp)) then
!        if (tmp%value == val) then
!            ans => tmp
!            return
!        else if (associated(tmp%left) .and. tmp%value > val) then
!            ans => heap_find(tmp%left, val)
!            if (associated(ans)) return
!        else if (associated(tmp%right) .and. tmp%value < val) then
!            ans => heap_find(tmp%right, val)
!            if (associated(ans)) return
!        end if
!    end if
!end function heap_find
!
!subroutine heap_remove(self, val)
!    ! Remove the occurrence of val from the heap
!    integer, intent(in) :: val
!    type(node), pointer :: self
!    type(node), pointer :: tmp, loc, parent
!    tmp => self
!
!    loc => heap_find(tmp, val)
!
!    if (associated(loc)) then
!        ! Three options, leaf, 1 child, 2 children
!        ! Leaf
!        if ( .not. associated(loc%left) .and. .not. associated(loc%right) ) then
!            ! Remove link from parent
!            parent => loc%parent
!            if ( parent%left%value == val) then
!                parent%left => null()
!            else if ( parent%right%value == val ) then
!                parent%right => null()
!            end if
!            ! Free memory
!            deallocate(loc)
!        ! 1 child
!        else if ( associated(loc%left) .neqv. associated(loc%right) )  then
!            ! Replace current with child
!            if ( associated(loc%left)) then
!                loc%value = loc%left%value
!                deallocate(loc%left)
!                loc%left => null()
!            else if ( associated(loc%right)) then
!                loc%value = loc%right%value
!                deallocate(loc%right)
!                loc%right => null()
!            end if
!        ! 2 children, chase down first right, then left
!        else
!            tmp => loc%right
!            do while (associated(tmp%left))
!                tmp => tmp%left
!            end do
!            loc%value = tmp%value
!            tmp%parent%left => null()
!            deallocate(tmp)
!        end if
!    end if
!end subroutine heap_remove
!
!recursive subroutine heap_free(self)
!    ! Safely demolish this list
!    type(node), pointer :: self, tmp
!    
!    if (associated(self%left)) call heap_free(self%left)
!    if (associated(self%right)) call heap_free(self%right)
!    deallocate(self)
!end subroutine heap_free

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
!    ! Ah, the position starts from the right!

    write(*,*) "Min Heap"
    call heap_init(head, 1)
    call heap_add(head, 0)
    call heap_add(head, 7)
    call heap_add(head, 4)
    call heap_add(head, 3)
    call heap_add(head, 4)
    call heap_add(head, 2)
    call heap_add(head, 6)
    call heap_add(head, 8)
    call heap_add(head, 5)
    call heap_print(head)
    write(*,*) "Pop root"
    write(*,*) heap_pop(head)
    call heap_print(head)
    write(*,*) "Pop root"
    write(*,*) heap_pop(head)
    call heap_print(head)
!    write(*,*) "Remove 4"
!    call heap_remove(head,4)
!    call heap_print(head)
!!    write(*,*) "Len:", list_len(head)
!!    val = list_pop(head)
!!    write(*,*) "Popped:", val
!!    call list_print(head)
!!    write(*,*) "Len:", list_len(head)
!!    write(*,*) "Where's 3?:", list_find(head,3)
!!    write(*,*) "Where's 22?:", list_find(head,22)
!    call heap_free(head)
!    call heap_print(head)
!!    nullify(head)
!!    write(*,*) "Len:", list_len(head)

end program heaptest
