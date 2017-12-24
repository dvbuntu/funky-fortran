module dict
    use ll
    implicit none

    ! Fortran is actually pretty dumb about pointers
    ! Can't get pointer to arbitrary object
    ! But can't cleanly declare an array of pointers
    type :: fakell
        type(node), pointer :: list
    end type fakell

    type :: hasht
        ! tlen hash table
        type(fakell), dimension(:), allocatable :: arr
        integer :: tlen = 0
    end type hasht

contains

integer function int_arr_hash(C) result(hash)
    integer,intent(in) :: C(16)
    integer :: k

    hash = 5381

    do k=1,16
        hash = (ishft(hash,5) + hash) + C(k)
    end do
	return
end function int_arr_hash

integer function hasht_modhash(self,val)
    ! Hash the value, based on the properties of the hasht
    type(hasht), intent(inout) :: self
    integer :: val
    
    hasht_modhash = modulo(val,self%tlen)
    return
end function hasht_modhash


subroutine hasht_init(self, tlen)
    ! Create a new linked list with the base value
    type(hasht), intent(inout) :: self
    integer :: tlen, ierror

    self%tlen = tlen
    allocate(self%arr(0:tlen-1), stat=ierror)
    !self%arr(:) => null()

end subroutine hasht_init

logical function hasht_check(self, val)
    ! Check if val is in the hash table
    type(hasht), intent(inout) :: self
    integer :: val, idx
    logical :: assoc

    idx = hasht_modhash(self,val)

    assoc = associated(self%arr(idx)%list)

    if (assoc) then
        if (self%arr(idx)%list%initialized) then
            if ( list_find(self%arr(idx)%list, val) .ne. -1 ) then
                hasht_check = .true.
            else
                hasht_check = .false.
            end if
        else
            hasht_check = .false.
        end if
    else
        hasht_check = .false.
    end if
    return

end function hasht_check
    
subroutine hasht_add(self, val)
    ! Add a value to the hash table if it isn't already there
    type(hasht), intent(inout) :: self
    integer :: val, ierror, idx
    type(node), pointer :: tmp

    !tmp => self%arr(idx)


    if (.not. hasht_check(self,val) ) then
        idx = hasht_modhash(self,val)
        !if (.not. associated(self%arr(idx)%list)) then
        !    call list_init(self%arr(idx)%list)
        !end if
        call list_push(self%arr(idx)%list,val)
        !call list_print(tmp)
    end if
end subroutine hasht_add

subroutine hasht_remove(self, val)
    ! Remove a value from the hash table
    type(hasht), intent(inout) :: self
    integer :: val, ierror, idx
    type(node), pointer :: tmp

    
    idx = hasht_modhash(self,val)
    if (hasht_check(self,val) ) then
        call list_remove(self%arr(idx)%list,val)
    end if

end subroutine hasht_remove

subroutine hasht_print(self)
    ! Show the hash table, one line per idx
    type(hasht), intent(inout) :: self
    integer :: idx, ierror
    logical :: assoc

    nuke: do idx = 0,self%tlen-1
        write(*,"(I10,A)", advance="no") idx, " : "
        assoc = associated(self%arr(idx)%list)
        if (assoc) then
            if (self%arr(idx)%list%initialized) then
                call list_print(self%arr(idx)%list)
            else
                write(*,*) ""
            end if
        else
            write(*,*) ""
        end if
    end do nuke

end subroutine hasht_print

subroutine hasht_free(self)
    ! Safely nuke a hash table
    type(hasht), intent(inout) :: self
    integer :: idx, ierror
    logical :: assoc

    nuke: do idx = 0,self%tlen-1
        assoc = associated(self%arr(idx)%list)
        if (assoc) then
            if (self%arr(idx)%list%initialized) then
                call list_free(self%arr(idx)%list)
            end if
        end if
    end do nuke
    deallocate(self%arr, stat=ierror)

end subroutine hasht_free

end module dict


program dict_test
! Make a hash table for integers, storing...integers
! Hash function will be val % tabl_len
    use dict
    type(node), pointer :: head
    type(hasht) :: H ! Don't use a pointer
    integer :: ii, val

    write(*,*) "Linked List"
    call list_init(head, 1)
    do ii = 2, 10
        call list_push(head, ii)
    end do
    call list_print(head)
    write(*,*) "Len:", list_len(head)
    val = list_pop(head)
    write(*,*) "Popped:", val
    call list_print(head)
    write(*,*) "Len:", list_len(head)
    write(*,*) "Where's 3?:", list_find(head,3)
    write(*,*) "Where's 22?:", list_find(head,22)
    write(*,*) "Remove 4"
    call list_remove(head,4)
    call list_print(head)
    call list_free(head)
    nullify(head)
    write(*,*) "Len:", list_len(head)

    write(*,*) ""
    write(*,*) "Hash Table fun"
    val = 16
    call hasht_init(H,val)

    ! Check if a value is in there
    write(*,*) "hash(4)?: ", hasht_modhash(H,4)
    write(*,*) "hash(17)?: ", hasht_modhash(H,17)
    write(*,*) "Have we seen 4?: ", hasht_check(H,4)
    write(*,*) "Adding 4!"
    call hasht_add(H,4)
    write(*,*) "Have we seen 4?: ", hasht_check(H,4)
    write(*,*) "Have we seen 4 + 16?: ", hasht_check(H,4+16)
    write(*,*) "Adding 4 + 16!"
    call hasht_add(H,4+16)
    write(*,*) "Have we seen 4 + 16?: ", hasht_check(H,4+16)
    write(*,*) "Remove 4"
    call hasht_remove(H,4)
    write(*,*) "Have we seen 4?: ", hasht_check(H,4)
    write(*,*) "Add a bunch"
    do ii = 10, 30
        call hasht_add(H, ii)
    end do
    write(*,*) "Print it!"
    call hasht_print(H)
    write(*,*) "Nuking it"
    call hasht_free(H)
end program dict_test
