module dict_str
    use ll_str
    implicit none

    ! Fortran is actually pretty dumb about pointers
    ! Can't get pointer to arbitrary object
    ! But can't cleanly declare an array of pointers
    type :: fakell_str
        type(node), pointer :: list
    end type fakell_str

    type :: hasht_str
        ! tlen hash table
        type(fakell_str), dimension(:), allocatable :: arr
        integer :: tlen = 0
    end type hasht_str

contains

integer function hasht_str_modhash(self,str)
    !character(len=*),intent(in) :: str
    type(string) :: str
    type(hasht_str),intent(inout) :: self
    integer :: k, hash

    hash = 5381

    do k=1,len(str%str)
        hash = (ishft(hash,5) + hash) + ichar(str%str(k:k))
    end do
    ! Make sure to keep within length
    hasht_str_modhash = modulo(hash,self%tlen)
	return
end function hasht_str_modhash

subroutine hasht_str_init(self, tlen)
    ! Create a new linked list with the base value
    type(hasht_str), intent(inout) :: self
    integer :: tlen, ierror

    self%tlen = tlen
    allocate(self%arr(0:tlen-1), stat=ierror)

end subroutine hasht_str_init

logical function hasht_str_check(self, val)
    ! Check if val is in the hash table
    type(hasht_str), intent(inout) :: self
    integer :: idx
    type(string) :: val
    logical :: assoc

    idx = hasht_str_modhash(self,val)

    assoc = associated(self%arr(idx)%list)

    if (assoc) then
        if (self%arr(idx)%list%initialized) then
            if ( list_find(self%arr(idx)%list, val) .ne. -1 ) then
                hasht_str_check = .true.
            else
                hasht_str_check = .false.
            end if
        else
            hasht_str_check = .false.
        end if
    else
        hasht_str_check = .false.
    end if
    return

end function hasht_str_check
!    
!subroutine hasht_str_add(self, val)
!    ! Add a value to the hash table if it isn't already there
!    type(hasht_str), intent(inout) :: self
!    integer :: val, ierror, idx
!    type(node), pointer :: tmp
!
!    !tmp => self%arr(idx)
!
!
!    if (.not. hasht_str_check(self,val) ) then
!        idx = hasht_str_modhash(self,val)
!        !if (.not. associated(self%arr(idx)%list)) then
!        !    call list_init(self%arr(idx)%list)
!        !end if
!        call list_push(self%arr(idx)%list,val)
!        !call list_print(tmp)
!    end if
!end subroutine hasht_str_add
!
!subroutine hasht_str_remove(self, val)
!    ! Remove a value from the hash table
!    type(hasht_str), intent(inout) :: self
!    integer :: val, ierror, idx
!    type(node), pointer :: tmp
!
!    
!    idx = hasht_str_modhash(self,val)
!    if (hasht_str_check(self,val) ) then
!        call list_remove(self%arr(idx)%list,val)
!    end if
!
!end subroutine hasht_str_remove
!
!subroutine hasht_str_print(self)
!    ! Show the hash table, one line per idx
!    type(hasht_str), intent(inout) :: self
!    integer :: idx, ierror
!    logical :: assoc
!
!    nuke: do idx = 0,self%tlen-1
!        write(*,"(I10,A)", advance="no") idx, " : "
!        assoc = associated(self%arr(idx)%list)
!        if (assoc) then
!            if (self%arr(idx)%list%initialized) then
!                call list_print(self%arr(idx)%list)
!            else
!                write(*,*) ""
!            end if
!        else
!            write(*,*) ""
!        end if
!    end do nuke
!
!end subroutine hasht_str_print
!
!subroutine hasht_str_free(self)
!    ! Safely nuke a hash table
!    type(hasht_str), intent(inout) :: self
!    integer :: idx, ierror
!    logical :: assoc
!
!    nuke: do idx = 0,self%tlen-1
!        assoc = associated(self%arr(idx)%list)
!        if (assoc) then
!            if (self%arr(idx)%list%initialized) then
!                call list_free(self%arr(idx)%list)
!            end if
!        end if
!    end do nuke
!    deallocate(self%arr, stat=ierror)
!
!end subroutine hasht_str_free

end module dict_str


program dict_test
! Make a hash table for strings!
! Hash function will be val % tabl_len
    use dict_str
    implicit none
    type(node), pointer :: head
    integer :: ii 
    type(string) :: foo, val
    type(string) :: strs(0:9)
    type(hasht_str) :: H ! Don't use a pointer

    foo%str = "FOO"

    do ii = 0, 9
        allocate(character(len=10) :: strs(ii)%str)
        write(strs(ii)%str,'(i10)') ii
        !strs(ii)%str = char(i+30)
        write(*,*) strs(ii)%str
        strs(ii)%str = adjustl(strs(ii)%str)
    end do

    write(*,*) "Linked List"
    call list_init(head, strs(0))
    do ii = 1,9
        call list_push(head, strs(ii))
    end do
    call list_print(head)
    write(*,*) "Len:", list_len(head)
    val = list_pop(head)
    write(*,*) "Popped:", val%str
    call list_print(head)
    write(*,*) "Len:", list_len(head)
    val%str = 'val'
    write(*,*) "Where's 'val'?:", list_find(head,val)
    val%str = '3'
    write(*,*) "Where's '3'?:", list_find(head,val)
    call list_remove(head,val)
    call list_print(head)
    call list_free(head)
    nullify(head)
    write(*,*) "Len:", list_len(head)
    write(*,*) ""
    write(*,*) "Hash Table fun"
    ! Create a hash table of certain size
    call hasht_str_init(H,16)

    ! Check if a value is in there
    write(*,*) "hash('FOO')?: ", hasht_str_modhash(H,foo)
!    write(*,*) "hash(17)?: ", hasht_str_modhash(H,17)
!    write(*,*) "Have we seen 4?: ", hasht_str_check(H,4)
!    write(*,*) "Adding 4!"
!    call hasht_str_add(H,4)
!    write(*,*) "Have we seen 4?: ", hasht_str_check(H,4)
!    write(*,*) "Have we seen 4 + 16?: ", hasht_str_check(H,4+16)
!    write(*,*) "Adding 4 + 16!"
!    call hasht_str_add(H,4+16)
!    write(*,*) "Have we seen 4 + 16?: ", hasht_str_check(H,4+16)
!    write(*,*) "Remove 4"
!    call hasht_str_remove(H,4)
!    write(*,*) "Have we seen 4?: ", hasht_str_check(H,4)
!    write(*,*) "Add a bunch"
!    do ii = 10, 30
!        call hasht_str_add(H, ii)
!    end do
!    write(*,*) "Print it!"
!    call hasht_str_print(H)
!    write(*,*) "Nuking it"
!    call hasht_str_free(H)
end program dict_test
