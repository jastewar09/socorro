!* ------------------------------------------------------------------------------------------------------------------------------ *!
!  Socorro is a plane-wave density functional theory code for solid-state electronic structure calculations.                       !
!  See the README file in the top-level directory.                                                                                 !
!                                                                                                                                  !
!  Copyright 2011 National Technology and Engineering Solutions of Sandia, LLC (NTESS).                                            !
!  This software is distributed under the modified Berkeley Software Distribution (BSD) License.                                   !
!  Under the terms of contract DE-NA0003525 with NTESS, the U.S. Government retains certain rights to this software.               !
!* ------------------------------------------------------------------------------------------------------------------------------ *!

#include "macros.h"

      module arg_mod
!doc$ module arg_mod

!     This module provides routines for reading and distributing control parameters from the
!     input file. The input file is read once and the results are cached for later retrieval.

      use error_mod
      use interrupt_mod
      use kind_mod
      use mpi_mod
      use path_mod

!cod$
      implicit none ; private

      type :: arg_param
         private
         character(line_len) :: var        ! keyword
         character(line_len) :: val        ! value associated with the keyword
      end type

      type(arg_param), dimension(:), allocatable :: arg_params

!doc$
      public :: arg_start
      public :: arg_stop
      public :: arg
      public :: arglc

!cod$
      interface arg
         module procedure read_arg_int, read_arg_int_1d, &
                          read_arg_dpr, read_arg_dpr_1d, &
                          read_arg_dpc, &
                          read_arg_log, &
                          read_arg_str
      end interface

      interface arglc
         module procedure read_arglc
      end interface

      contains

! *** Public routines

      subroutine arg_start()
!doc$ subroutine arg_start()
!        effects: Starts the argument retrieval system.
!        requires:
!        modifies:
!        errors: Problems locating or opening the arguments file.
!        warns:
!        notes:

!cod$
         integer :: citeflag, inflag, logflag, screenflag
         integer :: inunit, argc, iarg, iostat, nlines, il
         logical :: found
         character(line_len) :: infile, value, buffer

         ! Process the command-line arguments

         citeflag = 0
         inflag = 0
         logflag = 0
         screenflag = 0

         if (mpi_first(WORLD)) argc = command_argument_count()
         call broadcast_seh(argc)
         if (argc == 0) call interrupt(FLERR,"No command-line arguments were given")

         iarg = 1
         do while (iarg < argc + 1)
            if (mpi_first(WORLD)) call get_command_argument(iarg,value)
            call broadcast_seh(value)
            select case (trim(value))
            case ("-i","-in")
               if (iarg + 1 > argc) call interrupt(FLERR,"Invalid command-line argument")
               inflag = iarg + 1
               iarg = iarg + 2
            case default
               iarg = iarg + 1
            end select
         end do

         ! Check the status of required command-line arguments

         if (inflag == 0) call interrupt(FLERR,"The '-in' command-line argument was not found")

         ! Process the input file

         if (mpi_first(WORLD)) then
            call get_command_argument(inflag,infile)
            inquire(file=trim(infile),exist=found)
         end if
         call broadcast_seh(found)
         if (.not.found) call interrupt(FLERR,"The input file '"//trim(infile)//"' was not found")

         if (mpi_first(WORLD)) open(newunit=inunit,file=trim(infile),status="unknown",iostat=iostat)
         call broadcast_seh(iostat)
         if (iostat /= 0) call interrupt(FLERR,"The input file '"//trim(infile)//"' could not be opened")

         ! Allocate space for the input file parameters

         if (mpi_first(WORLD)) then
            nlines = -1
            iostat = 0
            do while (iostat == 0)
               nlines = nlines + 1
               read(inunit,'(a)',iostat=iostat) buffer
            end do
            rewind(inunit)
         end if

         call broadcast_seh(nlines)
         allocate(arg_params(nlines))

         ! Read and broadcast the input file parameters

         do il = 1,nlines
            if (mpi_first(WORLD)) read(inunit,'(a)') buffer
            call broadcast_seh(buffer)
            call arg_split_i(buffer,arg_params(il))
         end do

         if (mpi_first(WORLD)) close(inunit)

      end subroutine arg_start

      subroutine arg_stop()
!doc$ subroutine arg_stop()
!        effects: Stops the argument retrieval system.
!        requires:
!        modifies:
!        errors:
!        warns:
!        notes:

!cod$
         if (allocated(arg_params)) deallocate(arg_params)

      end subroutine arg_stop

      subroutine read_arg_int(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         integer, intent(out) :: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd
         integer :: ios

         ios = 0
         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         read(buf,*,iostat=ios) value

         if (error(FLERR,ios /= 0,"Problem converting '"//buf(1:len_trim(buf))//"' to integer")) goto 100
100      if (error(FLERR,"Exiting arg_mod::read_arg_int")) continue

      end subroutine read_arg_int

      subroutine read_arg_int_1d(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         integer, dimension(:), intent(out) :: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd
         integer :: ios

         ios = 0
         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         read(buf,*,iostat=ios) value

         if (error(FLERR,ios /= 0,"Problem converting '"//buf(1:len_trim(buf))//"' to integer(:)")) goto 100
100      if (error(FLERR,"Exiting arg_mod::read_arg_int_1d")) continue

      end subroutine read_arg_int_1d

      subroutine read_arg_dpr(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         real(double), intent(out):: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd
         integer :: ios

         ios = 0
         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         read(buf,*,iostat=ios) value

         if (error(FLERR,ios /= 0,"Problem converting '"//buf(1:len_trim(buf))//"' to real(double)")) goto 100
100      if (error(FLERR,"Exiting arg_mod::read_arg_dpr")) continue

      end subroutine read_arg_dpr

      subroutine read_arg_dpr_1d(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         real(double), dimension(:), intent(out) :: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd
         integer :: ios

         ios = 0
         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         read(buf,*,iostat=ios) value

         if (error(FLERR,ios /= 0,"Problem converting '"//buf(1:len_trim(buf))//"' to real(double)(:)")) goto 100
100      if (error(FLERR,"Exiting arg_mod::read_arg_dpr_1d")) continue

      end subroutine read_arg_dpr_1d

      subroutine read_arg_dpc(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         complex(double), intent(out) :: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd
         integer :: ios

         ios = 0
         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         read(buf,*,iostat=ios) value

         if (error(FLERR,ios /= 0,"Problem converting '"//buf(1:len_trim(buf))//"' to complex")) goto 100
100      if (error(FLERR,"Exiting arg_mod::read_arg_dpc")) continue

      end subroutine read_arg_dpc

      subroutine read_arg_log(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         logical, intent(out):: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd
         integer :: ios

         ios = 0
         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         read(buf,*,iostat=ios) value

         if (error(FLERR,ios /= 0,"Problem converting '"//buf(1:len_trim(buf))//"' to logical")) goto 100
100      if (error(FLERR,"Exiting arg_mod::read_arg_log")) continue

      end subroutine read_arg_log

      subroutine read_arg_str(name,value,found)
!doc$ subroutine arg(name,value,found)
         character(*), intent(in) :: name
         character(*), intent(out) :: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd

         var = name

         call read_arg_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         value = buf

100      if (error(FLERR,"Exiting arg_mod::read_arg_str")) continue

      end subroutine read_arg_str

      subroutine read_arglc(name,value,found)
!doc$ subroutine arglc(name,value,found)
         character(*), intent(in) :: name
         character(*), intent(out) :: value
         logical, intent(out), optional :: found

!cod$
         character(line_len) :: var, buf
         logical :: fnd

         var = name

         call read_arglc_i(var,buf,fnd)
         if (present(found)) then
            found = fnd
            if (.not.fnd) goto 100
         else
            if (error(FLERR,.not.fnd,"The input flag '"//trim(name)//"' was not found")) goto 100
         end if
         value = buf

100      if (error(FLERR,"Exiting arg_mod::read_arglc")) continue

      end subroutine read_arglc

! *** Private routines

      subroutine arg_split_i(s,p)

         character(line_len), intent(inout) :: s
         type(arg_param), intent(out) :: p

         integer :: k

         call trim_comments_i(s)
         call trim_whitespace_i(s)

         do k = 1,line_len
            if (s(k:k) == ' ') exit
         end do

         p%var = s(1:k-1)
         p%val = s(k+1:line_len)

         call upcase_arg_i(p%var)
         call trim_whitespace_i(p%val)

      end subroutine arg_split_i

      subroutine trim_comments_i(s)

         character(line_len) :: s

         integer :: k

         do k = 1,line_len
            if (s(k:k) == '!') exit
         end do

         s = s(1:k-1)

      end subroutine trim_comments_i

      subroutine trim_whitespace_i(s)

         character(line_len) :: s

         integer :: k

         do k = 1,line_len
            if (s(k:k) == ' ') cycle
            if (s(k:k) == '	') cycle
            exit
         end do

         s = s(k:line_len)

      end subroutine trim_whitespace_i

      subroutine read_arg_i(var,val,found)

         character(line_len), intent(inout)  :: var
         character(line_len), intent(out)  :: val
         logical, intent(out) :: found

         integer :: i, j

         found = .false.

         call trim_whitespace_i(var)
         call upcase_arg_i(var)

         j = len_trim(var)
         do i = 1,size(arg_params)
            if (var(1:j) == arg_params(i)%var(1:j)) then
               found = .true.
               val = arg_params(i)%val
               exit
            end if
         end do

      end subroutine read_arg_i

      subroutine read_arglc_i(var,val,found)

         character(line_len), intent(inout)  :: var
         character(line_len), intent(out)  :: val
         logical, intent(out) :: found

         integer :: i, j

         val = ''
         found = .false.

         call trim_whitespace_i(var)
         call upcase_arg_i(var)

         j = len_trim(var)
         do i = 1,size(arg_params)
            if (var(1:j) == arg_params(i)%var(1:j)) then
               found = .true.
               val = arg_params(i)%val
               exit
            end if
         end do

         call locase_arg_i(val)

      end subroutine read_arglc_i

      subroutine upcase_arg_i(s)

         character(line_len) :: s

         integer lstr, i, c
         integer, parameter :: a = iachar('a'), z = iachar('z'), shift = (iachar('A') - iachar('a'))

         lstr = len_trim(s)
         do i = 1,lstr
            c = iachar(s(i:i))
            if ((c >= a).and.(c <= z)) c = c + shift
            s(i:i) = achar(c)
         end do

      end subroutine upcase_arg_i

      subroutine locase_arg_i(s)

         character(line_len) :: s

         integer lstr, i, c
         integer, parameter :: a = iachar('A'), z = iachar('Z'), shift = (iachar('A') - iachar('a'))

         lstr = len_trim(s)
         do i = 1,lstr
            c = iachar(s(i:i))
            if ((c >= a).and.(c <= z)) c = c - shift
            s(i:i) = achar(c)
         end do

      end subroutine locase_arg_i

      end module arg_mod
