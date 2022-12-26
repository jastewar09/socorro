!* ------------------------------------------------------------------------------------------------------------------------------ *!
!  Socorro is a plane-wave density functional theory code for solid-state electronic structure calculations.                       !
!  See the README file in the top-level directory.                                                                                 !
!                                                                                                                                  !
!  Copyright 2011 National Technology and Engineering Solutions of Sandia, LLC (NTESS).                                            !
!  This software is distributed uner the modified Berkeley Software Distribution (BSD) License.                                    !
!  Under the terms of contract DE-NA0003525 with NTESS, the U.S. Government retains certain rights to this software.               !
!* ------------------------------------------------------------------------------------------------------------------------------ *!

#include "macros.h"

      module interrupt_mod
!doc$ module interrupt_mod

!     This module provides routines for interrupting a calculation.

      use kind_mod
      use mpi_mod
      use path_mod
      use io_mod
      use utilities_mod

!cod$
      implicit none ; private

!doc$
      public :: user_stop
      public :: interrupt

!cod$
      contains

      function user_stop() result(us)
!doc$ function user_stop() result(us)
         logical :: us
!        effects: Returns .true. iff file "stop_name" exists and contains the command "STOP".
!        requires:
!        modifies:
!        errors:
!        warns:
!        notes:

!cod$
         logical :: ex
         character(line_len) :: cmd
         integer :: ios
         type(file_obj) :: f

         call my(file(trim(stop_name)),f)

         us = .false.

         if (i_access(f)) inquire(file=x_name(f),exist=ex)
         if (i_comm(f)) call broadcast(FILE_SCOPE,ex)
         if (ex) then
            if (i_access(f)) open(unit=x_unit(f),file=x_name(f),status='old',iostat=ios)
            if (i_access(f)) read(x_unit(f),'(a)',iostat=ios) cmd
            if (i_comm(f)) call broadcast(FILE_SCOPE,cmd)
            select case (trim(adjustl(cmd)))
            case ("STOP","Stop","stop","S","s")
               us = .true.
            case ("ABORT","Abort","abort","A","a")
               us = .true.
            end select
            if (i_access(f)) close(unit=x_unit(f))
         end if

         call glean(thy(f))

      end function

      subroutine interrupt(file,line,mesg)
!doc$ subroutine system_stop()
         character(*) :: file, mesg
         integer :: line
!        effects: Calls routines that write an error message and immediately terminates execution.
!        requires:
!        modifies:
!        errors:
!        warns:
!        notes:

!cod$
         if (mpi_first(WORLD)) write(*,'(/,"ERROR: ",a," (src/",a,":",i0,")")') trim(mesg),basename(file),line

         call mpi_stop()
         stop

      end subroutine interrupt

      end module interrupt_mod
