!* ------------------------------------------------------------------------------------------------------------------------------ *!
!  Socorro is a plane-wave density functional theory code for solid-state electronic structure calculations.                       !
!  See the README file in the top-level directory.                                                                                 !
!                                                                                                                                  !
!  Copyright 2011 National Technology and Engineering Solutions of Sandia, LLC (NTESS).                                            !
!  This software is distributed uner the modified Berkeley Software Distribution (BSD) License.                                    !
!  Under the terms of contract DE-NA0003525 with NTESS, the U.S. Government retains certain rights to this software.               !
!* ------------------------------------------------------------------------------------------------------------------------------ *!

#include "macros.h"

      module system_mod
!doc$ module system_mod

!     This module provides routines for starting and stopping the runtime environment.

      use arg_mod
      use diary_mod
      use error_mod
      use fft_mod
      use interrupt_mod
      use io_mod
      use kind_mod
      use math_mod
      use mpi_mod
      use path_mod
      use point_blas_mod
      use timing_mod

!cod$
      implicit none ; private

!doc$
      public :: system_start
      public :: system_stop

!cod$
      contains

      subroutine system_start()
!doc$ subroutine system_start()
!        effects: Calls routines that start the runtime environment.
!        requires:
!        modifies:
!        errors: configurations < 1 or > 98. Passes errors.
!        warns:
!        notes: Errors here and in called routines cause program to halt.

!cod$
         logical :: found
         integer :: nc, nsg, nkg
         character(line_len) :: ef_status, tag

         ! Start the MPI system

         call mpi__start()

         ! Read the input file

         call arg_start()

         ! Set the number of configurations

         call arg('configs',nc,found)
         if (.not.found) nc = 1
         if (nc < 1) then
            call interrupt_stop(FLERR,"Number of configurations is less than 1")
         elseif (nc == 1) then
            continue
         elseif ((nc > 1).and.(nc < 99)) then
            if (mod(mpi_nprocs(WORLD),nc) /= 0) then
               call interrupt_stop(FLERR,"Non-equal division of world processes among configurations")
            else
               call mpi_config_split(nc)
            end if
         else
            call interrupt_stop(FLERR,"Number of configurations is greater than 98")
         end if

         ! Set the number of spin groups

         call arg('sgroups',nsg,found)
         if (.not.found) nsg = 1
         if (nsg < 1) then
            call interrupt_stop(FLERR,"Number of spin groups is less than 1")
         elseif (nsg == 1) then
            continue
         elseif (nsg == 2) then
            if (mod(mpi_nprocs(CONFIG),nsg) /= 0) then
               call interrupt_stop(FLERR,"Non-equal division of config processes among spin groups")
            else
               call mpi_sgroup_split(nsg)
            end if
         else
            call interrupt_stop(FLERR,"Number of spin groups is greater than 2")
         end if

         ! Set the number of k-point groups

         call arg('kgroups',nkg,found)
         if (.not.found) nkg = 1
         if (nkg < 1) then
            call interrupt_stop(FLERR,"Number of k-point groups is less than 1")
         elseif (nkg == 1) then
            continue
         else
            if (mod(mpi_nprocs(SGROUP),nkg) /= 0) then
               call interrupt_stop(FLERR,"Non-equal division of spin group processes among k-point groups")
            else
               call mpi_kgroup_split(nkg)
            end if
         end if

         ! Check the transition-state method w.r.t. configs

         call arglc('ts_method',tag,found)
         if (.not.found) tag = "none"
         select case (trim(tag))
         case ("dimer","dmr")
            if (nc /= 2) call interrupt_stop(FLERR,"Number of configurations is not equal to 2 for a dimer calculation")
         case ("neb")
            if (nc == 1) call interrupt_stop(FLERR,"Number of configurations is equal to 1 for a neb calculation")
         end select

         ! Set the input/ouput file paths

         !call arg("data_dir",data_dir,found) ; if (.not.found) data_dir = "./"
         !call arg("input_dir",input_dir,found) ; if (.not.found) input_dir = "./"
         !call arg("output_dir",output_dir,found) ; if (.not.found) output_dir = "./"

         call set_paths(mpi_nconfigs(),mpi_myconfig(),mpi_myproc(CONFIG))

         ! Start the error-handling system

         call arglc('error_file_mode',tag,found)
         if (.not.found) tag = "first"
         select case (trim(tag))
         case ("all")
            ef_status = "on"
         case ("first")
            ef_status = "off"
            if (mpi_first(CONFIG)) ef_status = "on"
         case ("none")
            ef_status = "off"
         case default
            call interrupt_stop(FLERR,"error_file_mode tag must be all, first, or none")
         end select

         call error_start(mpi_comm(CONFIG),mpi_comm(SGROUP),mpi_comm(KGROUP),mpi_myproc(CONFIG),mpi_first(WORLD),tag,ef_status)

         ! Write MPI information to the error files

         select case (trim(tag))
         case ("all")
            call notify("MPI information:")
            call notify("")
            call notify("WORLD")
            call notify("communicator       ",mpi_comm(WORLD))
            call notify("processes          ",mpi_nprocs(WORLD))
            call notify("  rank             ",mpi_myproc(WORLD))
            call notify("")
            call notify("CONFIG")
            call notify("number of configs  ",mpi_nconfigs())
            call notify("communicator       ",mpi_comm(CONFIG))
            call notify("processes          ",mpi_nprocs(CONFIG))
            call notify("  config number    ",mpi_myconfig())
            call notify("  rank             ",mpi_myproc(CONFIG))
            call notify("")
            call notify("  XCONFIG")
            call notify("  communicator     ",mpi_comm(XCONFIG))
            call notify("  processes        ",mpi_nprocs(XCONFIG))
            call notify("    rank           ",mpi_myproc(XCONFIG))
            call notify("")
            call notify("SGROUP")
            call notify("number of sgroups  ",mpi_nsgroups())
            call notify("communicator       ",mpi_comm(SGROUP))
            call notify("processes          ",mpi_nprocs(SGROUP))
            call notify("  sgroup number    ",mpi_mysgroup())
            call notify("  rank             ",mpi_myproc(SGROUP))
            call notify("")
            call notify("  XSGROUP")
            call notify("  communicator     ",mpi_comm(XSGROUP))
            call notify("  processes        ",mpi_nprocs(XSGROUP))
            call notify("    rank           ",mpi_myproc(XSGROUP))
            call notify("")
            call notify("KGROUP")
            call notify("number of kgroups  ",mpi_nkgroups())
            call notify("communicator       ",mpi_comm(KGROUP))
            call notify("processes          ",mpi_nprocs(KGROUP))
            call notify("  kgroup number    ",mpi_mykgroup())
            call notify("  rank             ",mpi_myproc(KGROUP))
            call notify(" ")
            call notify("  XKGROUP")
            call notify("  communicator     ",mpi_comm(XKGROUP))
            call notify("  processes        ",mpi_nprocs(XKGROUP))
            call notify("    rank           ",mpi_myproc(XKGROUP))
            call notify("")
         end select

         ! Start the input/output system

         call io_start() ; if(error()) goto 100

         ! Initialize the diary file

         call diary_start()
         call diary_socorro_env()
         call sync_configuration_errors() ; if (error()) goto 100

         ! Initialize the math systems

         call fft_start()
         call random_seed()
         call set_machine_constants()

         ! The tag point_mode is currently hardwired to the value f90 so that point
         ! blas operations are performed with (normal) Fortran 90 constructs. It is
         ! noted that, consistent with this hardwiring, the point_mode tag is not
         ! described in the README file.

         call arglc('point_mode',tag,found)
         if (.not.found) tag = "f90"
         select case (trim(tag))
         case ("f77")
            call point_mode(POINT_F77)
         case ("f90")
            call point_mode(POINT_F90)
         end select

         call timer_start()

         call start_timer("Socorro: total time")
100      if (error(FLERR,"Exiting system_mod::system_start()")) continue

      end subroutine system_start

      subroutine system_stop()
!doc$ subroutine system_stop()
!        effects: Calls routines that stop the runtime system.
!        requires:
!        modifies:
!        errors:
!        warns:
!        notes:

!cod$
         if (.not.error()) then
            call stop_timer("Socorro: total time")
            call write_timers()
         end if

         call interrupt()
         call diary_stop()
         call io_stop()
         call error_stop()
         call arg_stop()
         call timer_stop()
         call mpi_stop()

      end subroutine system_stop

      end module system_mod
