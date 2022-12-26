!* ------------------------------------------------------------------------------------------------------------------------------ *!
!  Socorro is a plane-wave density functional theory code for solid-state electronic structure calculations.                       !
!  See the README file in the top-level directory.                                                                                 !
!                                                                                                                                  !
!  Copyright 2011 National Technology and Engineering Solutions of Sandia, LLC (NTESS).                                            !
!  This software is distributed uner the modified Berkeley Software Distribution (BSD) License.                                    !
!  Under the terms of contract DE-NA0003525 with NTESS, the U.S. Government retains certain rights to this software.               !
!* ------------------------------------------------------------------------------------------------------------------------------ *!

#include "macros.h"

      module create_eos_mod
!doc$ module create_eos_mod

!     This module provides routines to fit energy vs. volume data to an equation-of-state (EOS).

      use diary_mod
      use error_mod
      use kind_mod
      use mpi_mod
      use timing_mod

!cod$
      implicit none ; private

!doc$
      public :: create_eos

!cod$
      contains

! *** Public routines

      subroutine create_eos()
!doc$ subroutine create_eos()
!        effects:
!        requires:
!        modifies:
!        errors:
!        warns:
!        notes:

!cod$
         call start_timer("create_eos: total time")

900      if (error(FLERR,"Exiting create_eos")) continue
         if (.not.error()) call stop_timer("create_eos: total time")

      end subroutine create_eos

      end module create_eos_mod
