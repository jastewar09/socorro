!* ------------------------------------------------------------------------------------------------------------------------------ *!
!  Socorro is a plane-wave density functional theory code for solid-state electronic structure calculations.                       !
!  See the README file in the top-level directory.                                                                                 !
!                                                                                                                                  !
!  Copyright 2011 National Technology and Engineering Solutions of Sandia, LLC (NTESS).                                            !
!  This software is distributed under the modified Berkeley Software Distribution (BSD) License.                                   !
!  Under the terms of contract DE-NA0003525 with NTESS, the U.S. Government retains certain rights to this software.               !
!* ------------------------------------------------------------------------------------------------------------------------------ *!

#include "macros.h"

module many_body_mod

   use config_sc_mod
   use kind_mod

   implicit none ; private

   !*

   public :: many_body_theory

   !*

   interface many_body_theory
      module procedure many_body_theory_
   end interface

contains

   function many_body_theory_(cfg) result(changed)

      type(config_sc_obj), intent(inout) :: cfg
      logical :: changed

      changed = .false.

   end function

end module many_body_mod
