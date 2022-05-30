! Copyright 2011 National Technology & Engineering Solutions of Sandia, LLC (NTESS). Under the terms
! of Contract DE-NA0003525 with NTESS, the U.S. Government retains certains rights to this software.

      module kind_mod
!doc$ module kind_mod

!     The module which holds kind info.

!cod$

      use cpointer_mod

      implicit none
      public

!doc$

      ! Data types

      integer, parameter :: single = kind(1.0e0)
      integer, parameter :: double = kind(1.0d0)

      integer, parameter :: longlong = selected_int_kind(16)
      integer, parameter :: long = selected_int_kind(9)

      integer, parameter :: sizeof_longlong = 8
      integer, parameter :: sizeof_long     = 4
      integer, parameter :: sizeof_single = 4
      integer, parameter :: sizeof_double = 8

      integer, parameter :: tag_sz = 8
      integer, parameter :: line_len = 132

!cod$

      end module