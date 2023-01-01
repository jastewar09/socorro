!* ------------------------------------------------------------------------------------------------------------------------------ *!
!  Socorro is a plane-wave density functional theory code for solid-state electronic structure calculations.                       !
!  See the README file in the top-level directory.                                                                                 !
!                                                                                                                                  !
!  Copyright 2011 National Technology and Engineering Solutions of Sandia, LLC (NTESS).                                            !
!  This software is distributed uner the modified Berkeley Software Distribution (BSD) License.                                    !
!  Under the terms of contract DE-NA0003525 with NTESS, the U.S. Government retains certain rights to this software.               !
!* ------------------------------------------------------------------------------------------------------------------------------ *!

#include "macros.h"

      module check_eos_mod
!doc$ module check_eos_mod

!     This module provides routines to fit "energy vs. volume" or
!     "energy vs. lattice constant" data to an equation-of-state.

      use arg_mod
      use diary_mod
      use error_mod
      use io_mod
      use kind_mod
      use math_mod
      use mpi_mod
      use path_mod
      use timing_mod

!cod$
      implicit none ; private

      ! EOS formulations

      integer, parameter :: EOS_BIRCH_MURNAGHAN = 1                    ! 3rd Order Birch-Murnaghan
      integer, parameter :: EOS_MURNAGHAN = 2                          ! Murnaghan
      integer, parameter :: EOS_UNIVERSAL = 3                          ! Rose-Vinet
      integer, parameter :: EOS_POIRIER_TARANTOLA = 4                  ! Poirier-Tarantola

      ! Derived type encapsulating EOS and optimization information

      type :: eos_obj
         private
         integer :: equation                                           ! Equation-of-state to be fit
         logical :: is_volume                                          ! Flag to identify data and volume or lattice spacing
         real(double), allocatable, dimension(:) :: xdata              ! Input volume or lattice spacing data
         real(double), allocatable, dimension(:) :: ydata              ! Input energies at the given xdata points
         real(double), allocatable, dimension(:) :: alat               ! Extra storage is xdata is lattice spacing
         integer :: ndigits                                            ! Number of digits for printing results
         integer :: m                                                  ! Number of data points
         integer :: n                                                  ! Number of variables to be optimized
         integer :: info                                               ! Error flag
         real(double) :: tol                                           ! Residual tolerance for optimization
         real(double), allocatable, dimension(:) :: x                  ! Optimized parameters
         real(double), allocatable, dimension(:) :: fvec               ! Function evaluations at x
      end type

      type(eos_obj) :: eos

!doc$
      public :: check_eos

!cod$
      contains

! *** Public routines

      subroutine check_eos()
!doc$ subroutine check_eos()
!        effects:
!        requires:
!        modifies:
!        errors:
!        warns:
!        notes:

!cod$
         logical :: i_participate, found, exists
         integer :: iostat, il
         real(double) :: mat(3,3)
         character(line_len) :: tag, buffer
         type(file_obj) :: f

         call start_timer("check_eos: total time")

         ! Read the EOS data file

         i_participate = mpi_first(WORLD)

         call arglc("data_file",tag,found)
         if (found) then
            call my(file(trim(tag)),f)
         else
            call my(file(trim(eos_path)),f)
         end if

         if (i_participate) inquire(file=x_name(f),exist=exists)
         call broadcast(WORLD,exists)
         if (error(FLERR,.not.exists,"EOS data file was not found")) goto 200

         if (i_participate) open(x_unit(f),file=x_name(f),status='old',iostat=iostat)
         call broadcast(WORLD,iostat)
         if (error(FLERR,iostat /= 0,"EOS data file could not be opened")) goto 200

         if (i_participate) then
            eos%m = -1
            iostat = 0
            do while (iostat == 0)
               eos%m = eos%m + 1
               read(x_unit(f),'(a)',iostat=iostat) buffer
            end do
            rewind(x_unit(f))
         end if

         call broadcast(WORLD,eos%m)
         allocate(eos%xdata(eos%m),eos%ydata(eos%m),eos%alat(eos%m))

         do il = 1,eos%m
            if (i_participate) read(x_unit(f),*,iostat=iostat) eos%xdata(il),eos%ydata(il)
            call broadcast(WORLD,iostat)
            if (error(FLERR,iostat /= 0,"EOS data file could not be read")) goto 100
            call broadcast(WORLD,eos%xdata(il))
            call broadcast(WORLD,eos%ydata(il))
         end do

100      if (i_participate) close(x_unit(f))
         if (error()) goto 200

         ! Set the EOS equation

         call arglc("equation",tag,found) ; if (.not.found) tag = "birch-murnaghan"
         select case (trim(tag))
         case ("bm","birch-murnaghan")
            eos%equation = EOS_BIRCH_MURNAGHAN
            eos%n = 4
         case ("m","murnaghan")
            eos%equation = EOS_MURNAGHAN
            eos%n = 4
         case ("u","universal")
            eos%equation = EOS_UNIVERSAL
            eos%n = 4
         case ("pt","poirier-tarantola")
            eos%equation = EOS_POIRIER_TARANTOLA
            eos%n = 4
         case default
            if (error(FLERR,.true.,"equation tag was not recognized")) goto 200
         end select

         ! Set the residual tolerance for optimization

         call arg("tolerance",eos%tol,found) ; if (.not.found) eos%tol = 1.0e-10_double
         if (error(FLERR,eos%tol < 0.0_double,"tolerance is less than 0")) goto 200

         ! Set the number of digits for ouputting data

         call arg("ndigits",eos%ndigits,found) ; if (.not.found) eos%ndigits = 5
         if (error(FLERR,eos%ndigits < 0,"ndigits is less than 0")) goto 200

         ! Determine if the data is volume or lattice spacing units

         call arglc("is_volume",tag,found) ; if (.not.found) tag = "yes"
         select case (trim(tag))
         case ("y","yes")
            eos%is_volume = .true.
         case ("n","no")
            eos%is_volume = .false.
         case default
            if (error(FLERR,.true.,"is_volume tag was not recognized")) goto 200
         end select

         if (.not.eos%is_volume) then
            eos%alat = eos%xdata
            call arglc("lattice",tag,found) ; if (.not.found) tag = "none"
            select case(trim(tag))
            case ("sc")
               mat(:,1) = (/ 1.0_double, 0.0_double, 0.0_double/)
               mat(:,2) = (/ 0.0_double, 1.0_double, 0.0_double/)
               mat(:,3) = (/ 0.0_double, 0.0_double, 1.0_double/)
            case ("fcc")
               mat(:,1) = (/-0.5_double, 0.0_double, 0.5_double/)
               mat(:,2) = (/ 0.0_double, 0.5_double, 0.5_double/)
               mat(:,3) = (/-0.5_double, 0.5_double, 0.0_double/)
            case ("bcc")
               mat(:,1) = (/ 0.5_double, 0.5_double, 0.5_double/)
               mat(:,2) = (/-0.5_double, 0.5_double, 0.5_double/)
               mat(:,3) = (/-0.5_double,-0.5_double, 0.5_double/)
            case ("user")
               call arg("v1",mat(:,1))
               call arg("v2",mat(:,2))
               call arg("v3",mat(:,3))
            case ("none")
               if (error(FLERR,.true.,"lattice tag was not supplied")) goto 200
            end select
            do il = 1,eos%m
               eos%xdata(il) = abs(determinant(eos%xdata(il)*mat))
            end do
         end if

         ! Perform a fit to the EOS

         call diary_constructor_i()
         call curve_fit_i() ; if (error()) goto 200
         call diary_eos_i()

200      call glean(thy(f))

900      if (error(FLERR,"Exiting check_eos_mod::check_eos")) continue
         if (.not.error()) call stop_timer("check_eos: total time")

      end subroutine

! *** Private routines

      subroutine diary_constructor_i()

         integer :: un

         un = x_unit(diaryfile())

         if (i_access(diaryfile())) then
            write(un,'(/,"Equation-of-state construction:")')
            select case (eos%equation)
            case (EOS_BIRCH_MURNAGHAN)
               write(un,'(/,t4,"Using the 3rd order Birch-Murnaghan EOS")')
            case (EOS_MURNAGHAN)
               write(un,'(/,t4,"Using the Murnaghan EOS")')
            case (EOS_UNIVERSAL)
               write(un,'(/,t4,"Using the Universal EOS")')
            case (EOS_POIRIER_TARANTOLA)
               write(un,'(/,t4,"Using the Poirier-Tarantola EOS")')
            end select
            write(un,'(/,t4,"MINPACK optimization (tolerance = ",es7.1,")")') eos%tol
         end if

      end subroutine

      subroutine diary_eos_i()

         integer :: un,nd,np,il
         character(line_len) :: fmt

         nd = eos%ndigits
         un = x_unit(diaryfile())

         if (i_access(diaryfile())) then

            np = max(10,2 + nd + ceiling(log10(maxval(abs(eos%x)))))
            write(fmt,'("(f",i0,".",i0,")")') np,nd

            write(un,'(/,t4,"Optimized parameters (info = ",i0,"):",/)') eos%info
            write(un,"(t7,a"//trim(fmt)//")") "V0 = ",eos%x(1)
            write(un,"(t7,a"//trim(fmt)//")") "E0 = ",eos%x(2)
            write(un,"(t7,a"//trim(fmt)//")") "B0 = ",eos%x(3)
            write(un,"(t7,a"//trim(fmt)//")") "B' = ",eos%x(4)

            np = max(10,2 + nd + ceiling(log10(maxval(abs(eos%ydata)))))
            write(fmt,*) np

            write(un,'(/,t4,"Original and predicted data points:")')
            if (eos%is_volume) then
               write(un,"(/,t7,4(a"//trim(fmt)//",3x))") "V","E(V)","E_Fit(V)","E - E_Fit"
               write(un,"(t7,4("//trim(fmt)//"('-'),3x))")
               write(fmt,'("(f",i0,".",i0,")")') np,nd
               do il = 1,eos%m
                  write(un,"(t7,4("//trim(fmt)//",3x))") &
                        eos%xdata(il),eos%ydata(il),(eos%ydata(il)-eos%fvec(il)),eos%fvec(il)
               end do
            else
               write(un,"(/,t7,5(a"//trim(fmt)//",3x))") "a","V","E(V)","E_Fit(V)","E - E_Fit"
               write(un,"(t7,5("//trim(fmt)//"('-'),3x))")
               write(fmt,'("(f",i0,".",i0,")")') np,nd
               do il = 1,eos%m
                  write(un,"(t7,5("//trim(fmt)//",3x))") &
                        eos%alat(il),eos%xdata(il),eos%ydata(il),(eos%ydata(il)-eos%fvec(il)),eos%fvec(il)
               end do
            end if

         end if

      end subroutine

      subroutine curve_fit_i()

         integer :: lwa
         integer, allocatable, dimension(:) :: iwa
         real(double), allocatable, dimension(:) :: wa,xi

         ! Allocate work space

         lwa = eos%m*eos%n + 5*eos%n + eos%m

         allocate(eos%x(eos%n))
         allocate(eos%fvec(eos%m))
         allocate(iwa(eos%n))
         allocate(wa(lwa))
         allocate(xi(3))

         ! Generate initial guesses to the solution with a quadratic fit

         xi = 1.0d0
         call lmdif1(quadratic_i,eos%m,eos%n,xi,eos%fvec,eos%tol,eos%info,iwa,wa,lwa)

         eos%x(1) = -xi(2)/(2.0_double*xi(3))
         eos%x(2) = xi(3)*eos%x(1)**2 + xi(2)*eos%x(1) + xi(1)
         eos%x(3) = 2.0_double*xi(3)*eos%x(1)
         eos%x(4) = 4.0_double

         ! Generate a least-squares fit to the EOS

         select case (eos%equation)
         case (EOS_BIRCH_MURNAGHAN)
            call lmdif1(birch_murnaghan_i,eos%m,eos%n,eos%x,eos%fvec,eos%tol,eos%info,iwa,wa,lwa)
         case (EOS_MURNAGHAN)
            call lmdif1(murnaghan_i,eos%m,eos%n,eos%x,eos%fvec,eos%tol,eos%info,iwa,wa,lwa)
         case (EOS_UNIVERSAL)
            call lmdif1(universal_i,eos%m,eos%n,eos%x,eos%fvec,eos%tol,eos%info,iwa,wa,lwa)
         case (EOS_POIRIER_TARANTOLA)
            call lmdif1(poirier_tarantola_i,eos%m,eos%n,eos%x,eos%fvec,eos%tol,eos%info,iwa,wa,lwa)
         end select

         ! Check eos%info for errors

         if (error(FLERR,eos%info == 0,"Improper input parameters for lmdif1")) goto 900

900      if (error(FLERR,"Exiting check_eos_mod::curve_fit")) continue

      end subroutine

      subroutine quadratic_i(m,n,x,fvec,iflag)

         integer :: m,n,iflag
         real(double) :: x(n),fvec(m)

         fvec = eos%ydata - ( x(1) + x(2)*eos%xdata + x(3)*eos%xdata*eos%xdata )

      end subroutine

      subroutine birch_murnaghan_i(m,n,x,fvec,iflag)

         integer :: m,n,iflag
         real(double) :: x(n),fvec(m)

         real(double) :: v0,e0,b0,bp,eta(m),t1(m),t2(m),t3(m)

         v0 = x(1)
         e0 = x(2)
         b0 = x(3)
         bp = x(4)

         eta = (v0/eos%xdata)**(2.0_double/3.0_double)

         t1 = ( b0*v0*9.0_double/16.0_double )
         t2 = ( eta - 1.0_double )**2.0_double
         t3 = ( 6.0_double + bp*(eta - 1.0_double) - 4.0_double*eta )

         fvec = eos%ydata - ( e0 + t1*t2*t3 )

      end subroutine

      subroutine murnaghan_i(m,n,x,fvec,iflag)

         integer :: m,n,iflag
         real(double) :: x(n),fvec(m)

         real(double) :: v0,e0,b0,bp,eta(m),t1(m),t2(m),t3(m)

         v0 = x(1)
         e0 = x(2)
         b0 = x(3)
         bp = x(4)

         eta = (v0/eos%xdata)

         t1 = ( b0*eos%xdata/bp )
         t2 = ( (eta**bp)/(bp - 1.0_double) + 1.0_double )
         t3 = ( v0*b0/(bp - 1.0_double) )

         fvec = eos%ydata - ( e0 + t1*t2 - t3 )

      end subroutine

      subroutine universal_i(m,n,x,fvec,iflag)

         integer :: m,n,iflag
         real(double) :: x(n),fvec(m)

         real(double) :: v0,e0,b0,bp,eta(m),t1(m),t2(m),t3(m)

         v0 = x(1)
         e0 = x(2)
         b0 = x(3)
         bp = x(4)

         eta = (eos%xdata/v0)**(1.0_double/3.0_double)

         t1 = ( 2.0_double*b0*v0/(bp - 1.0_double)**2 )
         t2 = ( 5.0_double + 3.0_double*bp*(eta - 1.0_double) - 3.0_double*eta )
         t3 = exp( -3.0_double*(bp - 1.0_double)*(eta - 1.0_double)/2.0_double )

         fvec = eos%ydata - ( e0 + t1*(2.0_double - t2*t3) )

      end subroutine

      subroutine poirier_tarantola_i(m,n,x,fvec,iflag)

         integer :: m,n,iflag
         real(double) :: x(n),fvec(m)

         real(double) :: v0,e0,b0,bp,eta(m),t1(m),t2(m)

         v0 = x(1)
         e0 = x(2)
         b0 = x(3)
         bp = x(4)

         eta = (eos%xdata/v0)**(1.0_double/3.0_double)

         t1 = ( 9.0_double*b0*v0*log(eta)*log(eta)/6.0_double )
         t2 = ( 3.0_double - 3.0_double*log(eta)*(bp - 2.0_double) )

         fvec = eos%ydata - ( e0 + t1*t2 )

      end subroutine

      end module check_eos_mod
