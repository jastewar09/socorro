"""
# Purpose: Fit 4 equations-of-state (EOS) to energy vs. volume data and optionally plot the results.
#
#
"""

import sys
import numpy as np

from scipy.optimize import leastsq

# --- Beginning of the main file --------------------------------------------------------------------------------------------- #

def birch_murnaghan(params, vol):
    """
    Physical Review B 70, 224107 (2004) - Eq. (3) Modified
    """

    V0, E0, B0, BP = params

    eta = (V0/vol)**(2.0/3.0)

    E = E0 + ( 9.0*B0*V0/16.0 )*( (eta - 1.0)**2 )*( 6.0 + BP*(eta - 1.0) - 4.0*eta )

    return E

# ---

def murnaghan(params, vol):
    """
    Physical Review B 28(10), 5480 (1983) - Eq. (4)
    """

    V0, E0, B0, BP = params

    eta = (V0/vol)**(BP)

    E = E0 + ( B0*vol/BP )*( (eta/(BP - 1.0)) + 1.0 ) - ( B0*V0/(BP - 1.0) )

    return E

# ---

def universal(params, vol):
    """
    Physical Review B 70, 224107 (2004) - Eq. (1)
    """

    V0, E0, B0, BP = params

    eta = (vol/V0)**(1.0/3.0)

    E = E0 + ( 2.0*B0*V0/((BP - 1.0)**2) )*\
             ( 2.0 - (5.0 + 3.0*BP*(eta - 1.0) - 3.0*eta)*(np.exp(-3.0*(BP - 1.0)*(eta - 1.0)/2.0)) )

    return E

# ---

def poirier_tarantola(params, vol):
    """
    Physical Review B 70, 224107 (2004) - Eq. (2)
    """

    V0, E0, B0, BP = params

    eta = np.log( (vol/V0)**(1.0/3.0) )

    E = E0 + ( 9.0*B0*V0*eta*eta/2.0 )*( 1.0 - eta*(BP - 2.0) )

    return E

# ---

def initial_solutions(vol, nrg):
    """
    Fit a quadratic equation to the data to initialize the EOS parameters.
    """

    a, b, c = np.polyfit(vol, nrg, 2)

    V0 = -b/(2.0*a)
    E0 = a*V0**2 + b*V0 + c
    B0 = 2.0*a*V0
    BP = 4.0

    return V0, E0, B0, BP

# ---

def print_fit(tag, fit):
    """
    Write the optimized EOS parameters to the screen.
    """

    V0, E0, B0, BP = fit

    print("\n%s:\n"       % (tag))
    print("  V0 = %f A^3" % (V0))
    print("  E0 = %f eV"  % (E0))
    print("  B0 = %f GPa" % (B0*160.21766208))
    print("  B' = %f"     % (BP))

    return None

# ---

def read_arg(prompt, default, accepted = []):
    """
    Read command-line options with prescribed default and accepted values.
    """

    result = input(prompt + " [default = %s]: " % (default))

    if (result == ""):
        result = default
    elif (len(accepted) != 0) and (result not in accepted):
        print("\nERROR: Invalid input option - accepted values are:", *accepted)
        sys.exit()

    return result

# ---

def update_data(vol, nrg):
    """
    Convert the input data units, if necessary.
    """

    is_volume = ("vol" == read_arg("\nIs the data in volume or lattice spacing units (vol/lat)", "vol", ["vol","lat"]))

    if (is_volume):
        vol_unit = read_arg("\nWhat are the volume units (ang3/bohr3)", "ang3", ["ang3","bohr3"])
        if (vol_unit == "bohr3"): vol *= (0.529177249**3)
    else:
        vol_unit = read_arg("\nWhat are the lattice units (ang/bohr)", "ang", ["ang","bohr"])
        if (vol_unit == "bohr"): vol *= 0.529177249
        lat = read_arg("\nWhat is the lattice type (sc/fcc/bcc)", "sc", ["sc","bcc","fcc"])
        fac = 1.0
        if (lat == "fcc"): fac = 0.25
        if (lat == "bcc"): fac = 0.50
        vol = fac*(vol**3)

    nrg_unit = read_arg("\nWhat are the energy units (eV/Ry/Ha)", "eV", ["eV","Ry","Ha"])

    if (nrg_unit == "Ry"): nrg *= 13.605698066
    if (nrg_unit == "Ha"): nrg *= 27.211396132

    return vol, nrg

# ---

def read_data():
    """
    Load data from the input file.
    """

    fname = read_arg("\nWhat is the filename containing the data", "energy.dat")

    try:
        f = open(fname, "rt") ; f.close()
    except:
        print("\nERROR: The input file %s could not be opened" % (fname))
        sys.exit()

    data = np.genfromtxt(fname, dtype = float, skip_header = 0, delimiter = " ")

    return data[:,0], data[:,1]

# ---

def main():
    """
    Execute the fitting and plotting routines.
    """

    print("Welcome to eos_fit.py!")

    vol, nrg = read_data()
    vol, nrg = update_data(vol, nrg)
    x0 = initial_solutions(vol, nrg)

    fcn = lambda params, y, x : y - birch_murnaghan(params, x)
    fit_bm, info = leastsq(fcn, x0, args = (nrg, vol))
    print_fit("Birch-Murnaghan EOS", fit_bm)

    fcn = lambda params, y, x : y - murnaghan(params, x)
    fit_m, info = leastsq(fcn, x0, args = (nrg, vol))
    print_fit("Murnaghan EOS", fit_m)

    fcn = lambda params, y, x : y - universal(params, x)
    fit_u, info = leastsq(fcn, x0, args = (nrg, vol))
    print_fit("Universal EOS", fit_u)

    fcn = lambda params, y, x : y - poirier_tarantola(params, x)
    fit_pt, info = leastsq(fcn, x0, args = (nrg, vol))
    print_fit("Poirier-Tarantola EOS", fit_pt)

    no_plot = ("no" == read_arg("\nCreate a plot of the results (yes/no)", "no", ["yes","no"]))

    if (not no_plot):

        try:
            import pylab as plt
        except:
            print("\nERROR: pylab module not available, aborting plot")
            sys.exit()

        dat = np.linspace(np.min(vol),np.max(vol),100)

        plt.rcParams['mathtext.fontset'] = 'cm'
        plt.rcParams['font.family'] = 'serif'
        plt.rcParams['font.serif'] = 'Times'
        plt.rcParams['font.size'] = 12
        plt.rcParams['figure.figsize'] = 6.0,4.5

        plt.plot(vol, nrg, "ro", label = "Data")
        plt.plot(dat, birch_murnaghan(fit_bm, dat), label = 'Birch-Murnaghan EOS')
        plt.plot(dat, murnaghan(fit_m, dat), label = 'Murnaghan EOS')
        plt.plot(dat, universal(fit_u, dat), label = 'Universal EOS')
        plt.plot(dat, poirier_tarantola(fit_pt, dat), label = 'Poirier-Tarantola EOS')

        plt.xlabel(r'Volume ($\mathrm{\AA}^3$)')
        plt.ylabel(r'Energy (eV)')

        plt.legend(loc = 'best')
        plt.tight_layout()
        plt.show()

    print("\nTask Finished.")

    return None

# ---

if  __name__ == "__main__":
    """
    Instantiate the main routine.
    """

    sys.exit(main())

# --- End of the main file --------------------------------------------------------------------------------------------------- #
