#!/usr/bin/env python

"""
Install.py is a helper tool to download, unpack, and build this library.
This is used to automate steps described in the README file in this dir.
"""

# --- Python Modules ------------------------------------------------- #

from __future__ import print_function
from argparse import ArgumentParser
import os, shutil, subprocess, sys

# --- Library Version ------------------------------------------------ #

version = "v3.11.0"

# --- Input Arguments ------------------------------------------------ #

parser = ArgumentParser(prog = 'Install.py', description = "Helper script to download and build the LAPACK library")

parser.add_argument("-d", action = "store_true", help = "clone the LAPACK library to lib/lapack")
parser.add_argument("-b", action = "store_true", help = "build the LAPACK library")
parser.add_argument("-c", action = "store_true", help = "clean the LAPACK build space")

args = parser.parse_args()

cloneflag = args.d
buildflag = args.b
cleanflag = args.c

clonepath = os.path.join(os.path.abspath(os.path.expanduser(".")),"lapack")
buildpath = clonepath
libprefix = os.path.join(buildpath,"bin")

# --- Help Message --------------------------------------------------- #

HELP_MESSAGE = """
Examples:

Syntax from lib dir: python Install.py -d
                 or: python Install.py -b
                 or: python Install.py -c

Syntax from src dir: make lib-lapack args="-d"
                 or: make lib-lapack args="-b"
                 or: make lib-lapack args="-c"
"""

if not cloneflag and not buildflag and not cleanflag:
   parser.print_help()
   sys.exit(HELP_MESSAGE)

# --- Download the Library ------------------------------------------- #

cmd = 'git clone --depth 1 --branch %s https://github.com/Reference-LAPACK/lapack.git %s' % (version,clonepath)

if cloneflag:
   success = False
   if os.path.isdir(clonepath):
      print("ERROR: The download destination '" + clonepath + "' already exists")
      sys.exit(1)
   if not success:
      print("Downloading the LAPACK library using git ...")
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
      except subprocess.CalledProcessError as e:
         print("Download using git failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Build the Library ---------------------------------------------- #

cmd = 'cd %s && cp make.inc.example make.inc && make blaslib lapacklib FC=mpif90 FFLAGS="-O3" CC=mpicc CFLAGS="-O3" TIMER="NONE"' % (buildpath)

if buildflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The build destination '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Building the LAPACK library using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Build using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Clean the Build Space ------------------------------------------ #

cmd = 'cd %s && cp make.inc.example make.inc && make clean' % (buildpath)

if cleanflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The destination path '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Cleaning the LAPACK build space using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Clean using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- End of the File ------------------------------------------------ #
