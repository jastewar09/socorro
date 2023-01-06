#!/usr/bin/env python

"""
Install.py is a helper tool to download, unpack, and build this library.
This is used to automate steps described in the README file in this dir.
"""

# --- Python Modules ------------------------------------------------- #

from __future__ import print_function
from argparse import ArgumentParser
import os,shutil,subprocess,sys

# --- Library Version ------------------------------------------------ #

version = "v2.2.1"

# --- Input Arguments ------------------------------------------------ #

parser = ArgumentParser(prog = 'Install.py', description = "Helper script to download and build the SCALAPACK library")

parser.add_argument("-d", action = "store_true", help = "clone the SCALAPACK library into lib/scalapack")
parser.add_argument("-b", action = "store_true", help = "build the SCALAPACK library")
parser.add_argument("-c", action = "store_true", help = "clean the SCALAPACK build space")
parser.add_argument("-x", action = "store_true", help = "build with the -fallow-argument-mismatch flag")

args = parser.parse_args()

cloneflag = args.d
buildflag = args.b
cleanflag = args.c
flagsflag = args.x

clonepath = os.path.join(os.path.abspath(os.path.expanduser(".")),"scalapack")
buildpath = clonepath
libprefix = os.path.join(buildpath,"bin")

# --- Help Message --------------------------------------------------- #

HELP_MESSAGE = """
Examples:

Syntax from lib dir: python Install.py -d
                 or: python Install.py -b
                 or: python Install.py -c

Syntax from src dir: make lib-scalapack args="-d"
                 or: make lib-scalapack args="-b"
                 or: make lib-scalapack args="-c"
                 or: make lib-scalapack args="-b -x"
"""

if not cloneflag and not buildflag and not cleanflag:
   parser.print_help()
   sys.exit(HELP_MESSAGE)

# --- Download the Library ------------------------------------------- #

cmd = 'git clone --depth 1 --branch %s https://github.com/Reference-ScaLAPACK/scalapack.git %s' % (version,clonepath)

if cloneflag:
   success = False
   if os.path.isdir(clonepath):
      print("ERROR: The download destination '" + clonepath + "' already exists")
      sys.exit(1)
   if not success:
      print("Downloading the SCALAPACK library using git ...")
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
      except subprocess.CalledProcessError as e:
         print("Download using git failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Build the Library ---------------------------------------------- #

if args.x:
   cmd = 'cd %s && cp SLmake.inc.example SLmake.inc && make lib FC=mpif90 FCFLAGS="-O3 -fallow-argument-mismatch" CC=mpicc CCFLAGS="-O3 -Wno-error=implicit-function-declaration"' % (buildpath)
else:
   cmd = 'cd %s && cp SLmake.inc.example SLmake.inc && make lib FC=mpif90 FCFLAGS="-O3" CC=mpicc CCFLAGS="-O3 -Wno-error=implicit-function-declaration"' % (buildpath)

if buildflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The build destination '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Building the SCALAPACK library using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Build using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Clean the Build Space ------------------------------------------ #

cmd = 'cd %s && cp SLmake.inc.example SLmake.inc && make clean' % (buildpath)

if cleanflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The destination path '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Cleaning the SCALAPACK build space using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Clean using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- End of the File ------------------------------------------------ #
