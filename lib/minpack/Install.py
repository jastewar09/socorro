#!/usr/bin/env python

"""
Install.py is a helper tool to download, unpack, and build this library.
This is used to automate steps described in the README file in this dir.
"""

# --- Python Modules ------------------------------------------------- #

from __future__ import print_function
from argparse import ArgumentParser
import os, shutil, subprocess, sys

# --- Input Arguments ------------------------------------------------ #

parser = ArgumentParser(prog = 'Install.py', description = "Helper script to download and build the MINPACK library")

parser.add_argument("-b", action = "store_true", help = "build the MINPACK library")
parser.add_argument("-c", action = "store_true", help = "clean the MINPACK build space")

args = parser.parse_args()

buildflag = args.b
cleanflag = args.c

buildpath = os.path.join(os.path.abspath(os.path.expanduser(".")),"minpack")
libprefix = buildpath

# --- Help Message --------------------------------------------------- #

HELP_MESSAGE = """
Examples:

Syntax from lib dir: python Install.py -b
                 or: python Install.py -c

Syntax from src dir: make lib-lapack args="-b"
                 or: make lib-lapack args="-c"
"""

if not buildflag and not cleanflag:
   parser.print_help()
   sys.exit(HELP_MESSAGE)

# --- Build the Library ---------------------------------------------- #

cmd = 'cd %s && make minpack FC=mpif90 FCFLAGS="-O3"' % (buildpath)

if buildflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The build destination '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Building the MINPACK library using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Build using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Clean the Build Space ------------------------------------------ #

cmd = 'cd %s && make clean' % (buildpath)

if cleanflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The destination path '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Cleaning the MINPACK build space using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Clean using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- End of the File ------------------------------------------------ #
