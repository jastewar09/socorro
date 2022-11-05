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

version = "4.3.4"

# --- Input Arguments ------------------------------------------------ #

parser = ArgumentParser(prog = 'Install.py', description = "Helper script to download and build the LIBXC library")

parser.add_argument("-d", action = "store_true", help = "clone the LIBXC library into lib/libxc")
parser.add_argument("-b", action = "store_true", help = "build the LIBXC library")
parser.add_argument("-c", action = "store_true", help = "clean the LIBXC build space")

args = parser.parse_args()

cloneflag = args.d
buildflag = args.b
cleanflag = args.c

clonepath = os.path.join(os.path.abspath(os.path.expanduser(".")),"libxc")
buildpath = clonepath
libprefix = os.path.join(buildpath,"bin")

# --- Help Message --------------------------------------------------- #

HELP_MESSAGE = """
Examples:

Syntax from lib dir: python Install.py -d
                 or: python Install.py -b
                 or: python Install.py -c

Syntax from src dir: make lib-libxc args="-d"
                 or: make lib-libxc args="-b"
                 or: make lib-libxc args="-c"
"""

if not cloneflag and not buildflag and not cleanflag:
   parser.print_help()
   sys.exit(HELP_MESSAGE)

# --- Download the Library ------------------------------------------- #

cmd = 'git clone --depth 1 --branch %s https://gitlab.com/libxc/libxc.git %s' % (version,clonepath)

if cloneflag:
   success = False
   if os.path.isdir(clonepath):
      print("ERROR: The download destination '" + clonepath + "' already exists")
      sys.exit(1)
   if not success:
      print("Downloading the LIBXC library using git ...")
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
      except subprocess.CalledProcessError as e:
         print("Download using git failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Build the Library ---------------------------------------------- #

cmd = 'cd %s && autoreconf -i && ./configure FC=mpif90 FCFLAGS="-O3" CC=mpicc CFLAGS="-O3" --prefix=%s && make && make install' % (buildpath,libprefix)

if buildflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The build destination '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Building the LIBXC library using make ...")
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
      print("Cleaning the LIBXC build space using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Clean using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- End of the File ------------------------------------------------ #
