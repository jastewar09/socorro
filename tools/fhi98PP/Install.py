#!/usr/bin/env python

"""
Install.py is a helper tool to download, unpack, and build this package.
This is used to automate steps described in the README file in this dir.
"""

# --- Python Modules ------------------------------------------------- #

from __future__ import print_function
from argparse import ArgumentParser
import os, shutil, subprocess, sys

# --- Input Arguments ------------------------------------------------ #

parser = ArgumentParser(prog = 'Install.py', description = "Helper script to download and build the fhi98PP package")

parser.add_argument("-d", action = "store_true", help = "clone the fhi98PP library into tools/fhi98PP")
parser.add_argument("-b", action = "store_true", help = "build the fhi98PP library")
parser.add_argument("-c", action = "store_true", help = "clean the fhi98PP build space")
parser.add_argument("-x", action = "store_true", help = "build with the -fallow-argument-mismatch flag")

args = parser.parse_args()

cloneflag = args.d
buildflag = args.b
cleanflag = args.c
flagsflag = args.x

clonepath = os.path.join(os.path.abspath(os.path.expanduser(".")),"fhi98PP")
buildpath = os.path.join(clonepath,"src")
libprefix = None

# --- Help Message --------------------------------------------------- #

HELP_MESSAGE = """
Examples:

Syntax from tools dir: python Install.py -d
                   or: python Install.py -b
                   or: python Install.py -c
                   or: python Install.py -b -x

"""

if not cloneflag and not buildflag and not cleanflag:
   parser.print_help()
   sys.exit(HELP_MESSAGE)

# --- Download the Library ------------------------------------------- #

out = 'fhi98PP.tar.gz'
url = 'https://th.fhi-berlin.mpg.de/th/fhi98md/download/fhi98PP.tar.gz'

if cloneflag:
   success = False
   if os.path.isdir(clonepath):
      print("ERROR: The download destination '" + clonepath + "' already exists")
      sys.exit(1)
   if not success:
      print("Downloading the fhi98PP package using wget ...")
      cmd = 'wget -O %s %s' % (out,url)
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         success = True
      except subprocess.CalledProcessError as e:
         print("Download using wget failed with:\n%s" % e.output.decode('UTF-8'))
   if not success:
      print("Downloading the fhi98PP package using curl ...")
      cmd = 'curl -L -o %s %s' % (out,url)
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         success = True
      except subprocess.CalledProcessError as e:
         print("Download using curl failed with:\n%s" % e.output.decode('UTF-8'))
   if not success:
      print('Failed to download the package with "wget" or "curl"')
      sys.exit(1)
   else:
      cmd = 'tar -zxf %s && rm %s' % (out,out)
      subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')

# --- Build the Library ---------------------------------------------- #

if args.x:
    cmd = 'cd %s && make all FC=mpif90 FFLAGS="-O3 -fd-lines-as-comments -fallow-argument-mismatch" LDFLAGS="$(FFLAGS)"' % (buildpath)
else:
    cmd = 'cd %s && make all FC=mpif90 FFLAGS="-O3 -fd-lines-as-comments" LDFLAGS="$(FFLAGS)"' % (buildpath)

if buildflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The build destination '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Building the fhi98PP package using make ...")
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
      print("Cleaning the fhi98PP build space using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Clean using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- End of the File ------------------------------------------------ #
