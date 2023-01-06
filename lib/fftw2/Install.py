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

version = "2.1.5"

# --- Input Arguments ------------------------------------------------ #

parser = ArgumentParser(prog = 'Install.py', description = "Helper script to download and build the FFTW2 library")

parser.add_argument("-d", action = "store_true", help = "clone the FFTW2 library into lib/fftw2")
parser.add_argument("-b", action = "store_true", help = "build the FFTW2 library")
parser.add_argument("-c", action = "store_true", help = "clean the FFTW2 build space")

args = parser.parse_args()

cloneflag = args.d
buildflag = args.b
cleanflag = args.c

clonepath = os.path.join(os.path.abspath(os.path.expanduser(".")),"fftw2")
buildpath = clonepath
libprefix = os.path.join(buildpath,"bin")

# --- Help Message --------------------------------------------------- #

HELP_MESSAGE = """
Examples:

Syntax from lib dir: python Install.py -d
                 or: python Install.py -b
                 or: python Install.py -c

Syntax from src dir: make lib-fftw2 args="-d"
                 or: make lib-fftw2 args="-b"
                 or: make lib-fftw2 args="-c"
"""

if not cloneflag and not buildflag and not cleanflag:
   parser.print_help()
   sys.exit(HELP_MESSAGE)

# --- Download the library ------------------------------------------- #

src = 'fftw-' + version
out = src + '.tar.gz'
url = 'https://fftw.org/pub/fftw/' + out

if cloneflag:
   success = False
   if os.path.isdir(clonepath):
      print("ERROR: The download destination '" + clonepath + "' already exists")
      sys.exit(1)
   if not success:
      print("Downloading the FFTW2 library using wget ...")
      cmd = 'wget -O %s %s' % (out,url)
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         success = True
      except subprocess.CalledProcessError as e:
         print("Download using wget failed with:\n%s" % e.output.decode('UTF-8'))
   if not success:
      print("Downloading the FFTW2 library using curl ...")
      cmd = 'curl -L -o %s %s' % (out,url)
      try:
         subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         success = True
      except subprocess.CalledProcessError as e:
         print("Download using curl failed with:\n%s" % e.output.decode('UTF-8'))
   if not success:
      print('Failed to download source code with "wget" or "curl"')
      sys.exit(1)
   else:
      print("Applying kludge to prevent FFTW2 vs. FFTW3 race condition ...")
      cmd = 'tar -zxf %s && mv %s %s && rm %s' % (out,src,clonepath,out)
      subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
      try:
         f = open(buildpath+"/fftw/fftw.h.in","r")
         contents = f.readlines()
         f.close()
         contents.insert(23,'#define fftw_destroy_plan fftw2_destroy_plan\n')
         f = open(buildpath+"/fftw/fftw.h.in","w")
         contents = "".join(contents)
         f.write(contents)
         f.close()
      except subprocess.CalledProcessError as e:
         print("Applying kludge failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Build the library ---------------------------------------------- #

cmd = 'cd %s && ./configure --prefix=%s CC=mpicc CFLAGS="-O3" && make && make install' % (buildpath,libprefix)

if buildflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The build destination '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Building the FFTW2 library using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Build using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- Clean the build space ------------------------------------------ #

cmd = 'cd %s && make clean' % (buildpath)

if cleanflag:
   success = False
   if not os.path.isdir(buildpath):
      print("ERROR: The destination path '" + buildpath + "' does not exist")
      sys.exit(1)
   if not success:
      print("Cleaning the FFTW2 build space using make ...")
      try:
         txt = subprocess.check_output(cmd, shell = True, stderr = subprocess.STDOUT).decode('UTF-8')
         print(txt)
      except subprocess.CalledProcessError as e:
         print("Clean using make failed with:\n%s" % e.output.decode('UTF-8'))
         sys.exit(1)

# --- End of the file ------------------------------------------------ #
