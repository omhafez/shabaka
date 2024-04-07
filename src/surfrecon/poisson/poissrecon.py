# -----------------------------------------------------------------------------
#
# Shabaka is the proprietary property of The Regents of the University of California.
#
# Copyright (C) 2017 The Regents of the University of California, Davis campus. All Rights Reserved.
#
# This software may be patent pending.
#
# The software program and documentation are supplied "as is", without any accompanying services from The Regents, for purposes of confidential discussions only. The Regents does not warrant that the operation of the program will be uninterrupted or error-free. The end-user understands that the program was developed for research purposes and is advised not to rely exclusively on the program for any reason.
#
# IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. THE REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE REGENTS HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#
# -----------------------------------------------------------------------------


import os, sys, time, subprocess, platform, commands
from subprocess import call

start = time.time()
base = str(sys.argv[1])
numpoints = str(sys.argv[2])
verbose = int(sys.argv[3])

if (verbose == 1):
   log = ''
   log2 = ''
   log4 = " > log 2>&1"
else:
   log = " > log"
   log2 = " > log 2>&1"
   log4 = " > log 2>&1"

meshlabserver = os.environ["mshlbsrvr"]

print ''
print "==============================================================================="
print ''
print "Screened Poisson Surface Reconstruction"
print "-------------------------------------------------------------------------------"
print "Input point cloud:", base+"-ptcloud.ply"
print "Output surface mesh:", base+".ply"

call(["rm", "-f", base+"-fine.ply", base+".ply", base+".stl"])

callstr = "CALL: "


#meshlab surface reconstruction
if ("Darwin" in platform.system()):
   command = meshlabserver + " -i " + base+"-ptcloud.ply" + " -o " + base+"-fine.ply" + " -s poissrecon.mlx"
else:
   status, output = commands.getstatusoutput("grep -q Microsoft /proc/version")
   if (status != 0):
      command = meshlabserver + " -i " + base+"-ptcloud.ply" + " -o " + base+"-fine.ply" + " -s poissrecon.mlx"
   else:
      pwd = os.getcwd()
      pwd = pwd.replace("/mnt/c","C:")
      pwd = pwd.replace("/","\\\\")
      pwd = pwd+"\\\\"
      status, poissloc = commands.getstatusoutput("readlink -f poissrecon.mlx")
      poissloc = poissloc.replace("/mnt/c","C:")
      poissloc = poissloc.replace("/","\\\\")
      command = "/mnt/c/Program\ Files/VCG/MeshLab/meshlabserver.exe -i " + pwd + base+"-ptcloud.ply" + " -o " + pwd + base+"-fine.ply" + " -s " + poissloc
      print command

print "-------------------------------------------------------------------------------"
print "Meshlab Screened Poisson Surface Reconstruction (~1 min)"
if (verbose == 1):
    print callstr + command
if (verbose == 0):
    print '\rworking...',
    sys.stdout.flush()
os.system(command+log4)


#connectivity
command = "pvpython connectivity.py " + base+"-fine.ply" + " " + base+"-fine.ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Paraview Connectivity: " + base+"-fine.ply"
   print callstr + command
os.system(command + log)
command = meshlabserver + " -i " + base+"-fine.ply" + " -o " + base+"-fine.ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Meshlab Convert to Binary: " + base+"-fine.ply"
   print callstr + command
os.system(command + log4)


# ##Taubin smooth
# # NOTE: NOT NECESSARY BUT GIVES SMOOTHER/MORE AESTHETICALLY PLEASING RESULTS
# command = meshlabserver + " -i " + base+"-fine.ply" + " -o " + base+"-fine.ply -s taubin.mlx"
# os.system(command + " > log2 2>&1")
# os.system('rm -rf log2')


#ACVD decimation
command = "ACVD " + base+"-fine.ply "+numpoints+" 0.5 -m 1 -sf 2 -d 0 -of "+base+".ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
print "\rCoarsen (~1 min)"
if (verbose == 1):
   print callstr + command
if (verbose == 0):
   print '\rworking...',
   sys.stdout.flush()
os.system(command + log)


# ##Taubin smooth
# # NOTE: NOT NECESSARY BUT GIVES SMOOTHER/MORE AESTHETICALLY PLEASING RESULTS
# command = meshlabserver + " -i " + base+".ply" + " -o " + base+".ply -s taubin.mlx"
# os.system(command + " > log2 2>&1")
# os.system('rm -rf log2')


#connectivity
command = "pvpython connectivity.py " + base+".ply" + " " + base+".ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Paraview Connectivity: " + base+".ply"
   print callstr + command
os.system(command + log)


#export STL file
command = meshlabserver + " -i " + base+".ply" + " -o " + base+".stl"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Meshlab Export STL: " + base+".ply"
   print callstr + command
os.system(command + log4)


#remove intermediate files
call(["rm", "-f", "smooth_"+base+".ply"])
call(["rm", "-f", "log"])


end = time.time()
elapsed = (end - start)/60.
#print
print "\r-------------------------------------------------------------------------------"
print "Screened Poisson Surface Reconstruction Complete"
print "Elapsed time:", format(elapsed, '.2f'), "minutes"
