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


import os, sys, time
from subprocess import call

start = time.time()

base = str(sys.argv[1])
numpoints = str(sys.argv[2])
vsitedist = str(sys.argv[3])
verbose = int(sys.argv[4])
xval0 = str(sys.argv[5])
cspacing = str(sys.argv[6])
if len(sys.argv) == 8: 
   debug = str(sys.argv[7])
else:
   debug = ''
   
if (verbose == 1):
   log = ''
   log2 = ''
   log3 = " 2>&-"
else:
   log = " > log"
   log2 = " > log 2>&1"
   log3 = " > log 2>&1"

meshlabserver = os.environ["mshlbsrvr"]

print ''
print "==============================================================================="
print ''
print "Voronoi Surface Reconstruction"
print "-------------------------------------------------------------------------------"
print "Input point cloud:", base+"-ptcloud.ply"
print "Output surface mesh:", base+".ply"

call(["rm", "-f", base+".xyz", base+".vor", base+"-init.ply", base+"-init.vtk", base+"-fine.ply", base+"-fine.vtk", base+".ply", base+".stl"])

call(["rm", "-f", base+"-snapsegs.vtk", base+"-cyclesegs.vtk", base+"-badfcts.vtk", base+"-goodfcts.vtk"])

callstr = "CALL: "

#compute Voronoi sites
command = "./vsitegen " + base + " " + vsitedist
if (xval0 == '0'):
   command = command + ' ' + cspacing
if (verbose == 1):
   print "-------------------------------------------------------------------------------"
   print "Compute Voronoi sites"
   print callstr + command
os.system(command)
if (verbose == 1):
   print
print "-------------------------------------------------------------------------------"


#compute Voronoi partition
command = "qvoronoi Qz p Fv < "+base+".xyz" + " > " + base+".vor"
print "Compute Voronoi partition (~1 min)"
if (verbose == 1):
   print callstr + command
if (verbose == 0):
   print '\rworking...',
   sys.stdout.flush()
os.system(command)


#Generate b-rep
if debug == 'debug':
   command = "./qvor2vtk " + base + ' ' + debug
   if (verbose == 1):
      print
      print "-------------------------------------------------------------------------------"
   print "\rGenerate b-rep (~1 min)"
   if (verbose == 1):
      print callstr + command   
   if (verbose == 0):
      print '\rworking...',
      sys.stdout.flush()
   os.system(command+log3)
else:
   command = "./qvor2vtk " + base
   if (verbose == 1):
      print
      print "-------------------------------------------------------------------------------"         
   print "\rGenerate b-rep (~1 min)"
   if (verbose == 1):
      print callstr + command   
   if (verbose == 0):
      print '\rworking...',
      sys.stdout.flush()
   os.system(command+log3)


#Clean b-rep
command = "pvpython clean.py " + base+"-fine.vtk " + base+"-fine.ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"      
print "\rClean (~1 min)"
if (verbose == 1):
   print "-------------------------------------------------------------------------------"         
   print "Paraview Clean: " + base+"-init.vtk"
   print callstr + command
if (verbose == 0):
   print '\rworking...',
   sys.stdout.flush()
os.system("pvpython clean.py " + base+"-init.vtk " + base+"-init.ply" + log)
os.system(command + log)
command1 = "pvpython connectivity.py " + base+"-init.ply " + base+"-init.ply"
command2 = "pvpython connectivity.py " + base+"-fine.ply " + base+"-fine.ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Paraview Connectivity: " + base+"-init.ply"
   print callstr + command1
os.system(command1 + log)
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Paraview Connectivity: " + base+"-fine.ply"
   print callstr + command2
os.system(command2 + log)
command = meshlabserver + " -i " + base+"-init.ply" + " -o " + base+"-init.ply -s reorient.mlx"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Meshlab Reorient Faces: " + base+"-init.ply"
   print callstr + command
os.system(command + log2)
command = meshlabserver + " -i " + base+"-fine.ply" + " -o " + base+"-fine.ply -s reorient.mlx"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Meshlab Reorient Faces: " + base+"-fine.ply"
   print callstr + command
os.system(command + log2)
command = "pvpython connectivity.py " + base+"-fine.ply " + base+"-fine.ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Paraview Connectivity: " + base+"-fine.ply"
   print callstr + command
os.system(command + log2)
command = meshlabserver + " -i " + base+"-fine.ply" + " -o " + base+"-fine.ply"
if (verbose == 1):
   print
   print "-------------------------------------------------------------------------------"
   print "Meshlab Convert to Binary: " + base+"-fine.ply"
   print callstr + command
os.system(command + log2)

##Taubin smooth
# NOTE: NOT NECESSARY BUT GIVES SMOOTHER/MORE AESTHETICALLY PLEASING RESULTS
command = meshlabserver + " -i " + base+"-fine.ply" + " -o " + base+"-fine.ply -s taubin.mlx"
os.system(command + " > log2 2>&1")
os.system('rm -rf log2')


#ACVD decimation
command = "ACVD " + base+"-fine.ply " + numpoints + " 0 -m 1 -sf 2 -d 0 -of " + base+".ply"
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


##Taubin smooth
# NOTE: NOT NECESSARY BUT GIVES SMOOTHER/MORE AESTHETICALLY PLEASING RESULTS
command = meshlabserver + " -i " + base+".ply" + " -o " + base+".ply -s taubin.mlx"
os.system(command + " > log2 2>&1")
os.system('rm -rf log2')


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
os.system(command + log2)


#remove intermediate files
call(["rm", "-f", "smooth_"+base+".ply"])
if (debug == ''):
   call(["rm", "-f", base+"-init.vtk"])
   call(["rm", "-f", base+"-init.ply"])
   call(["rm", "-f", base+"-fine.vtk"])
call(["rm", "-f", "log"])


end = time.time()
elapsed = (end - start)/60.
if (verbose == 1):
   print
print "\r-------------------------------------------------------------------------------"
print "Voronoi-based surface reconstruction complete"
print "Elapsed time:", format(elapsed, '.2f'), "minutes"


