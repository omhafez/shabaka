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


import os, sys, time, subprocess, glob
from subprocess import call, check_output

sh_dir = os.environ["shabaka_dir"]

if(len(sys.argv)) == 1:
   print "------------------------------------------------------------------------------"
   os.system("cat $shabaka_dir/doc/usage")
   print "------------------------------------------------------------------------------"
   print "Please read README for more information"
   print

elif(len(sys.argv) == 2 and str(sys.argv[1]) == '--help'):
   print "------------------------------------------------------------------------------"
   os.system("cat $shabaka_dir/doc/usage")
   print "------------------------------------------------------------------------------"
   print "Please read README for more information"
   print

else:
   print "==============================================================================="
   start = time.time()

   infile = str(sys.argv[1])
   sp = infile.split('.');
   base = '.'.join(sp[0:len(sp)-1])
   base = base.split('-ptcloud')[0]
   ext = sp[len(sp)-1]

   ## 1. read input parameters
   #set default params
   xval = '11'
   smoothpc =  0
   surfrecon = 'v'
   numsurfpoints = 30000
   sitepairdist = 1.1
   verbose = 0
   allfiles = 0
   debug = ''
   topt = 'pqY'
   qq = 'Q'

   #read in command line options
   ArgumentsIndex = 2
   xflag = 0; rflag = 0; nflag = 0; dflag = 0; tflag = 0;
   kflag = 0; bflag = 0; lflag = 0; fflag = 0; cflag = 0;
   cspacing = 0.4;

   while (ArgumentsIndex < len(sys.argv)):
      key = sys.argv[ArgumentsIndex]
      val = key[2:len(key)]
      key = key[0:2]
      if (key == '-x'):
         if (xflag == 0):
            xval = val
            xflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')
      if (key == '-s'):
         smoothpc = 1
      if (key == '-n'):
         if (nflag == 0):
            if (len(val) > 0):
               numsurfpoints = int(val)
            nflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')
      if (key == '-p'):
         surfrecon = 'p'
      if (key == '-d'):
         if (dflag == 0):
            if (len(val) > 0):
               sitepairdist = float(val)
            dflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')
      if (key == '-t'):
         if (tflag == 0):
            if (len(val) > 0):
               topt = val
            tflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')
      if (key == '-k'):
         kflag = 1;
      if (key == '-b'):
         bflag = 1;
      if (key == '-v'):
         verbose = 1
      if (key == '-a'):
         allfiles = 1
      if (key == '-l'):
         lflag = 1;
      if (key == '-g'):
         debug = 'debug'
      if (key == '-f'):
         fflag = 1
      if (key == '-c'):
         if (cflag == 0):
            if (len(val) > 0):
               cspacing = float(val)
            cflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')

      ArgumentsIndex += 1

   if (ext == 'ply'):
      xflag = 1
      xval = '01'

   if (verbose == 1):
      log = ''
      log2 = ''
   else:
      log = " > log"
      log2 = " > log 2>&1"


   ## 2. determine whether to remove previous output files

   filelist = [base+'.xyz',           base+'.vor',           base+'-resampled.nrrd',
               base+'-fine.ply',      base+'-init.ply',      base+'-init.vtk',
               base+'-fine.ply',      base+'-fine.vtk',      'smooth_'+base+'.ply',
               base+'-snapsegs.vtk',  base+'-cyclesegs.vtk', base+'-badfcts.vtk',
               base+'-goodfcts.vtk',  base+'.ply',           base+'.stl',
               base+'.vtk',           base+'.inp',           base+'-tetgen',
               'log',                 'log2']
   if (xval[0] == '1'):
      filelist.extend([base+'-ptcloud.ply'])

   proceed = False
   if (fflag == 0 and (any([os.path.exists(f) for f in filelist]))):
      usrinput = raw_input('Old job files exist. Wipe all files and proceed? (y/n): ')
      if (usrinput == 'y' or usrinput == 'Y'):
         proceed = True
   else:
      proceed = True

   if (proceed == True):
      os.system('rm -rf ptcloudgen.py'); os.system('rm -rf connectivity.py');
      os.system('rm -rf qvor2vtk'); os.system('rm -rf poissrecon.mlx');
      os.system('rm -rf vorrecon.py'); os.system('rm -rf poissrecon.py');
      os.system('rm -rf smooth.mlx'); os.system('rm -rf reorient.mlx');
      os.system('rm -rf vsitegen'); os.system('rm -rf ptcloudgen.py');
      os.system('rm -rf ptcloudgen'); os.system('rm -rf smooth.mlx');
      os.system('rm -rf clean.py'); os.system('rm -rf taubin.mlx');

      if (xval[0] == '1'):
        os.system('rm -rf '+base+'-ptcloud.ply');

      os.system('rm -rf '+base+'.xyz');
      os.system('rm -rf '+base+'.vor'); os.system('rm -rf '+base+'-fine.ply');
      os.system('rm -rf '+base+'-init.ply'); os.system('rm -rf '+base+'-init.vtk');
      os.system('rm -rf '+base+'-fine.ply'); os.system('rm -rf '+base+'-fine.vtk');
      os.system('rm -rf '+'smooth_'+base+'.ply'); os.system('rm -rf '+base+'-snapsegs.vtk');
      os.system('rm -rf '+base+'-cyclesegs.vtk'); os.system('rm -rf '+base+'-badfcts.vtk');
      os.system('rm -rf '+base+'-goodfcts.vtk');

      os.system('rm -rf '+base+'.ply'); os.system('rm -rf '+base+'.stl');
      os.system('rm -rf '+base+'.vtk'); os.system('rm -rf '+base+'.inp');
      os.system('rm -rf '+base+'-tetgen'); os.system('rm -rf log log2');


      ## 3. make links
      os.system('ln -sf '+sh_dir+'/src/ptcloudgen/ptcloudgen');
      os.system('ln -sf '+sh_dir+'/src/ptcloudgen/resample.py');
      os.system('ln -sf '+sh_dir+'/src/ptcloudgen/smooth.mlx');
      for name in glob.glob(sh_dir+'/src/surfrecon/misc/*'):
         os.system('ln -sf '+name)
      for name in glob.glob(sh_dir+'/src/surfrecon/poisson/*'):
         os.system('ln -sf '+name)
      os.system('ln -sf '+sh_dir+'/src/surfrecon/voronoi/vorrecon.py')
      os.system('ln -sf '+sh_dir+'/src/surfrecon/voronoi/vsitegen/vsitegen')
      os.system('ln -sf '+sh_dir+'/src/surfrecon/voronoi/qvor2vtk/qvor2vtk')


      #print options
      print ''
      print 'Input file: ' + infile

      print 'Steps to perform:'
      if (xval[0] == '1'):
         print '* Point Cloud Generation'
         if (smoothpc == 1):
            print '  - Point cloud smoothing on'

      if (xval[1] == '1'):
         print '* Surface Reconstruction'
         if(surfrecon == 'v'):
            print '  - Surface reconstruction method: Voronoi'
            print '  - Site pair distance: ' + str(sitepairdist) + ' voxels'
         elif(surfrecon == 'p'):
            print '  - Surface reconstruction method: Screened Poisson'
         else:
            sys.exit('ERROR: surface reconstruction method must be \'v\' vor Voronoi or \'p\' for Screneed Poisson')
         print '  - Desired surface points: ' + str(numsurfpoints)

      if (tflag == 1):
         print '* Tetrahedral Volume Meshing'
         if (lflag == 1):
            print '  - Linear tetrahedra'
         else:
            print '  - Quadratic tetrahedra'
         print '  - Tetgen options: -' + topt
         if (kflag == 1):
            print '  - Output VTK file'
         if (bflag == 1):
            print '  - Output Abaqus .inp file'

      ## 4. generate point cloud
      if(xval[0] == '1'):
         if smoothpc == 0:
            os.system('./ptcloudgen ' + infile + " " + str(verbose))
         elif smoothpc == 1:
            os.system('./ptcloudgen ' + infile + " " + str(verbose) + " smooth")


      ## 5. perform surface reconstruction
      if(xval[1] == '1'):
         if (surfrecon == 'v'):
            os.system('python vorrecon.py ' + base + ' ' + str(numsurfpoints) + ' ' + str(sitepairdist) + ' ' + str(verbose) + ' ' + xval[0] + ' ' + str(cspacing) + ' ' + debug)
         else:
            os.system('python poissrecon.py ' + base + ' ' + str(numsurfpoints) + ' ' + str(verbose))


      if (lflag == 0):
         oopt = 'o2'
         orderopt = '-order 2'
      else:
         oopt = ''
         orderopt = '-order 1'


      ## 6. tetgen tetrahedral meshing
      if(tflag == 1):
         print "==============================================================================="
         print
         print 'Tetgen tetrahedral meshing'
         print "-------------------------------------------------------------------------------"
         if (verbose == 1):
            qq = ''
         else:
            qq = 'Q'
         os.system('tetgen -' + qq + topt + oopt + ' ' + base+'.ply')
         os.system('mkdir '+ base+'-tetgen')
         os.system('mv ' + base+'.1.edge ' + base+'.1.ele ' + base+'.1.face ' + base+'.1.node ' + base+'.1.smesh ' + base+'-tetgen')
         sys.stdout.flush()
         print '\rDone. Tetrahedral mesh generated.'


      if (kflag == 1 or bflag == 1):
         os.system('tetgen -' + qq + topt + 'k' + ' ' + base+'.ply' + '> tmplog 2>&1')
         os.system('rm tmplog ' + base+'.1.edge ' + base+'.1.ele ' + base+'.1.face ' + base+'.1.node ' + base+'.1.smesh ')
         os.system('mv ' + base+'.1.vtk ' + base + '.vtk')


      ## 7. VTK file generation
      if (kflag == 1):
         print "==============================================================================="
         print
         print 'Gmsh Output VTK file'
         print "-------------------------------------------------------------------------------"
         os.system('gmsh ' + base+'.vtk -3 -format vtk '+ orderopt+' -o '+base+'.vtk' + log)
         os.system('rm -rf log')
         print 'VTK mesh file generated.'


      ## 8. Abaqus input file generation
      if (bflag == 1):
         print "==============================================================================="
         print
         print 'Gmsh Output Abaqus .inp file'
         print "-------------------------------------------------------------------------------"
         os.system('gmsh ' + base+'.vtk -3 -format inp '+ orderopt+' -o '+base+'.inp' + log)
         os.system('rm -rf log')
         print 'Abaqus input file generated.'
         if (kflag == 0):
            os.system('rm -rf ' + base+'.vtk')


      ## 9. remove intermediate files
      os.system('rm -rf ptcloudgen.py'); os.system('rm -rf connectivity.py');
      os.system('rm -rf qvor2vtk'); os.system('rm -rf poissrecon.mlx');
      os.system('rm -rf vorrecon.py'); os.system('rm -rf poissrecon.py');
      os.system('rm -rf smooth.mlx'); os.system('rm -rf reorient.mlx');
      os.system('rm -rf vsitegen'); os.system('rm -rf ptcloudgen.py');
      os.system('rm -rf ptcloudgen'); os.system('rm -rf smooth.mlx');
      os.system('rm -rf clean.py'); os.system('rm -rf taubin.mlx');
      os.system('rm -rf resample.py');


      if (allfiles == 0):
         os.system('rm -rf '+base+'-resampled.nrrd')
         os.system('rm -rf '+base+'.xyz');
         os.system('rm -rf '+base+'.vor');
         os.system('rm -rf '+base+'-fine.ply');
         os.system('rm -rf '+base+'-pretaub-fine.ply')


      if (ext == '.nrrd'):
         os.system('rm -rf ' + base+'-ptcloud.ply');


      ## 10. close out
      end = time.time()
      elapsed = (end - start)/60.
      print ''
      print "==============================================================================="
      print "MESH GENERATION COMPLETE"
      print "TOTAL ELAPSED TIME:", format(elapsed, '.2f'), "minutes"
      print "==============================================================================="
      print ''
