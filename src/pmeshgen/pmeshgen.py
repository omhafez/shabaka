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
    os.system('cat '+sh_dir+'/doc/pmeshgen-usage')

elif(len(sys.argv) == 2 and str(sys.argv[1]) == '--help'):
   os.system('cat '+sh_dir+'/doc/pmeshgen-usage')

else:
   print "==============================================================================="   
   start = time.time()
   infile = str(sys.argv[1])
   base = infile[0:len(infile)-4]
   
   hexmeshspacing = '2.0'
   tol = '0.0000001'
   verbose = 0
   
   hflag = 0; tflag = 0
   
   ArgumentsIndex = 2
   while (ArgumentsIndex < len(sys.argv)):
      key = sys.argv[ArgumentsIndex]
      val = key[2:len(key)]
      key = key[0:2]
      
      if (key == '-s'):
         if (hflag == 0):
            hexmeshspacing = str(val)
            hflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')
      if (key == '-t'):
         if (tflag == 0):
            tol = str(val)
            tflag = 1
         else:
            sys.exit('ERROR: duplicate specification of input parameter!')      
      if (key == '-v'):
         verbose = 1
      ArgumentsIndex += 1
         
         
   if (verbose == 1):
      log = ''
   else:
      log = " > log 2>&1"
      
         
   sfile = "import-"+base+"-smfe.csf"

   #make links
   os.system('ln -sf '+sh_dir+'/src/pmeshgen/import-smfe.csf')
   os.system('ln -sf '+sh_dir+'/src/pmeshgen/ply2smfe/ply2smfe')
   call(["rm", "-rf", base+".m", base+"-celeris"])
   
   #print options
   print
   print 'Input surface mesh: ' + base+".ply"
   print "Output Celeris database:", base+"-celeris"
   print 'Hex mesh spacing: ' + str(hexmeshspacing) + ' (in physical units, e.g. mm)'
   print 'Tolerance: ' + str(tol)
   print ''

   print "==============================================================================="

   callstr = "CALL: "
   
   #convert from PLY to SMFE
   print "Convert PLY to SMFE"
   strcall = "./ply2smfe "+base+".ply"
   print callstr + strcall
   call(["./ply2smfe", base+".ply"]);
   print

   print "-------------------------------------------------------------------------------"
   #Generate Celeris script file
   print "Generate Celeris script file"

   #copy import-smfe.csf file
   call(["cp","import-smfe.csf",sfile])

   #update new Celeris script file
   strcall = "sed -i '/IMPORT SMFE/c\\IMPORT SMFE "+ base.upper()+"_SMFE FROM \"./" + base+".m" + "\" " + tol + " RGN_1' " + sfile
   #print strcall
   os.system(strcall)
   
   strcall = "sed -i '/BOUNDARY/c\\" + base.upper() + "_BREP = BOUNDARY OF " + base.upper()+"_SMFE' " + sfile
   #print strcall
   os.system(strcall)

   strcall = "sed -i '/CREATE HEX_MESH/c\\CREATE HEX_MESH " + base.upper()+"_HEX ["+hexmeshspacing+","+hexmeshspacing+","+hexmeshspacing+"], " + tol + ", " + base.upper() + "_BREP' " + sfile
   #print strcall
   os.system(strcall)
   
   strcall = "sed -i '/MESH_BREP SCULPT/c\\" + base.upper() + "_MESH = " + base.upper()+"_BREP SCULPT " + base.upper() + "_HEX' " + sfile
   #print strcall
   os.system(strcall)
   
   strcall = "sed -i '/PHYS_MESH/c\\CREATE PHYS_MESH " + base.upper() + "_PHYS " + base.upper()+"_MESH' " + sfile
   #print strcall
   os.system(strcall)

   strcall = "sed -i '/OPEN NEW CELERIS_DB/c\\OPEN NEW CELERIS_DB " + base.upper()+"_CELERIS " + "\"./"+base+"_celeris/"+base+".cdb\" OVERWRITE' " + sfile
   #print strcall
   os.system(strcall)

   strcall = "sed -i '/SAVE/c\\SAVE {" + base.upper() + "_HEX, " + base.upper() + "_BREP, " + base.upper() + "_MESH, " + base.upper() + "_PHYS} TO " + base.upper() + "_CELERIS' " + sfile
   #print strcall
   os.system(strcall)

   strcall = "sed -i '/CLOSE/c\\CLOSE CELERIS_DB " + base.upper()+"_CELERIS' " + sfile
   #print strcall
   os.system(strcall)
   

   #call Celeris script file
   command = "celeris --batch-mode " + sfile
   print callstr + command + ' (~3 minutes)'
   print '\rworking...',
   sys.stdout.flush()
   os.system(command + log)
   print "\r                    "
   os.system('mv ' + base+'_celeris ' + base+'-celeris')
   
   #remove intermediate files
   os.system('rm -rf ply2smfe');
   os.system('rm -rf import-smfe.csf');
   os.system('rm -rf ' + base + '.m');
   os.system("rm -rf import-"+base+"-smfe.csf");
   call(["rm", "-f", "log"])


   end = time.time()
   elapsed = (end - start)/60.
   print "==============================================================================="
   print "POLYHEDRAL MESH GENERATED"
   print "TOTAL ELAPSED TIME:", format(elapsed, '.2f'), "minutes"
   print "==============================================================================="
   print


