import os, sys, time, subprocess, glob, commands, platform
from subprocess import call, check_output

# FUNCTIONALITIES
# TEST #        1     2     3     4     5     6     7    8 (ptcloud)   9 (multobjs)   10 (pmeshgen)
verbose =  [' -v',' -v',' -v',' -v',' -v',' -v',' -v']
tetgen =   [   '',   '',   '',' -t',' -t',' -t',' -t']
vtk =      [   '',   '',   '',   '',' -k',   '',' -k']
abaqus =   [   '',   '',   '',' -b',' -b',' -b',' -b']
linear =   [   '',   '',   '',   '',   '',' -l',' -l']
allfiles = [' -a',' -a',' -a',   '',   '',   '',   '']
smooth =   [   '',' -s',   '',   '',   '',   '',   '']
poiss =    [   '',   '',' -p',   '',   '',   '',   '']

numtests = len(verbose)

shabaka_dir = os.environ["shabaka_dir"]
shbk = os.environ.get('shbk')
pmshgn = os.environ.get('pmshgn')

# SET UP 
start = time.time()
os.system('make -C '+shabaka_dir+'/src/ptcloudgen/')
os.system('make -C '+shabaka_dir+'/src/surfrecon/voronoi/vsitegen/')
os.system('make -C '+shabaka_dir+'/src/surfrecon/voronoi/qvor2vtk/')
os.system('make -C '+shabaka_dir+'/src/pmeshgen/ply2smfe/')
print
print "==============================================================================="
print


for i in range(0, numtests+3):
   os.system('rm -rf '+ shabaka_dir+'/reslt/tests/'+str(i+1))

for i in range(0, numtests):
   os.system('mkdir '+ shabaka_dir+'/reslt/tests/'+str(i+1))
   os.chdir(''+shabaka_dir+'/reslt/tests/'+str(i+1))
   os.system('rm -rf *')
   os.system('ln -sf ../sphere.nrrd')

   opts = verbose[i] + tetgen[i] + vtk[i] + abaqus[i] + linear[i] + allfiles[i] + smooth[i] + poiss[i]   
   print "TEST "+str(i+1)+": " + 'shabaka sphere.nrrd' + opts
   print os.getcwd()
   command = shbk+' sphere.nrrd'+ ' ' + opts
   os.system(command)
   os.chdir(''+shabaka_dir+'/reslt/tests/')


# test starting with point cloud as input
os.system('mkdir '+ shabaka_dir+'/reslt/tests/'+str(numtests+1))
os.chdir(''+shabaka_dir+'/reslt/tests/'+str(numtests+1))
os.system('rm -rf *')
os.system('ln -sf ../sphere-ptcloud.ply')
   
print 'TEST '+str(numtests+1)+': shabaka sphere-ptcloud.ply -c0.390625 -v -a'
print os.getcwd()
command = shbk+' sphere-ptcloud.ply -c0.390625 -v -a'
os.system(command)
os.chdir(''+shabaka_dir+'/reslt/tests/')


# test out meshing multiple objects
os.system('mkdir '+ shabaka_dir+'/reslt/tests/'+str(numtests+2))
os.chdir(''+shabaka_dir+'/reslt/tests/'+str(numtests+2))
os.system('rm -rf *')
os.system('ln -sf ../../examples/multobjs/multobjs.nrrd')

opts = ' -v -t -k -b -l -a' 
print "TEST "+str(numtests+2)+": " + 'shabaka multobjs.nrrd' + opts
print os.getcwd()
command = shbk+' multobjs.nrrd'+ ' ' + opts
os.system(command)
os.chdir(''+shabaka_dir+'/reslt/tests/')


# test pmeshgen if on native Ubuntu
if ("Darwin" not in platform.system()):
   status, output = commands.getstatusoutput("grep -q Microsoft /proc/version")
   if (status != 0):
      status, output = commands.getstatusoutput("which celeris")
      if ("celeris" in output):
         os.system('mkdir '+ shabaka_dir+'/reslt/tests/'+str(numtests+3))
         os.chdir(''+shabaka_dir+'/reslt/tests/'+str(numtests+3))
         os.system('rm -rf *')
         os.system('ln -sf ../1/sphere.ply')
         
         print 'TEST '+str(numtests+3)+': pmeshgen sphere.ply -v'
         os.system(pmshgn+' sphere.ply -v')  
   

end = time.time()
elapsed = (end - start)/60.
print "==============================================================================="
print "TEST SUITE COMPLETE"
print "TOTAL ELAPSED TIME:", format(elapsed, '.2f'), "minutes"
print "==============================================================================="
