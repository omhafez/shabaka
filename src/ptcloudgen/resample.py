import os, sys, commands, platform
from subprocess import call

def isclose(a, b, rel_tol=1e-09, abs_tol=0.0):
    return abs(a-b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol)

infile = str(sys.argv[1])
sp = infile.split('.');
base = '.'.join(sp[0:len(sp)-1])

infile = base + '.nrrd'
outfile = base + '-resampled.nrrd'

status, output = commands.getstatusoutput('head '+infile+' | grep sizes')
output = output.split()

nx = output[1]
ny = output[2]
nz = output[3]

status, output = commands.getstatusoutput('head '+infile+' | grep spacings')
output = output.split()

dxp = float(output[1])
dyp = float(output[2])
dzp = float(output[3])

d = [dxp, dyp, dzp]
mind = min(d)

print "-------------------------------------------------------------------------------"
print "Image mask: "+infile
print "Image resolution: " + nx  + " x " + ny + " x " + nz
print "Image spacing: " + str(dxp) + " x " + str(dyp) + " x " + str(dzp)
print "-------------------------------------------------------------------------------"

if (isclose(dxp,dyp) and isclose(dyp, dzp)):
   os.system('cp '+infile+' '+outfile)
else:
   sys.stdout.write('Resample mask for isotropic voxel spacing...')
   sys.stdout.flush()
   
   scalars = [x / mind for x in d]

   os.system('unu resample -s x'+str(scalars[0])+' x'+str(scalars[1])+' x'+str(scalars[2])+' -k box -i '+ infile + ' -o '+outfile)
   os.system('unu axinfo -a 0 1 2 -sp '+str(mind)+' -i '+outfile+' -o '+outfile)
   sys.stdout.write('Done.\n')


sys.stdout.write('Pad mask...')
sys.stdout.flush()
os.system('unu pad -b pad -min -10 -10 -10 -max M+10 M+10 M+10 -i '+outfile+' -o '+outfile)
os.system('unu save -f nrrd -e gzip -i '+outfile+' -o '+outfile)

status, output = commands.getstatusoutput('head '+infile+' | grep \"axis mins:\"')
output = output.split()
oxmin = float(output[2])
oymin = float(output[3])
ozmin = float(output[4])

xmin = oxmin - 10.0 * mind
ymin = oymin - 10.0 * mind
zmin = ozmin - 10.0 * mind

newstring = 'axis mins: '
newstring = newstring + str(xmin) + ' ' + str(ymin) + ' ' + str(zmin)
status, linenum = commands.getstatusoutput('head '+outfile+' | grep centerings -n | cut -f1 -d:')

if ("Darwin" in platform.system()):
   command = 'gsed -i\'\' \''+linenum+'i\\'+newstring+'\' '+outfile
else:
   command = 'sed -i\'\' \''+linenum+'i\\'+newstring+'\' '+outfile
os.system(command)

sys.stdout.write('Done.\n')
