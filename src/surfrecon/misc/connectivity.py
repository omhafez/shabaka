try: paraview.simple
except: from paraview.simple import *
from paraview import *

import sys

print 'Input file:', str(sys.argv[1])
print 'Output file:', str(sys.argv[2])

inputfile = simple.OpenDataFile(str(sys.argv[1]))

Connectivity1 = Connectivity()
Connectivity1.UpdatePipeline()

di=Connectivity1.GetDataInformation()
pdi=di.GetPointDataInformation()

numRegions = int(max(pdi.GetArrayInformation(0).GetComponentRange(-1))) + 1
Threshold1 = Threshold()
volumes = [0 for y in range(numRegions)]

print "There are ", numRegions, " connected regions in the surface mesh.\n"
for x in range(0, numRegions):
   Threshold1.ThresholdRange = [x,x]
   Threshold1.UpdatePipeline()
   bb=Threshold1.GetDataInformation().GetBounds()
   print "Region %d: " % (x)
   print "bounding box size: ", (bb[1]-bb[0]), "x", (bb[3]-bb[2]), "x", (bb[5]-bb[4])
   volumes[x] = (bb[1] - bb[0])*(bb[3]-bb[2])*(bb[5]-bb[4])
   print "bounding box volume: ", volumes[x], "\n"


max_value = max(volumes)
max_index = volumes.index(max_value)
indices = [i for i in range(len(volumes)) if volumes[i]/max_value > 0.01]
for i in indices:
   print "Region", i, "retained"

SetActiveSource(Connectivity1)
MyThresh = Threshold()
MyThresh.ThresholdRange = [indices[0],indices[0]]

for i in indices[1:]:
   SetActiveSource(Connectivity1)
   Thold = Threshold()
   Thold.ThresholdRange = [i,i]
   MyThresh = AppendDatasets( Input=[ MyThresh, Thold] )   

if numRegions < 2:
   print "No disconnected regions"
else:
   print numRegions-len(indices), "remaining regions removed"


ExtractSurface1 = ExtractSurface()

writer = simple.CreateWriter(str(sys.argv[2]), ExtractSurface1)
writer.FileType = 'Ascii'
writer.UpdatePipeline()
