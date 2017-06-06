try: paraview.simple
except: from paraview.simple import *
from paraview import *

import sys

print 'Input file:', str(sys.argv[1])
print 'Output file:', str(sys.argv[2])

inputfile = simple.OpenDataFile(str(sys.argv[1]))

Clean1 = Clean()
Clean1.PointMerging = 0
Clean1.UpdatePipeline()

writer = simple.CreateWriter(str(sys.argv[2]), Clean1)
writer.FileType = 'Binary'
writer.UpdatePipeline()
