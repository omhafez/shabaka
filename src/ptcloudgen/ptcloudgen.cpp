// -----------------------------------------------------------------------------
// 
// Shabaka is the proprietary property of The Regents of the University of California.
//
// Copyright (C) 2017 The Regents of the University of California, Davis campus. All Rights Reserved.
//
// This software may be patent pending.
//
// The software program and documentation are supplied "as is", without any accompanying services from The Regents, for purposes of confidential discussions only. The Regents does not warrant that the operation of the program will be uninterrupted or error-free. The end-user understands that the program was developed for research purposes and is advised not to rely exclusively on the program for any reason.
//
// IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. THE REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE REGENTS HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
// 
// -----------------------------------------------------------------------------

#include <iostream>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include "classes.h"
#include "relerrorfunctional.h"

using namespace std;

int main(int argc, char **argv) 
{   
// ***********************************************************************
// DECLARE LOCAL VARIABLES
// ***********************************************************************
   Mask mask;
   PtCloud ptcloud;
   bool smooth = false;
   int verbose = 0;
// ***********************************************************************
   
   // DECIPHER COMMAND LINE ARGUMENTS
   if(argc < 2){
      cerr << "ERROR" << endl;
      cerr << "Please provide a NRRD file" << endl;
      cerr << "For example: ./ptcloudgen sphere.nrrd" << endl;
      exit(1);
   }
   
   verbose = atoi(argv[2]);
   
   if(argc == 4){
      if(strcmp(argv[3],"smooth") == 0){
         smooth = true;
      }
   }
   
   time_t start;
   time(&start);
   cout << endl;
   printf("===============================================================================\n\n");
   printf("Point Cloud Generation\n");
   
   // READ IMAGE
   mask.readNRRD(argv[1]);
   
   // GENERATE POINT CLOUD
   ptcloud.generate(mask);
   
   // EXPORT TO STANFORD PLY FORMAT
   ptcloud.writePLY(mask, smooth, verbose);
   
   time_t end;
   time(&end);
   double elapsed = (double) difftime(end,start)/60.;
   printf("-------------------------------------------------------------------------------\n");
   printf("Point cloud generation complete\n");
   printf("Elapsed time: %.2f minutes\n", elapsed);

   return 0;
}
