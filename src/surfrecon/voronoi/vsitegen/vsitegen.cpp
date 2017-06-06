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
#include <algorithm>
#include <iomanip>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <string.h>
#include <string>
#include <vector>
#include <teem/nrrd.h>
#include <omp.h>

using namespace std;

int main(int argc, char **argv) 
{
   // DECIPHER COMMAND LINE ARGUMENTS
   if(argc < 2){
      cerr << "ERROR" << endl;
      cerr << "Please provide a base file name" << endl;
      cerr << "For example: ./vsitegen sphere 1.5" << endl;
      exit(1);
   }
   if(argc < 3){
      cerr << "ERROR" << endl;
      cerr << "Please provide a Voronoi site distance" << endl;
      cerr << "For example: ./vsitegen sphere 1.5" << endl;
      exit(1);
   }
   
   string base = argv[1];
   string name;
   double dxp;
   if (argc == 4){
      dxp = atof(argv[3]);
   }
   else{
      // READ IMAGE
      char me[]="demoIO", *err;
      Nrrd *nin;
      double d[3];
      name = base + ".nrrd";

      nin = nrrdNew();
      if (nrrdLoad(nin, name.c_str(), NULL)) {
         err = biffGetDone(NRRD);
         fprintf(stderr, "%s: trouble reading \"%s\":\n%s", me, argv[1], err);
         free(err);
         exit(1);
      }
      for (int i = 0; i < 3; i++){
         d[i] = nin->axis[i].spacing;
      }
      dxp = min(min(d[0],d[1]),d[2]);
      nrrdNuke(nin);
   }
   
   // READ POINT CLOUD
   string dummy, type;
   int numpoints;
   bool ascii;
   ifstream fin;
   name = base+"-ptcloud.ply";
   fin.open(name.c_str());
   
   getline(fin, dummy);
   fin >> dummy >> type >> dummy >> dummy;
   if (type == "ascii"){
      ascii = true;
   } else {
      ascii = false;
   }
   getline(fin, dummy);
   fin >> dummy >> dummy >> numpoints;
   fin >> dummy >> type >> dummy >> dummy;
   for (int i = 0; i < 8; i++){
      getline(fin, dummy);
   }
   long here = fin.tellg();
   
   if(ascii == true){
      vector<double> loc(numpoints*3);
      vector<double> normal(numpoints*3);
   
      for (int i = 0; i < numpoints; i++){
         fin >> loc[3*i + 0] >> loc[3*i + 1] >> loc[3*i + 2] >> normal[3*i + 0] >> normal[3*i + 1] >> normal[3*i + 2];
      }    
    
      // EXPORT VORONOI SITES
      double sitepairdist = atof(argv[2]) * dxp;
      int numsites = 2 * numpoints;
      vector<double> sites(numsites*3);
      
      printf("Number of Voronoi sites: %d\n", numsites);

      for (int i = 0; i < numpoints; i++){
         sites[3*i + 0] = loc[3*i + 0] - sitepairdist * normal[3*i + 0];
         sites[3*i + 1] = loc[3*i + 1] - sitepairdist * normal[3*i + 1];
         sites[3*i + 2] = loc[3*i + 2] - sitepairdist * normal[3*i + 2];
      }
      for (int i = 0; i < numpoints; i++){
         sites[3*(numpoints + i) + 0] = loc[3*i + 0] + sitepairdist * normal[3*i + 0];
         sites[3*(numpoints + i) + 1] = loc[3*i + 1] + sitepairdist * normal[3*i + 1];
         sites[3*(numpoints + i) + 2] = loc[3*i + 2] + sitepairdist * normal[3*i + 2];
      }
      
      ofstream fout; // open a file in write mode
      name = base+".xyz";
      fout.open(name.c_str()); 
      fout << "3 " << numsites << endl;
      
      for (int i = 0; i < numsites; i++){
         fout << setprecision(10) << scientific;
         fout << sites[3*i + 0] << " " << sites[3*i + 1] << " " << sites[3*i + 2] << endl;
      }    
      fout.close();
   }
   
   else{
      fin.close();
      fin.open(name.c_str(), ios_base::binary);
      fin.seekg(here, fin.beg);
      
      if (type == "double"){
         vector<double> loc(numpoints*3);
         vector<double> normal(numpoints*3);
      
         for (int i = 0; i < numpoints; i++){
            fin.read((char*)&loc[3*i + 0], sizeof(double));
            fin.read((char*)&loc[3*i + 1], sizeof(double));
            fin.read((char*)&loc[3*i + 2], sizeof(double));
            fin.read((char*)&normal[3*i + 0], sizeof(double));
            fin.read((char*)&normal[3*i + 1], sizeof(double));
            fin.read((char*)&normal[3*i + 2], sizeof(double));
         }
         
         // EXPORT VORONOI SITES
         double sitepairdist = atof(argv[2]) * dxp;
         int numsites = 2 * numpoints;
         vector<double> sites(numsites*3);
         
         printf("Number of Voronoi sites: %d\n", numsites);

         for (int i = 0; i < numpoints; i++){
            sites[3*i + 0] = loc[3*i + 0] - sitepairdist * normal[3*i + 0];
            sites[3*i + 1] = loc[3*i + 1] - sitepairdist * normal[3*i + 1];
            sites[3*i + 2] = loc[3*i + 2] - sitepairdist * normal[3*i + 2];
         }
         for (int i = 0; i < numpoints; i++){
            sites[3*(numpoints + i) + 0] = loc[3*i + 0] + sitepairdist * normal[3*i + 0];
            sites[3*(numpoints + i) + 1] = loc[3*i + 1] + sitepairdist * normal[3*i + 1];
            sites[3*(numpoints + i) + 2] = loc[3*i + 2] + sitepairdist * normal[3*i + 2];
         }
         
         ofstream fout; // open a file in write mode
         name = base+".xyz";
         fout.open(name.c_str()); 
         fout << "3 " << numsites << endl;
         
         for (int i = 0; i < numsites; i++){
            fout << setprecision(10) << scientific;
            fout << sites[3*i + 0] << " " << sites[3*i + 1] << " " << sites[3*i + 2] << endl;
         }         
         fout.close();
      }
      else if(type == "float"){
         vector<float> loc(numpoints*3);
         vector<float> normal(numpoints*3);
      
         for (int i = 0; i < numpoints; i++){
            fin.read((char*)&loc[3*i + 0], sizeof(float));
            fin.read((char*)&loc[3*i + 1], sizeof(float));
            fin.read((char*)&loc[3*i + 2], sizeof(float));
            fin.read((char*)&normal[3*i + 0], sizeof(float));
            fin.read((char*)&normal[3*i + 1], sizeof(float));
            fin.read((char*)&normal[3*i + 2], sizeof(float));
         }
         
         // EXPORT VORONOI SITES
         double sitepairdist = atof(argv[2]) * dxp;
         int numsites = 2 * numpoints;
         vector<double> sites(numsites*3);
         
         printf("Number of Voronoi sites: %d\n", numsites);

         for (int i = 0; i < numpoints; i++){
            sites[3*i + 0] = loc[3*i + 0] - sitepairdist * normal[3*i + 0];
            sites[3*i + 1] = loc[3*i + 1] - sitepairdist * normal[3*i + 1];
            sites[3*i + 2] = loc[3*i + 2] - sitepairdist * normal[3*i + 2];
         }
         for (int i = 0; i < numpoints; i++){
            sites[3*(numpoints + i) + 0] = loc[3*i + 0] + sitepairdist * normal[3*i + 0];
            sites[3*(numpoints + i) + 1] = loc[3*i + 1] + sitepairdist * normal[3*i + 1];
            sites[3*(numpoints + i) + 2] = loc[3*i + 2] + sitepairdist * normal[3*i + 2];
         }
         
         ofstream fout; // open a file in write mode
         name = base+".xyz";
         fout.open(name.c_str()); 
         fout << "3 " << numsites << endl;
         
         for (int i = 0; i < numsites; i++){
            fout << setprecision(10) << scientific;
            fout << sites[3*i + 0] << " " << sites[3*i + 1] << " " << sites[3*i + 2] << endl;
         }
         fout.close();
      }
      else{
         cerr << "unrecognized float type in PLY" << endl;
         cerr << "aborting" << endl;
         exit(1);
      }
   }

   fin.close();

   return 0;
}
