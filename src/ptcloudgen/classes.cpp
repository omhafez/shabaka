#include <stdio.h>
#include <string.h>
#include <iostream>    // std::cout
#include <iomanip>     // std::setprecision
#include <numeric>     // std::accumulate
#include <algorithm>   // std::min_element
#include <fstream>     // for additional file processing
#include <string>
#include <sstream>
#include <stdio.h>
#include <vector>
#include <stdlib.h>
#include <math.h>
#include <omp.h>
#include <teem/nrrd.h>
#include <nlopt.hpp>

#include "classes.h"
#include "relerrorfunctional.h"

using namespace std;

Mask::~Mask(){
   for (int i = 0; i < nvox; ++i){
        delete [] vox[i];
   }
}

void Mask::readNRRD(char *filename)
{
   char me[]="demoIO", *err;
   Nrrd *nin;
   unsigned char* val;
   int i, j, k;
   int n[3];
   double d[3], o[3];

   name.assign(filename, filename + strlen(filename)-5);

   // RESAMPLE
   string pythoncall = "python resample.py " + name + ".nrrd";
   string resampled = name+"-resampled.nrrd";
   int pcode = system(pythoncall.c_str());

   // CREATE A NRRD
   nin = nrrdNew();

   // READ IN THE NRRD FROM FILE
   if (nrrdLoad(nin, resampled.c_str(), NULL)) {
      err = biffGetDone(NRRD);
      fprintf(stderr, "%s: trouble reading \"%s\":\n%s", me, resampled.c_str(), err);
      free(err);
      return;
   }

   for (int i = 0; i < 3; i++){
      n[i] = nin->axis[i].size;
      d[i] = nin->axis[i].spacing;
      o[i] = nin->axis[i].min;
   }
   nx = n[0]; ny = n[1]; nz = n[2];
   dxp = d[0]; dyp = d[1]; dzp = d[2];
   ox = o[0]; oy = o[1]; oz = o[2];
   nvox = nx*ny*nz;

   dx = 1./nx; dy = dx; dz = dy;
   Lxp = dxp * nx; Lyp = dyp * ny; Lzp = dzp * nz;
   Lx = dx * nx, Ly = dy * ny, Lz = dz * nz;

   val = (unsigned char *)nin->data;
   data.assign(val, val + nvox);

   // CLEAN UP
   nrrdNuke(nin);

// STORE COORDINATES OF ALL VOXELS
   vox = new double*[nvox];
   for (i = 0; i < nvox; i++){
      vox[i] = new double[3];
   }

   for (k = 0; k < nz; k++){
      for (j = 0; j < ny; j++){
         for (i = 0; i < nx; i++){
            vox[i + nx*(j + ny*k)][0] = i * dx;
            vox[i + nx*(j + ny*k)][1] = j * dy;
            vox[i + nx*(j + ny*k)][2] = k * dz;
         }
      }
   }

   return;
}

bool Point::operator< (const Point &other) const{
   return id < other.id;
}

void PtCloud::generate(Mask &mask)
{
// ***********************************************************************
// declare local variables
// ***********************************************************************
   int ijk, ia = 0, ib = 0, nwx, nwy, nwz;
   int i = 0, j = 0, k = 0, ii = 0;
   vector<int>::iterator it, toofar;
   int numpixx, numpixy, numpixz, numpixs;
   vector<int> fixx, fixy, fixz, ones, xl, yl, zl;
   int tempx, tempy, tempz;
   int zloc, yloc, xloc, zlocmin, zlocmax, ylocmin, ylocmax, numthislevel, numthisrow;
   double sampleradc, xcoord, ycoord, zcoord, pixVol, Vt, R;
   const double pi = atan(1)*4.;

   int samplerad = 5;
   double beta0 = 0.5; // weighting parameter for functional minimization
   double beta1 = 0.3; // weighting parameter for functional minimization
   int sampledist[3] = {2, 2, 2}; // distance between windows, in number of pixels
   double keepRatio = 0.925; // maximum allowable percentage of given material in a window to be considered
   double functionalTol = 1e-14; // tolerance for minimization
   int maxiter = 500;

   // threshold values
   double acceptableFunc = 0.15;
   double accD = 0.8;
   double accdiff = 0.55;
// ***********************************************************************

   int numthreads = omp_get_max_threads();
   omp_set_num_threads(numthreads);
   cout.precision(16);

   // PREP VARIABLES FOR LOOPING
   nwx = (mask.nx - 2*samplerad) / sampledist[0] + 1;
   nwy = (mask.ny - 2*samplerad) / sampledist[1] + 1;
   nwz = (mask.nz - 2*samplerad) / sampledist[2] + 1;
   pixVol = mask.dx * mask.dy * mask.dz;

   // DETERMINE WINDOW PIXEL TEMPLATE
   numpixx = 2 * samplerad;
   numpixy = numpixx;
   numpixz = numpixy;

   fixx.assign(numpixx * numpixy * numpixz,0);
   fixy = fixx;
   fixz = fixx;
   numpixs = 0;

   zlocmin = -samplerad;
   zlocmax = samplerad;

   for (zloc = zlocmin; zloc < zlocmax; zloc++){
      zcoord = zloc + 0.5;
      sampleradc = sqrt(pow(samplerad,2) - pow(zcoord,2));
      ylocmin = -round(sampleradc);
      ylocmax = round(sampleradc);
      numthislevel = 0;

      for (yloc = ylocmin; yloc < ylocmax; yloc++){
         ycoord = yloc + 0.5;
         xcoord = sqrt(pow(sampleradc,2) - pow(ycoord,2));
         xloc = round(xcoord);
         numthisrow = 2 * xloc;

         for (i = -xloc; i < xloc; i++){
            xl.push_back(i);
         }
         yl.assign(numthisrow, yloc);
         ia = numpixs + numthislevel;
         ib = ia + numthisrow;

         for (ii = ia, i = 0; ii < ib; ii++, i++){
            fixx[ii] = xl[i];
            fixy[ii] = yl[i];
         }
         xl.clear();
         yl.clear();
         numthislevel = numthislevel + numthisrow;
      }
      ia = numpixs;
      ib = ia + numthislevel;
      for (i = ia; i < ib; i++){
         fixz[i] = zloc;
      }
      zl.clear();
      numpixs = numpixs + numthislevel;
   }

   Vt = numpixs * pixVol;
   R = pow(0.75 / pi * Vt,1./3.);

   // LOOP THROUGH IMAGE AND POPULATE POINT CLOUD
   unsigned long step_size   = 500ul;
   unsigned long total_steps = nwx * nwy * nwz / step_size;
   size_t steps_completed = 0;

   #pragma omp parallel
   {
      size_t local_count = 0;

      #pragma omp for private (ii, i,j,k)
      for(ijk = 0; ijk < nwx*nwy*nwz; ijk++){

         vector<int> winpixels(numpixs), winM1s, winM0s;
         double xT[3];
         double Vp, Ixp, Iyp, Izp, Ixyp, Ixzp, Iyzp, Ixxp, Iyyp, Izzp;
         double fp, gp[3], hp[6];
         double fp2, gp2, hp2;
         double fval; //functional value
         double d; //value of d from interface approximation
         double closest; //distance from closest voxel to center
         double normlen, normal[3], loc[3];
         bool conditions = true;
         int numwinM1s;

         k = ijk / (nwx * nwy);
         j = (ijk / nwx) % nwy;
         i = ijk % nwx;

         xT[0] = samplerad + i * sampledist[0];
         xT[1] = samplerad + j * sampledist[1];
         xT[2] = samplerad + k * sampledist[2];

         for (ii = 0; ii < numpixs; ii++){
            tempx = fixx[ii] + (int)xT[0];
            tempy = fixy[ii] + (int)xT[1];
            tempz = fixz[ii] + (int)xT[2];
            winpixels[ii] = tempx + mask.nx*tempy + mask.nx*mask.ny*tempz;
         }

         // DETERMINE GEOMETRIC PROPERTIES IN WINDOW
         for (auto it = winpixels.begin(), toofar = winpixels.end(); it != toofar; ++it){
            if (mask.data[*it] == 1){
               winM1s.push_back(*it);
            }
            else{
               winM0s.push_back(*it);
            }
         }
         numwinM1s = (int)winM1s.size();
         Vp = numwinM1s * pixVol;

         if (Vp > (1. - keepRatio) * Vt && Vp < keepRatio * Vt){
            Point point;
            double voxx[numwinM1s][3];
            double dist[numwinM1s];

            for (ii = 0; ii < 3; ii++){
               xT[ii] = (xT[ii] - 0.5) * mask.dx;
            }

            for (ii = 0; ii < numwinM1s; ii++){
               voxx[ii][0] = mask.vox[winM1s[ii]][0] - xT[0];
               voxx[ii][1] = mask.vox[winM1s[ii]][1] - xT[1];
               voxx[ii][2] = mask.vox[winM1s[ii]][2] - xT[2];
            }

            // COMPUTE FIRST MOMENTS OF VOLUME, PRODUCTS OF VOLUME, AND SECOND MOMENTS
            // OF VOLUME ABOUT CENTER OF SPHERE
            Ixp = Iyp = Izp = 0.;
            Ixyp = Ixzp = Iyzp = 0.;
            Ixxp = Iyyp = Izzp = (double)winM1s.size() * 1./6. * pow(mask.dx,2);
            for (ii = 0; ii < numwinM1s; ii++){
               Ixp += voxx[ii][0];
               Iyp += voxx[ii][1];
               Izp += voxx[ii][2];

               Ixyp += voxx[ii][0] * voxx[ii][1];
               Ixzp += voxx[ii][0] * voxx[ii][2];
               Iyzp += voxx[ii][1] * voxx[ii][2];

               Ixxp += pow(voxx[ii][1],2) + pow(voxx[ii][2],2);
               Iyyp += pow(voxx[ii][0],2) + pow(voxx[ii][2],2);
               Izzp += pow(voxx[ii][0],2) + pow(voxx[ii][1],2);
            }
            Ixp = pixVol * Ixp;    Iyp = pixVol * Iyp;    Izp = pixVol * Izp;
            Ixyp = pixVol * Ixyp;  Ixzp = pixVol * Ixzp;  Iyzp = pixVol * Iyzp;
            Ixxp = pixVol * Ixxp;  Iyyp = pixVol * Iyyp;  Izzp = pixVol * Izzp;

            // COMPUTE FUNCTIONAL QUANTITIES
            fp = Vp;
            gp[0] = Ixp; gp[1] = Iyp; gp[2] = Izp;

            hp[0] = Ixxp;   hp[1] =  Iyyp;  hp[2] =  Izzp;
                hp[3] = -Iyzp;  hp[4] = -Ixzp;  hp[5] = -Ixyp;

            fp2 = pow(fp,2);
            gp2 = pow(gp[0],2) + pow(gp[1],2) + pow(gp[2],2);
            hp2 = pow(hp[0],2) + pow(hp[1],2) + pow(hp[2],2) +
                      2.*(pow(hp[3],2) + pow(hp[4],2) + pow(hp[5],2));

            // COMPUTE CLOSEST VOXEL
            for (ii = 0; ii < numwinM1s; ii++){
               dist[ii] = sqrt(pow(voxx[ii][0],2) + pow(voxx[ii][1],2) + pow(voxx[ii][2],2));
            }
            closest = *min_element(dist, dist + numwinM1s);

            // MINIMIZE RELATIVE ERROR FUNCTIONAL TO DETERMINE POINT AND NORMAL
            vector<double> u(3);
            double d0, psi0, theta0, rp;
            double psi, theta;
            double func_data[16] = {beta0, beta1, R, fp,
                                        gp[0], gp[1], gp[2],
                                        hp[0], hp[1], hp[2],
                                        hp[3], hp[4], hp[5],
                                        fp2, gp2, hp2};

            // USE SUBPLEX METHOD
            nlopt::opt opt(nlopt::LN_SBPLX, 3);
            opt.set_min_objective(relerrorfunctional, func_data);
            opt.set_xtol_rel(functionalTol);
            opt.set_ftol_rel(functionalTol);
            opt.set_maxeval(maxiter);

            // GUESS
            d0 = closest;
            psi0 = atan2(Iyp,Ixp);
            rp = sqrt(pow(Ixp,2) + pow(Iyp,2));
            theta0 = -atan2(Izp,rp);

            u[0] = d0;
            u[1] = psi0;
            u[2] = theta0;

            // SOLVE
            nlopt::result result = opt.optimize(u, fval);
            if (result < 0) { cerr << "nlopt failed!" << endl; }

            d = u[0];
            psi = u[1];
            theta = u[2];

            // POPULATE POINT CLOUD
            normal[0] = -cos(psi)*cos(theta);
            normal[1] = -sin(psi)*cos(theta);
            normal[2] = sin(theta);

            conditions = Ixp*normal[0] + Iyp*normal[1] + Izp*normal[2] < 0 &&
                             !std::isnan(fval) && !std::isinf(fval) &&
                             fval < acceptableFunc &&
                             fabs(d)/mask.dx < accD*samplerad &&
                             fabs(fabs(d) - fabs(closest))/mask.dx < accdiff*samplerad;

            // IF POINT IS ACCEPTABLE, ADD IT TO THE VECTOR OF ORIENTED POINTS
            if(conditions){
               loc[0] = xT[0] - d * normal[0];
               loc[1] = xT[1] - d * normal[1];
               loc[2] = xT[2] - d * normal[2];

               d = d/mask.dx;
               closest = closest/mask.dx;

               point.loc[0] = loc[0] / mask.dx * mask.dxp + mask.ox;
               point.loc[1] = loc[1] / mask.dy * mask.dyp + mask.oy;
               point.loc[2] = loc[2] / mask.dz * mask.dzp + mask.oz;

               point.normal[0] = normal[0]/mask.dx*mask.dxp;
               point.normal[1] = normal[1]/mask.dy*mask.dyp;
               point.normal[2] = normal[2]/mask.dz*mask.dzp;

               normlen = sqrt(pow(point.normal[0],2) + pow(point.normal[1],2) + pow(point.normal[2],2));

               point.normal[0] = point.normal[0] / normlen;
               point.normal[1] = point.normal[1] / normlen;
               point.normal[2] = point.normal[2] / normlen;

               point.id = ijk;

               #pragma omp critical
               data.push_back(point);
            }
         }

         if (local_count++ % step_size == step_size - 1){
            #pragma omp atomic
            ++steps_completed;
            if (steps_completed % 100 == 0){
               #pragma omp critical
               cout << "\rProgress: " << fixed << setprecision(1) << (100.0*steps_completed/total_steps) << "%" << std::flush;
            }
         }
      }
   }
   cout << "\rProgress: 100% " << endl;
   cout << "Done. Points Generated: " << data.size() << endl;
}

void PtCloud::sort(){
    vector<Point> copy = data;
    std::sort(copy.begin(), copy.end());
    data.clear();
    data = copy;
}

void PtCloud::writePLY(Mask &mask, bool smooth, int verbose)
{
   //  EXPORT POINT CLOUD TO .PLY
   ofstream fileID; // open a file in write mode

   fileID.open(mask.name+"-ptcloud.ply");

   fileID << "ply" << endl;
   fileID << "format binary_little_endian 1.0" << endl;
   fileID << "comment Shabaka generated" << endl;
   fileID << "element vertex " << data.size() << endl;

   fileID << "property double x" << endl; fileID << "property double y" << endl;
   fileID << "property double z" << endl; fileID << "property double nx" << endl;
   fileID << "property double ny" << endl; fileID << "property double nz" << endl;
   fileID << "element face 0" << endl; fileID << "property list uchar int vertex_indices" << endl;
   fileID << "end_header" << endl;
   fileID.close();

   fileID.open(mask.name+"-ptcloud.ply", ios_base::in | ios_base::app | ios_base::binary);

   this->sort();
   for (unsigned int i = 0; i < data.size(); i++){

      fileID.write(reinterpret_cast<const char *>(&data[i].loc[0]), sizeof(double));
      fileID.write(reinterpret_cast<const char *>(&data[i].loc[1]), sizeof(double));
      fileID.write(reinterpret_cast<const char *>(&data[i].loc[2]), sizeof(double));
      fileID.write(reinterpret_cast<const char *>(&data[i].normal[0]), sizeof(double));
      fileID.write(reinterpret_cast<const char *>(&data[i].normal[1]), sizeof(double));
      fileID.write(reinterpret_cast<const char *>(&data[i].normal[2]), sizeof(double));
   }
   fileID.close();

   if(smooth){
      string log2;
      if (verbose == 1){ log2 = " > log 2>&1"; }
      else{ log2 = " > log 2>&1"; }

      if (verbose == 1){
         cout << endl;
         cout << "-------------------------------------------------------------------------------" << endl;
      }
      cout << "Smooth point cloud" << endl;
      string meshlabserver(getenv("mshlbsrvr"));
      string command = meshlabserver + " -i "+mask.name+"-ptcloud.ply -o "+mask.name+"-ptcloud.ply -m vn -s smooth.mlx" + log2;
      if (verbose == 1){
         string callstr = "CALL: "+command;
         cout << callstr << endl;
      }
      int code = system(command.c_str());

        if (code == -1){
            cerr << "error" << endl;
            exit(1);
        }
      if (verbose == 1){
         cout << endl;
      }
   }
}
