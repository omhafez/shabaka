#ifndef CLASSES_H
#define CLASSES_H

#include <vector>
#include <string>

using namespace std;

class PtCloud;

class Mask
{
friend class PtCloud;
private:
   string name; //image mask name
   int nx, ny, nz; //number of voxels in each directions
   double ox, oy, oz; //origin of image
   int nvox; //total number of voxels
   double dxp, dyp, dzp; //actual (physical) spacing of image
   double dx, dy, dz; //rescaled spacing of image
   double Lxp, Lyp, Lzp; //actual (physical) dimensions of image   
   double Lx, Ly, Lz; //rescaled dimensions of image
   vector<unsigned char> data; //mask data
   double **vox; //voxel coordinates

public:
   ~Mask(); //destructor
   void readNRRD(char *filename); //read NRRD file
};

class Point
{
friend class PtCloud;
private:
   double loc[3]; //point locations
   double normal[3]; //normal directions
   int id; //point ID
public:
   bool operator< (const Point &other) const; //overloaded operator for sorting
};

class PtCloud
{
private:
   vector<Point> data; //vector containing oriented point cloud

public:
   void generate(Mask &mask); //generate point cloud
   void writePLY(Mask &mask, bool smooth, int verbose); //export to Stanford PLY format
   void sort(); //sort the point cloud
};

#endif
