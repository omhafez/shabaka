#include <math.h>    // sqrt, atan, etc.

#include "classes.h"
#include "relerrorfunctional.h"

using namespace std;

double relerrorfunctional(unsigned int n, const double *u, double *grad, void *my_func_data)
{
   // EXTRACT FUNCTION DATA
   double *mfd = (double *)my_func_data;
   double beta0 = mfd[0];
   double beta1 = mfd[1];
   double R = mfd[2];
   double fp = mfd[3];
   double gp[3] = {mfd[4], mfd[5], mfd[6]};
   double hp[6] = {mfd[7], mfd[8], mfd[9], mfd[10], mfd[11], mfd[12]};
   double fp2 = mfd[13];
   double gp2 = mfd[14];
   double hp2 = mfd[15];
   
   // DECLARE LOCAL VARIABLES
   const double pi = atan(1)*4.;
   double d = u[0], psi = u[1], theta = u[2];
   double f, V, Ixr, Ixxr, Iyyr, Izzr, cp, sp, ct, st, e0, e1, e2;
   double g[3], h[6], dg[3], dh[6];
   double R2 = pow(R,2), R4 = pow(R,4), R5 = pow(R,5);
   double p2, p4, d2 = pow(d,2), d3 = pow(d,3);
   double func;

   // COMPUTE P
   p2 = R2 - d2;
   p4 = pow(p2,2);

   // COMPUTE ZEROTH, FIRST, AND SECOND MOMENTS IN PRIME COORDINATE SYSTEM
   V = 2.*pi * (-0.5*d*p2 + 1./3.*(pow(R,3) - fabs(d3)));
   Ixxr = pi/30. * (-15.*d*p4 + fabs(d)*(12.*p4 - 4.*p2*R2-8.*R4) + 8.*R5);
   Iyyr = pi/480. * (-160.*d3*p2 + 128.*R5 - 32.*fabs(d3)*(4.*R2 + p2) - 120.*d*p4);
   
   // CONSIDER CASE OF d < 0
   if (d < 0) {
      V = V + 4.*pi/3.*fabs(d3);
      Ixxr = Ixxr + 4.*pi/15.*fabs(d3) * (3.*p2 + 2.*R2);
      Iyyr = Iyyr + 2.*pi/15.*fabs(d3) * (p2 + 4.*R2);
   }
   
   Ixr = pi/4.*p4;
   Izzr = Iyyr;

   // COMPUTE ROTATION QUANTITIES
   cp = cos(psi);
   sp = sin(psi);
   ct = cos(theta);
   st = sin(theta);

   // STORE VOLUME
   f = V;

   // ROTATE FIRST MOMENTS INTO UNPRIMED COORDINATE SYSTEM
   g[0] = cp*ct*Ixr;
   g[1] = sp*ct*Ixr;
   g[2] = -st*Ixr;

   // ROTATE SECOND MOMENTS INTO UNPRIMED COORDINATE SYSTEM
   h[0] = Iyyr*pow(sp,2) + pow(cp,2)*(pow(ct,2)*Ixxr + Izzr*pow(st,2));
   h[1] = (pow(cp,2)*Iyyr + pow(sp,2)*(pow(ct,2)*Ixxr + Izzr*pow(st,2)));  
    h[2] = pow(ct,2)*Izzr + Ixxr*pow(st,2);
    h[3] = ct*(-Ixxr + Izzr)*sp*st;
    h[4] = cp*ct*(-Ixxr + Izzr)*st;
    h[5] = cp*sp*(pow(ct,2)*Ixxr - Iyyr + Izzr*pow(st,2));

   // COMPUTE RELATIVE ERROR FUNCTIONAL
   e0 = sqrt(pow(f - fp,2) / fp2);

   for (int i = 0; i < 3; i++){
      dg[i] = g[i] - gp[i];
   }
   e1 = sqrt((pow(dg[0],2) + pow(dg[1],2) + pow(dg[2],2)) / gp2);

   for (int i = 0; i < 6; i++){
        dh[i] = h[i] - hp[i];
   }
   e2 = sqrt((pow(dh[0],2) + pow(dh[1],2) + pow(dh[2],2)
           + 2.*(pow(dh[3],2) + pow(dh[4],2) + pow(dh[5],2))) / hp2);

   func = beta0*e0 + beta1*e1 + (1. - beta0 - beta1)*e2;
   
   return func;
}
