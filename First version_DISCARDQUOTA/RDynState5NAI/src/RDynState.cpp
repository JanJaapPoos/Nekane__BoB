#include <Rdefines.h>
#include <Rinternals.h>
#include <R.h>

#include <vector>
#include <math.h>
#include <stdlib.h>
#include <time.h>   
#include <algorithm>
#include <numeric>
#include "matalloc.h"
#include <omp.h>

using namespace std;

int             outofbounds_int;
bool            outofbounds_bool;
double          outofbounds_double;

// 160 by 98  works fine
#define SPP1CAPACITY          180    // max number of first choke sp (larger than MAXNOINC * MAXHORIZON *nosizes= 64 * 5 * 2 = 640) 
#define SPP2CAPACITY          180    // max number of second choke sp (MAXNOINC * MAXHORIZON *nosizes= 64* 5* 2 = 640)
#define NOSPEC                  5     // number of species used in the analysis
#define NOSIZES                 2     // number of size classes used in the analysis
#define MAXNOINC               45     // max number of increments
#define MAXHORIZON              4     // number of seasons to fish (= time)

typedef unsigned UFINT;

typedef float         (****FTYPE); /* make pointer called FTYPE */
typedef UFINT         (****ITYPE); /* make pointer called ITYPE optimal choice array*/
typedef float         (*****PTYPE);/* make pointer called PTYPE for information on statistical distribution prey*/
typedef float         (****ATYPE);/* make pointer called ATYPE for information on statistical distribution prey aggregated over sizes*/
typedef float         (***PITYPE); /* make pointer called PITYPE for price information*/


/* random number generator*/
float ranf ()
{
  return ((float)rand()/RAND_MAX); //genereert random getal tussen 0 en 1
}

/* initialise utility function */
float utility (int aLndSpp1,  int aDisSpp1,  int aLndSpp1Quota, int aDisSpp1Quota, float aSpp1LndQuotaFine, float aSpp1DisQuotaFine,
	       int aLndSpp2,  int aDisSpp2,  int aLndSpp2Quota, int aDisSpp2Quota, float aSpp2LndQuotaFine, float aSpp2DisQuotaFine)
{                                                              
  float Fine1 = 0;
  float Fine2 = 0;
  float Fine3 = 0;
  float Fine4 = 0;
  if ( aLndSpp1 >= aLndSpp1Quota)      Fine1  = (aLndSpp1 - aLndSpp1Quota)     * aSpp1LndQuotaFine;
  if ( aLndSpp2 >= aLndSpp2Quota)      Fine2  = (aLndSpp2 - aLndSpp2Quota)     * aSpp2LndQuotaFine;
  if ( aDisSpp1 >= aDisSpp1Quota)      Fine3  = (aDisSpp1 - aDisSpp1Quota)     * aSpp1DisQuotaFine;
  if ( aDisSpp2 >= aDisSpp2Quota)      Fine4  = (aDisSpp2 - aDisSpp2Quota)     * aSpp2DisQuotaFine;
  return ( - (Fine1 + Fine2 + Fine3 + Fine4));
}

/* Calc economic return per choice to include in  */  /* only the landings take part on the short gains  */
float shortTermGains (int aTime, int aNoInc, PTYPE aLndParms, int aPatch, PITYPE aSppPrice)
{      
 float mean0 = 0;
 for (int inc = 0; inc < aNoInc; inc++){
       for (int s = 0; s < NOSPEC; s++){
             for (int si = 0; si < NOSIZES; si++){
                   mean0  += aLndParms[aPatch][aTime][s][si][inc] * inc * aSppPrice[aTime][s][si];     /* NA; only LndParms */ 
        }
     }
  }
  return (mean0) ;
}

float shortTermCosts (int aTime, int aPatch, float aPriceEffort, int anEffortArray[][MAXHORIZON])
{      
 return ((anEffortArray[aPatch][aTime] * aPriceEffort) ) ;
 //aPrice Effort is (fuel use* fuel price) + gear maintenance
}

float FFF (int aLndSpp1,int aLndSpp2, int aDisSpp1,int aDisSpp2,int aNoInc, ATYPE aLndParmsAgg,  ATYPE aDisParmsAgg, int aTime, int aNPatch, double aShortTermEcon[], FTYPE anFF1, ITYPE aIStar)
{      
  if ((aLndSpp1 + aNoInc) >  SPP1CAPACITY) {Rprintf("Fixing capacity issue for state matrice by taking edges (aLndSpp1, NoInc, sppcapacity)=");Rprintf("%d ",aLndSpp1);Rprintf("%d ",aNoInc); Rprintf("%d ",SPP1CAPACITY); R_FlushConsole();}

 double rhs;
 double vMax = -2E120;
 int theLndSpp1Val, theDisSpp1Val,theLndSpp2Val,theDisSpp2Val;
  
  for (int i = 0; i < aNPatch; i++){
    rhs = 0;
    int incL1, incL2, incD1, incD2;
    float xL1,  xL2, xD1, xD2; /* L for landings and D for discards */

    for ( incL1 = 0; incL1 < aNoInc; incL1++) {
      xL1= aLndParmsAgg[i][aTime][0][incL1];
      theLndSpp1Val = min(aLndSpp1 + incL1,SPP1CAPACITY-1);
      for ( incL2 = 0; incL2 <  aNoInc; incL2++) {
	xL2= aLndParmsAgg[i][aTime][1][incL2];
	theLndSpp2Val = min(aLndSpp2 + incL2, SPP2CAPACITY-1);
	for ( incD1 = 0; incD1 < aNoInc; incD1++) {
	  xD1= aDisParmsAgg[i][aTime][0][incD1];
	  theDisSpp1Val = min(aDisSpp1 + incD1,SPP1CAPACITY-1);
	  for ( incD2 = 0; incD2 <  aNoInc; incD2++) {
	    xD2= aDisParmsAgg[i][aTime][1][incD2];
	    theDisSpp2Val = min(aDisSpp2 + incD2,SPP2CAPACITY-1);
	    rhs += xL1 * xL2 *  xD1 * xD2 *anFF1[theLndSpp1Val][theLndSpp2Val][theDisSpp1Val][theDisSpp2Val];                                     
	  }  
	}                                        
      }                                  
    }                            
                                
    rhs = rhs + aShortTermEcon[i] ; /* the mean should come from the economic funtion */ 
    if (rhs > vMax)  {
          vMax = rhs;
          aIStar[aLndSpp1][aLndSpp2][aDisSpp1][aDisSpp2] = i; 	/* Vmax is defined as a very small number */ 
    }								/* so the best choice will be higher than this Vmax */ 
  }								/* rhs is stored in Istar; best choice */ 
  return vMax;
}

/* initialise Simulation experiment (SEXP= S-expressie) */

SEXP SimulateF ( int aSimNumber, int aHorizon, vector< int > maxspp, vector<vector<UFINT > > val, 
                vector<vector< unsigned short > > rep, int aNoInc, vector< float > aSizeSppInc, 
                PTYPE aLndParms, PTYPE aDisParms, int anEffortArray[][MAXHORIZON]){
  SEXP ReturnObject, SimDims2, SimDims3, choice, spp1Rand, spp2Rand, spp3Rand, spp4Rand,spp5Rand, 
    spp1Landings, spp2Landings, spp3Landings, spp4Landings, spp5Landings,spp1LndHold, spp2LndHold,
    spp3LndHold, spp4LndHold, spp5LndHold,  spp1Discards, spp2Discards, spp3Discards, spp4Discards, 
    spp5Discards,spp1DisHold, spp2DisHold, spp3DisHold, spp4DisHold, spp5DisHold,anEffort;

  int noSpec  = NOSPEC;
  int noSizes = NOSIZES;
  
  
  PROTECT(ReturnObject      = NEW_OBJECT(MAKE_CLASS("Sim")));
  PROTECT(SimDims2          = allocVector(INTSXP,3));
  INTEGER(SimDims2)[0]      = 1; 
  INTEGER(SimDims2)[1]      = aSimNumber; 
  INTEGER(SimDims2)[2]      = aHorizon; 
  PROTECT(SimDims3          = allocVector(INTSXP,3));
  INTEGER(SimDims3)[0]      = noSizes; //number of size classes
  INTEGER(SimDims3)[1]      = aSimNumber; 
  INTEGER(SimDims3)[2]      = aHorizon; 

  PROTECT(choice         = allocArray(INTSXP,SimDims2));
  PROTECT(spp1Rand       = allocArray(REALSXP,SimDims3));
  PROTECT(spp2Rand       = allocArray(REALSXP,SimDims3));
  PROTECT(spp3Rand       = allocArray(REALSXP,SimDims3));
  PROTECT(spp4Rand       = allocArray(REALSXP,SimDims3));
  PROTECT(spp5Rand       = allocArray(REALSXP,SimDims3));
  PROTECT(spp1Landings   = allocArray(REALSXP,SimDims3)); //landings by time step
  PROTECT(spp2Landings   = allocArray(REALSXP,SimDims3));
  PROTECT(spp3Landings   = allocArray(REALSXP,SimDims3));
  PROTECT(spp4Landings   = allocArray(REALSXP,SimDims3));
  PROTECT(spp5Landings   = allocArray(REALSXP,SimDims3));
  PROTECT(spp1LndHold    = allocArray(REALSXP,SimDims3)); //cumulative sum of landings
  PROTECT(spp2LndHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp3LndHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp4LndHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp5LndHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp1Discards   = allocArray(REALSXP,SimDims3)); //discards by time step
  PROTECT(spp2Discards   = allocArray(REALSXP,SimDims3));
  PROTECT(spp3Discards   = allocArray(REALSXP,SimDims3));
  PROTECT(spp4Discards   = allocArray(REALSXP,SimDims3));
  PROTECT(spp5Discards   = allocArray(REALSXP,SimDims3));
  PROTECT(spp1DisHold    = allocArray(REALSXP,SimDims3)); //cumulative sum of discards
  PROTECT(spp2DisHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp3DisHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp4DisHold    = allocArray(REALSXP,SimDims3));
  PROTECT(spp5DisHold    = allocArray(REALSXP,SimDims3));
  PROTECT(anEffort       = allocArray(INTSXP,SimDims2));
  
  for (int s= 0; s < aSimNumber; s++)	 { /* go through the x simulations */   
      
      float Q;
      vector <int> aSpp1LndHold  (noSizes,0);
      vector <int> aSpp2LndHold  (noSizes,0);
      vector <int> aSpp3LndHold  (noSizes,0); 
      vector <int> aSpp4LndHold  (noSizes,0);
      vector <int> aSpp5LndHold  (noSizes,0); 
      vector <int> aSpp1DisHold  (noSizes,0);
      vector <int> aSpp2DisHold  (noSizes,0);
      vector <int> aSpp3DisHold  (noSizes,0); 
      vector <int> aSpp4DisHold  (noSizes,0);
      vector <int> aSpp5DisHold  (noSizes,0); 
    
      int Effort = 0;
      int aChoice;
      
      for (int t = 0; t < aHorizon; t++) { 
                  // get best patch from the array
          // get best patch from the array
            int sp1dis=0;
            int sp2dis=0; 
            int sp1lnd=0; 
            int sp2lnd=0;
      
            for(int si=0; si < noSizes; si++) {
                  sp2dis = sp2dis + aSpp2DisHold[si];
                  sp1dis = sp1dis + aSpp1DisHold[si];
                  sp2lnd = sp2lnd + aSpp2LndHold[si];
                  sp1lnd = sp1lnd + aSpp1LndHold[si];    
            }
      
            int choicepos= sp2dis + sp1dis*maxspp[t] + 
            sp2lnd*maxspp[t]*maxspp[t] + sp1lnd*maxspp[t]*maxspp[t]*maxspp[t]; /* creating an array of arrays */
            
          int ii=0;
          int nn=0;
          do { ii = ii + rep[t][nn]; 			/* simplify the array with the rep trick */
            nn++;
          } while ( ii <= choicepos );
          int aChoice =  val[t][nn-1];
          INTEGER(choice)[s+ t*aSimNumber] = aChoice + 1;  /* +1 because choice in c++ starts at 0 */
          INTEGER(anEffort)[s+ t*aSimNumber]= anEffortArray[aChoice][t];
          Effort = Effort + INTEGER(anEffort)[s+ t*aSimNumber]; /*calculate fueluse*/

          /********************************************************************************/
          /* GET CONSEQUENCES OF CHOICE                                                   */
          /********************************************************************************/
          float Lprobl  = 0;
          float Lprobup = 0;
          float Dprobl  = 0;
          float Dprobup = 0;
        
          /* calculate spp1[1] landings and discards in the patch */        
          for (int si = 0; si < noSizes; si++){
               Q = ranf ();	/* set starting ITQ for first sp = random number 0-1 */
               REAL(spp1Rand)[si + s*noSizes + t*aSimNumber*noSizes] = Q;
               Lprobl  = 0;
               Lprobup = aLndParms[aChoice][t][0][si][0];
               Dprobl  = 0;
               Dprobup = aDisParms[aChoice][t][0][si][0];
               if (Q <= Lprobup) REAL(spp1Landings)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
               if (Q <= Dprobup) REAL(spp1Discards)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
               Lprobl = Lprobup;
               Dprobl = Dprobup;
         
               for (int stick = 1; stick < aNoInc; stick++) {
                   Lprobup = aLndParms[aChoice][t][0][si][stick] + Lprobl;
                   Dprobup = aDisParms[aChoice][t][0][si][stick] + Dprobl;
                   if ((Q > Lprobl) && (Q <= Lprobup)){
                      aSpp1LndHold[si] = aSpp1LndHold[si] + stick;
                      REAL(spp1Landings)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[0];
                   }
                   Lprobl = Lprobup;
                   if ((Q > Dprobl) && (Q <= Dprobup)){
                      aSpp1DisHold[si] = aSpp1DisHold[si] + stick;
                      REAL(spp1Discards)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[0];
                   }
                   Dprobl = Dprobup;
               }
          }
      
          /* calculate spp2  catch in the patch */
          for (int si = 0; si < noSizes; si++){
              Q = ranf ();	/* set starting ITQ for first sp = random number 0-1 */
              REAL(spp2Rand)[si + s*noSizes + t*aSimNumber*noSizes] = Q;
              Lprobl  = 0;
              Lprobup = aLndParms[aChoice][t][1][si][0];
              Dprobl  = 0;
              Dprobup = aDisParms[aChoice][t][1][si][0];
              if (Q <= Lprobup) REAL(spp2Landings)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              if (Q <= Dprobup) REAL(spp2Discards)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              Lprobl = Lprobup;
              Dprobl = Dprobup;
            
              for (int stick = 1; stick < aNoInc; stick++) {
                  Lprobup = aLndParms[aChoice][t][1][si][stick] + Lprobl;
                  Dprobup = aDisParms[aChoice][t][1][si][stick] + Dprobl;
                  if ((Q > Lprobl) && (Q <= Lprobup)){
                     aSpp2LndHold[si] = aSpp2LndHold[si] + stick;
                     REAL(spp2Landings)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[1];
                  }
                  Lprobl = Lprobup;
                  if ((Q > Dprobl) && (Q <= Dprobup)){
                     aSpp2DisHold[si] = aSpp2DisHold[si] + stick;
                     REAL(spp2Discards)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[1];
                  }
                  Dprobl = Dprobup;
              }
          }
        
          /* calculate spp3  catch in the patch */
          for (int si = 0; si < noSizes; si++){
              Q = ranf ();	/* set starting ITQ for first sp = random number 0-1 */
              REAL(spp3Rand)[si + s*noSizes + t*aSimNumber*noSizes] = Q;
              Lprobl  = 0;
              Lprobup = aLndParms[aChoice][t][2][si][0];
              Dprobl  = 0;
              Dprobup = aDisParms[aChoice][t][2][si][0];
              if (Q <= Lprobup) REAL(spp3Landings)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              if (Q <= Dprobup) REAL(spp3Discards)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              Lprobl = Lprobup;
              Dprobl = Dprobup;
            
              for (int stick = 1; stick < aNoInc; stick++) {
                  Lprobup = aLndParms[aChoice][t][2][si][stick] + Lprobl;
                  Dprobup = aDisParms[aChoice][t][2][si][stick] + Dprobl;
                  if ((Q > Lprobl) && (Q <= Lprobup)){
                     aSpp3LndHold[si] = aSpp3LndHold[si] + stick;
                     REAL(spp3Landings)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[2];
                  }
                  Lprobl = Lprobup;
                  if ((Q > Dprobl) && (Q <= Dprobup)){
                     aSpp3DisHold[si] = aSpp3DisHold[si] + stick;
                     REAL(spp3Discards)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[2];
                  }
                  Dprobl = Dprobup;
              }
          }
        
          /* calculate spp4  catch in the patch */
          for (int si = 0; si < noSizes; si++){
              Q = ranf ();	/* set starting ITQ for first sp = random number 0-1 */
              REAL(spp4Rand)[si + s*noSizes + t*aSimNumber*noSizes] = Q;
              Lprobl  = 0;
              Lprobup = aLndParms[aChoice][t][3][si][0];
              Dprobl  = 0;
              Dprobup = aDisParms[aChoice][t][3][si][0];
              if (Q <= Lprobup) REAL(spp4Landings)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              if (Q <= Dprobup) REAL(spp4Discards)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              Lprobl = Lprobup;
              Dprobl = Dprobup;
         
              for (int stick = 1; stick < aNoInc; stick++) {
                  Lprobup = aLndParms[aChoice][t][3][si][stick] + Lprobl;
                  Dprobup = aDisParms[aChoice][t][3][si][stick] + Dprobl;
                  if ((Q > Lprobl) && (Q <= Lprobup)){
                     aSpp4LndHold[si] = aSpp4LndHold[si] + stick;
                     REAL(spp4Landings)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[3];
                  }
                  Lprobl = Lprobup;
                  if ((Q > Dprobl) && (Q <= Dprobup)){
                     aSpp4DisHold[si] = aSpp4DisHold[si] + stick;
                     REAL(spp4Discards)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[3];
                  }
                  Dprobl = Dprobup;
              }
          }
        
          /* calculate spp5  catch in the patch */
          for (int si = 0; si < noSizes; si++){
              Q = ranf ();	/* set starting ITQ for first sp = random number 0-1 */
              REAL(spp5Rand)[si + s*noSizes + t*aSimNumber*noSizes] = Q;
              Lprobl  = 0;
              Lprobup = aLndParms[aChoice][t][4][si][0];
              Dprobl  = 0;
              Dprobup = aDisParms[aChoice][t][4][si][0];
              if (Q <= Lprobup) REAL(spp5Landings)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              if (Q <= Dprobup) REAL(spp5Discards)[si + s*noSizes + t*aSimNumber*noSizes] = 0;
              Lprobl = Lprobup;
              Dprobl = Dprobup;
            
              for (int stick = 1; stick < aNoInc; stick++) {
                  Lprobup = aLndParms[aChoice][t][4][si][stick] + Lprobl;
                  Dprobup = aDisParms[aChoice][t][4][si][stick] + Dprobl;
                  if ((Q > Lprobl) && (Q <= Lprobup)){
                     aSpp5LndHold[si] = aSpp5LndHold[si] + stick;
                     REAL(spp5Landings)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[4];
                  }
                  Lprobl = Lprobup;
                  if ((Q > Dprobl) && (Q <= Dprobup)){
                     aSpp5DisHold[si] = aSpp5DisHold[si] + stick;
                     REAL(spp5Discards)[si + s*noSizes + t*aSimNumber*noSizes] = stick * aSizeSppInc[4];
                  }
                  Dprobl = Dprobup;
              }
          }

          for (int si = 0; si < noSizes; si++){   
              REAL(spp1LndHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp1LndHold[si] * aSizeSppInc[0];
              REAL(spp2LndHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp2LndHold[si] * aSizeSppInc[1];
              REAL(spp3LndHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp3LndHold[si] * aSizeSppInc[2];
              REAL(spp4LndHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp4LndHold[si] * aSizeSppInc[3];
              REAL(spp5LndHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp5LndHold[si] * aSizeSppInc[4];
              REAL(spp1DisHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp1DisHold[si] * aSizeSppInc[0];
              REAL(spp2DisHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp2DisHold[si] * aSizeSppInc[1];
              REAL(spp3DisHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp3DisHold[si] * aSizeSppInc[2];
              REAL(spp4DisHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp4DisHold[si] * aSizeSppInc[3];
              REAL(spp5DisHold)[si + s*noSizes + t*aSimNumber*noSizes]  = aSpp5DisHold[si] * aSizeSppInc[4];
          }
      }
  }
  
  SET_SLOT(ReturnObject, install("choice"),  choice);
  SET_SLOT(ReturnObject, install("spp1Rand"),   spp1Rand);
  SET_SLOT(ReturnObject, install("spp2Rand"),   spp2Rand);
  SET_SLOT(ReturnObject, install("spp3Rand"),   spp3Rand);
  SET_SLOT(ReturnObject, install("spp4Rand"),   spp4Rand);
  SET_SLOT(ReturnObject, install("spp5Rand"),   spp5Rand);
  SET_SLOT(ReturnObject, install("spp1Landings"),   spp1Landings);
  SET_SLOT(ReturnObject, install("spp2Landings"),   spp2Landings);
  SET_SLOT(ReturnObject, install("spp3Landings"),   spp3Landings);
  SET_SLOT(ReturnObject, install("spp4Landings"),   spp4Landings);
  SET_SLOT(ReturnObject, install("spp5Landings"),   spp5Landings);
  SET_SLOT(ReturnObject, install("spp1LndHold"),  spp1LndHold);
  SET_SLOT(ReturnObject, install("spp2LndHold"),  spp2LndHold);
  SET_SLOT(ReturnObject, install("spp3LndHold"),  spp3LndHold);
  SET_SLOT(ReturnObject, install("spp4LndHold"),  spp4LndHold);
  SET_SLOT(ReturnObject, install("spp5LndHold"),  spp5LndHold);
  SET_SLOT(ReturnObject, install("spp1Discards"),   spp1Discards);
  SET_SLOT(ReturnObject, install("spp2Discards"),   spp2Discards);
  SET_SLOT(ReturnObject, install("spp3Discards"),   spp3Discards);
  SET_SLOT(ReturnObject, install("spp4Discards"),   spp4Discards);
  SET_SLOT(ReturnObject, install("spp5Discards"),   spp5Discards);
  SET_SLOT(ReturnObject, install("spp1DisHold"),  spp1DisHold);
  SET_SLOT(ReturnObject, install("spp2DisHold"),  spp2DisHold);
  SET_SLOT(ReturnObject, install("spp3DisHold"),  spp3DisHold);
  SET_SLOT(ReturnObject, install("spp4DisHold"),  spp4DisHold);
  SET_SLOT(ReturnObject, install("spp5DisHold"),  spp5DisHold);
  SET_SLOT(ReturnObject, install("effort"),  anEffort);
  
  return ReturnObject;
}

void interpolateF ( FTYPE anFF1, int idis, int maxsp){

  float tdist1, tdist2, tdist3, tdist4;
  int Lspp1, Lspp2, Dspp1, Dspp2;
    
  /// Now do interpolation
  #pragma omp parallel private(tdist1,tdist2,tdist3,tdist4,Lspp1,Lspp2,Dspp1,Dspp2)
  {
  #pragma omp for schedule(static)    
  for (Lspp1 = 0; Lspp1 < maxsp-1; Lspp1++) {
    for (Lspp2 = 0; Lspp2 < maxsp-1; Lspp2 ++) {
      for (Dspp1 = 0; Dspp1 < maxsp-1; Dspp1++) {
	for (Dspp2 = 0; Dspp2 < maxsp-1; Dspp2++) {
	  
	  tdist1 = (1 - ((float) (Lspp1 % idis) / idis));
	  tdist2 = (1 - ((float) (Lspp2 % idis) / idis));
	  tdist3 = (1 - ((float) (Dspp1 % idis) / idis));
	  tdist4 = (1 - ((float) (Dspp2 % idis) / idis));
	  
	  anFF1[Lspp1][Lspp2][Dspp1][Dspp2] =
            (tdist1     * tdist2     * tdist3     * tdist4     * anFF1[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * tdist3     * tdist4     * anFF1[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * (1-tdist2) * tdist3     * tdist4     * anFF1[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * tdist2     * (1-tdist3) * tdist4     * anFF1[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * tdist2     * tdist3     * (1-tdist4) * anFF1[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * tdist3     * tdist4     * anFF1[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * (1-tdist3) * tdist4     * anFF1[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * tdist3     * (1-tdist4) * anFF1[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    (tdist1     * (1-tdist2) * (1-tdist3) * tdist4     * anFF1[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * (1-tdist2) * tdist3     * (1-tdist4) * anFF1[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    (tdist1     * tdist2     * (1-tdist3) * (1-tdist4) * anFF1[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * (1-tdist3) * tdist4     * anFF1[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * (1-tdist3) * (1-tdist4) * anFF1[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis])+
	    (tdist1     * (1-tdist2) * (1-tdist3) * (1-tdist4) * anFF1[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * tdist3     * (1-tdist4) * anFF1[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * (1-tdist3) * (1-tdist4) * anFF1[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis]);
	}
      }
    }
  }
  }
}

void interpolateI ( ITYPE aIStar, int idis, int maxsp){

  float tdist1, tdist2, tdist3, tdist4;
  int Lspp1, Lspp2, Dspp1, Dspp2;
    
  /// Now do interpolation
  #pragma omp parallel private(tdist1,tdist2,tdist3,tdist4,Lspp1,Lspp2,Dspp1,Dspp2)
  {
  #pragma omp for schedule(static)    
  for (Lspp1 = 0; Lspp1 < maxsp-1; Lspp1++) {
    for (Lspp2 = 0; Lspp2 < maxsp-1; Lspp2 ++) {
      for (Dspp1 = 0; Dspp1 < maxsp-1; Dspp1++) {
	for (Dspp2 = 0; Dspp2 < maxsp-1; Dspp2++) {
	  
	  tdist1 = (int)((1 - ((float) (Lspp1 % idis) / idis)) + 0.5);
	  tdist2 = (int)((1 - ((float) (Lspp2 % idis) / idis)) + 0.5);
	  tdist3 = (int)((1 - ((float) (Dspp1 % idis) / idis)) + 0.5);
	  tdist4 = (int)((1 - ((float) (Dspp2 % idis) / idis)) + 0.5);
	  
	  aIStar[Lspp1][Lspp2][Dspp1][Dspp2] =
            (tdist1     * tdist2     * tdist3     * tdist4     * aIStar[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * tdist3     * tdist4     * aIStar[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * (1-tdist2) * tdist3     * tdist4     * aIStar[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * tdist2     * (1-tdist3) * tdist4     * aIStar[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * tdist2     * tdist3     * (1-tdist4) * aIStar[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * tdist3     * tdist4     * aIStar[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * (1-tdist3) * tdist4     * aIStar[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * tdist3     * (1-tdist4) * aIStar[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    (tdist1     * (1-tdist2) * (1-tdist3) * tdist4     * aIStar[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    (tdist1     * (1-tdist2) * tdist3     * (1-tdist4) * aIStar[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    (tdist1     * tdist2     * (1-tdist3) * (1-tdist4) * aIStar[(Lspp1/ idis)    * idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * (1-tdist3) * tdist4     * aIStar[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][(Dspp2/ idis)    * idis])+
	    ((1-tdist1) * tdist2     * (1-tdist3) * (1-tdist4) * aIStar[((Lspp1/idis) +1)* idis][(Lspp2/ idis)    * idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis])+
	    (tdist1     * (1-tdist2) * (1-tdist3) * (1-tdist4) * aIStar[(Lspp1/ idis)    * idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * tdist3     * (1-tdist4) * aIStar[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][(Dspp1/ idis)    * idis][((Dspp2/idis) +1)* idis])+
	    ((1-tdist1) * (1-tdist2) * (1-tdist3) * (1-tdist4) * aIStar[((Lspp1/idis) +1)* idis][((Lspp2/idis) +1)* idis][((Dspp1/idis) +1)* idis][((Dspp2/idis) +1)* idis]);
	}
      }
    }
  }
  }
}

void printPatchDetails ( int aPatch, int aTime, int aNoInc, PTYPE aLndParms){

  Rprintf("Patch ");Rprintf("%d, ",aPatch), Rprintf(" time"); Rprintf("%d\n ",aTime);
  for (int s = 0; s < NOSPEC; s++){
    for (int si = 0; si < NOSIZES; si++){
      for (int inc = 0; inc < aNoInc; inc++){
        Rprintf("%1.4f ",aLndParms[aPatch][aTime][s][si][inc]);
      }
      Rprintf("\n");
    }
  }  
}


void nonZeroRanges ( int aHorizon, int aNoInc, int aNPatch, ATYPE aLndParmsAgg, int whatRangeLT[MAXHORIZON]){

  // estimate ranges for which we have nonzeros
  float whatRangeL[MAXNOINC*2] = {0};
  
  for (int t = 0; t < aHorizon; t++){
    for (int inc = 0; inc < aNoInc; inc++){
      whatRangeL[inc] = 0;
    }       
    for (int i = 0; i < aNPatch; i++){
      for (int s = 0; s < 2; s++){                     // only loop over sp0 and 1, because those are the only ones used in backward
	for (int inc = 0; inc < aNoInc; inc++){
	  whatRangeL[inc] += aLndParmsAgg[i][t][s][inc]; 
	}
      }
    }
    for (int inc =  aNoInc; inc >=0; inc--)  if( whatRangeL[inc]<0.00000001) whatRangeLT[t]=inc ;
  }
}


extern "C" SEXP DynStateF(SEXP cLndParms, SEXP cDisParms, SEXP eParms, SEXP pParms, SEXP xControl) {

Rprintf("Start of DynStateF\n");

  /*******************************************************************************************************************/
  /* INITIALISE VARIABLES, READ VARIABLES,                                                                           */
  /*******************************************************************************************************************/
  int noSpec  = NOSPEC;
  int noSizes = NOSIZES;
  
  SEXP a       = GET_DIM(cLndParms);
  int kNPatch  = INTEGER(a)[0];
  int kHorizon = INTEGER(a)[1];
  int noInc    = INTEGER(a)[4];
  
  int kSpp1Capacity  = SPP1CAPACITY ;
  int kSpp2Capacity  = SPP2CAPACITY ;
    
  double vSpp1LndQuota     =            REAL(GET_SLOT(xControl,install("spp1LndQuota"    )))[0];
  double vSpp1DisQuota     =            REAL(GET_SLOT(xControl,install("spp1DisQuota"    )))[0];
  double vSpp1LndQuotaFine =            REAL(GET_SLOT(xControl,install("spp1LndQuotaFine")))[0];
  double vSpp1DisQuotaFine =            REAL(GET_SLOT(xControl,install("spp1DisQuotaFine")))[0];
  double vSpp2LndQuota     =            REAL(GET_SLOT(xControl,install("spp2LndQuota"    )))[0];
  double vSpp2DisQuota     =            REAL(GET_SLOT(xControl,install("spp2DisQuota"    )))[0];
  double vSpp2LndQuotaFine =            REAL(GET_SLOT(xControl,install("spp2LndQuotaFine")))[0];
  double vSpp2DisQuotaFine =            REAL(GET_SLOT(xControl,install("spp2DisQuotaFine")))[0];
  int    kSimNumber        =    (short) INTEGER(GET_SLOT(xControl,install("simNumber"    )))[0];
  double kPriceEffort      =            (REAL(GET_SLOT(xControl,install("fuelUse"      )))[0] * REAL(GET_SLOT(xControl,install("fuelPrice")))[0])  + REAL(GET_SLOT(xControl,install("gearMaintenance")))[0] ;
  double sizeSpp1Inc       =            REAL(GET_SLOT(xControl,install("spp1Incs"     )))[0];
  double sizeSpp2Inc       =            REAL(GET_SLOT(xControl,install("spp2Incs"     )))[0];
  double sizeSpp3Inc       =            REAL(GET_SLOT(xControl,install("spp3Incs"     )))[0];
  double sizeSpp4Inc       =            REAL(GET_SLOT(xControl,install("spp4Incs"     )))[0];
  double sizeSpp5Inc       =            REAL(GET_SLOT(xControl,install("spp5Incs"     )))[0];
  int    numthreads        =    (short) INTEGER(GET_SLOT(xControl,install("numThreads")))[0];

  int    idis              =    (short) INTEGER(GET_SLOT(xControl,install("interpolationdistance")))[0];
  
  double   kSpp1LndQuota      = vSpp1LndQuota/sizeSpp1Inc;
  double   kSpp1LndQuotaFine  = vSpp1LndQuotaFine * sizeSpp1Inc;
  double   kSpp1DisQuota      = vSpp1DisQuota/sizeSpp1Inc;
  double   kSpp1DisQuotaFine  = vSpp1DisQuotaFine * sizeSpp1Inc;
  double   kSpp2LndQuota      = vSpp2LndQuota/sizeSpp2Inc;
  double   kSpp2LndQuotaFine  = vSpp2LndQuotaFine * sizeSpp2Inc;
  double   kSpp2DisQuota      = vSpp2DisQuota/sizeSpp2Inc;
  double   kSpp2DisQuotaFine  = vSpp2DisQuotaFine * sizeSpp2Inc;


  SEXP ReturnObject, Simulations ;

  Rprintf("%f \n",kPriceEffort);
  
  /********************************************************************************************************************/
  /* Check if khorizon is not bigger than HORIZON, if so simulations will not extend arrays                           */
  /********************************************************************************************************************/
  if (kHorizon > MAXHORIZON) Rprintf("Warning, horizon dimensions exceed array \n");
  if (kSpp1Capacity > SPP1CAPACITY) Rprintf("Warning, horizon*noinc for spp1 dimensions exceed array for spp1 1\n");
  if (kSpp2Capacity > SPP2CAPACITY) Rprintf("Warning, horizon*noinc for spp2 dimensions exceed array for spp2 1\n");
  if (noInc > MAXNOINC) Rprintf("Warning, noinc larger than MAXNOINC \n");
 
  /********************************************************************************************************************/
  /* INITIALISE SRAND FOR RANDOM NUMBER GENERATOR                                                                     */
  /********************************************************************************************************************/
  // srand (1);
  srand(time(0));
  
  /**********************************************************************************************************************/
  /* DEFINE THE ARRAYS                                                                                                  */
  /**********************************************************************************************************************/

  FTYPE theFF0              = (FTYPE) matalloc(sizeof(float), (void*) 0,4, SPP1CAPACITY, SPP2CAPACITY, SPP1CAPACITY, SPP2CAPACITY);
  FTYPE theFF1              = (FTYPE) matalloc(sizeof(float), (void *)0, 4, kSpp1Capacity, kSpp2Capacity, kSpp1Capacity,kSpp2Capacity);
  ITYPE theIStar            = (ITYPE) matalloc(sizeof(UFINT), (void *)0, 4, kSpp1Capacity, kSpp2Capacity, kSpp1Capacity, kSpp2Capacity);
  PTYPE theLndParms         = (PTYPE) matalloc(sizeof(float), (void *)0, 5, kNPatch, MAXHORIZON, NOSPEC, NOSIZES, noInc);
  PTYPE theDisParms         = (PTYPE) matalloc(sizeof(float), (void *)0, 5, kNPatch, MAXHORIZON, NOSPEC, NOSIZES, noInc);
  ATYPE theLndParmsAgg      = (ATYPE) matalloc(sizeof(float), (void *)0, 4, kNPatch, MAXHORIZON, NOSPEC,  (NOSIZES*noInc) - 1);
  ATYPE theDisParmsAgg      = (ATYPE) matalloc(sizeof(float), (void *)0, 4, kNPatch, MAXHORIZON, NOSPEC,  (NOSIZES*noInc) - 1);
  PITYPE thePriceParms      = (PITYPE) matalloc(sizeof(float), (void *)0, 3, MAXHORIZON, NOSPEC, NOSIZES);
  
  Rprintf("Size of FF0: %i\n", sizeof(*theFF0));
  
  int theEffortArray[320000][MAXHORIZON];
  double *theIncrementArray = (double *) malloc((size_t)noSpec * sizeof (double));
  double *theShortTermGains = (double *) malloc((size_t)kNPatch * sizeof (double));
  double *theShortTermCosts = (double *) malloc((size_t)kNPatch * sizeof (double));
  double *theShortTermEcon  = (double *) malloc((size_t)kNPatch * sizeof (double));
  
  /*********************************************************************************************************************/
  /* CHECK WHETHER ARRAY ALLOCTION SUCCEEDED                                                                           */
  /*********************************************************************************************************************/
  if (theFF0 == 0l) Rprintf("error in memory allocation FF0 \n");
  if (theFF1 == 0l) Rprintf("error in memory allocation FF1 \n");
  if (theIStar == 0l) Rprintf("error in memory allocation IStar \n");
  if (theLndParms == 0l) Rprintf("error in memory allocation theLndParms \n");
  if (theDisParms == 0l) Rprintf("error in memory allocation theDisParms \n");

  /********************************************************************************************************************/
  /* WARN IF UPLIMITS +INCREMENTS EXCEED MODEL CAPACITIES                                                             */
  /********************************************************************************************************************/
  Rprintf("kSpp1LndQuota: %f, noInc: %d, kSpp1Capacity: %d\n", kSpp1LndQuota, noInc, kSpp1Capacity);
  if ((kSpp1LndQuota + noInc > kSpp1Capacity) )    Rprintf("Warning: Spp1LndQuota + NoInc exceeds Capacity for spp1 \n");
  if ((kSpp1DisQuota + noInc > kSpp1Capacity) )    Rprintf("Warning: Spp1DisQuota + NoInc exceeds Capacity for spp1 \n");
  if ((kSpp2LndQuota + noInc > kSpp2Capacity) )    Rprintf("Warning: Spp2LndQuota + NoInc exceeds Capacity for spp2 \n");
  if ((kSpp2DisQuota + noInc > kSpp2Capacity) )    Rprintf("Warning: Spp2DisQuota + NoInc exceeds Capacity for spp2 \n");

  /*********************************************************************************************************************/
  /* PUT Parmdata from xParms in theLndParms and theDisParms arrays                                                    */
  /*********************************************************************************************************************/  
  for (int i = 0; i < kNPatch; i++){
    for (int t = 0; t < kHorizon; t++){
      for (int s = 0; s < noSpec; s++){
        for (int si = 0; si < noSizes; si++){
	  for (int inc = 0; inc < noInc; inc++){
	      theLndParms[i][t][s][si][inc] = REAL(cLndParms)[i + t*kNPatch + s*kHorizon*kNPatch + si*noSpec*kHorizon*kNPatch +
					                      inc*noSizes*noSpec*kHorizon*kNPatch]; 
	      theDisParms[i][t][s][si][inc] = REAL(cDisParms)[i + t*kNPatch + s*kHorizon*kNPatch + si*noSpec*kHorizon*kNPatch +
								inc*noSizes*noSpec*kHorizon*kNPatch]; 
	  }
        }
      }
    }
  }

  printPatchDetails ( 0, 0, noInc,  theLndParms);
  
  /*************************************************************************************************************************************/
  /* PUT theLndParms & theDisParms into aggregate arrays DOES NOT SCALE AUTOMATICALLY WITH NOSIZES BUT IT SHOULD IF THOSE ARE ALTERED  */
  /*************************************************************************************************************************************/  
  for (int i = 0; i < kNPatch; i++){
    for (int t = 0; t < kHorizon; t++){
      for (int s = 0; s < noSpec; s++){
	for (int inc = 0; inc < ((NOSIZES * noInc)-1); inc++){
	  theLndParmsAgg[i][t][s][inc] =  0; 
	  theDisParmsAgg[i][t][s][inc] =  0;
	}
      }
    }
  }
  
  for (int i = 0; i < kNPatch; i++){
    for (int t = 0; t < kHorizon; t++){
      for (int s = 0; s < noSpec; s++){
	for (int inc0 = 0; inc0 < noInc; inc0++){
	  for (int inc1 = 0; inc1 < noInc; inc1++){
	    theLndParmsAgg[i][t][s][inc0+inc1] +=  theLndParms[i][t][s][0][inc0] * theLndParms[i][t][s][1][inc1]; 
	    theDisParmsAgg[i][t][s][inc0+inc1] +=  theDisParms[i][t][s][0][inc0] * theDisParms[i][t][s][1][inc1];
	  }
	}
      }
    }
  }
  Rprintf("Generated aggregated distribution functions \n"); R_FlushConsole();

  /*************************************************************************************************************************************/
  /*  estimate ranges for which we have nonzeros
  /*************************************************************************************************************************************/  
  
  int whatRangeLT[MAXHORIZON] = {(NOSIZES * noInc) -1};

  nonZeroRanges(kHorizon,(2* noInc)-1,kNPatch, theLndParmsAgg, whatRangeLT);
  for (int t = 0; t < kHorizon; t++){
    Rprintf("%d \n", whatRangeLT[t]);
  }
  Rprintf("Defined ranges for distribution functions per timestep\n"); R_FlushConsole();
  
  /*************************************************************************************************************************************/
  /* PUT incement size information per species in a a vector so we can use it when putting price data in thePriceParns                 */
  /*************************************************************************************************************************************/  
  vector <float> sizeSppInc (noSpec,0); 

  sizeSppInc[0] = sizeSpp1Inc;
  sizeSppInc[1] = sizeSpp2Inc;
  sizeSppInc[2] = sizeSpp3Inc;
  sizeSppInc[3] = sizeSpp4Inc;
  sizeSppInc[4] = sizeSpp5Inc;
  
  /*************************************************************************************************************************************/
  /* PUT pricedata from pParms in epriceParms array                                                                                    */
  /*************************************************************************************************************************************/  
  for (int t = 0; t < kHorizon; t++){
    for (int s = 0; s < noSpec; s++){
      for (int si = 0; si < noSizes; si++){
          thePriceParms[t][s][si] = REAL(pParms)[ si  + t*noSizes + s*kHorizon*noSizes] * sizeSppInc[s]; //check how r part is structured
      }
    }
  } 

  Rprintf("Price parameters for all species\n");
  for (int t = 0; t < kHorizon; t++){
    for (int s = 0; s < noSpec; s++){
      for (int si = 0; si < noSizes; si++){
	Rprintf ("%f ", thePriceParms[t][s][si]);
      }
      Rprintf("\n");
    }
  }
  
  /*************************************************************************************************************************************/
  /*INITIALISE THE EFFORTCOST FOR THE DIFFERENT PATCHES                                                                                */
  /*************************************************************************************************************************************/
  Rprintf("Effort array initialised\n");
  for (int i = 0; i < kNPatch; i++){ 
    for (int t = 0; t < kHorizon; t++){
      theEffortArray[i][t] = REAL(eParms)[i + t* kNPatch];
    }
  }

  /*************************************************************************************************************************************/
  /*DEBUGCHECK                                                                                                                         */
  /*************************************************************************************************************************************/
  Rprintf("kSpp1LndQuota ");                  Rprintf("%f\n", kSpp1LndQuota);
  Rprintf("kSpp2LndQuota ");                  Rprintf("%f\n", kSpp2LndQuota);
  Rprintf("kSpp1DisQuota ");                  Rprintf("%f\n", kSpp1DisQuota);
  Rprintf("kSpp2DisQuota ");                  Rprintf("%f\n", kSpp2DisQuota);
  Rprintf("noInc ");                          Rprintf("%d\n", noInc);
  Rprintf("Capacity ");                       Rprintf("%d\n", kSpp1Capacity);

  Rprintf("vSpp1LndQuota ");                  Rprintf("%f\n", vSpp1LndQuota);
  Rprintf("sizeSpp1Inc   ");                  Rprintf("%f\n", sizeSpp1Inc);

  for (int s = 0; s < noSpec; s++){ 
    Rprintf("spp");  Rprintf("%d", s+1); Rprintf(" Increment size "); Rprintf("%f\n",  sizeSppInc[s]);
  }

  int Lndspp1, Lndspp2, Disspp1, Disspp2, idisn, cntr, p;

  /*************************************************************************************************************************************/
  /* INITIALISE F0 AND F1 ARRAY TO 0 IF IN OPENMP ENVIRONMENT                                                                          */
  /*************************************************************************************************************************************/
  Rprintf("get_max threads ");
  Rprintf("%d\n",omp_get_max_threads());

  omp_set_num_threads(numthreads);
  Rprintf("OPEN_MP environment: number of threads is "); Rprintf("%d\n",numthreads);
  Rprintf("%d\n",omp_get_num_threads());

  
  #pragma omp parallel private(Lndspp1,Lndspp2,Disspp1,Disspp2)
  {
  #pragma omp for schedule(static)    
  for (Lndspp1 = 0; Lndspp1 < kSpp1Capacity; Lndspp1++) {
    for (Lndspp2 = 0; Lndspp2 < kSpp2Capacity; Lndspp2++) {
       for (Disspp1 = 0; Disspp1 < kSpp1Capacity; Disspp1++) {
	  for (Disspp2 = 0; Disspp2 < kSpp2Capacity; Disspp2++) {
              theFF0[Lndspp1][Lndspp2][Disspp1][Disspp2] = 0.0;
              theFF1[Lndspp1][Lndspp2][Disspp1][Disspp2] = 0.0;  
	  }
       }
    }
  }
  }
  Rprintf("Init memory F0 and F1 \n");   R_FlushConsole();
  
  /*************************************************************************************************************************************/
  /* INITIALISE THE F1 ARRAY AT FINAL TIMESTEP (FINAL FITNESS FUNCTION)                                                                */
  /*************************************************************************************************************************************/
  #pragma omp parallel private(Lndspp1,Lndspp2,Disspp1,Disspp2)
  {
  #pragma omp for schedule(static)
  for (Lndspp1 = 0; Lndspp1 < kSpp1Capacity; Lndspp1++) {
    for (Lndspp2 = 0; Lndspp2 < kSpp2Capacity; Lndspp2++) {
      for (Disspp1 = 0; Disspp1 < kSpp1Capacity; Disspp1++) {
        for (Disspp2 = 0; Disspp2 < kSpp2Capacity; Disspp2++) {
            theFF1[Lndspp1][Lndspp2][Disspp1][Disspp2] = utility(Lndspp1,  Disspp1, kSpp1LndQuota, kSpp1DisQuota, kSpp1LndQuotaFine,kSpp1DisQuotaFine,Lndspp2,  Disspp2, kSpp2LndQuota, kSpp2DisQuota, kSpp2LndQuotaFine,kSpp2DisQuotaFine); 
        }
      }
    }
  }
  }
  Rprintf("Utility function initialised \n");  R_FlushConsole();
  
  /*************************************************************************************************************************************/
  /* MAIN PART OF MODEL :  do backward calculations (contains interpolation, RLE encoding, and swapping F arrays)                      */
  /*************************************************************************************************************************************/
  vector <int> maxspp (MAXHORIZON,0);
  vector < vector<UFINT> > val(MAXHORIZON, vector<UFINT> (1,0));                        // for RLE
  vector < vector<unsigned short> > rep(MAXHORIZON, vector<unsigned short> (1,1));      // for RLE
  vector < vector<int> > whichpos( ( idis+10) * (idis+10) , vector<int>(2  ,0));        // for determining which positions for FFF  
  
  for (int t = kHorizon - 1; t >= 0; t--) {                                             // start the backward calculation

    Rprintf("Timestep %d Calc short term economics ", t+1);  R_FlushConsole();          // for each choice calc mean short term economic gains and costs  
    for (int i = 0; i < kNPatch; i++){     
      theShortTermGains[i] = shortTermGains(t, noInc, theLndParms, i, thePriceParms);
      theShortTermCosts[i] = shortTermCosts(t, i, kPriceEffort, theEffortArray);        //*, noInc, theLndParms, thePriceParms);
      theShortTermEcon[i]  =  theShortTermGains[i] - theShortTermCosts[i];
    }

    maxspp[t]  = accumulate(whatRangeLT,whatRangeLT+t,0) +2;                            // calc max number of increments to loop over for this timestep 
    idisn      =  max( maxspp[t]/idis, 1); 
    maxspp[t] += idisn;                                                                 // maxspp is same for all species and Land and discs;d chosen so that interpol dist is accounted for
    Rprintf(" maxspp %d ", maxspp[t]);  Rprintf(" interpolation dist %d ", idisn);  R_FlushConsole();
                                                                                        // make small data frame with combinations Lndspp1, Lndspp2, used in backward, to make parallel more efficient
    cntr = 0 ;
    for (Lndspp1 = 0; Lndspp1 < maxspp[t]; Lndspp1 += idisn) {
      for (Lndspp2 = 0; Lndspp2 < maxspp[t]; Lndspp2 += idisn) {
	whichpos[cntr][0] = Lndspp1;
	whichpos[cntr][1] = Lndspp2;
	cntr++;
      }
    }
    Rprintf(" Defined positions "); R_FlushConsole() ;
    
    // do backward calcs 
    #pragma omp parallel private(p,Lndspp1,Lndspp2,Disspp1,Disspp2)
    {
    #pragma omp for schedule(static)    
    for (p = 0; p < cntr; p++) {
      Lndspp1 = whichpos[p][0];
      Lndspp2 = whichpos[p][1];
      for (Disspp1 = 0; Disspp1 < maxspp[t]; Disspp1 += idisn) {
	for (Disspp2 = 0; Disspp2 < maxspp[t]; Disspp2 += idisn) {
	  theFF0[Lndspp1][Lndspp2][Disspp1][Disspp2]= FFF(Lndspp1, Lndspp2, Disspp1, Disspp2, whatRangeLT[t], 
							  theLndParmsAgg, theDisParmsAgg, t, kNPatch, 
							  theShortTermEcon, theFF1, theIStar);
	}
      }
    }
    }
    Rprintf("Finished FF0 calculations ");  R_FlushConsole();

    /***********************************************************************************************************************************/
    /* INTERPOLATION                                                                                                                   */
    /***********************************************************************************************************************************/

    if (idisn > 1 ){
      interpolateF(theFF0,idisn,maxspp[t]);
      interpolateI(theIStar,idisn,maxspp[t]);
    }
    Rprintf("Finished interpolations ");  R_FlushConsole();
    
    /***********************************************************************************************************************************/
    /* RLE ENCODING                                                                                                                    */
    /***********************************************************************************************************************************/
    Rprintf(" RLE encoding "); R_FlushConsole();
    int ii = 0;
    val[t][0]= theIStar[0][0][0][0];
    rep[t][0]= 1;
    
    for (Lndspp1 = 0; Lndspp1 < maxspp[t]; Lndspp1++) {
      for (Lndspp2 = 0; Lndspp2 < maxspp[t]; Lndspp2++) {
        for (Disspp1 = 0; Disspp1 < maxspp[t]; Disspp1++) {
	  for (Disspp2 = 0; Disspp2 < maxspp[t]; Disspp2++) {
              if (ii == 0) { // Stepping into procedure skip evaluationstep but move on
	          ii++;
              } else { // do evaluation
	            if((theIStar[Lndspp1][Lndspp2][Disspp1][Disspp2] != val[t][ii-1]) || (rep[t][ii-1] > 65531)) {
	               ii++;
	               val[t].push_back(theIStar[Lndspp1][Lndspp2][Disspp1][Disspp2]);
	               rep[t].push_back(1);
	            } else {
	           	 rep[t][ii-1]= rep[t][ii-1]+1;
	            }
              }
	  }
        }
      }
    }
     
    Rprintf("Finished RLE encoding\n");  R_FlushConsole();
   
    /***********************************************************************************************************************************/
    /* PUT FO ARRAY IN F1 ARRAY AFTER TIMESTEP IS CALCULATED                                                                           */
    /***********************************************************************************************************************************/
#pragma omp parallel private(Lndspp1,Lndspp2,Disspp1,Disspp2)
    {
    #pragma omp for schedule(static)
    for (Lndspp1 = 0; Lndspp1 < maxspp[t]; Lndspp1++) {
      for (Lndspp2 = 0; Lndspp2 < maxspp[t]; Lndspp2++) {
        for (Disspp1 = 0; Disspp1 < maxspp[t]; Disspp1++) {
	   for (Disspp2 = 0; Disspp2 < maxspp[t]; Disspp2++) {
               theFF1[Lndspp1][Lndspp2][Disspp1][Disspp2] =  theFF0[Lndspp1][Lndspp2][Disspp1][Disspp2];
	   }
        }
      }
    }
    }
  }
  Rprintf("Backward calculations done \n"); R_FlushConsole();
 
  matfree(theFF0);
  matfree(theFF1);
  matfree(theIStar);

  /***************************************************************************************************************/
  /* DO MONTE CARLO SIMULATIONS                                                                                  */
  /***************************************************************************************************************/
  Simulations = SimulateF(kSimNumber, kHorizon, maxspp, val, rep, noInc, sizeSppInc, theLndParms, theDisParms, theEffortArray);
  Rprintf("Simulations done \n"); R_FlushConsole();
  PROTECT(ReturnObject = NEW_OBJECT(MAKE_CLASS("DynState")));
  SET_SLOT(ReturnObject, install("sim"),  Simulations);
  UNPROTECT(31);
  matfree(theLndParms);
  matfree(theDisParms);
  matfree(theLndParmsAgg);
  matfree(theDisParmsAgg);
  return ReturnObject;
}
