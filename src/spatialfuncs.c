#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>


/****************************************************/
/* convienence method for evaluating an R function  */
/* that returns 1,2 or 3 and takes in two rows of a */
/* matrix.                                          */
/* @param Rfun  the function                        */
/* @param Rvect1 the first vector for the comparison*/
/* @param Rvect2 the second vector for th comparison*/
/**/
/* @return the return value of Rfun                 */
/****************************************************/
double run_fun(SEXP Rfun, SEXP Rvect1, SEXP Rvect2) {
  SEXP e, result;

  PROTECT(e = lang3(Rfun, Rvect1, Rvect2));
  result = eval(e, R_GlobalEnv);
  UNPROTECT(1);

  return (REAL(result)[0]);
}


/*******************************************************************/
/* convienence method to extract a row from a matrix as an R vector*/
/*                                                                 */
/* @param matrix the matrix to extract from                        */
/* @param row    the row to extract                                */
/* @return an SEXP pointing to an R vector                         */
/*******************************************************************/
SEXP extract_row(SEXP matrix, int row) {
  int i;
  SEXP rc;
  SEXP dim = getAttrib(matrix, R_DimSymbol);
  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  PROTECT(rc = allocVector(REALSXP, cols));
  for (i=0; i<cols;i++) {
    REAL(rc)[i] = REAL(matrix)[row+i*rows];
  }
  UNPROTECT(1);
  return rc;
}

/***********************************************************************/
/* pi function. Can take a generic function in and                     */
/* calculates the pi function based on the return of fun               */
/*                                                                     */
/* @param Rpostmat the matrix with the data in it.                     */
/* @param Rfun the function to evaluate the relation between points    */
/* @param Rr the maximum distances to look at                          */
/* @param Rr_min the minimum distances                                 */
/* @param Rinds  indices into the original array, to help with bootstrapping*/
/* @param Rxcol the column containing the x coordinate                 */
/* @param Rycol the column containing the y coordinate                 */
/* @param Renvr the enviroment for function evaluatoin                 */
/***********************************************************************/
SEXP get_pi (SEXP Rpostmat,
	     SEXP Rfun,
	     SEXP Rr,
	     SEXP Rr_low,
	     SEXP Rinds,
	     SEXP Rxcol,
	     SEXP Rycol) {

  SEXP rc = R_NilValue;
  int i,j,k;
  double dist;
  int num_cnt, denom_cnt; /*counters for those filling conditions*/
  int f_ans; /*used to hold the result of the function*/


  /*turn all of the R stuff passed in to the type of stuff we can
   referene in C*/
  double *r = REAL(Rr);
  double *r_low = REAL(Rr_low);
  int *inds = INTEGER(Rinds);
  int xcol = INTEGER(Rxcol)[0]-1;
  int ycol = INTEGER(Rycol)[0]-1;

  SEXP postmat_dim = getAttrib(Rpostmat, R_DimSymbol);
  double *postmat = REAL(Rpostmat);
  int rows = INTEGER(postmat_dim)[0];

  /*some sanity checking*/
  if (!isFunction(Rfun)) error("Rfun must be a function");

  /*prepare the return information*/
  PROTECT(rc=allocVector(REALSXP, length(Rr)));

  /*repeat calculation for all r*/
  for (i=0;i<length(Rr);i++) {
    //Rprintf("%1.1f,%1.1f\n", r_low[i],r[i]); //DEBUG
    /*zero out counts*/
    num_cnt = 0;
    denom_cnt = 0;

    /*might be faster to have some scraning criteria, but
      we will loop throufh every pair...j is from */
    for (j=0;j<rows;j++) {

      /*k is to*/
      for (k=0; k<rows;k++) {
  	/*do not compare someone with themself*/
  	if (inds[k] == inds[j]) continue;

  	/*calculate the distance*/
  	dist = sqrt(pow(postmat[j+xcol*rows] - postmat[k+xcol*rows],2) +
  		    pow(postmat[j+ycol*rows] - postmat[k+ycol*rows],2));

  	if ((dist>r[i]) | (dist<r_low[i])) continue;

  	/*call the user supplied function*/
  	f_ans = (int)run_fun(Rfun,
  			     extract_row(Rpostmat,j),
  			     extract_row(Rpostmat,k));

  	/*update the counts appropriately*/
  	if (f_ans==1) {
  	  denom_cnt++;
  	  num_cnt++;
  	} else if (f_ans==2) {
  	  denom_cnt++;
  	}
      }
    }
    //Rprintf("%d/%d\n",num_cnt,denom_cnt); // DEBUG
    REAL(rc)[i] = (double)num_cnt/denom_cnt;
  }

  UNPROTECT(1);

  return(rc);

}


/***********************************************************************/
/* theta function. Can take a generic function in and                  */
/* calculates the theta function based on the return of fun            */
/*                                                                     */
/* @param Rpostmat the matrix with the data in it.                     */
/* @param Rfun the function to evaluate the relation between points    */
/* @param Rr the maximum distances to look at                          */
/* @param Rr_min the minimum distances                                 */
/* @param Rinds  indices into the original array, to help with bootstrapping*/
/* @param Rxcol the column containing the x coordinate                 */
/* @param Rycol the column containing the y coordinate                 */
/* @param Renvr the enviroment for function evaluatoin                 */
/***********************************************************************/
SEXP get_theta (SEXP Rpostmat,
		SEXP Rfun,
		SEXP Rr,
		SEXP Rr_low,
		SEXP Rinds,
		SEXP Rxcol,
		SEXP Rycol) {

  SEXP rc = R_NilValue;
  int i,j,k;
  double dist;
  int num_cnt, denom_cnt; /*counters for those filling conditions*/
  int f_ans; /*used to hold the result of the function*/

  /*turn all of the R stuff passed in to the type of stuff we can
   referene in C*/
  double *r = REAL(Rr);
  double *r_low = REAL(Rr_low);
  int *inds = INTEGER(Rinds);
  int xcol = INTEGER(Rxcol)[0]-1;
  int ycol = INTEGER(Rycol)[0]-1;

  SEXP postmat_dim = getAttrib(Rpostmat, R_DimSymbol);
  double *postmat = REAL(Rpostmat);
  int rows = INTEGER(postmat_dim)[0];

  /*some sanity checking*/
  if (!isFunction(Rfun)) error("Rfun must be a function");

  /*prepare the return information*/
  PROTECT(rc=allocVector(REALSXP, length(Rr)));

  /*repeat calculation for all r*/
  for (i=0;i<length(Rr);i++) {
    //Rprintf("%1.1f,%1.1f\n", r_low[i],r[i]); //DEBUG
    /*zero out counts*/
    num_cnt = 0;
    denom_cnt = 0;

    /*might be faster to have some scraning criteria, but
      we will loop throufh every pair...j is from */
    for (j=0;j<rows;j++) {

      /*k is to*/
      for (k=0; k<rows;k++) {
  	/*do not compare someone with themself*/
  	if (inds[k] == inds[j]) continue;

  	/*calculate the distance*/
  	dist = sqrt(pow(postmat[j+xcol*rows] - postmat[k+xcol*rows],2) +
  		    pow(postmat[j+ycol*rows] - postmat[k+ycol*rows],2));

  	if ((dist>r[i]) | (dist<r_low[i])) continue;

  	/*call the user supplied function*/
  	f_ans = (int)run_fun(Rfun,
  			     extract_row(Rpostmat,j),
  			     extract_row(Rpostmat,k));

  	/*update the counts appropriately*/
  	if (f_ans==1) {
  	  num_cnt++; //related cases contribute only to the numerator
  	} else if (f_ans==2) {
  	  denom_cnt++; //unrelated cases cotnribute to the denominator
  	}
      }
    }
    //Rprintf("%d/%d\n",num_cnt,denom_cnt); // DEBUG
    REAL(rc)[i] = (double)num_cnt/denom_cnt;
  }

  UNPROTECT(1);

  return(rc);

}


/*****************************************************************/
/*pi function optimized for iterating through a list of two types*/
/*finding ones that fulfill the distance requirement.            */
/*                                                               */
/*@param type    the type column                                 */
/*@param x       the x coordinate                                */
/*@param y       the y coordinate                                */
/*@param len     the length of the three data arrays             */
/*@param typeA   the "from" type                                 */
/*@param typeB   the "to" type                                   */
/*@param r_low   the low end of the range of values to look at   */
/*@param r       the sequence of upper distances to consider     */
/*@param len_r   the number of different Rs to consider          */
/*                   usually will be just the indicies           */
/*@param inds    the indices into the original array, helps boot */
/*@param rc      the array of values that we are going to return */
/*****************************************************************/
void get_pi_typed (int *type,
		   double *x,
		   double *y,
		   int *len,
		   int *typeA,
		   int *typeB,
		   double *r_low,
		   double *r,
		   int *len_r,
		   int *inds,
		   double *rc) {

  int i,j,k;
  int num_cnt, denom_cnt; /*counters for those filling conditions*/
  double dist;

  /*repeat the calculation for all r*/
  for (i=0;i<*len_r;i++) {
    //Rprintf("%1.1f,%1.1f\n", r_low[i],r[i]); //DEBUG
    /*zero out counts*/
    num_cnt = 0;
    denom_cnt = 0;

    if (*typeA != -1) {

      for (j=0;j<*len;j++) {
	if (type[j] != *typeA) continue;

	for (k=0;k<*len;k++) {
	  /*ignore pairs of the same observation*/
	  if (inds[k]==inds[j]) continue;

	  dist = sqrt(pow(x[j]-x[k],2)+pow(y[j]-y[k],2));
	  if ((dist<=r[i])  & (dist>=r_low[i])) denom_cnt++;

	  if (type[k] != *typeB) continue;
	  if ((dist<=r[i])  & (dist>=r_low[i])) num_cnt++;
	}
      }

    } else {
      Rprintf("To be implemented\n");
      return;
    }
    //Rprintf("%d/%d\n",num_cnt,denom_cnt);//DEBUG
    rc[i] = (double)num_cnt/denom_cnt;
  }
}


/********************************************************************/
/*theta function optimized for iterating through a list of two types*/
/*finding ones that fulfill the distance requirement.               */
/*                                                                  */
/*@param type    the type column                                    */
/*@param x       the x coordinate                                   */
/*@param y       the y coordinate                                   */
/*@param len     the length of the three data arrays                */
/*@param typeA   the "from" type                                    */
/*@param typeB   the "to" type                                      */
/*@param r_low   the low end of the range of values to look at      */
/*@param r       the sequence of upper distances to consider        */
/*@param len_r   the number of different Rs to consider             */
/*                   usually will be just the indicies              */
/*@param inds    the indices into the original array, helps boot    */
/*@param rc      the array of values that we are going to return    */
/********************************************************************/
void get_theta_typed (int *type,
		   double *x,
		   double *y,
		   int *len,
		   int *typeA,
		   int *typeB,
		   double *r_low,
		   double *r,
		   int *len_r,
		   int *inds,
		   double *rc) {

  int i,j,k;
  int num_cnt, denom_cnt; /*counters for those filling conditions*/
  double dist;

  /*repeat the calculation for all r*/
  for (i=0;i<*len_r;i++) {
    //Rprintf("%1.1f,%1.1f\n", r_low[i],r[i]); //DEBUG
    /*zero out counts*/
    num_cnt = 0;
    denom_cnt = 0;

    if (*typeA != -1) {

      for (j=0;j<*len;j++) {
	if (type[j] != *typeA) continue;

	for (k=0;k<*len;k++) {
	  /*ignore pairs of the same observation*/
	  if (inds[k]==inds[j]) continue;

	  dist = sqrt(pow(x[j]-x[k],2)+pow(y[j]-y[k],2));

	  if ((dist<=r[i])  & (dist>=r_low[i])) {
	    if (type[k] == *typeB) {
	      num_cnt++;
	    } else {
	      denom_cnt++;
	    }
	  }
	}
      }

    } else {
      Rprintf("To be implemented\n");
      return;
    }
    //Rprintf("%d/%d\n",num_cnt,denom_cnt);//DEBUG
    rc[i] = (double)num_cnt/denom_cnt;
  }
}





/***********************************************************************/
/*tau function for generic functions                                   */
/*                                                                     */
/* @param Rpostmat the matrix with the data in it.                     */
/* @param Rfun the function to evaluate the relation between points    */
/* @param Rr the maximum distances to look at                          */
/* @param Rr_min the minimum distances                                 */
/* @param Rcomparison_type 0 for represnetative, 1 for unrelated        */
/* @param Rinds  indices into the original array, to help with bootstrapping*/
/* @param Rxcol the column containing the x coordinate                 */
/* @param Rycol the column containing the y coordinate                 */
/***********************************************************************/
SEXP get_tau (SEXP Rpostmat,
	      SEXP Rfun,
	      SEXP Rr,
	      SEXP Rr_low,
	      SEXP Rcomparison_type,
	      SEXP Rinds,
	      SEXP Rxcol,
	      SEXP Rycol) {

  int i;
  SEXP divisor;
  SEXP rc;
  SEXP highR;
  SEXP lowR;
  int comparison_type = asInteger(Rcomparison_type);


  PROTECT(highR=allocVector(REALSXP, 1));
  PROTECT(lowR=allocVector(REALSXP, 1));
  REAL(highR)[0] = R_PosInf;
  REAL(lowR)[0] = 0;

  if (comparison_type==0) {
    PROTECT(divisor=get_pi(Rpostmat,Rfun, highR, lowR, Rinds, Rxcol, Rycol));
    PROTECT(rc=get_pi(Rpostmat, Rfun, Rr, Rr_low, Rinds, Rxcol, Rycol));
  } else if (comparison_type==1) {
    PROTECT(divisor=get_theta(Rpostmat,Rfun, highR, lowR, Rinds, Rxcol, Rycol));
    PROTECT(rc=get_theta(Rpostmat, Rfun, Rr, Rr_low, Rinds, Rxcol, Rycol));
  } else {
    error("Invalid comparison_type.");
  }

  for (i=0; i<length(Rr);i++) {
   if (REAL(rc)[i]== REAL(divisor)[0]) { //here to deal with the Inf/Inf problem
     if (REAL(rc)[i] != 0) {
       REAL(rc)[i] = 1;
     } else {
       REAL(rc)[i] = R_NaN;
     }
   } else {
     REAL(rc)[i] = REAL(rc)[i]/REAL(divisor)[0];
   }
  }
  UNPROTECT(4);

  return(rc);
}


/*****************************************************************/
/* tau function for typed data                                   */
/* interates through a list of two types finding ones            */
/*that fulfill the distance requirement.                         */
/* this version takes in coordinates instead of distances        */
/*                                                               */
/*@param type_1  the  first type column                          */
/*@param type_2  the second type column                          */
/*@param x       the x coordinate                                */
/*@param y       the y coordinate                                */
/*@param len     the length of the three data arrays              */
/*@param typeA   the "from" type                                 */
/*@param typeB   the "to" type                                   */
/*@param r_low   the low end of the range of values to look at   */
/*@param r       the sequence of upper distances to consider     */
/*@param len_r   the number of different Rs to consider          */
/*                   usually will be just the indicies           */
/*@param inds    the indices into the original array, helps boot */
/*@param comp_type the comparison type                           */
/*@param rc      the array of values that we are going to return */
/*****************************************************************/
void get_tau_typed (int *type,
		    double *x,
		    double *y,
		    int *len,
		    int *typeA,
		    int *typeB,
		    double *r_low,
		    double *r,
		    int *len_r,
		    int *inds,
		    int *comp_type,
		    double *rc) {

    int i = 0;
    double divisor;
    double tmp_r_low = 0;
    double tmp_r = DBL_MAX;
    int tmp_len_r = 1;

    if (*comp_type==0) {
      /*get the divisor in the pi function*/
      get_pi_typed(type,x,y,len,typeA,typeB,&tmp_r_low, &tmp_r,
		   &tmp_len_r,inds,&divisor);

      /*get the main pi function*/
      get_pi_typed(type, x, y, len, typeA, typeB, r_low, r, len_r,inds, rc);
    } else if (*comp_type==1) {
      /*get the divisor in the pi function*/
      get_theta_typed(type,x,y,len,typeA,typeB,&tmp_r_low, &tmp_r,
		   &tmp_len_r,inds,&divisor);

      /*get the main pi function*/
      get_theta_typed(type, x, y, len, typeA, typeB, r_low, r, len_r,inds, rc);
    }

    for (i = 0; i < *len_r; i++) {
      if (rc[i] == divisor) { //deals with the Inf/Inf problem
	if (rc[i] != 0) {
	  rc[i] = 1;
	} else {
	  rc[i] = NAN;
	}
      } else  {
	rc[i] = rc[i]/divisor;
      }
    }

}
