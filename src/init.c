#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void get_pi_typed(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void get_tau_typed(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void get_theta_typed(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

/* .Call calls */
extern SEXP get_pi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP get_tau(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP get_theta(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"get_pi_typed",    (DL_FUNC) &get_pi_typed,    11},
    {"get_tau_typed",   (DL_FUNC) &get_tau_typed,   12},
    {"get_theta_typed", (DL_FUNC) &get_theta_typed, 11},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"get_pi",    (DL_FUNC) &get_pi,    7},
    {"get_tau",   (DL_FUNC) &get_tau,   8},
    {"get_theta", (DL_FUNC) &get_theta, 7},
    {NULL, NULL, 0}
};

void R_init_IDSpatialStats(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}