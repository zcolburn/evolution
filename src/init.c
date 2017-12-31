#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _evolution_deletion_mutator(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _evolution_find_start_codon(SEXP);
extern SEXP _evolution_find_stop_codon(SEXP);
extern SEXP _evolution_insertion_mutator(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _evolution_substitution_mutator(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _evolution_translate(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_evolution_deletion_mutator",     (DL_FUNC) &_evolution_deletion_mutator,      5},
    {"_evolution_find_start_codon",     (DL_FUNC) &_evolution_find_start_codon,      1},
    {"_evolution_find_stop_codon",      (DL_FUNC) &_evolution_find_stop_codon,       1},
    {"_evolution_insertion_mutator",    (DL_FUNC) &_evolution_insertion_mutator,     5},
    {"_evolution_substitution_mutator", (DL_FUNC) &_evolution_substitution_mutator, 13},
    {"_evolution_translate",            (DL_FUNC) &_evolution_translate,             2},
    {NULL, NULL, 0}
};

void R_init_evolution(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
