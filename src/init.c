#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _evolution_deletion_mutator_cpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _evolution_find_start_codon_cpp(SEXP);
extern SEXP _evolution_find_stop_codon_cpp(SEXP);
extern SEXP _evolution_insertion_mutator_cpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _evolution_substitution_mutator_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _evolution_translate_cpp(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_evolution_deletion_mutator_cpp",     (DL_FUNC) &_evolution_deletion_mutator_cpp,      5},
    {"_evolution_find_start_codon_cpp",     (DL_FUNC) &_evolution_find_start_codon_cpp,      1},
    {"_evolution_find_stop_codon_cpp",      (DL_FUNC) &_evolution_find_stop_codon_cpp,       1},
    {"_evolution_insertion_mutator_cpp",    (DL_FUNC) &_evolution_insertion_mutator_cpp,     5},
    {"_evolution_substitution_mutator_cpp", (DL_FUNC) &_evolution_substitution_mutator_cpp, 13},
    {"_evolution_translate_cpp",            (DL_FUNC) &_evolution_translate_cpp,             2},
    {NULL, NULL, 0}
};

void R_init_evolution(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
