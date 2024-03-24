#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP template_dfa(SEXP);
extern SEXP new_tag(SEXP);

// Defined below
SEXP htmltools_initialize(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"template_dfa", (DL_FUNC) &template_dfa, 1},
    {"new_tag", (DL_FUNC) &new_tag, 4},
    {"htmltools_initialize", (DL_FUNC) &htmltools_initialize, 1},
    {NULL, NULL, 0}
};

void R_init_htmltools(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

// utils.c
void htmltools_initialize_utils(SEXP);

SEXP htmltools_initialize(SEXP ns) {
  htmltools_initialize_utils(ns);
  return R_NilValue;
}
