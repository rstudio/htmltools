#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP template_dfa(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"template_dfa", (DL_FUNC) &template_dfa, 1},
    {NULL, NULL, 0}
};

void R_init_htmltools(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
