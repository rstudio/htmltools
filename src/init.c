#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP htmltools_template_dfa(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"htmltools_template_dfa", (DL_FUNC) &htmltools_template_dfa, 1},
    {NULL, NULL, 0}
};

void R_init_htmltools(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
