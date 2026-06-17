#ifndef HTMLTOOLS_UTILS_H
#define HTMLTOOLS_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

extern SEXP tag_class;

extern SEXP chr_empty;

extern SEXP chr_name;
extern SEXP chr_attribs;
extern SEXP chr_children;
extern SEXP chr_nows;
extern SEXP chr_renderhooks;

#endif
