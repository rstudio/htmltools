#include "utils.h"

SEXP tag_class = NULL;

SEXP chr_empty = NULL;

SEXP chr_name = NULL;
SEXP chr_attribs = NULL;
SEXP chr_children = NULL;
SEXP chr_nows = NULL;
SEXP chr_renderhooks = NULL;

void htmltools_initialize_utils(SEXP ns) {
  tag_class = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(tag_class);
  SET_STRING_ELT(tag_class, 0, Rf_mkChar("shiny.tag"));

  R_PreserveObject(chr_empty = Rf_mkChar(""));

  R_PreserveObject(chr_name = Rf_mkChar("name"));
  R_PreserveObject(chr_attribs = Rf_mkChar("attribs"));
  R_PreserveObject(chr_children = Rf_mkChar("children"));
  R_PreserveObject(chr_nows = Rf_mkChar(".noWS"));
  R_PreserveObject(chr_renderhooks = Rf_mkChar(".renderHooks"));
}
