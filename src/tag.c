#include "utils.h"

SEXP have_name(SEXP x) {
  SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  R_xlen_t n = Rf_xlength(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  if (nms == R_NilValue) {
    for (R_xlen_t i = 0; i < n; ++i) {
      SET_LOGICAL_ELT(out, i, 0);
    }
  } else {
    for (R_xlen_t i = 0; i < n; ++i) {
      SEXP nm_i = STRING_ELT(nms, i);
      SET_LOGICAL_ELT(out, i, nm_i != NA_STRING & nm_i != chr_empty);
    }
  }

  UNPROTECT(2);
  return out;
}

SEXP new_tag(SEXP tagName, SEXP varArgs, SEXP noWS, SEXP renderHook) {
  R_xlen_t n = Rf_xlength(varArgs);

  // TODO validate that varArgs is a list

  // Unnamed arguments are flattened and added as children.
  // Named arguments become attribs, dropping NULL and length-0 values
  SEXP namedFlag = PROTECT(have_name(varArgs));

  // Calculate number of attributes and children
  R_xlen_t n_attributes = 0;
  R_xlen_t n_children = n;
  for (R_xlen_t i = 0; i < n; ++i) {
    int arg_i_empty = Rf_xlength(VECTOR_ELT(varArgs, i)) == 0;
    n_attributes = n_attributes + (arg_i_empty ? 0 : LOGICAL_ELT(namedFlag, i));
    n_children = n_children - LOGICAL_ELT(namedFlag, i);
  }

  // Create attributes and children
  SEXP varArgNms = Rf_getAttrib(varArgs, R_NamesSymbol);
  SEXP attributes = PROTECT(Rf_allocVector(VECSXP, n_attributes));
  SEXP attribute_nms = PROTECT(Rf_allocVector(STRSXP, n_attributes));
  Rf_setAttrib(attributes, R_NamesSymbol, attribute_nms);

  SEXP children = PROTECT(Rf_allocVector(VECSXP, n_children));
  R_xlen_t i_attributes = 0;
  R_xlen_t i_children = 0;

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP arg_i = VECTOR_ELT(varArgs, i);
    bool arg_i_empty = Rf_xlength(arg_i) == 0;
    if (LOGICAL_ELT(namedFlag, i)) {
      if (!arg_i_empty) {
        SET_VECTOR_ELT(attributes, i_attributes, arg_i);
        SEXP arg_i_nm = STRING_ELT(varArgNms, i);
        SET_STRING_ELT(attribute_nms, i_attributes, arg_i_nm);
        ++i_attributes;
      }
    } else {
      SET_VECTOR_ELT(children, i_children, arg_i);
      ++i_children;
    }
  }

  // Create tag
  R_xlen_t n_fields = 3;
  if (noWS != R_NilValue) {
    ++n_fields;
  }
  if (renderHook != R_NilValue) {
    ++n_fields;
  }
  SEXP tag = PROTECT(Rf_allocVector(VECSXP, n_fields)) ;
  SEXP field_nms = PROTECT(Rf_allocVector(STRSXP, n_fields));
  Rf_setAttrib(tag, R_NamesSymbol, field_nms);
  Rf_classgets(tag, tag_class);

  SET_VECTOR_ELT(tag, 0, tagName);
  SET_STRING_ELT(field_nms, 0, chr_name);
  SET_VECTOR_ELT(tag, 1, attributes);
  SET_STRING_ELT(field_nms, 1, chr_attribs);
  SET_VECTOR_ELT(tag, 2, children);
  SET_STRING_ELT(field_nms, 2, chr_children);

  R_xlen_t field_i = 3;
  // Conditionally include the `.noWS` field.
  // We do this to avoid breaking the hashes of existing tags that weren't leveraging .noWS.
  if (noWS != R_NilValue) {
    SET_VECTOR_ELT(tag, field_i, noWS);
    SET_STRING_ELT(field_nms, field_i, chr_nows);
    ++field_i;
  }
  // Conditionally include the `.renderHooks` field.
  // We do this to avoid breaking the hashes of existing tags that weren't leveraging .renderHooks.
  if (renderHook != R_NilValue) {
    SET_STRING_ELT(field_nms, field_i, chr_renderhooks);
    if (TYPEOF(renderHook) == VECSXP) {
      SET_VECTOR_ELT(tag, field_i, renderHook);
    } else {
      SEXP renderHookList = PROTECT(Rf_allocVector(VECSXP, 1));
      SET_VECTOR_ELT(renderHookList, 0, renderHook);
      SET_VECTOR_ELT(tag, field_i, renderHookList);
      UNPROTECT(1);
    }
  }

  UNPROTECT(6);
  return tag;
}
