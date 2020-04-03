#include <Rinternals.h>

// A macro similar to SET_STRING_ELT, it assumes a string vector protected with
// PROTECT_WITH_INDEX, will automatically grow it if needed.
#define SET_STRING_ELT2(X, I, VAL, P_IDX) ({     \
  R_xlen_t len = Rf_xlength(X);                  \
  R_xlen_t i = I;                                \
  if (i >= len) {                                \
    len *= 2;                                    \
    REPROTECT(X = Rf_lengthgets(X, len), P_IDX); \
  }                                              \
  SET_STRING_ELT(X, i, VAL);                     \
})

Rboolean str_is_ASCII(const char *str) {
    const char *p;
    for(p = str; *p; p++) {
      if((unsigned int)*p > 0x7F) return FALSE;
    }
    return TRUE;
}

// Break template text into character vector. The first element element of the
// resulting vector is HTML, the next is R code, and they continue alternating.
// [[export]]
SEXP template_dfa(SEXP x_sxp) {
  typedef enum {
    html,
    code,
    html_oneOpenBracket,
    code_oneCloseBracket,
    code_string1,
    code_string1_backslash,
    code_string2,
    code_string2_backslash,
    code_backtick,
    code_backtick_backslash,
    code_percentOp,
    code_comment,
    code_comment_oneCloseBracket
  } State;

  if (Rf_xlength(x_sxp) != 1) {
    Rf_error("Input HTML must be a character vector of length 1");
  }

  SEXP input_sxp = STRING_ELT(x_sxp, 0);

  if (!(Rf_getCharCE(input_sxp) == CE_UTF8 || str_is_ASCII(CHAR(input_sxp)))) {
    Rf_warning("Input HTML must have a UTF-8 encoding");
  }

  SEXP str;
  SEXP pieces = Rf_allocVector(STRSXP, 10);
  R_xlen_t pieces_num = 0;
  PROTECT_INDEX pieces_idx;
  PROTECT_WITH_INDEX(pieces, &pieces_idx);

  const char* input = CHAR(input_sxp);

  int pieceStartIdx = 0;
  R_xlen_t len = Rf_xlength(input_sxp);
  char c;
  State state = html;
  for (R_xlen_t i=0; i < len; i++) {
    c = input[i];
    switch (state) {

    case html:
      switch (c) {
      case '{':
        state = html_oneOpenBracket; break;
      }
      break;

    case html_oneOpenBracket:
      switch (c) {
      case '{':
        state = code;
        str = PROTECT(Rf_mkCharLenCE(input + pieceStartIdx, i - pieceStartIdx - 1, CE_UTF8));
        SET_STRING_ELT2(pieces, pieces_num++, str, pieces_idx);
        UNPROTECT(1);
        pieceStartIdx = i + 1;
        break;
      default:
        state = html;
      }
      break;

    case code:
      switch (c) {
      case '}':
        state = code_oneCloseBracket; break;
      case '\'':
        state = code_string1; break;
      case '"':
        state = code_string2; break;
      case '`':
        state = code_backtick; break;
      case '%':
        state = code_percentOp; break;
      case '#':
        state = code_comment; break;
      }
      break;

    case code_oneCloseBracket:
      switch (c) {
      case '}':
        state = html;
        str = PROTECT(Rf_mkCharLenCE(input + pieceStartIdx, i - pieceStartIdx - 1, CE_UTF8));
        SET_STRING_ELT2(pieces, pieces_num++, str, pieces_idx);
        UNPROTECT(1);
        pieceStartIdx = i + 1;
        break;
      default: state = code;
      }
      break;

    case code_string1:
      switch (c) {
      case '\\':
        state = code_string1_backslash; break;
      case '\'':
        state = code; break;
      }
      break;

    case code_string1_backslash:
      state = code_string1;
      break;

    case code_string2:
      switch (c) {
      case '\\':
        state = code_string2_backslash; break;
      case '\"':
        state = code; break;
      }
      break;

    case code_string2_backslash:
      state = code_string2;
      break;

    case code_backtick:
      switch (c) {
      case '\\':
        state = code_backtick_backslash; break;
      case '`':
        state = code; break;
      }
      break;

    case code_backtick_backslash:
      state = code_backtick;
      break;

    case code_percentOp:
      switch (c) {
      case '%':
        state = code; break;
      }
      break;

    case code_comment:
      switch (c) {
      case '}':
        state = code_comment_oneCloseBracket; break;
      case '\n':
        state = code; break;
      }
      break;

    case code_comment_oneCloseBracket:
      switch (c) {
      case '}':
        state = html;
        str = PROTECT(Rf_mkCharLenCE(input + pieceStartIdx, i - pieceStartIdx - 1, CE_UTF8));
        SET_STRING_ELT2(pieces, pieces_num++, str, pieces_idx);
        UNPROTECT(1);
        pieceStartIdx = i + 1;
        break;
      default:
        state = code;
      }
      break;

    }
  }

  if (!(state == html || state == html_oneOpenBracket)) {
    Rf_error("HTML template did not end in html state (missing closing \"}}\").");
  }

  // Add ending HTML piece
  str = PROTECT(Rf_mkCharLenCE(input + pieceStartIdx, len - pieceStartIdx, CE_UTF8));
  SET_STRING_ELT2(pieces, pieces_num++, str, pieces_idx);
  UNPROTECT(1);

  if (pieces_num < Rf_xlength(pieces)) {
    // Using SETLENGTH and SET_TRUELENGTH in this way allows us to resize the
    // vector without an extra copy.
    SETLENGTH(pieces, pieces_num);
    SET_TRUELENGTH(pieces, pieces_num);
  }

  UNPROTECT(1);

  return pieces;
}
