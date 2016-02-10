#include <Rcpp.h>
using namespace Rcpp;

// Break template text into character vector. The first element element of the
// resulting vector is HTML, the next is R code, and they continue alternating.
// [[Rcpp::export]]
std::vector<std::string> template_dfa(CharacterVector x) {
  enum State {
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
    code_comment,
    code_comment_oneCloseBracket
  };

  if (x.length() != 1) {
    stop("Input HTML must be a character vector of length 1");
  }
  std::string input = Rcpp::as<std::string>(x[0]);
  std::vector<std::string> pieces(0);

  int pieceStartIdx = 0;
  int len = input.length();
  char c;
  State state = html;
  for (int i=0; i < len; i++) {
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
        pieces.push_back(input.substr(pieceStartIdx, i - pieceStartIdx - 1));
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
      case '#':
        state = code_comment; break;
      }
      break;

    case code_oneCloseBracket:
      switch (c) {
      case '}':
        state = html;
        pieces.push_back(input.substr(pieceStartIdx, i - pieceStartIdx - 1));
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
        pieces.push_back(input.substr(pieceStartIdx, i - pieceStartIdx - 1));
        pieceStartIdx = i + 1;
        break;
      default:
        state = code;
      }
      break;

    }
  }

  if (!(state == html || state == html_oneOpenBracket)) {
    stop("HTML template did not end in html state (missing closing \"}}\").");
  }

  // Add ending HTML piece
  pieces.push_back(input.substr(pieceStartIdx, len - pieceStartIdx));

  return pieces;
}
