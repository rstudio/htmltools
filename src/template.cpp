#include <Rcpp.h>
using namespace Rcpp;

// Positive substring: return a substring of `str``going from `start` to `end`;
// if `end` is earlier than `start`, return empty string.
std::string pos_substr(std::string str, int start, int end) {
  if (end < start)
    return "";
  else
    return str.substr(start, end - start + 1);
}

// Break template text into character vector. The first element element of the
// resulting vector is HTML, the next is R code, and they continue alternating.
// [[Rcpp::export]]
CharacterVector template_dfa(CharacterVector x) {
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
    code_backslash
  };

  if (x.length() != 1) {
    stop("Input HTML must be a character vector of length 1");
  }
  std::string input = Rcpp::as<std::string>(x[0]);
  CharacterVector pieces(0);

  int i = 0;
  int pieceStartIdx = 0;
  int len = input.length();
  char c;
  State state = html;
  while (i < len) {
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
        pieces.push_back(pos_substr(input, pieceStartIdx, i-2));
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
      case '\\':
        state = code_backslash; break;
      }
      break;

    case code_oneCloseBracket:
      switch (c) {
      case '}':
        state = html;
        pieces.push_back(pos_substr(input, pieceStartIdx, i-2));
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

    case code_backslash:
      state = code;
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
    }

    i++;
  }

  if (!(state == html || state == html_oneOpenBracket)) {
    stop("HTML template did not end in html state (missing closing \"}}\").");
  }

  // Add ending HTML piece
  pieces.push_back(pos_substr(input, pieceStartIdx, i-1));

  return pieces;
}
