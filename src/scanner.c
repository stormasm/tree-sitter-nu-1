#include "tree_sitter/parser.h"
#include <stdio.h>
#include <wctype.h>

enum TokenType {
  FLOAT,
};

void *tree_sitter_nu_external_scanner_create() { return NULL; }
void tree_sitter_nu_external_scanner_destroy(void *p) {}
void tree_sitter_nu_external_scanner_reset(void *p) {}
unsigned tree_sitter_nu_external_scanner_serialize(void *p, char *buffer) {
  return 0;
}
void tree_sitter_nu_external_scanner_deserialize(void *p, const char *b,
                                                 unsigned n) {}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static bool is_numeric(int32_t c) { return c == '_' || iswdigit(c); }

bool scan_float(TSLexer *lexer) {
  bool has_fraction = false, has_exponent = false;
  lexer->result_symbol = FLOAT;

  advance(lexer);

  while (is_numeric(lexer->lookahead))
    advance(lexer);

  if (lexer->lookahead == '.') {
    has_fraction = true;
    advance(lexer);

    if (iswalpha(lexer->lookahead))
      return false;

    if (lexer->lookahead == '.')
      return false;

    while (is_numeric(lexer->lookahead))
      advance(lexer);
  }

  if (lexer->lookahead == 'e' || lexer->lookahead == 'E') {
    has_exponent = true;
    advance(lexer);

    if (lexer->lookahead == '+' || lexer->lookahead == '-')
      advance(lexer);

    if (!is_numeric(lexer->lookahead))
      return true;

    advance(lexer);

    while (is_numeric(lexer->lookahead))
      advance(lexer);
  }

  lexer->mark_end(lexer);

  return (has_fraction || has_exponent);
}

bool tree_sitter_nu_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
  if (valid_symbols[FLOAT] && iswdigit(lexer->lookahead)) {
    return scan_float(lexer);
  }

  return false;
}
