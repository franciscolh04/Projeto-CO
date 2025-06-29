%option c++ prefix="udf_scanner_" outfile="udf_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{
// make relevant includes before including the parser's tab file
#include <string>
#include <cstring>
#include <vector>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "udf_parser.tab.h"

// output stream for building string literals
static std::ostringstream strlit;

// don't change this
#define yyerror LexerError
%}

SPACE     ([ \t\r]|\n)
INT       [[:digit:]]+

%x X_COMMENT
%x X_STRING X_BACKSLASH

%%
  yydebug=1; set_debug(1);

  /* ====================================================================== */
  /* ====[                       COMMENTS                             ]==== */
  /* ====================================================================== */

"//".*$                /* line comment (it is ignored) */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|"\n"        /* ignore comment content */

  /* ====================================================================== */
  /* ====[                   DATA TYPES & NULL POINTER                ]==== */
  /* ====================================================================== */

"auto"                   return tTYPE_AUTO;
"int"                    return tTYPE_INT;
"real"                   return tTYPE_REAL;
"string"                 return tTYPE_STRING;
"ptr"                    return tTYPE_POINTER;
"tensor"                 return tTYPE_TENSOR;
"void"                   return tTYPE_VOID;

"nullptr"                return tNULLPTR;
"sizeof"                 return tSIZEOF;

  /* ====================================================================== */
  /* ====[                   GLOBAL SYMBOLS                           ]==== */
  /* ====================================================================== */

"public"                 return tPUBLIC;
"forward"                return tFORWARD;

  /* ====================================================================== */
  /* ====[                   CONTROL STRUCTURES                       ]==== */
  /* ====================================================================== */

"if"                     return tIF;
"elif"                   return tELIF;
"else"                   return tELSE;
"for"                    return tFOR;
"break"                  return tBREAK;
"continue"               return tCONTINUE;
"return"                 return tRETURN;

  /* ====================================================================== */
  /* ====[                   OPERATORS                                ]==== */
  /* ====================================================================== */

"&&"                     return tAND;
"||"                     return tOR;
"=="                     return tEQ;
"!="                     return tNE;
"<="                     return tLE;
">="                     return tGE;
"**"                     return tCONTRACT;
"objects"                return tMEM_ALLOC;
"input"                  return tINPUT;

[\[\](){};,.:@?=<>+*/%\-~#!] return *yytext;

  /* ====================================================================== */
  /* ====[                   TENSOR OPERATIONS                      ]==== */
  /* ====================================================================== */

"capacity"               return tCAPACITY;
"rank"                   return tRANK;
"dims"                   return tDIMS;
"dim"                    return tDIM;
"reshape"                return tRESHAPE;

  /* ====================================================================== */
  /* ====[                   I/O OPERATIONS                           ]==== */
  /* ====================================================================== */

"write"                  return tWRITE;
"writeln"                return tWRITELN;

  /* ====================================================================== */
  /* ====[                   INTEGER LITERALS                         ]==== */
  /* ====================================================================== */

0[xX][A-Fa-f0-9]+             { 
                                try { 
                                  yylval.i = std::stoi(yytext, nullptr, 16);
                                  return tINTEGER;
                                } catch (std::out_of_range& e) { 
                                  yyerror("Error: hexadecimal number exceeds allowed range.");
                                }
                              }

[0-9]+                        {  
                                try { 
                                  yylval.i = std::stoi(yytext, nullptr, 10);
                                  return tINTEGER;
                                } catch (std::out_of_range& e) { 
                                  yyerror("Error: integer value out of range.");
                                }
                              }

0[xX][0-9a-zA-Z]+             { 
                                yyerror("Error: invalid hexadecimal number.");
                              }

  /* ====================================================================== */
  /* ====[                   REAL LITERALS                            ]==== */
  /* ====================================================================== */

[0-9]+"."                   { 
                              try { 
                                yylval.d = std::stod(yytext, nullptr);
                                return tREAL; 
                              } catch (std::out_of_range& e) { 
                                yyerror("Error: real number out of range."); 
                              } 
                            }

"."[0-9]+                   { 
                              try { 
                                yylval.d = std::stod(yytext, nullptr);
                                return tREAL; 
                              } catch (std::out_of_range& e) { 
                                yyerror("Error: real number out of range."); 
                              } 
                            }

[0-9]+"."[0-9]+             { 
                              try { 
                                yylval.d = std::stod(yytext, nullptr);
                                return tREAL; 
                              } catch (std::out_of_range& e) { 
                                yyerror("Error: real number out of range."); 
                              } 
                            }

[0-9]+("."[0-9]+)?[eE][-+]?[0-9]+  { 
                              try { 
                                yylval.d = std::stod(yytext, nullptr);
                                return tREAL; 
                              } catch (std::out_of_range& e) { 
                                yyerror("Error: real number out of range."); 
                              } 
                            }

  /* ====================================================================== */
  /* ====[                   IDENTIFIERS                              ]==== */
  /* ====================================================================== */

[A-Za-z][A-Za-z0-9_]*       { 
                              yylval.s = new std::string(yytext); 
                              return tID; 
                            }

  /* ====================================================================== */
  /* ====[                   STRING LITERALS                          ]==== */
  /* ====================================================================== */

"\""                     yy_push_state(X_STRING);
<X_STRING>\\             yy_push_state(X_BACKSLASH);
<X_STRING>"\""           {
                            yylval.s = new std::string(strlit.str());
                            strlit.str("");
                            yy_pop_state();
                            return tSTRING;
                         }
<X_STRING>.              strlit << *yytext;

<X_BACKSLASH>n           { strlit << '\n'; yy_pop_state(); }
<X_BACKSLASH>r           { strlit << '\r'; yy_pop_state(); }
<X_BACKSLASH>t           { strlit << '\t'; yy_pop_state(); }
<X_BACKSLASH>a           { strlit << '\n'; yy_pop_state(); }
<X_BACKSLASH>\\          { strlit << '\\'; yy_pop_state(); }
<X_BACKSLASH>\"          { strlit << '\"'; yy_pop_state(); }

<X_BACKSLASH>x[[:xdigit:]]{1,2} {
                           strlit << (char)strtoul(yytext+1, NULL, 16);
                           yy_pop_state();
                         }

<X_BACKSLASH>[0-9A-Fa-f]{2} {
                          strlit << static_cast<char>(strtol(yytext, nullptr, 16));
                          yy_pop_state();
}
                         
<X_BACKSLASH>.           {
                           strlit << *yytext;
                           yy_pop_state();
                         }

  /* ====================================================================== */
  /* ====[                   WHITESPACE                               ]==== */
  /* ====================================================================== */

[[:space:]]         /* ignore remaining white space inside the program */
.                      yyerror(yytext);    /* all other chars: error! */

  /* ====================================================================== */
  /* ====[                           THE END                          ]==== */
  /* ====================================================================== */

%%