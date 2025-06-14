#pragma once

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <set>
#include <stack>

namespace udf {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<udf::symbol> &_symtab;
    bool _errors, _inFunction, _inFunctionName, _inFunctionArgs, _inFunctionBody;
    bool _inForInit;
    bool _returnSeen; // when building a function
    std::shared_ptr<udf::symbol> _function; // for keeping track of the current function and its arguments
    int _offset;
    cdk::typename_type _lvalueType;
    std::string _currentBodyRetLabel; // where to jump when a return occurs of an exclusive section ends
    cdk::basic_postfix_emitter &_pf;
    int _lbl;

    std::set<std::string> _functions_to_declare;
    std::stack<int> _forIni, _forStep, _forEnd; // for break/repeat


  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<udf::symbol> &symtab, cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _errors(false), _inFunction(false), _inFunctionName(false), _inFunctionArgs(
            false), _inFunctionBody(false), _returnSeen(), _function(nullptr), _offset(0), _lvalueType(
            cdk::TYPE_VOID), _currentBodyRetLabel(""), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    void error(int lineno, std::string s) {
      std::cerr << "error: " << lineno << ": " << s << std::endl;
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // udf
