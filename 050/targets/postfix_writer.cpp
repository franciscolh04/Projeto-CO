#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "udf_parser.tab.h"

//---------------------------------------------------------------------------

static bool mem_init = false;

void udf::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void udf::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void udf::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.DOUBLE(node->value()); // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());    // double is on the DATA segment
  }
}
void udf::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void udf::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void udf::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

void udf::postfix_writer::do_block_node(udf::block_node * const node, int lvl) {
  _symtab.push(); // for block-local vars
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value()); // integer literal is on the stack: push an integer
  } else {
    _pf.SINT(node->value()); // integer literal is on the DATA segment
  }
}

void udf::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;
  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters
  if (_function) {
    // local variable initializer
    _pf.TEXT();
    _pf.ADDR(mklbl(lbl1));
  } else {
    // global variable initializer
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // process node data
  _pf.NEG(); // 
}

void udf::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // process node data
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;

  //tensor + tensor 
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_add");
    _pf.CALL("tensor_add");
    _pf.TRASH(8); 
    _pf.LDFVAL32(); 
    return;
  }

  // tensor + scalar 
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && ((node->right()->type()->name() == cdk::TYPE_INT) || (node->right()->type()->name() == cdk::TYPE_DOUBLE))) {
    node->right()->accept(this, lvl + 2);
    if(node->right()->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    node->left()->accept(this, lvl + 2);

    _functions_to_declare.insert("tensor_add_scalar");
    _pf.CALL("tensor_add_scalar");
    _pf.TRASH(12); 
    _pf.LDFVAL32(); 
    return;
  }

  //scalar + tensor 
  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    if (node->left()->is_typed(cdk::TYPE_INT)) {
      node->left()->accept(this, lvl + 2);
      _pf.I2D();
    } else {
      node->left()->accept(this, lvl + 2);
    }
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_add_scalar");
    _pf.CALL("tensor_add_scalar");
    _pf.TRASH(12); 
    _pf.LDFVAL32(); 
    return;
  }

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DADD();
  else
    _pf.ADD();
}


void udf::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  //tensor - tensor 
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_sub");
    _pf.CALL("tensor_sub");
    _pf.TRASH(8); 
    _pf.LDFVAL32(); 
    return;
  }

  // tensor - scalar 
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && ((node->right()->type()->name() == cdk::TYPE_INT) 
      || (node->right()->type()->name() == cdk::TYPE_DOUBLE))) {
    node->right()->accept(this, lvl + 2);
    if (node->right()->type()->name() == cdk::TYPE_INT) 
    _pf.I2D();
    node->left()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_sub_scalar");
    _pf.CALL("tensor_sub_scalar");
    _pf.TRASH(8); 
    _pf.LDFVAL32(); 
    return;
  }

  //scalar - tensor 
  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
       node->right()->type()->name() == cdk::TYPE_TENSOR) {
    if (node->left()->is_typed(cdk::TYPE_INT)) {
      node->left()->accept(this, lvl + 2);
      _pf.I2D();
    } else {
      node->left()->accept(this, lvl + 2);
    }
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_sub_scalar");
    _pf.CALL("tensor_sub_scalar");
    _pf.TRASH(12); 
    _pf.LDFVAL32(); 
    return;
  }

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DSUB();
  else
    _pf.SUB();
}

void udf::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;

  //tensor * tensor 
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_mul");
    _pf.CALL("tensor_mul");
    _pf.TRASH(8); 
    _pf.LDFVAL32(); 
    return;
  }

  // tensor * scalar 
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && ((node->right()->is_typed(cdk::TYPE_INT)) || (node->right()->is_typed(cdk::TYPE_DOUBLE)))) {

    node->right()->accept(this, lvl + 2); 

    if(node->right()->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    node->left()->accept(this, lvl + 2);

    _functions_to_declare.insert("tensor_mul_scalar");
    _pf.CALL("tensor_mul_scalar");
    _pf.TRASH(12); 
    _pf.LDFVAL32(); 
    return;
  }

  //scalar * tensor 
  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    if (node->left()->is_typed(cdk::TYPE_INT)) {
      node->left()->accept(this, lvl + 2);
      _pf.I2D();
    } else {
      node->left()->accept(this, lvl + 2);
    }
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_mul_scalar");
    _pf.CALL("tensor_mul_scalar");
    _pf.TRASH(12); 
    _pf.LDFVAL32(); 
    return;
  }

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->is_typed(cdk::TYPE_INT)) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->is_typed(cdk::TYPE_INT)) _pf.I2D();

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DMUL();
  else
    _pf.MUL();
}

void udf::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;

  //tensor / tensor case
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_div");
    _pf.CALL("tensor_div");
    _pf.TRASH(8); 
    _pf.LDFVAL32(); 
    return;
  }

  // tensor / scalar case
  if (node->left()->type()->name() == cdk::TYPE_TENSOR && ((node->right()->type()->name() == cdk::TYPE_INT) || (node->right()->type()->name() == cdk::TYPE_DOUBLE))) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    if (node->right()->type()->name() == cdk::TYPE_INT) 
      _pf.I2D();
    _functions_to_declare.insert("tensor_div_scalar");
    _pf.CALL("tensor_div_scalar");
    _pf.TRASH(8); 
    _pf.LDFVAL32(); 
    return;
  }

  //scalar / tensor case
  if ((node->left()->type()->name() == cdk::TYPE_INT || node->left()->type()->name() == cdk::TYPE_DOUBLE) && node->right()->type()->name() == cdk::TYPE_TENSOR) {
    node->right()->accept(this, lvl + 2);
    node->left()->accept(this, lvl + 2);
    if (node->left()->type()->name() == cdk::TYPE_INT) 
      _pf.I2D();
    _functions_to_declare.insert("tensor_div_scalar");
    _pf.CALL("tensor_div_scalar");
    _pf.TRASH(12); 
    _pf.LDFVAL32(); 
    return;
  }

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DDIV();
  else
    _pf.DIV();
}
void udf::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void udf::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LT();
}
void udf::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LE();
}
void udf::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GE();
}
void udf::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GT();
}
void udf::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.NE();
}
void udf::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol->global()) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
    //std::cerr << "LVAL " << symbol->name() << ":" << symbol->type()->size() << ":" << symbol->offset() << std::endl;
  }
 
}

void udf::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;
  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, and strings
    _pf.LDINT();
  }
}

void udf::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;

  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_evaluation_node(udf::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
}

void udf::postfix_writer::do_print_node(udf::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t ix = 0; ix < node->argument()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->argument()->node(ix));

    std::shared_ptr<cdk::basic_type> etype = child->type();
    child->accept(this, lvl); // expression to print

    if (etype->name() == cdk::TYPE_INT) {
      _functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    } else if (etype->name() == cdk::TYPE_DOUBLE) {
      _functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    } else if (etype->name() == cdk::TYPE_STRING) {
      _functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    } else if (etype->name() == cdk::TYPE_TENSOR) {
      _functions_to_declare.insert("tensor_print");
      _pf.CALL("tensor_print");
      _pf.TRASH(4); // trash tensor pointer
    } else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }

  }

  if (node->newline()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_for_node(udf::for_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;

  _forIni.push(++_lbl); // after init, before body
  _forStep.push(++_lbl); // after intruction
  _forEnd.push(++_lbl); // after for

  os() << "        ;; FOR initialize" << std::endl;
  // initialize: be careful with variable declarations:
  // they are done here, but the space is occupied in the function
  _symtab.push();
  _inForInit = true;  // remember this for local declarations

  // initialize
  node->init()->accept(this, lvl + 2);

  os() << "        ;; FOR test" << std::endl;
  // prepare to test
  _pf.ALIGN();
  _pf.LABEL(mklbl(_forIni.top()));
  node->condition()->accept(this, lvl + 2);
  _pf.JZ(mklbl(_forEnd.top()));

  os() << "        ;; FOR instruction" << std::endl;
  // execute instruction
  node->block()->accept(this, lvl + 2);

  os() << "        ;; FOR increment" << std::endl;
  // prepare to increment
  _pf.ALIGN();
  _pf.LABEL(mklbl(_forStep.top()));
  node->step()->accept(this, lvl + 2);
  //DAVID FIXME trash
  os() << "        ;; FOR jump to test" << std::endl;
  _pf.JMP(mklbl(_forIni.top()));

  os() << "        ;; FOR end" << std::endl;
  _pf.ALIGN();
  _pf.LABEL(mklbl(_forEnd.top()));

  _inForInit = false;  // remember this for local declarations
  _symtab.pop();

  _forIni.pop();
  _forStep.pop();
  _forEnd.pop();
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_continue_node(udf::continue_node * const node, int lvl) {
  if (_forIni.size() != 0) {
    _pf.JMP(mklbl(_forStep.top())); // jump to next cycle
  } else
    error(node->lineno(), "'restart' outside 'for'");
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_break_node(udf::break_node * const node, int lvl) {
  if (_forIni.size() != 0) {
    _pf.JMP(mklbl(_forEnd.top())); // jump to for end
  } else
    error(node->lineno(), "'break' outside 'for'");
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_if_node(udf::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_if_else_node(udf::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//===========================================================================

void udf::postfix_writer::do_function_call_node(udf::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->identifier());

  size_t argsSize = 0;
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, lvl + 2);
      if (symbol->argument_is_typed(ax, cdk::TYPE_DOUBLE) && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      argsSize += symbol->argument_size(ax);
    }
  }
  _pf.CALL(node->identifier());
  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_POINTER) || symbol->is_typed(cdk::TYPE_STRING)) {
    _pf.LDFVAL32();
  } else if (symbol->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else {
    std::cerr << node->lineno() << ": should not happen: unknown type" << std::endl;
  }
} 

void udf::postfix_writer::do_function_declaration_node(udf::function_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (_inFunctionBody || _inFunctionArgs) {
    error(node->lineno(), "cannot declare function in body or in args");
    return;
  }

  if (!new_symbol()) return;

  auto function = new_symbol();
  _functions_to_declare.insert(function->name());
  reset_new_symbol();
}

void udf::postfix_writer::do_function_definition_node(udf::function_definition_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (_inFunctionBody || _inFunctionArgs) {
    error(node->lineno(), "cannot define function in body or in arguments");
    return;
  }

  // remember symbol so that args and body know
  _function = new_symbol();
  _functions_to_declare.erase(_function->name());  // just in case
  reset_new_symbol();

  _currentBodyRetLabel = mklbl(++_lbl);

  _offset = 8; // prepare for arguments (4: remember to account for return address)
  _symtab.push(); // scope of args

  if (node->arguments()->size() > 0) {
    _inFunctionArgs = true; //FIXME really needed?
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
      cdk::basic_node *arg = node->arguments()->node(ix);
      if (arg == nullptr) break; // this means an empty sequence of arguments
      arg->accept(this, 0); // the function symbol is at the top of the stack
    }
    _inFunctionArgs = false; //FIXME really needed?
  }

  _pf.TEXT();
  _pf.ALIGN();
  if (node->qualifier() == tPUBLIC) _pf.GLOBAL(_function->name(), _pf.FUNC());
  _pf.LABEL(_function->name());

  if (!mem_init && node->identifier() == "udf") {
    _functions_to_declare.insert("mem_init");
    _pf.CALL("mem_init");
    mem_init = true;
  }

  // compute stack size to be reserved for local variables
  frame_size_calculator lsc(_compiler, _symtab, _function);
  node->accept(&lsc, lvl);
  _pf.ENTER(lsc.localsize()); // total stack size reserved for local variables

  _offset = 0; // prepare for local variable

  // the following flag is a slight hack: it won't work with nested functions
  _inFunctionBody = true;
  os() << "        ;; before body " << std::endl;
  node->block()->accept(this, lvl + 4); // block has its own scope
  os() << "        ;; after body " << std::endl;
  _inFunctionBody = false;

  if(!_returnSeen){
    _pf.LEAVE();
    _pf.RET();
  }
  _returnSeen = false;

  _symtab.pop(); // scope of arguments

  if (node->identifier() == "udf") {
    // declare external functions
    for (std::string s : _functions_to_declare)
      _pf.EXTERN(s);
  }
}

void udf::postfix_writer::do_index_node(udf::index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;
  if (node->base()) {
    node->base()->accept(this, lvl);
  } else {
    if (_function) {
      _pf.LOCV(-_function->type()->size());
    } else {
      std::cerr << "FATAL: " << node->lineno() << ": trying to use return value outside function" << std::endl;
    }
  }
  node->index()->accept(this, lvl);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ADD(); // add pointer and index
}

void udf::postfix_writer::do_input_node(udf::input_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (_lvalueType == cdk::TYPE_DOUBLE) {
    _functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else if (_lvalueType == cdk::TYPE_INT) {
    _functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else {
    std::cerr << "FATAL: " << node->lineno() << ": cannot read type" << std::endl;
    return;
  }
}

void udf::postfix_writer::do_nullptr_node(udf::nullptr_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ; // a pointer is a 32-bit integer
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

void udf::postfix_writer::do_return_node(udf::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS; 
  
   // should not reach here without returning a value (if not void)
  if (_function->type()->name() != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);

    if (_function->type()->name() == cdk::TYPE_INT || _function->type()->name() == cdk::TYPE_STRING
        || _function->type()->name() == cdk::TYPE_POINTER) {
      _pf.STFVAL32();
    } else if (_function->type()->name() == cdk::TYPE_DOUBLE) {
      if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
      _pf.STFVAL64();
    } else if (_function->type()->name() == cdk::TYPE_TENSOR) {
      _pf.STFVAL32(); //pointer to tensor
    } else {
      std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
    }
  }

  _pf.LEAVE(); // restore stack pointer
  _pf.RET(); // return to caller
}

void udf::postfix_writer::do_sizeof_node(udf::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->expression()->type()->name() == cdk::TYPE_TENSOR) {
    node->expression()->accept(this, lvl + 2);

    _functions_to_declare.insert("tensor_size");
    _pf.CALL("tensor_size");

    _pf.TRASH(4); 
    _pf.LDFVAL32();
    _pf.INT(3);       
    _pf.SHTL(); 
  } else {
    _pf.INT(node->expression()->type()->size()); 
  }
}

void udf::postfix_writer::do_variable_declaration_node(udf::variable_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto id = node->identifier();

  std::cout << "INITIAL OFFSET: " << _offset << std::endl;

  // type size?
  int offset = 0, typesize = node->type()->size(); // in bytes
  std::cout << "ARG: " << id << ", " << typesize << std::endl;
  if (_inFunctionBody) {
    std::cout << "IN BODY" << std::endl;
    _offset -= typesize; // local variable, so we reserve space in the stack
    offset = _offset;
  } else if (_inFunctionArgs) {
    std::cout << "IN ARGS" << std::endl;
    offset = _offset;
    _offset += typesize;
  } else {
    std::cout << "GLOBAL!" << std::endl;
    offset = 0; // global variable
  }
  std::cout << "OFFSET: " << id << ", " << offset << std::endl;

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset); // remember offset in the symbol table
    reset_new_symbol();
  }

  if (_inFunctionBody) {
    // if we are dealing with local variables, then no action is needed
    // unless an initializer exists
    if (node->initializer()) {
      node->initializer()->accept(this, lvl);
      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_TENSOR)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->initializer()->is_typed(cdk::TYPE_INT))
          _pf.I2D();
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } 
    } else{
      if (node->is_typed(cdk::TYPE_TENSOR)) {
        _pf.TEXT();
        auto dimensions = (std::dynamic_pointer_cast<cdk::tensor_type>(node->type()))->dims();
        if (dimensions.size() > 0) {
          for (int i = dimensions.size() - 1; i >= 0; --i) {
            size_t dim = dimensions[i];
            _pf.INT(dim);
          }
        }
        // create tensor with dimensions
        _pf.INT(dimensions.size()); 
        _functions_to_declare.insert("tensor_create");
        _pf.CALL("tensor_create");
        _pf.TRASH((dimensions.size()+1)*4); // trash the tensor pointer and dimensions
        _pf.LDFVAL32();
        _pf.LOCAL(symbol->offset()); // store the pointer to the tensor
        _pf.STINT();
      }
    } 
  } else {
    if (!_function) {
      if (node->initializer() == nullptr) {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(id);
        _pf.SALLOC(typesize);
      } else {

        if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_POINTER)) {
          if (node->constant()) {
            _pf.RODATA();
          } else {
            _pf.DATA();
          }
          _pf.ALIGN();
          _pf.LABEL(id);

          if (node->is_typed(cdk::TYPE_INT)) {
            node->initializer()->accept(this, lvl);
          } else if (node->is_typed(cdk::TYPE_POINTER)) {
            node->initializer()->accept(this, lvl);
          } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
            if (node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
              node->initializer()->accept(this, lvl);
            } else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
              cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node->initializer());
              cdk::double_node ddi(dclini->lineno(), dclini->value());
              ddi.accept(this, lvl);
            } else {
              std::cerr << node->lineno() << ": '" << id << "' has bad initializer for real value\n";
              _errors = true;
            }
          }
        } else if (node->is_typed(cdk::TYPE_STRING)) {
          if (node->constant()) {
            int litlbl;
            // HACK!!! string literal initializers must be emitted before the string identifier
            _pf.RODATA();
            _pf.ALIGN();
            _pf.LABEL(mklbl(litlbl = ++_lbl));
            _pf.SSTRING(dynamic_cast<cdk::string_node*>(node->initializer())->value());
            _pf.ALIGN();
            _pf.LABEL(id);
            _pf.SADDR(mklbl(litlbl));
          } else {
            _pf.DATA();
            _pf.ALIGN();
            _pf.LABEL(id);
            node->initializer()->accept(this, lvl);
          }
        } else {
          std::cerr << node->lineno() << ": '" << id << "' has unexpected initializer\n";
          _errors = true;
        }

      }
    }
  }
}

void flatten_tensor_elements(cdk::sequence_node *elements, std::vector<cdk::expression_node*> &out) {
  for (size_t i = 0; i < elements->size(); ++i) {
    auto element = dynamic_cast<cdk::expression_node*>(elements->node(i)); 
    if (auto tensor = dynamic_cast<udf::tensor_node*>(element)) {
      flatten_tensor_elements(tensor->elements(), out); 
    } else {
      out.push_back(element); // add the element to the flat list
    }
  }
}

cdk::sequence_node* flattened_elements(udf::tensor_node *node) {
  std::vector<cdk::expression_node*> flat_elements;
  flatten_tensor_elements(node->elements(), flat_elements);
  auto seq = new cdk::sequence_node(node->lineno());
  for (auto *expr : flat_elements)
    seq->nodes().push_back(expr); // add the flattened elements to the sequence
  return seq;
}

void udf::postfix_writer::do_tensor_node(udf::tensor_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _pf.TEXT();
  auto dimensions = (std::dynamic_pointer_cast<cdk::tensor_type>(node->type()))->dims();

  if (dimensions.size() > 0) {
    for (int i = dimensions.size() - 1; i >= 0; --i) {  // push dimensions in reverse order
      size_t dim = dimensions[i];
      _pf.INT(dim);
    }
  }
  _pf.INT(dimensions.size());
  _functions_to_declare.insert("tensor_create");
  _pf.CALL("tensor_create");
  _pf.TRASH((dimensions.size()+1)* 4 );
  _pf.LDFVAL32();
  
  auto flat_elements = flattened_elements(node);  // flatten the elements of the tensor
  
  _functions_to_declare.insert("tensor_put");
  for (size_t i = 0; i < flat_elements->size(); i++) {
    _pf.INT(i);
    
    flat_elements->node(i)->accept(this, lvl + 2);
    _pf.I2D(); // convert int to double if needed
    
    _pf.CALL("tensor_put");
    _pf.TRASH(12);
  }
}

void udf::postfix_writer::do_tensor_capacity_node(udf::tensor_capacity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->tensor()->accept(this, lvl + 2);
  _functions_to_declare.insert("tensor_size");
  _pf.CALL("tensor_size");
  _pf.TRASH(4); // trash the pointer to the tensor
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_rank_node(udf::tensor_rank_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->tensor()->accept(this, lvl + 2);

  _functions_to_declare.insert("tensor_get_n_dims");
  _pf.CALL("tensor_get_n_dims");
  _pf.TRASH(4); // trash the pointer to the tensor
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_dim_node(udf::tensor_dim_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->index()->accept(this, lvl + 2);
  node->tensor()->accept(this, lvl + 2);

  _functions_to_declare.insert("tensor_get_dim_size");
  _pf.CALL("tensor_get_dim_size");
  _pf.TRASH(8); // trash the index and the tensor pointer
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_index_node(udf::tensor_index_node * const node, int lvl) { 
  ASSERT_SAFE_EXPRESSIONS;

  node->indexes()->accept(this, lvl + 2);
  node->tensor()->accept(this, lvl);

  _functions_to_declare.insert("tensor_getptr");
  _pf.CALL("tensor_getptr");
  _pf.TRASH((node->indexes()->size()*4) + 4); // trash the indexes and the tensor pointer
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_reshape_node(udf::tensor_reshape_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (int i = node->dimensions()->size() - 1; i >= 0; --i) {
    auto element = dynamic_cast<cdk::expression_node*>(node->dimensions()->node(i));
    if (!element) throw std::string("expected expression node in sizes sequence");
    element->accept(this, lvl + 2);
  }

  size_t num_dims = node->dimensions()->size();
  _pf.INT(num_dims);
  node->tensor()->accept(this, lvl + 2);
  _functions_to_declare.insert("tensor_reshape"); // declare the function to reshape a tensor
  _pf.CALL("tensor_reshape");
  _pf.TRASH((num_dims + 2) * 4); // trash the tensor pointer and dimensions
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_contraction_node(udf::tensor_contraction_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->right()->accept(this, lvl + 2);
  node->left()->accept(this, lvl + 2);

  _functions_to_declare.insert("tensor_matmul");
  _pf.CALL("tensor_matmul");
  _pf.TRASH(8); // 4 for left tensor, 4 for right tensor
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_address_of_node(udf::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;
  // since the argument is an lvalue, it is already an address
  node->lvalue()->accept(this, lvl + 2);
}

void udf::postfix_writer::do_mem_alloc_node(udf::mem_alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  ;
  node->argument()->accept(this, lvl);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ALLOC(); // allocate
  _pf.SP(); // put base pointer in stack
}

void udf::postfix_writer::do_tensor_dims_node(udf::tensor_dims_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->tensor()->accept(this, lvl + 2);
  _functions_to_declare.insert("tensor_get_dims");
  _pf.CALL("tensor_get_dims");
  _pf.TRASH(4);  // trash the pointer to the tensor
  _pf.LDFVAL32();
}

