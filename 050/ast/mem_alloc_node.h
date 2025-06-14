#pragma once

#include <cdk/ast/unary_operation_node.h>
#include <cdk/ast/expression_node.h>

namespace udf {

  class mem_alloc_node: public cdk::unary_operation_node {
  public:
    mem_alloc_node(int lineno, cdk::expression_node *argument) :
        cdk::unary_operation_node(lineno, argument) {
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_mem_alloc_node(this, level);
    }

  };

} // udf
