#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  class tensor_node: public cdk::expression_node {
    cdk::sequence_node *_elements;

  public:
    tensor_node(int lineno, cdk::sequence_node *elements)
      : cdk::expression_node(lineno), _elements(elements) {}

    cdk::sequence_node *elements() const {
      return _elements;
    }

    cdk::expression_node *element(size_t i) const {
      return dynamic_cast<cdk::expression_node*>(_elements->node(i));
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_node(this, level);
    }
  };

} // udf
