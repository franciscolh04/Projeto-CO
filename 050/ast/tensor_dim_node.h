#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  class tensor_dim_node : public cdk::expression_node {
    cdk::expression_node *_tensor;
    cdk::expression_node *_index;

  public:

    tensor_dim_node(int lineno, cdk::expression_node *tensor, cdk::expression_node *index)
        : cdk::expression_node(lineno), _tensor(tensor), _index(index) {
    }

    cdk::expression_node *tensor() const {
      return _tensor;
    }

    cdk::expression_node *index() const {
      return _index;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_dim_node(this, level);
    }
  };

} // udf
