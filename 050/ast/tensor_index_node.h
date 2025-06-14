#pragma once

#include <cdk/ast/lvalue_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  class tensor_index_node : public cdk::lvalue_node {
    cdk::expression_node *_tensor;
    cdk::sequence_node *_indexes;

  public:
    tensor_index_node(int lineno, cdk::expression_node *tensor, cdk::sequence_node *indexes)
        : cdk::lvalue_node(lineno), _tensor(tensor), _indexes(indexes) {
    }

    cdk::expression_node *tensor() const {
      return _tensor;
    }

    cdk::sequence_node *indexes() const {
      return _indexes;
    }

    cdk::expression_node *index(size_t i) const {
      return dynamic_cast<cdk::expression_node*>(_indexes->node(i));
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_index_node(this, level);
    }
  };

} // udf