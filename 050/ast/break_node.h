#pragma once

#include <cdk/ast/basic_node.h>

namespace udf {

  class break_node: public cdk::basic_node {

  public:
    break_node(int lineno) :
        cdk::basic_node(lineno) {
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_break_node(this, level); }

  };

} // udf
