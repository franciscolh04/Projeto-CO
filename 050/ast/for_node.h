#pragma once

#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  class for_node: public cdk::basic_node {
    cdk::sequence_node *_init;
    cdk::sequence_node *_condition;
    cdk::sequence_node *_step;
    cdk::basic_node *_block;

  public:
    for_node(int lineno, cdk::sequence_node *init, cdk::sequence_node *condition, cdk::sequence_node *step,
             cdk::basic_node *block) :
        cdk::basic_node(lineno), _init(init), _condition(condition), _step(step), _block(block) {
    }

    cdk::sequence_node* init() { return _init; }

    cdk::sequence_node* condition() { return _condition; }

    cdk::sequence_node* step() { return _step; }

    cdk::basic_node* block() { return _block; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_for_node(this, level); }

  };

} // udf


