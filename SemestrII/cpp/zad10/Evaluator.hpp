#ifndef EVALUATOR_HPP
#define EVALUATOR_HPP

#include"Expression.hpp"
#include<queue>
#include<stack>
#include<iostream>

using std::queue;
using std::stack;

namespace rpn_calculator
{
    class Evaluator
    {
    public:
        double eval(queue<Expression*>);
    };
}

#endif