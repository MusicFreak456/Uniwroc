#ifndef EXPRESSION_HPP
#define EXPRESSION_HPP

#include<stack>

namespace rpn_calculator
{
    class Expression
    {
    public:
        virtual void evaluate(std::stack<double> &)=0;
    };
}

#endif