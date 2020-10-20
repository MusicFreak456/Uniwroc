#include"Evaluator.hpp"

namespace rpn_calculator
{

    double Evaluator::eval(queue<Expression*> queue)
    {
        stack<double> op_stack;

        while (!queue.empty())
        {
            queue.front()->evaluate(op_stack);
            queue.pop();
        }

        if(op_stack.size() != 1) throw std::invalid_argument("Invalid RPN expression");

        return op_stack.top();

    }
}