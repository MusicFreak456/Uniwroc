#include"Function.hpp"

namespace rpn_calculator
{
    Function::Function(string op_symbol): op_symbol(op_symbol)
    {}

    void Function::evaluate(stack<double> &stck)
    {
        if(this->op_symbol == "+")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(b + a);
        }
        else if (this->op_symbol == "-")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(b - a);
        }
        else if (this->op_symbol == "*")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(b * a);
        }
        else if (this->op_symbol == "/")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(b / (double)a);
        }
        else if (this->op_symbol == "modulo")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push((int)b % (int)a);
        }
        else if (this->op_symbol == "min")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(std::min(a,b));
        }
        else if (this->op_symbol == "max")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(std::max(a,b));
        }
        else if (this->op_symbol == "log")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(std::log(b) / std::log(a));
        }
        else if (this->op_symbol == "pow")
        {
            if(stck.size() < 2) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            double b = stck.top();
            stck.pop();
            stck.push(std::pow(b,a));
        }
        else if (this->op_symbol == "abs")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::abs(a));
        }
        else if (this->op_symbol == "sgn")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            if(a < 0)stck.push(-1);
            else if(a == 0) stck.push(0);
            else stck.push(1);
        }
        else if (this->op_symbol == "floor")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::floor(a));
        }
        else if (this->op_symbol == "ceil")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::ceil(a));
        }
        else if (this->op_symbol == "sin")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::sin(a));
        }
        else if (this->op_symbol == "cos")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::cos(a));
        }
        else if (this->op_symbol == "atan")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::atan(a));
        }
        else if (this->op_symbol == "acot")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(M_PI / 2 - std::atan(a));
        }
        else if (this->op_symbol == "ln")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::log(a));
        }
        else if (this->op_symbol == "exp")
        {
            if(stck.size() < 1) throw std::invalid_argument("Invalid RPN expression");
            double a = stck.top();
            stck.pop();
            stck.push(std::exp(a));
        }
    }
}