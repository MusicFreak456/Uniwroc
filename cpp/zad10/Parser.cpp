#include"Parser.hpp"

namespace rpn_calculator
{
    queue<Expression*> Parser::parse(Syntax& syntax)
    {
        queue<Expression*> to_return;

        string next;
        syntax >> next;

        while (next != "" && next != "to")
        {
            if(this->ops_keywords.find(next) != this->ops_keywords.end())
            {
                to_return.push(new Function(next));
            }
            else if(this->const_keywords.find(next) != this->const_keywords.end())
            {
                to_return.push(new Const(next));
            }
            else if(Variable::member(next))
            {
                to_return.push(new Variable(next));
            }
            else if(((int)next[0] >= 49 && (int)next[0] <= 57) || (int)next[0] == 45)
            {
                to_return.push(new Number(next));
            }
            else
            {
                std::clog << "unknown identifier: " + next << std::endl;
            }
            syntax >> next;
        }
        

        return to_return;
    }

    void Parser::name_check(string name)
    {
        if(name.length() >= 7) 
        throw std::invalid_argument("variable name " + name + " is too long");

        if(this->ops_keywords.find(name) != this->ops_keywords.end()) 
        throw std::invalid_argument("variable can't be named \"" + name + "\" because it's a operation keyword");

        if(this->const_keywords.find(name) != this->const_keywords.end()) 
        throw std::invalid_argument("variable can't be named \"" + name + "\" because it's a name of a constant");

        if(this->other_keywords.find(name) != this->other_keywords.end()) 
        throw std::invalid_argument("variable can't be named \"" + name + "\" because it's a keyword");

        if(((int)name[0] >= 49 && (int)name[0] <= 57))
        {
            std::clog << "Warning: assigning new value to number or variable that begins with number can cause confusion" << std::endl;
        }
    }
}