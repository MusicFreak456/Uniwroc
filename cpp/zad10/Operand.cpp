#include"Operand.hpp"

namespace rpn_calculator
{
    Number::Number(string discr)
    {
        this->value = std::stod(discr);
    }

    void Number::evaluate(stack<double> & stck)
    {
        stck.push(this->value);
    }

    map<string,double> Variable::var_set; 

    Variable::Variable(string label): label(label)
    {}

    void Variable::add_var(string label, double value)
    {
        std::pair<string,double> new_pair(label,value);
        var_set.insert(new_pair);
    }

    void Variable::clear()
    {
        var_set.clear();
    }

    bool Variable::member(string label)
    {
        return (var_set.find(label) != var_set.end());
    }

    void Variable::evaluate(stack<double> & stck)
    {
        stck.push( var_set.find(this->label)->second);
    }

    Const::Const(string label): label(label)
    {}

    void Const::evaluate(stack<double> & stck)
    {
        if(this->label == "pi")
        {
            stck.push(this->PI);
        }
        else if(this->label == "e")
        {
            stck.push(this->E);
        }
        else
        {
            stck.push(this->PHI);
        }
    }
}