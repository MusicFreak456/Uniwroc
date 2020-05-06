#include"Expression.hpp"

Add::Add(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Add::eval()
{
    return this->l_side->eval() + this->r_side->eval();
}

std::string Add::to_string()
{
    return this->l_side->to_string() + " + " + this->r_side->to_string();
}

Sub::Sub(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Sub::eval()
{
    return this->l_side->eval() - this->r_side->eval();
}

std::string Sub::to_string()
{
    return this->l_side->to_string() + " - " + this->r_side->to_string();
}

Mult::Mult(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Mult::eval()
{
    return this->l_side->eval() * this->r_side->eval();
}

std::string Mult::to_string()
{
    return "(" + this->l_side->to_string() + ") * (" + this->r_side->to_string() + ")";
}

Div::Div(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Div::eval()
{
    return this->l_side->eval() / this->r_side->eval();
}

std::string Div::to_string()
{
    return "(" + this->l_side->to_string() + ") / (" + this->r_side->to_string() + ")";
}

Log::Log(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Log::eval()
{
    return std::log(this->r_side->eval()) / std::log(this->l_side->eval());
}

std::string Log::to_string()
{
    return "log(" + this->l_side->to_string() + " , " + this->r_side->to_string() + ")";
}

Mod::Mod(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Mod::eval()
{
    return (int)this->l_side->eval() % (int)this->r_side->eval();
}

std::string Mod::to_string()
{
    return "(" + this->l_side->to_string() + ") % (" + this->r_side->to_string() + ")";

    
}


Expt::Expt(Expression * l_side, Expression * r_side)
{
    this->l_side=l_side;
    this->r_side=r_side;
}

double Expt::eval()
{
    return pow(this->l_side->eval(), this->r_side->eval());
}

std::string Expt::to_string()
{
    return "(" + this->l_side->to_string() + ") ^ (" + this->r_side->to_string() + ")";

    
}


