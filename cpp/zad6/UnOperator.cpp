#include"Expression.hpp"

Cos::Cos(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Cos::to_string()
{
    return "cos(" + r_side->to_string() + ")";
}

double Cos::eval()
{
    return std::cos(r_side->eval());
}

Sin::Sin(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Sin::to_string()
{
    return "sin(" + r_side->to_string() + ")";
}

double Sin::eval()
{
    return std::sin(r_side->eval());
}

Exp::Exp(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Exp::to_string()
{
    return "e ^ (" + r_side->to_string() + ")";
}

double Exp::eval()
{
    return std::exp(r_side->eval());
}

Ln::Ln(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Ln::to_string()
{
    return "ln(" + r_side->to_string() + ")";
}

double Ln::eval()
{
    return std::log(r_side->eval());
}

Abs::Abs(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Abs::to_string()
{
    return "|" + r_side->to_string() + "|";
}

double Abs::eval()
{
    return std::abs(r_side->eval());
}

Oppos::Oppos(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Oppos::to_string()
{
    return "opposite(" + r_side->to_string() + ")";
}

double Oppos::eval()
{
    return -(r_side->eval());
}

Inverse::Inverse(Expression * r_side)
{
    this->r_side = r_side;
}

std::string Inverse::to_string()
{
    return "inverse(" + r_side->to_string() + ")";
}

double Inverse::eval()
{
    return 1.0/r_side->eval();
}

