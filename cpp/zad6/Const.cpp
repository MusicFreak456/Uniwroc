#include"Expression.hpp"

Pi::Pi()
{
    this->value = M_PI;
}
double Pi::eval()
{
    return this->value;
}
std::string Pi::to_string()
{
    return std::to_string(value);
}

e::e()
{
    this->value = M_E;
}
double e::eval()
{
    return this->value;
}
std::string e::to_string()
{
    return std::to_string(value);
}

Phi::Phi()
{
    this->value = 1.61803398875;
}
double Phi::eval()
{
    return this->value;
}
std::string Phi::to_string()
{
    return std::to_string(value);
}