#ifndef Expression_hpp
#define Expression_hpp

#include<cstdlib>
#include<iostream>
#include<vector>
#include<cmath>

class Expression
{
public:
    virtual double eval()=0;
    virtual std::string to_string() = 0;
};

class Number : public Expression
{
private:
    double value;
public:
    Number(double);
    double eval();
    std::string to_string();
};

class Variable : public Expression
{
private:
    std::string id;
    static std::vector<std::pair<std::string,double>> list;
public:
    Variable(std::string);
    std::string to_string();
    double eval();

    static void add_variable(std::string,double);
    static void remove_variable(std::string);
    static void change_variable_value(std::string,double);
};

class Const : public Expression
{
protected:
    double value;
};

class Pi :public Const
{
public:
    Pi();virtual
    double eval();
    std::string to_string();
};

class e :public Const
{
public:
    e();
    double eval();
    std::string to_string();
};

class Phi :public Const
{
public:
    Phi();
    double eval();
    std::string to_string();
};

class UnOperator :public Expression
{
protected:
    Expression * r_side;
};

class Cos : public UnOperator
{
public:
    Cos(Expression *);
    std::string to_string();
    double eval();
};

class Sin : public UnOperator
{
public:
    Sin(Expression *);
    std::string to_string();
    double eval();
};

class Exp : public UnOperator
{
public:
    Exp(Expression *);
    std::string to_string();
    double eval();
};

class Ln : public UnOperator
{
public:
    Ln(Expression *);
    std::string to_string();
    double eval();
};

class Abs : public UnOperator
{
public:
    Abs(Expression *);
    std::string to_string();
    double eval();
};

class Oppos : public UnOperator
{
public:
    Oppos(Expression *);
    std::string to_string();
    double eval();
};

class Inverse : public UnOperator
{
public:
    Inverse(Expression *);
    std::string to_string();
    double eval();
};

class BinOperator :public UnOperator
{
protected:
    Expression * l_side;
};

class Add :public BinOperator
{
public:
    Add(Expression *,Expression *);
    std::string to_string();
    double eval();
};

class Sub :public BinOperator
{
public:
    Sub(Expression *,Expression *);
    std::string to_string();
    double eval();
};

class Mult :public BinOperator
{
public:
    Mult(Expression *,Expression *);
    std::string to_string();
    double eval();
};

class Div :public BinOperator
{
public:
    Div(Expression *,Expression *);
    std::string to_string();
    double eval();
};

class Log :public BinOperator
{
public:
    Log(Expression *,Expression *);
    std::string to_string();
    double eval();
};

class Mod :public BinOperator
{
public:
    Mod(Expression *,Expression *);
    std::string to_string();
    double eval();
};

class Expt :public BinOperator
{
public:
    Expt(Expression *,Expression *);
    std::string to_string();
    double eval();
};



#endif


