#include<cstdlib>
#include<iostream>
#include<exception>
#include<string>
#include<set>

using namespace std;

namespace calculations
{
    
    class Rational
    {
    private:
        int nominator, denominator;

        int gcd(int,int);
        bool multiplication_overflow(int,int);
        bool addition_overflow(int,int);

    public:
        friend ostream& operator<< (ostream &out, const Rational &rat);

        Rational(int, int) noexcept(false);
        explicit Rational(int) noexcept;

        Rational& operator!() const noexcept(false);
        Rational& operator-() const noexcept(false);
        Rational& operator+(const Rational&) noexcept(false);
        Rational& operator-(const Rational&) noexcept(false);
        Rational& operator*(const Rational&) noexcept(false);
        Rational& operator/(const Rational&) noexcept(false);

        explicit operator int() const;
        operator double() const;

        void display() const noexcept;
        int get_denominator() const noexcept;
        int get_nominator() const noexcept;
    };

    ostream& operator<< (ostream &out, const Rational &rat);
}

class RationalException :public exception
{
private:
    string error_message;
public:
    RationalException(){}
    RationalException(const string&);
    
    virtual const char* what() const noexcept override;
};

class DivisionByZero :public RationalException
{
private:
    string error_message;
public:
    DivisionByZero(const string&);
    
    virtual const char* what() const noexcept override;
};

class OutOfRange :public RationalException
{
private:
    string error_message;
public:
    OutOfRange(const string&);
    
    virtual const char* what() const noexcept override;
};