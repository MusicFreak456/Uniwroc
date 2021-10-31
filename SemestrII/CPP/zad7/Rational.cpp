#include"Rational.hpp"

using namespace calculations;

ostream& calculations::operator<< (ostream &out, const Rational &rat)
{
    set<int> rests;
    set<int>::iterator it;
    
    string result;
    int nominator = rat.get_nominator();
    int denominator = rat.get_denominator();
    int integ = nominator / denominator;

    result+=to_string(integ) + ".";

    nominator = nominator % denominator;
    rests.insert(nominator);

    while(true)
    {
        nominator = nominator * 10;

        integ = nominator / denominator;
        nominator = nominator % denominator;
        result += to_string(integ);
        it = rests.find(nominator);
        if(it!=rests.end())
        {
            result.insert(*it+1,"(");
            result+=")";
            break;
        }

        rests.insert(nominator);

        
        if(nominator==0) break;
    }
    out << result; 
    return out;
}

void Rational::display() const noexcept
{
    cout << this->nominator << "/" << this->denominator << endl;
}

bool Rational::multiplication_overflow(int a, int b)
{
    int res = a * b;
    if(a == res/b)
    {
        return false;
    }
    else
    {
        return true;
    }   
}

bool Rational::addition_overflow(int a, int b)
{
    if(((a >= 0) && (b > (INT32_MAX - a))) || ((a < 0) && (b < (INT32_MIN - a))))
    {
        return true;
    }
    else
    {
        return false;
    }
    
}

int Rational::gcd(int a, int b)
{
    while(b!=0)
    {
        int t = b;
        b = a%b;
        a = t;
    }
    return a;
}

Rational::Rational(int nominator, int denominator) noexcept(false): nominator(nominator), denominator(denominator)
{
    if(denominator == 0) throw DivisionByZero("Division by zero");
    int gcd = this->gcd(this->nominator,this->denominator);
    this->denominator = this->denominator / gcd;
    this->nominator = this->nominator / gcd;

    if(this->denominator < 0)
    {
        this->nominator = -this->nominator;
        this->denominator = -this->denominator;
    }
}

Rational::Rational(int integer) noexcept: Rational(integer, 1) {}

int Rational::get_denominator() const noexcept
{
    return this->denominator;
}

int Rational::get_nominator() const noexcept
{
    return this->nominator;
}

Rational& Rational::operator!() const
{
    int new_nominator;
    int new_denominator;

    new_nominator = this->denominator;
    new_denominator = this->nominator;
    Rational* new_rat = new Rational(new_nominator,new_denominator);

    return *new_rat;
}

Rational& Rational::operator-() const
{
    int new_nominator;

    new_nominator = -this->nominator;
    Rational* new_rat = new Rational(new_nominator,this->denominator);

    return *new_rat;
}

Rational& Rational::operator+(const Rational& rside)
{
    int new_nominator;
    int new_denominator;

    if(multiplication_overflow(this->nominator, rside.denominator)
    || multiplication_overflow(rside.nominator, this->denominator)
    || addition_overflow(this->nominator*rside.denominator, rside.nominator* this->denominator)
    ) throw OutOfRange("Overflow occured");

    new_nominator = this->nominator*rside.denominator + rside.nominator* this->denominator;
    new_denominator = this->denominator * rside.denominator;
    Rational* new_rat = new Rational(new_nominator,new_denominator);

    return *new_rat;
}

Rational& Rational::operator-(const Rational& rside)
{
    Rational neg = -rside;

    Rational* new_rat = new Rational(*this + neg);

    return *new_rat;
}

Rational& Rational::operator*(const Rational& rside)
{
    int new_nominator;
    int new_denominator;

    if(multiplication_overflow(this->nominator, rside.nominator)
    || multiplication_overflow(this->denominator, rside.denominator))
    throw OutOfRange("Overflow occured");

    new_nominator = this->nominator * rside.nominator;
    new_denominator = this->denominator * rside.denominator;
    Rational* new_rat = new Rational(new_nominator,new_denominator);

    return *new_rat;
}

Rational& Rational::operator/(const Rational& rside)
{
    Rational inv = !rside;

    Rational* new_rat = new Rational(*this * inv);

    return *new_rat;
}

Rational::operator int() const
{
    double frac = this->nominator/ (double)this->denominator;
    int integ = (int)frac;
    if(frac-integ >= 0.5)
    {
        return integ+1;
    }
    else
    {
        return integ;
    }
    
}

Rational::operator double() const
{
    return this->nominator/(double)this->denominator;
}

RationalException::RationalException(const string& message): error_message(message){}
DivisionByZero::DivisionByZero(const string& message): error_message(message) {}
OutOfRange::OutOfRange(const string& message): error_message(message){}

const char* RationalException::what() const noexcept 
{
    return this->error_message.c_str();
}

const char* DivisionByZero::what() const noexcept 
{
    return this->error_message.c_str();
}
const char* OutOfRange::what() const noexcept 
{
    return this->error_message.c_str();
}