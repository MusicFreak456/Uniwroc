#include"manipulators.hpp"

istream& clearline(istream &in)
{
    char c = 'a';
    
    do
    {
        c = in.get();
    } while (c != '\n' && c != EOF);
    return in;
}

ignore::ignore(int x): x(x) {}

istream& operator>> (istream& in, const ignore& ig)
{
    char c;
    for(int i=1 ; i<=ig.x ;i++)
    {
        c = in.get();
        if(c == '\n' || c == EOF) break;
    }

    return in;
}

ostream& coma(ostream& out)
{
    out << ", ";
    return out;
}

ostream& colon(ostream& out)
{
    out << ": ";
    return out;
}

ostream& operator<< (ostream& out, const index& indx)
{
    out << "[";
    string x_rep;
    int y = indx.x;
    do
    {
        int rest = y % 10;
        x_rep += to_string(rest);
        y = y /10;

    } while (y != 0);

    if(x_rep.length() > indx.w) throw invalid_argument("Not enough space given");

    int len_remaining = indx.w - x_rep.length();

    for(int i = 1; i <= len_remaining; i++)
    {
        out << " ";
    }

    out << indx.x << "]";

    return out;
}

index::index(int x, unsigned int w): x(x), w(w){}