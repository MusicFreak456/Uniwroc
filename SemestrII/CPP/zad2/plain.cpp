#include"plain.hpp"

Wektor dodaj_wektory(Wektor v1,Wektor v2)
{
    Wektor v3(v1.dx+v2.dx,v2.dy+v2.dy);
    return v3;
}

bool czy_prostopadle(Prosta *l, Prosta* k)
{
    double la = l->get_a() / -(l->get_b());
    double ka = k->get_a() / -(l->get_b());
    double epsilon = 0.000001;

    if(std::abs((-1.0/la) - ka ) < epsilon) return true;
    return false;

}

bool czy_rownolegle(Prosta *l, Prosta *k)
{
    double la = l->get_a() / -(l->get_b());
    double ka = k->get_a() / -(l->get_b());
    double epsilon = 0.000001;

    if(std::abs(la - ka ) < epsilon) return true;
    return false;

}

double rozwiazanie(Prosta *l, Prosta *k)
{
    if(czy_rownolegle(l,k))throw std::invalid_argument("Równoległe proste");

    double a =l->get_a();
    double b =l->get_b();
    double c =l->get_c();
    double aprim =k->get_a();
    double bprim =k->get_b();
    double cprim =k->get_c();

    return -(c*bprim-cprim*b)/(bprim*a-b*aprim);

}


Wektor::Wektor(double dx,double dy) : dx(dx), dy(dy){};
Wektor::Wektor(const Wektor& v): dx(v.dx), dy(v.dy){};

void Wektor::print()
{
    std::cout << "dx: " << this->dx << " dy: " << this->dy << std::endl;
}

Punkt::Punkt(double x,double y): x(x), y(y){};
Punkt::Punkt(Punkt p, Wektor v): x(p.x+v.dx), y(p.y+v.dy){};
Punkt::Punkt(const Punkt& p): x(p.x), y(p.y){};

void Punkt::print()
{
    std::cout << "x: " << this->x << " y: " << this->y <<  std::endl;
}
Prosta::Prosta(Punkt a, Punkt b)
{
    if(a.x == b.x && a.y == b.y) throw std::invalid_argument("Nie można jednoznacznie utworzyć prostej.");
    if(a.x==b.x)
    {
        this->a=1;
        this->b=0;
        this->c=a.x;
    }
    if(a.y==b.y)
    {
        this->a=0;
        this->b=1;
        this->c=a.y;
    }
    else
    {
        this->a=(a.y-b.y)/(a.x-b.x);
        this->b=-1;
        this->c=(a.y - ((a.y-b.y)/(a.x-b.x))*a.x);
    }
    normalize();
}
Prosta::Prosta(double a, double b, double c): a(a),b(b),c(c)
{
    if(this->a==0&&this->b==0) throw std::invalid_argument("Współczynniki A i B nie mogą być jednocześnie równe 0.");
    normalize();
}
Prosta::Prosta(Wektor v)
{
    this -> a = -1/(v.dy/v.dx);
    this -> c = v.dy - this->a*v.dx;
    this -> b = -1;

    normalize();

}

Prosta::Prosta(Prosta *l, Wektor v)
{
    this->a=l->get_a();
    this->b=l->get_b();
    this->c=l->get_c()- l->get_a() * v.dx + v.dy;
}

double Prosta::odleglosc(Punkt p)
{
    if(czy_nalezy(p))return 0;
    return std::abs(this->get_a()*p.x + this->get_b()*p.y + this->get_c())/std::sqrt(std::pow(this->get_a(),2)+std::pow(this->get_b(),2));
}

void Prosta::normalize()
{
    double normalizer;
    normalizer=1.0/sqrt(pow(this->a,2)+pow(this->b,2));
    if(c>0)normalizer*=-1;

    this->a*=normalizer;
    this->b*=normalizer;
    this->c*=normalizer;
}

bool Prosta::czy_prostopadla(Wektor v)
{
    double a=this->get_a();
    double b=this->get_b();
    double vec_a = -1.0/(v.dy/v.dx);
    double line_a = a/-b;
    double epsilon = 0.000001;

    if(std::abs(vec_a-line_a)<epsilon) return true;
    else return false;
}

bool Prosta::czy_rownolegla(Wektor v)
{
    double a=this->get_a();
    double b=this->get_b();
    double vec_a = (v.dy/v.dx);
    double line_a = a/-b;
    double epsilon = 0.000001;

    if(std::abs(vec_a-line_a)<epsilon) return true;
    else false;

}
bool Prosta::czy_nalezy(Punkt p)
{
    double epsilon = 0.00001;
    if(std::abs(p.x*this->a+p.y*this->b+this->c)<epsilon)return true;
    else return false;
}

void Prosta::print()
{
    std::cout << this->get_a() << "x + " << this->get_b() << "y + " << this->get_c() << std::endl;
}

void Prosta::print_normalized()
{
    std::cout << this->get_normalized_a() << "x + " << this->get_normalized_b() << "y + " << this->get_normalized_c() << std::endl;
}

double Prosta::get_a()
{
    double normalizer = -this->b;
    return this->a/normalizer;
}
double Prosta::get_b()
{
    double normalizer =-this->b;
    return this->b/normalizer;
}

double Prosta::get_c()
{
    double normalizer =-this->b;
    return this->c/normalizer;
}

double Prosta::get_normalized_a(){return a;}
double Prosta::get_normalized_b(){return b;}
double Prosta::get_normalized_c(){return c;}