#ifndef PLAIN_H
#define PLAIN_H

#include<stdexcept>
#include<cmath>
#include<iostream>

class Wektor
{
public:
    const double dx=0;
    const double dy=0;

    Wektor() = default;
    Wektor(double,double);
    Wektor(const Wektor&);
    void operator=(const Wektor&) = delete;

    void print();


    ~Wektor(){}
};

class Punkt
{
public: 
    const double x=0;
    const double y=0;

    Punkt()=default;
    Punkt(double,double);
    Punkt(Punkt,Wektor);
    Punkt(const Punkt&);
    void operator=(const Punkt&)= delete;

    void print();

    ~Punkt(){}

};

class Prosta
{
private:
    double a;
    double b;
    double c;

    void normalize();
public:
    Prosta()=default;
    Prosta(Punkt,Punkt);
    Prosta(Wektor);
    Prosta(double,double,double);
    Prosta(Prosta*,Wektor);

    Prosta(const Prosta&) = delete;
    void operator=(const Prosta&) = delete;

    void print();
    void print_normalized();

    double get_a();
    double get_b();
    double get_c();

    double get_normalized_a();
    double get_normalized_b();
    double get_normalized_c();

    double odleglosc(Punkt);
    bool czy_prostopadla(Wektor);
    bool czy_rownolegla(Wektor);
    bool czy_nalezy(Punkt);
    
    ~Prosta(){};
};

Wektor dodaj_wektory(Wektor v1,Wektor v2);

bool czy_prostopadle(Prosta*,Prosta*);
bool czy_rownolegle(Prosta*,Prosta*);

double rozwiazanie(Prosta*, Prosta*);

#endif