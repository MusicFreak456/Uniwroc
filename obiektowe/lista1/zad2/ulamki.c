#include"ulamki.h"

int nwd(int a,int b)
{
    while (b!=0)
    {
        int c = a%b;
        a=b;
        b=c;
    }
    return a;
}


void skroc_ulamek(Ulamek* u)
{
    int nwd_val=nwd(u->mianownik,u->licznik);
    while(nwd_val!=1)
    {
        u->mianownik=u->mianownik/nwd_val;
        u->licznik=u->licznik/nwd_val;
        nwd_val=nwd(u->mianownik,u->licznik);
    }
}

Ulamek * nowy_ulamek(int licz, int mian)
{
    Ulamek* ulamek = malloc(sizeof(Ulamek));
    if(mian>0)
    {
        ulamek->licznik=licz;
        ulamek->mianownik=mian;
    }
    else if(mian<0)
    {
        ulamek->licznik=-licz;
        ulamek->mianownik=-mian;
    }
    else
    {
        printf("Mianownik musi być różny od zera!\n");
        return NULL;
    }
    
    
    skroc_ulamek(ulamek);

    return ulamek;
}

void usun_ulamek(Ulamek *u)
{
    if(u!=NULL)free(u);
}

void print_ulamek(Ulamek*u)
{
    printf("%d/%d\n",u->licznik,u->mianownik);
}


Ulamek* dodaj_i_zwroc(Ulamek *u1, Ulamek *u2)
{
    int a,b,c,d;
    a=u1->licznik;
    b=u1->mianownik;
    c=u2->licznik;
    d=u2->mianownik;

    Ulamek *nowy = nowy_ulamek(a*d+c*b,b*d);
    return nowy;
}

void dodaj(Ulamek * u1, Ulamek *u2)
{
    Ulamek *temp = dodaj_i_zwroc(u1,u2);
    *u2=*temp;
    free(temp);
}

Ulamek* pomnoz_i_zwroc(Ulamek *u1, Ulamek *u2)
{
    int a,b,c,d;
    a=u1->licznik;
    b=u1->mianownik;
    c=u2->licznik;
    d=u2->mianownik;

    Ulamek *nowy = nowy_ulamek(a*c,b*d);
    return nowy;
}

void pomnoz(Ulamek *u1, Ulamek *u2)
{
    Ulamek *temp = pomnoz_i_zwroc(u1,u2);
    *u2=*temp;
    free(temp);
}

Ulamek* podziel_i_zwroc(Ulamek *u1, Ulamek *u2)
{
    int a,b,c,d;
    a=u1->licznik;
    b=u1->mianownik;

    c=u2->licznik;
    d=u2->mianownik;

    Ulamek *nowy = nowy_ulamek(a*d,b*c);
    return nowy;
}

void podziel(Ulamek *u1, Ulamek *u2)
{
    Ulamek *temp = podziel_i_zwroc(u1,u2);
    *u2=*temp;
    free(temp);
}

Ulamek* odejmij_i_zwroc(Ulamek *u1, Ulamek *u2)
{
    Ulamek *nowy = dodaj_i_zwroc(u1,pomnoz_i_zwroc(u2,nowy_ulamek(-1,1)));
    return nowy;
}

void odejmij(Ulamek *u1, Ulamek *u2)
{
    Ulamek *temp = odejmij_i_zwroc(u1,u2);
    *u2=*temp;
    free(temp);
}
