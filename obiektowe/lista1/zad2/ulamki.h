#include<stdlib.h>
#include<stdio.h>

typedef struct ulamek_struct
{
    int licznik;
    int mianownik;
} Ulamek;


int nwd(int a,int b);
void skroc_ulamek(Ulamek* u);
Ulamek * nowy_ulamek(int licz, int mian);

Ulamek* dodaj_i_zwroc(Ulamek *u1, Ulamek *u2);
void dodaj(Ulamek * u1, Ulamek *u2);

Ulamek* pomnoz_i_zwroc(Ulamek *u1, Ulamek *u2);
void pomnoz(Ulamek *u1, Ulamek *u2);

Ulamek* podziel_i_zwroc(Ulamek *u1, Ulamek *u2);
void podziel(Ulamek *u1, Ulamek *u2);

Ulamek* odejmij_i_zwroc(Ulamek *u1, Ulamek *u2);
void odejmij(Ulamek *u1, Ulamek *u2);

void print_ulamek(Ulamek*u);