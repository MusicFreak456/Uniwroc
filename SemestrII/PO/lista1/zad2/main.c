#include"ulamki.h"


int main()
{
    Ulamek *u1;
    Ulamek *u2;
    Ulamek *u3;
    Ulamek *u4;
    Ulamek *u;
    
    u1 = nowy_ulamek(3, -9);
    print_ulamek(u1);

    u2 = nowy_ulamek(7, 3);
    print_ulamek(u2);

    u = dodaj_i_zwroc(u1, u2);
    print_ulamek(u);

    dodaj(u1,u2);
    print_ulamek(u2);

    u3 = nowy_ulamek(1,2);

    u = pomnoz_i_zwroc(u2,u3);
    print_ulamek(u);

    u = podziel_i_zwroc(u, u3);
    print_ulamek(u);

    u4 = nowy_ulamek(3,2);

    u=odejmij_i_zwroc(u,u4);
    print_ulamek(u);
    

    return 0;
}
