#include<cstdlib>
#include<iostream>
#include"tabbit.hpp"

using namespace std;

int main()
{
    tab_bit tablica(3);
    tab_bit tablica2(4);

    cout << "Tablica inicjowana listą: " << endl;
    tab_bit zlisty = {true,false,true,false};
    cout << zlisty << endl;;
    cout << "Tablica 3 i 4 bitowa: (operator <<)" << endl; 
    cout << tablica << endl;
    cout << tablica2 <<endl;
    cout << "Przypisanie pierwszemu i trzeciemu bitowi 1: " <<endl;
    tablica2[0]=true;
    tablica2[2]=true;
    cout << tablica2 << endl;
    cout << "Konstruktor kopiujący: " << endl;
    tab_bit tablica4(tablica2);
    cout << tablica4 <<endl;
    cout << "Operator ~ :" << endl;
    cout <<"~" << tablica4 << " = " << ~tablica4 << endl;
    cout << "Operator & :" << endl;
    tablica[1] = true;
    tab_bit and_tab = tablica4 & tablica;
    cout << tablica4 << " & " << tablica << " = " <<  and_tab << endl;
    cout << "Operator | :" << endl;
    tab_bit or_tab = tablica4 | tablica;
    cout << tablica4 << " | " << tablica << " = " <<  or_tab << endl;
    cout << "Operator ^ :" << endl;
    tablica[0] = true;
    tab_bit xor_tab = tablica4 ^ tablica;
    cout << tablica4 << " ^ " << tablica << " = " <<  xor_tab << endl;
    cout << "Rozmiar " << tablica4 << ": " << endl;
    cout << tablica4.rozmiar() << endl << endl;

    cout << "Testy z treści zadania" << endl;
    tab_bit t(46); // tablica 46-bitowa (zainicjalizowana zerami)
    cout << t << endl;
    tab_bit u((uint64_t)45);
    cout << u << endl;
    tab_bit v(t);
    cout << v << endl;

    tab_bit w(tab_bit(8));
    cout << w << endl;




    v[0] = 1;
    cout << v << endl;
    bool b = v[1];
    cout << b <<endl;
    u[45] = true;
    cout << u << endl;
    u[63] = true;
    u[45] = u[46] = u[63];
    cout << u << endl;
    


    return 0;
}