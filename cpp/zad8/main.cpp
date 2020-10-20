#include<cstdlib>
#include<iostream>
#include"list.hpp"

using namespace std;

int main()
{
    int choice;
    LinkedList<int> * intlist = new LinkedList<int>();
    bool exit = false;
    int val;
    int index;

    while (!exit)
    {
        cout << "Lista: " << *(intlist) <<endl; 
        cout << "Wybierz operację: " << endl;
        cout << "[1] Wstaw element na koniec" <<endl;
        cout << "[2] Wstaw element na wybraną pozycję" << endl;
        cout << "[3] Usuń element o wybranej wartości" << endl;
        cout << "[4] Znajdź indeks elementu o podanej wartości" <<endl;
        cout << "[5] Posortuj listę" << endl;
        cout << "[6] Sprawdź czy posortowana" <<endl;
        cout << "[7] Wyjdź" << endl <<endl;
        cout << "Wybór: ";
        cin >> choice;

        switch (choice)
        {
        case 1:
            cout << "Podaj wartość: ";
            cin >> val;
            intlist->pushback(val);
            break;
        case 2:
            cout << "Podaj wartość: ";
            cin >> val;
            cout << "Podaj indeks: ";
            cin >> index;
            intlist->insert(val,index);
            break;
        case 3:
            cout << "Podaj wartość: ";
            cin >> val;
            intlist->remove(val);
            break;
        case 4:
            cout << "Podaj wartość: ";
            cin >> val;
            cout << "Znaleziono na: " << intlist->locate(val) <<endl;
            break;
        case 5:
            intlist->sort<int,Desc<int>>();
            break;
        case 6:
            cout << ((intlist->check<int>())? "Posortowana" : "Nieposorotowana");
            break;

        default:
            exit = true;
            break;
        }

        cout << endl;
        cout << "-------------------------------------------" << endl;
    }
    

    if(intlist != nullptr) delete(intlist);
    return 0;
}