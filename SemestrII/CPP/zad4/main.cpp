#include<cstdlib>
#include<iostream>
#include"stringstack.hpp"

using namespace std;

Stringstack * new_stack()
{
    int capacity;
    cout << "Podaj pojemność: ";
    cin >> capacity;
    return new Stringstack(capacity);
}

void insert_elem(Stringstack* stack)
{
    string val;
    cout << "Podaj wartość: ";
    cin >> val;
    stack->insert(val);

}

void get_elem(Stringstack * stack)
{
    cout << stack->pop_head() << endl;
}
void print_elem(Stringstack * stack)
{
    cout << stack->head() << endl;
}

void print_stos(Stringstack * stack)
{
    while(!stack->is_empty())
    get_elem(stack);

}


int main()
{
    int input;
    Stringstack * stack = nullptr;
    cout << "Wybierz opcję: " <<endl;
    cout << "[0] Nowy stos" << endl;
    cout << "[1] Dodaj element" << endl;
    cout << "[2] Wypisz górny element" << endl;
    cout << "[3] Wyciągnij element" << endl;
    cout << "[4] Wypisz rozmiar" << endl;
    cout << "[5] Wypisz pojemność" <<endl;
    cout << "[6] Wypisz stos" << endl;
    cout << "[7] Odwróć stos" << endl;
    cout << "[8] Wyjdź" << endl;
    while(true)
    {
        cout << "Wybór: ";
        cin >> input;
        if(input == 8)break;

        try
        {
            if(input<8 && input>0 && stack==nullptr) throw invalid_argument("Musisz najpierw utoworzyć stos");
            switch (input)
            {
            case 0:
                stack = new_stack();
                break;
            case 1:
                insert_elem(stack);
                break;
            case 2:
                print_elem(stack);
                break;
            case 3:
                get_elem(stack);
                break;
            case 4:
                cout << stack->get_size() << endl;
                break;
            case 5:
                cout << stack->get_capacity() << endl;
                break;
            case 6:
                print_stos(stack);
                break;
            case 7:
                *stack = stack->reverse();
                break;
            default:
                cout << "Niepoprawne wejście" << endl;
                break;
            }
        }
        catch(const std::exception& e)
        {
            std::cerr << e.what() << '\n';
        }
        
        
    }
    delete stack;
    return 0;
}