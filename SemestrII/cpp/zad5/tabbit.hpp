#include<cstdlib>
#include<iostream>

using namespace std;


class tab_bit
{
    typedef uint64_t slowo; // komorka w tablicy
    static const int rozmiarSlowa = 64; // rozmiar slowa w bitach
    friend istream & operator >> (istream &we, tab_bit &tb);
    friend ostream & operator << (ostream &wy, const tab_bit &tb);
    class ref
    {
    private:
        slowo * segment;
        int index;
        int length;
    public:
        ref(slowo*, int, int);
        
        ref(const ref&)= default;
        ref(ref&&) = default;
        ref& operator=(const ref&);
        ref& operator=(ref&&);

        ref& operator=(const bool i);
        operator bool() const;
    }; // klasa pomocnicza do adresowania bitów

    
protected:
    int dl; // liczba bitów
    slowo *tab; // tablica bitów
    int number_of_tables;
public:
    explicit tab_bit (int rozm); // wyzerowana tablica bitow [0...rozm]
    explicit tab_bit (slowo tb); // tablica bitów [0...rozmiarSlowa]
    tab_bit (initializer_list<bool> list);// zainicjalizowana wzorcem
    tab_bit (const tab_bit &tb); // konstruktor kopiujący
    tab_bit (tab_bit &&tb); // konstruktor przenoszący
    tab_bit & operator = (const tab_bit &tb); // przypisanie kopiujące
    tab_bit & operator = (tab_bit &&tb); // przypisanie przenoszące
    ~tab_bit (); // destruktor
private:
    bool czytaj (int i) const; // metoda pomocnicza do odczytu bitu
    void pisz (int i, bool b); // metoda pomocnicza do zapisu bitu
public:
    bool operator[] (int i) const; // indeksowanie dla stałych tablic bitowych
    ref operator[] (int i); // indeksowanie dla zwykłych tablicbitowych
    inline int rozmiar () const {return this->dl;} // rozmiar tablicy w bitach
public:
    // operatory bitowe: | i |=, & i &=, ^ i ^= oraz ! //załóżmy, że chodziło o "~"
    tab_bit & operator&(const tab_bit &tb);
    tab_bit & operator&=(const tab_bit &tb);
    tab_bit & operator|(const tab_bit &tb);
    tab_bit & operator|=(const tab_bit &tb);
    tab_bit & operator^(const tab_bit &tb);
    tab_bit & operator^=(const tab_bit &tb);
    tab_bit & operator~();
};