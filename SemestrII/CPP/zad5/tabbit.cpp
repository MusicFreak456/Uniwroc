#include "tabbit.hpp"

tab_bit::tab_bit (int rozm): dl(rozm)
{
    this->number_of_tables = rozm/rozmiarSlowa;
    
    if(rozm%rozmiarSlowa!=0) this->number_of_tables++;
    
    this->tab = new slowo[this->number_of_tables];
    
    for(int i =0 ;i < this->number_of_tables; i++)
    {
        this->tab[i] = 0;
    }
}

tab_bit::tab_bit(slowo tb): dl(rozmiarSlowa)
{
    this->tab = new slowo[1];
    this->tab[0] = tb;
    this->number_of_tables=1;
}

tab_bit::tab_bit (const tab_bit &tb): dl(tb.dl), number_of_tables(tb.number_of_tables)
{
    this->tab = new slowo[this->number_of_tables];

    for(int i =0 ;i < number_of_tables; i++)
    {
        this->tab[i] = tb.tab[i];
    }
}

tab_bit::tab_bit (tab_bit &&tb) : dl(move(tb.dl)), tab(move(tb.tab)), number_of_tables(move(tb.number_of_tables))
{
    tb.tab = nullptr;
}

tab_bit::tab_bit (initializer_list<bool> list): tab_bit((int)list.size())
{
    int index = dl;
    
    for(bool x: list)
    {
        index--;
        this->pisz(index,x);
    }
}

tab_bit& tab_bit::operator= (const tab_bit &tb)
{
    this->number_of_tables = tb.number_of_tables;
    this->dl = tb.dl;

    for(int i =0 ;i < number_of_tables; i++)
    {
        this->tab[i] = tb.tab[i];
    }
    return *this;
}

tab_bit& tab_bit::operator= (tab_bit &&tb)
{
    this->number_of_tables = move(tb.number_of_tables);
    this->dl = move(tb.dl);
    this->tab = move(tb.tab);
    tb.tab = nullptr;

    return *this;
}

tab_bit::~tab_bit()
{
    delete[] tab;
}

ostream & operator << (ostream &wy, const tab_bit &tb)
{

    for(int i =tb.dl-1 ; i >=0 ;i--)
    {
        wy<<tb.czytaj(i);
    }
    return wy;
}

istream & operator >> (istream &we, tab_bit &tb)
{
    we >> tb.dl;
    tb.number_of_tables = tb.dl/tb.rozmiarSlowa;
    if(tb.dl%tb.rozmiarSlowa!=0) tb.number_of_tables++;
    tb.tab = new uint64_t[tb.number_of_tables];

    for(int j = (tb.number_of_tables-1); j>=0 ;j--)
    {
        int i = (tb.dl-1);
        if(j!=(tb.number_of_tables-1)) i=63;

        for(; i>=0 ;i--)
        {
            bool n;
            we >> n;
            tb.pisz(i,n);
        }
    }

    return we;
}

tab_bit & tab_bit::operator&(const tab_bit &tb)
{
    tab_bit * new_tab_bit = new tab_bit(*this);
    int col_num_of_tables;
    int col_dl;
    if(this->number_of_tables>tb.number_of_tables) col_num_of_tables = this->number_of_tables;
    else col_num_of_tables = tb.number_of_tables;
    if(this->dl > tb.dl) 
    {
        col_dl = this->dl;
    }
    else col_dl = tb.dl;
    slowo * new_tab;
    new_tab = new slowo[col_num_of_tables];
    for(int i = 0 ; i< col_num_of_tables; i++)
    {
        
        if(tb.number_of_tables > i && this->number_of_tables > i)
        {
            new_tab[i] = tb.tab[i] & this->tab[i];
        }
        else if (tb.number_of_tables > i)
        {
            new_tab[i] = tb.tab[i];
        }
        else
        {
            new_tab[i] = this->tab[i];
        }
    }
    new_tab_bit->number_of_tables=col_num_of_tables;
    new_tab_bit->dl = col_dl;
    new_tab_bit->tab = new_tab;

    return *new_tab_bit;
}
tab_bit & tab_bit::operator&=(const tab_bit &tb)
{
    return *this&tb;
}

tab_bit & tab_bit::operator|(const tab_bit &tb)
{
    tab_bit * new_tab_bit = new tab_bit(*this);
    int col_num_of_tables;
    int col_dl;
    if(this->number_of_tables>tb.number_of_tables) col_num_of_tables = this->number_of_tables;
    else col_num_of_tables = tb.number_of_tables;
    if(this->dl > tb.dl) 
    {
        col_dl = this->dl;
    }
    else col_dl = tb.dl;
    slowo * new_tab;
    new_tab = new slowo[col_num_of_tables];
    for(int i = 0 ; i< col_num_of_tables; i++)
    {
        
        if(tb.number_of_tables > i && this->number_of_tables > i)
        {
            new_tab[i] = tb.tab[i] | this->tab[i];
        }
        else if (tb.number_of_tables > i)
        {
            new_tab[i] = tb.tab[i];
        }
        else
        {
            new_tab[i] = this->tab[i];
        }
    }
    new_tab_bit->number_of_tables=col_num_of_tables;
    new_tab_bit->dl = col_dl;
    new_tab_bit->tab = new_tab;

    return *new_tab_bit;
}
tab_bit & tab_bit::operator|=(const tab_bit &tb)
{
    return *this|tb;
}

tab_bit & tab_bit::operator^(const tab_bit &tb)
{
    tab_bit * new_tab_bit = new tab_bit(*this);
    int col_num_of_tables;
    int col_dl;
    if(this->number_of_tables>tb.number_of_tables) col_num_of_tables = this->number_of_tables;
    else col_num_of_tables = tb.number_of_tables;
    if(this->dl > tb.dl) 
    {
        col_dl = this->dl;
    }
    else col_dl = tb.dl;
    slowo * new_tab;
    new_tab = new slowo[col_num_of_tables];
    for(int i = 0 ; i< col_num_of_tables; i++)
    {
        
        if(tb.number_of_tables > i && this->number_of_tables > i)
        {
            new_tab[i] = tb.tab[i] ^ this->tab[i];
        }
        else if (tb.number_of_tables > i)
        {
            new_tab[i] = tb.tab[i];
        }
        else
        {
            new_tab[i] = this->tab[i];
        }
    }
    new_tab_bit->number_of_tables=col_num_of_tables;
    new_tab_bit->dl = col_dl;
    new_tab_bit->tab = new_tab;

    return *new_tab_bit;
}
tab_bit & tab_bit::operator^=(const tab_bit &tb)
{
    return *this^tb;
}


tab_bit & tab_bit::operator~()
{
    tab_bit * new_tab = new tab_bit(*this);

 
    for(int i=0 ; i < this->dl ; i++)
    {
        if(new_tab->czytaj(i) == true)
        {
            new_tab->pisz(i,false);
        }
        else new_tab->pisz(i,true);
    }

    return *new_tab;
}

bool tab_bit::czytaj(int i) const
{
    int tab_num = i/this->rozmiarSlowa;
    int index = i%this->rozmiarSlowa;
    uint64_t mask = (1Ul<<index);
    return mask & this->tab[tab_num];
}

void tab_bit::pisz(int i,bool b)
{
    int tab_num = i/this->rozmiarSlowa;
    int index = i%this->rozmiarSlowa;
    uint64_t mask = (1UL<<index);

    if(b)
    {
        this->tab[tab_num] = this->tab[tab_num] | mask;
    }
    else
    {
        uint64_t aux_mask = 0xFFFFFFFFFFFFFFFF;
        mask = aux_mask ^ mask;
        this->tab[tab_num] = this->tab[tab_num] & mask;
    }
    
}

bool tab_bit::operator[] (int i) const
{
    return czytaj(i);
}

tab_bit::ref tab_bit::operator[] (int i)
{
    int tab_num = i/this->rozmiarSlowa;
    int index = i%this->rozmiarSlowa;
    tab_bit::ref reference(&(this->tab[tab_num]), index, this->dl);

    return reference;
}

tab_bit::ref& tab_bit::ref::operator=(const bool i)
{
    if(index > (length-1)) throw out_of_range("Index out of range");
    uint64_t mask = (1UL << index);
    if(i)
    {
        *(segment)|=mask;
    }
    else
    {
        uint64_t aux_mask = 0xFFFFFFFFFFFFFFFF;
        mask = aux_mask ^ mask;
        *(segment) &= mask;
    }
    return *this;
}

tab_bit::ref& tab_bit::ref::operator=(tab_bit::ref&& refer)
{
    *this = (bool)refer;
    return *this;
}

tab_bit::ref& tab_bit::ref::operator=(const tab_bit::ref& refer)
{
    *this = (bool)refer;
    return *this;
}


tab_bit::ref:: operator bool() const
{

    if(index > (length-1)) throw out_of_range("Index out of range");
    uint64_t mask = (1UL << index);

    return *(segment) & mask;
}

tab_bit::ref::ref(tab_bit::slowo * segment, int index, int length): segment(segment), index(index), length(length){}


