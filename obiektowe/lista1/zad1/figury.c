#include"figury.h"

Figura* nowy_kwadrat(float vecx, float vecy, float size)
{
    if(size<0)
    {
        printf("Rozmiar musi być większy lub równy 0\n");
        return NULL;
    }
    Figura* kwadrat = malloc(sizeof(Figura));
    kwadrat->fig_name=KWADRAT;
    kwadrat->size1=size;
    kwadrat->size2=-1;
    kwadrat->poczatek.x=vecx;
    kwadrat->poczatek.y=vecy;

    return kwadrat;
}

Figura* nowy_trojkat(float vecx, float vecy, float a,float h)
{
    if(a<0||h<0)
    {
        printf("Rozmiar musi być większy lub równy 0\n");
        return NULL;
    }
    Figura* trojkat = malloc(sizeof(Figura));
    trojkat->fig_name=TROJKAT;
    trojkat->size1=a;
    trojkat->size2=h;
    trojkat->poczatek.x=vecx;
    trojkat->poczatek.y=vecy;

    return trojkat;
}

Figura* nowe_kolo(float vecx, float vecy, float size)
{
    if(size<0)
    {
        printf("Rozmiar musi być większy lub równy 0\n");
        return NULL;
    }
    Figura* kolo = malloc(sizeof(Figura));
    kolo->fig_name=KOLO;
    kolo->size1=size;
    kolo->size2=-1;
    kolo->poczatek.x=vecx;
    kolo->poczatek.y=vecy;

    return kolo;
}





float pole(Figura *f)
{
    if(f==NULL)
    {
        return 0;
    }

    if(f->fig_name==KOLO)
    {
        float r=f->size1;
        return M_PI*r*r;
    }
    else if(f->fig_name==KWADRAT)
    {
        float a =f->size1;
        return a*a;
    }
    else if(f->fig_name==TROJKAT)
    {
        float a=f->size1;
        float h=f->size2;
        return a*h/2;
    }
}

void przesun(Figura *f, float x, float y)
{
    float nowy_x=f->poczatek.x+x;
    float nowy_y=f->poczatek.y+y;
    f->poczatek.x=nowy_x;
    f->poczatek.y=nowy_y;
}

float sumapol(Figura **f,int size)
{
    float suma=0;
    for(int i=0;i<size;i++)
    {
        if(f[i]!=NULL)
        suma+=pole(f[i]);
    }
    return suma;
}