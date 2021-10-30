#ifndef FIGURY_H
#define FIGURY_H

#include<math.h>
#include<stdlib.h>
#include<stdio.h>

typedef enum figtype {KOLO,KWADRAT,TROJKAT} typfig;

typedef struct wspolrzedne
{
    float x;
    float y;
} Wspolrzedne;


typedef struct struct_figura
{
    typfig fig_name;
    Wspolrzedne poczatek;
    float size1;
    float size2;
} Figura;

Figura* nowy_kwadrat(float vecx, float vecy, float size);
Figura* nowy_trojkat(float vecx, float vecy, float a,float h);
Figura* nowe_kolo(float vecx, float vecy, float size);
float pole(Figura *f);
void przesun(Figura *f, float x, float y);
float sumapol(Figura **f,int size);

#endif