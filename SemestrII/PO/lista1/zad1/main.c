#include<stdlib.h>
#include<stdio.h>
#include"figury.h"

int main()
{
    Figura *f[2];
    f[0]=nowe_kolo(1.0,-1.0,3.0);
    printf("%f\n", pole(f[0]));
    f[1]=nowy_kwadrat(1.0,-1.0,3.0);
    printf("%f\n",pole(f[1]));
    printf("%f\n",sumapol(f,2));

}