// dllmain.c : Definiuje punkt wejścia dla aplikacji DLL.
#include "pch.h"

#include <windows.h>
#include <math.h>
#ifdef __cplusplus
#define EXPORT extern "C" __declspec (dllexport)
#else
#define EXPORT __declspec (dllexport)
#endif

EXPORT int IsPrimeC(int n);

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, LPVOID lpvReserved) {
	return 1;
}

int IsPrimeC(int n) {
	int sqrt_of_n = sqrt(n);

	if (n == 1) return 0;
	for (int i = 2; i <= sqrt_of_n; i++) {
		if (n % i == 0) {
			return 0;
		}
	}

	return 1;
}
