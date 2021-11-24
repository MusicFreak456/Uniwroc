// dllmain.cpp : Definiuje punkt wejścia dla aplikacji DLL.
#include "pch.h"
#include <windows.h>
#include <math.h>
#ifdef __cplusplus
#define EXPORT extern "C" __declspec (dllexport)
#else
#define EXPORT __declspec (dllexport)
#endif

EXPORT int ExecuteC(int n, int (*f)(int));

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, LPVOID lpvReserved) {
	return 1;
}

int ExecuteC(int n, int (*f)(int)) {
	return f(n);
}

