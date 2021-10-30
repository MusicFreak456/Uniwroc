# Cezary Świtała
# Implementacja wyznaczania wartości funkcji
# gęstości rozkładu Fishera, oraz jego
# dystrybuanty.

# UWAGA: Kod nie będzie działał poprawnie w wersji pythona 
# starszej niż python3
from romberg import romberg # import własnej implementacji metody romberga
import math

def _gamma_of_natural(n):
    return math.factorial(n - 1)

def _gamma_of_n_div_2(n):
    if n == 1:
        return math.sqrt(math.pi) 
    elif n % 2 == 0:
        return _gamma_of_natural(n/2)
    else:
        k = (n - 1) / 2
        frac = _gamma_of_natural(2*k) / _gamma_of_natural(k)
        return 2**(1 - 2*k) * math.sqrt(math.pi) * frac

def _beta_of_halved_args(m,n):
    numerator = _gamma_of_n_div_2(m) * _gamma_of_n_div_2(n)
    denominator = _gamma_of_n_div_2(n + m)
    return numerator / denominator

def _get_dependant_part_of_fisher_distr_density_function(m,n):
    """Zwraca funkcję, która jest część funkcji gęstości rozkładu Fishera 
    dla zadanych parametrów i która jest zależna od argumentu"""
    def part_of_density_function(x):
        if m == 1 and x == 0: return 0
        numerator = x**(m-2)
        denominator = (m*x + n)**(m + n)
        return math.sqrt( numerator / denominator );
    return part_of_density_function

def _get_const_part_of_fisher_distr_density_function(m,n):
    """Zwraca stałą część funkcji gęstości rozkładu Fishera dla zadanych parametrów, 
    która jest zależna tylko od argumentów"""
    beta_val = _beta_of_halved_args(m,n)
    sqrt_val = math.sqrt( m**m * n**n )
    return sqrt_val / beta_val

def get_fisher_distr_density_function(m,n):
    """Tworzy i zwraca funkcję gęstość rozkładu Fishera z zadanymi parametrami"""
    f = _get_dependant_part_of_fisher_distr_density_function(m,n)
    const = _get_const_part_of_fisher_distr_density_function(m,n)
    def density_function(x):
        return const * f(x)
    return density_function

def get_cumulative_fisher_distr(m,n,romberg_size=10):
    """Tworzy i zwraca funkcję dystrybuanty rozkładu Fishera dla zadanych 
    parametrów"""
    const = _get_const_part_of_fisher_distr_density_function(m,n)
    f = _get_dependant_part_of_fisher_distr_density_function(m,n)
    def cumulative_distr(t):
        return const * romberg(f,0,t,romberg_size)
    return cumulative_distr