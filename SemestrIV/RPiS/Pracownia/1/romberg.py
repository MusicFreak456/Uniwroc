# Cezary Świtała
# Implementacja metody Romberga numerycznego
# wyznaczania wartości całki oznaczonej

# UWAGA: Kod nie będzie działał poprawnie w wersji pythona 
# starszej niż python3

def _interval_length(a,b,n):
    return (b - a) / n

def _T00(f,a,b):
    result = 1/2 * f(a)
    result += 1/2 * f(b)
    result *= _interval_length(a,b,1)
    return result

def _M_n(f,a,b,n):
    result = 0
    for i in range(1,n+1):
        result += f( a + (1/2) * (2 * i - 1) * _interval_length(a,b,n) )
    return result * _interval_length(a,b,n)

def _generate_first_column(f,a,b,size):
    result = [_T00(f,a,b)]
    for i in range(1,size + 1):
        result.append( 1/2 * (result[i-1] + _M_n(f,a,b,2**(i-1))) )
    return result

def romberg(f,a,b,size=10):
    """Liczy całkę oznaczoną na zadanym przedziale.

    Argumenty
    ----------
    f : int
        funkcja podcałkowa
    a : float
        początek przedziału całkowania
    b : float
        koniec przedziału całkowania
    size : int
        wysokość użytej tablicy Romberga (domyślnie 10)
    """

    result_vector = _generate_first_column(f,a,b,size)
    for m in range(1,size + 1):
        for k in range(size, m-1, -1):
            result_vector[k] = (4**m * result_vector[k] - result_vector[k-1]) / (4**m - 1)
    return result_vector[-1:][0]