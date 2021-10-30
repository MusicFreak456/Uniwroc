import pandas
from matplotlib import pyplot
import numpy
import math
from scipy import stats

data_frame = pandas.read_csv("covid.csv");

de_data = data_frame.loc[data_frame['Country'] == 'Germany']
de_new_cases = list(de_data["New_cases"])[365:-5]

ch_data = data_frame.loc[data_frame['Country'] == 'Switzerland']
ch_new_cases = list(ch_data["New_cases"])[365:-5]

at_data = data_frame.loc[data_frame['Country'] == 'Austria']
at_new_cases = list(at_data["New_cases"])[365:-5]


de_population = 83783942
de_number_of_thousands = de_population / 1000

ch_population = 8654622
ch_number_of_thousands = ch_population / 1000

at_population = 9006398
at_number_of_thousands = at_population / 1000

de_new_cases = list( map( lambda x: x/de_number_of_thousands, de_new_cases ) )
ch_new_cases = list( map( lambda x: x/ch_number_of_thousands, ch_new_cases ) )
at_new_cases = list( map( lambda x: x/at_number_of_thousands, at_new_cases ) )

# pyplot.plot(de_new_cases, label="Zachorowania na 1000 mieszkańców")
# pyplot.legend()
# pyplot.show()

# pyplot.plot(ch_new_cases, label="Zachorowania na 1000 mieszkańców")
# pyplot.legend()
# pyplot.show()

# pyplot.plot(at_new_cases, label="Zachorowania na 1000 mieszkańców")
# pyplot.legend()
# pyplot.show()

def S_squared(values):
    n = len(values)
    mean = numpy.mean(values)
    total_sum = 0
    for x in values:
        total_sum += (x - mean)**2
    return total_sum * (1 / n)

de_S_squared = S_squared(de_new_cases)
ch_S_squared = S_squared(ch_new_cases)
at_S_squared = S_squared(at_new_cases)

print("Estymowane wariancje:")
print("Niemcy: ", de_S_squared)
print("Szwajcaria: ", ch_S_squared)
print("Austria", at_S_squared)
print("")

def t_test(X, Y):
    n_y = len(Y)
    n_x = len(X)
    X_mean = numpy.mean(X)
    Y_mean = numpy.mean(Y)
    numerator = X_mean - Y_mean;
    denominator =  S_squared(Y) * n_y
    denominator += S_squared(X) * n_x
    denominator /= n_x * n_y
    denominator = math.sqrt(denominator)
    return numerator / denominator

def p_value(t, degrees_of_freedom):
    if t < 0:
        return 2 * stats.t.cdf(t, degrees_of_freedom)
    else:
        return 2 * (1 - stats.t.cdf(t, degrees_of_freedom))

print("Niemcy i Austria")
de_at_t = t_test(de_new_cases, at_new_cases)
de_at_t
print("Wartość testu: ", de_at_t)
print("p_value: ", p_value(de_at_t, len(de_new_cases) + len(at_new_cases) - 2 ))
print("")

print("Niemcy i Szwajcaria")
de_ch_t = t_test(de_new_cases, ch_new_cases)
print("Wartość testu: ", de_ch_t)
print("p_value: ", p_value(de_ch_t, len(de_new_cases) + len(ch_new_cases) - 2 ))
print("")

print("Austria i Szwajcaria")
at_ch_t = t_test(ch_new_cases, at_new_cases)
print("Wartość testu: ", at_ch_t)
print("p_value: ", p_value(at_ch_t, len(at_new_cases) + len(ch_new_cases) - 2 ))