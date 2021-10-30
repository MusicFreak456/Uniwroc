from scipy import stats
import pandas
from matplotlib import pyplot
import numpy
import math

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


new_cases = {}
countries = ["de", "ch", "at"]
new_cases["de"] = list( map( lambda x: x/de_number_of_thousands, de_new_cases ) )
new_cases["ch"] = list( map( lambda x: x/ch_number_of_thousands, ch_new_cases ) )
new_cases["at"] = list( map( lambda x: x/at_number_of_thousands, at_new_cases ) )

means = {}
for country in countries:
    means[country] = numpy.mean(new_cases[country])

means["global"] = numpy.mean( new_cases["de"] + new_cases["ch"] + new_cases["at"])

print( "Średnie z poszczególnych grup: ", means, "\n")

I = len(new_cases.keys())
J = len(new_cases[countries[0]])

ssa = 0
for country in countries:
    ssa += (means[country] - means["global"])**2
ssa *= J

print('SSA = ', ssa)


sse = 0
for country in countries:
    for x in new_cases[country]:
        sse += (x - means[country])**2
print("SSE = ", sse)


deg_of_freedom_ssa = I - 1
deg_of_freedom_sse = I * (J - 1)

msa = ssa / deg_of_freedom_ssa
mse = sse / deg_of_freedom_sse

print("MSA = ", msa)
print("MSE = ", mse, "\n")

f = msa / mse
print("f = ", f, "\n")

p_value = stats.f.sf(f, deg_of_freedom_ssa, deg_of_freedom_sse)
print("p_value: ", p_value)