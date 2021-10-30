# Cezary Świtała
# Kod użyty do generowania wykresów
# pojawiających się w sprawozdaniu.

from fisher import * # import własnych metod przydatnych przy wyliczaniu wartośći 
# funkcji gęstości rozkładu fishera
import math
import numpy
import matplotlib.pyplot as pyplot

def map_list(f, x): 
    return list(map(f,x))

x = numpy.linspace(0.001,4,num=200)

fisher_1_1 = get_fisher_distr_density_function(1,1)
fisher_2_1 = get_fisher_distr_density_function(2,1)
fisher_4_1 = get_fisher_distr_density_function(4,1)
fisher_4_2 = get_fisher_distr_density_function(4,2)
fisher_4_4 = get_fisher_distr_density_function(4,4)
fisher_6_4 = get_fisher_distr_density_function(6,4)

pyplot.plot(x, map_list(fisher_1_1, x), label="m=1,n=1")
pyplot.plot(x, map_list(fisher_2_1, x), label="m=2,n=1", color="red")
pyplot.plot(x, map_list(fisher_4_1, x), label="m=4,n=1", color="black")
pyplot.plot(x, map_list(fisher_4_2, x), label="m=4,n=2", color="green")
pyplot.plot(x, map_list(fisher_4_4, x), label="m=4,n=4", color="orange")
pyplot.plot(x, map_list(fisher_6_4, x), label="m=6,n=4", color="purple")
pyplot.axis([0,4,0,1.5])
pyplot.legend()
pyplot.show()
pyplot.clf()

fisher_1_1_cumulative = get_cumulative_fisher_distr(1,1,13);
fisher_2_1_cumulative = get_cumulative_fisher_distr(2,1);
fisher_4_1_cumulative = get_cumulative_fisher_distr(4,1);
fisher_4_2_cumulative = get_cumulative_fisher_distr(4,2);
fisher_4_4_cumulative = get_cumulative_fisher_distr(4,4);
fisher_6_4_cumulative = get_cumulative_fisher_distr(6,4);

pyplot.plot(x, map_list(fisher_1_1_cumulative, x), label="m=1,n=1")
pyplot.plot(x, map_list(fisher_2_1_cumulative, x), label="m=2,n=1", color="red")
pyplot.plot(x, map_list(fisher_4_1_cumulative, x), label="m=4,n=1", color="black")
pyplot.plot(x, map_list(fisher_4_2_cumulative, x), label="m=4,n=2", color="green")
pyplot.plot(x, map_list(fisher_4_4_cumulative, x), label="m=4,n=4", color="orange")
pyplot.plot(x, map_list(fisher_6_4_cumulative, x), label="m=6,n=4", color="purple")
pyplot.axis([0,4,0,1])
pyplot.legend()
pyplot.show()