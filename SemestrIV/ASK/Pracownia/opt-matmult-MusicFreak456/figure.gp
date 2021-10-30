
# # set terminal postscript eps enhanced
set terminal pngcairo size 640,480 enhanced font 'Arial,10'
set style line 1 linecolor rgb '#0060ad' linetype 1
set style line 2 linecolor rgb '#18dd50' linetype 1
set style line 3 linecolor rgb '#dd181f' linetype 1
set style line 4 linecolor rgb '#ff00f2' linetype 1

set title "Porównanie wersji mnożenia macierzy"
set xlabel "Rozmiar boku macierzy"
set ylabel "Średni czas wykonania(skala logarytmiczna)"
set logscale y 10
set xtics autofreq 64
set output 'versions.png'
plot "versions.dat" using 1:2 with lines title "ijk(jik)" linestyle 1, \
     "versions.dat" using 1:3 with lines title "kij(ikj)" linestyle 2, \
     "versions.dat" using 1:4 with lines title "jki(kji)" linestyle 3,

set title "Porównanie wersji mnożenia macierzy blokami z różnym offsetem"
set output 'blocks_offset.png'
set ylabel "Średni czas wykonania"
unset logscale y
plot "blocks_offset.dat" using 1:2 with lines title "domyślne" linestyle 1, \
     "blocks_offset.dat" using 1:3 with lines title "wyzerowane" linestyle 2

set title "Porównanie czasu wykonania matmult3 dla różnych rozmiarów kafelka"
set xlabel "Rozmiar boku kafelka"
set xtics autofreq 8
set output 'blocks_size.png'
plot "blocks_size.dat" using 1:2 with lines title "czas wykonania" linestyle 1

# vim: ft=gnuplot