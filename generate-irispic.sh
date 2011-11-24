gnuplot -e "set terminal png size 400, 300; set output 'iris.png'; set title 'kmeans-vector example'; plot 'iris.cls' using 1:2:3 with points linetype palette"
