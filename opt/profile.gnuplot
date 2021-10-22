## GNUPLOT
## usage:
## $ gnuplot -e "name='dataFileName'" profile.gnuplot
## dataFileName.txt is the harmonic profile of the sample
set terminal png size 400,300
set output name . '.png'
set title ''
set logscale y 10
set xtics 1
set tic scale 0
unset key; unset ytics; unset border
plot name . '.txt' using ($0+1):1 title '' with impulse
## from merged files in order to get the mean value of different profiles
## $ paste *.txt > data.txt
## $ gnuplot -e "name='data'" profile.gnuplot
# stats name . '.txt' 
# max_col = STATS_columns
# plot name . '.txt' using ($0+1):(sum [col=1:max_col] column(col)) title '' with impulse
## to get the result on the stdout
## $ awk '{c=0;for(i=1;i<=NF;++i){c+=$i};print c}' data.txt 