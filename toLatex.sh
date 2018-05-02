#!/bin/bash

paste -d'&' \
      data/ozone-320.arff.$1.report \
      data/parkinsons.arff.$1.report \
      data/spectf-heart.arff.$1.report \
      > data/$1.table

cat data/$1.table | sed -e 's/\&/\ /g' | sed -e 's/\,/\ /g' | \
    awk '{ for(i=1;i<=NF;i++) total[i]+=$i;} END {for(i=1;i<=NF;i++) printf "%.3f ",total[i]/NR;}' \
        >> data/$1.table

cat data/$1.table | sed -e 's/\ /\,/g' | sed -e 's/\,/\&/g' | \
    sed -e 's/^/\\multicolumn\{1\}\{\|c\|\}\{Partición nnn\}\&/' | \
    awk '{gsub("nnn",NR,$0);print}' | \
    awk '{gsub("Partición 6","Media",$0);print}' | \
    sed -e 's/$/\\\\/g' | \
    sed -e 's/&\\\\/\\\\/g' \
        > data/$1.table

cat > data/$1.tex <<- EOF
\begin{tabular}{c|c|c|c|c|c|c|c|c|c|c|c|c|}
\cline{2-13}
&\multicolumn{4}{|c|}{Ozone} & \multicolumn{4}{|c|}{Parkinsons} & \multicolumn{4}{|c|}{Spectf}\\\\
\cline{2-13}
& clas & red & Agr. & T(s)
& clas & red & Agr. & T(s)
& clas & red & Agr. & T(s) \\\\
\hline
$(cat data/$1.table)
\hline
\end{tabular}
EOF

