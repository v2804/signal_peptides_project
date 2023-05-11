#!/bin/bash

while IFS= read -r line ## чтение файла со списком родов
do
    GENERA=${line}
    mkdir $GENERA ## создание директории
    cd $GENERA
    ncbi-genome-download --format protein-fasta --genera $GENERA bacteria ## загрузка протеомов
    find . -type f -name '*.faa.gz' > allpaths.txt
    while read line;
    do
	    BASE=${line%%.gz}
	    gzip -kd $line ## разархивация
	    signalp6 --fastafile ./$BASE --organism other --output_dir ./ --format none --mode fast
	    awk '{if(NR==1) {print $0} else {if($0 ~ /^>/) {print "\n"$0} else {printf $0}}}' BASE > BASE.out0
	    awk '{ORS = (NR%2 ? " " : RS)} 1' BASE.out > BASE.singleline
	    echo
    done < allpaths.txt
    cd-
    echo
done < genera_list.txt #список с родами бактерий 

#убрать заголовки таблиц
find . -type f -name 'prediction_results.txt' > paths1.txt
while read line;
do
    LINE=${line%%.txt}
    sed -i '1d' LINE
    echo
done < paths1.txt


find . -type f -name 'region_output.gff3' > paths2.txt
while read line;
do
    LINE=${line%%.gff3}
    sed -i '1d' LINE
    echo
done < paths2.txt

#объединить файлы в один
find ./ -name *.singleline -exec cat {} + > merged.singleline 
find ./ -name prediction_results.txt -exec cat {} + > merged_prediction_results.txt
find ./ -name region_output.gff3 -exec cat {} + > merged_region_output.txt

