#!/bin/bash
# USE: advbm [input .dart file] [output .dart file] [first line to repeat] [last line to repeat] [number of times to increment] [number of replications per repeat]


IFILE=$1
OFILE=$2
START=$3
END=$4
REPEAT=$5
INCREMENT=$6

for ((m=0; m<$REPEAT;m++))
do
    # read the lines
    index=0
    while read line ; do
	LINESARRAY[$index]="$line"
	index=$(($index+1))
    done < $IFILE
    
    echo ${LINESARRAY[$START]}
    echo ${LINESARRAY[$END]}


    # write the lines
    for ((i=0; i<$START;i++))
    do
	echo -e "${LINESARRAY[$i]}"
    done > $OFILE

    # repeat specified lines
    for ((c=0; c<=$INCREMENT * m;c++))
    do
	for ((i=$START -1; i<$END;i++))
	do
	    echo -e "${LINESARRAY[$i]}"
	done >> $OFILE
    done

    for ((i=$END; i<$index;i++))
    do
	echo -e "${LINESARRAY[$i]}"
    done >> $OFILE
    # done writing lines

    startc $OFILE 
done
