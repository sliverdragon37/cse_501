#!/bin/bash

if [ -z "$2" ]
then
FILE=$1
ARGS=""

else
FILE=$2
OPTS=$1

ARGS=${OPTS#-opt=}
ARGS=${ARGS//,/ }
fi

IR=${FILE%.*}.sir
OPT=${FILE%.*}.opt

#grab and compile to IR
dart ../start/bin/start.dart -c $FILE > $IR

#get rid of the output file
#-f for silent (doesn't complain when file not there)
rm -f $OPT

#run our script to optimize the IR
scala -classpath ../code main $IR $OPT $ARGS

#run the IR
dart ../start/bin/start.dart -r --stats $OPT 




