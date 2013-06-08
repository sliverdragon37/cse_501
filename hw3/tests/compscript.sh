#!/bin/bash

FILE=$1

echo $'\njust SSA:'

./run.sh -opt=ssa $FILE

echo $'\nSSA + DTR:'

./run.sh -opt=ssa,dtr $FILE

#echo $'\nSSA + CSE + SCP:'

#./run.sh -opt=ssa,scp,cse $FILE

#echo $'\nSSA + DTR + CBR:'

#./run.sh -opt=ssa,cbr,dtr $FILE

#echo $'\nMEM:'

#./run.sh -opt=mem $FILE

#echo $'\nSSA +SCP +CSE + CBR + DTR:'

#./run.sh -opt=ssa,scp,cse,cbr,dtr $FILE


echo $'\nUNOPTIMIZED:'

dart ../start/bin/start.dart --stats $FILE
