#!/bin/bash

EUC=$'\nEuclidean'
MID=$'\n\nNEW DISTANCE\n\n'
COS=$'\nCosine'
for num in 1 2 3 4 5 6 7 8 9 10
   do
	echo "$EUC $num"
	anlp app Cluster -k $num -c -d euclidean -t z -r -f countries birth.dat
done






