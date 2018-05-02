#!/bin/sh
# Whatever you use to run things in parallel.  Maybe SGE?  Up to you. I run them in parallel on a machine with 8-64 cores.
CORES=8
P="parallel $CORES" # pipe your list of command lines into "parallel" to run them together on the same machine.
#P="distrib_stdin.new -f ../machines.89" # Wayne's personal SGE replacement

# Choose your value of k. For testing use k=7.
# For k=8, it's 1-2 months of single-core CPU time.
k=7

MC=../make-canon-maps # makes the initial canon maps in batch of 512 runs.
SIFT=../canon-sift # the executable used to do the sifting down to the final canon_map file.

# You should create this directory manually and remove it when done
TMP=/tmp/canon-sift  # use this one if you're running all on one machine
#TMP=canon-sift # use this one if you need to run things across a network, eg using SGE

if [ ! -d $TMP ]; then
    echo "You need to read the source code of this file and set parameters appropriately," >&2
    echo "and then create the directory $TMP and remove it yourself when you're done" >&2
    exit 2
fi
set -x
# First create all the initial graphettes
awk 'BEGIN{for(i=0;i<512;i++)printf "'$MC' '$k' 512 %d > '$TMP'/'$k'.0.%03d\n",i,i;exit}' | $P &&

# Note: the set of *all* commands generated by each line of awk must finished before the next set starts.
awk 'BEGIN{for(i=0;i<256;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.0.%03d '$TMP'/'$k'.0.%03d > '$TMP'/'$k'.1.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<128;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.1.%03d '$TMP'/'$k'.1.%03d > '$TMP'/'$k'.2.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<64;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.2.%03d '$TMP'/'$k'.2.%03d > '$TMP'/'$k'.3.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<32;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.3.%03d '$TMP'/'$k'.3.%03d > '$TMP'/'$k'.4.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<16;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.4.%03d '$TMP'/'$k'.4.%03d > '$TMP'/'$k'.5.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<8;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.5.%03d '$TMP'/'$k'.5.%03d > '$TMP'/'$k'.6.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<4;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.6.%03d '$TMP'/'$k'.6.%03d > '$TMP'/'$k'.7.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<2;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.7.%03d '$TMP'/'$k'.7.%03d > '$TMP'/'$k'.8.%03d\n",2*i,2*i+1,i;exit}' | $P &&
awk 'BEGIN{for(i=0;i<1;i++)printf "'$SIFT' '$k' '$TMP'/'$k'.8.%03d '$TMP'/'$k'.8.%03d > '$TMP'/'$k'.9.%03d\n",2*i,2*i+1,i;exit}' | $P &&

cut -f2- $TMP/$k.9.000 | tee canon_map$k.txt | awk '!seen[$1]{seen[$1]=1;map[n++]=$1}END{print n;for(i=0;i<n;i++)printf "%d ", map[i]; print ""}' | tee canon_list$k.txt | awk 'NR==2{for(i=1;i<=NF;i++) print i-1, $i}' > canon-ordinal-to-signature$k.txt 
set +x
echo "The hard work is now done. When you are happy, you can do the following:"
echo "mv canon*$k.txt ../canon_maps"
echo "(cd ..; gcc -O2 '-Dk=$k' '-DkString='\"$k\"' -o create-bin-data create-bin-data.c; ./create-bin-data)"

