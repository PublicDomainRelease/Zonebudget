#!/bin/sh
# usr/opt/wrdapp/zonebdgt.1_0/test/test.sh -- run ZONEBUDGET test data sets
#
# Usage: test.sh [start [stop]]
#        test.sh [start [stop] | tee test.out
#
#        where: start = starting test number
#                stop = ending test number (may be same as start)
#
# To use this script for another program in a different directory, globally
# change the program name (be sure to change both upper and lower case
# occurrences) and set the default value for the Stop variable as appropriate.
#
# History: 12/24/91, mblalock, initial coding
#          04/27/92, mygoze, restructuring
#          12/20/96, rsregan, updated for zonebdgt
#
# Variable definitions
# --------------------
#
  TOPDIR=..
  PROG=$TOPDIR/bin/zonebdgt
  DATA=$TOPDIR/data
  CHECK=./check.sh
  CLEAN=./clean.sh
  PROGNM=ZONEBUDGET
  END=1
  DIVD=========================================
#
#
  exec 2>&1                                # stderr shows up in .out file

  Start=${1:-1}                            # by default, start at 1
  Stop=${2:-$END}                          # by default, stop at 1
  if [ $Start -lt 1 ] ; then Start=1 ; fi
  if [ $Stop -lt 1 ] ; then Stop=$END ; fi
  if [ $Start -gt $END -o $Stop -gt $END ]
  then
    echo
    echo "Warning, invalid arguments--test range is 1 - $END for $PROGNM"
    echo "input arguments were $Start - $Stop"
    echo
    if [ $Stop -gt $END ] ; then Stop=$END ; fi
    if [ $Start -gt $END ]
    then
      echo "no $PROGNM tests will be performed"
    else
      echo "Tests $Start - $Stop will be performed"
    fi
    echo
  fi
#
# remove old output file
#
  $CLEAN
#
# begin test runs
#
  Test=$Start

  if [ $Test -ge $Start -a $Test -le $Stop ]
  then
    echo
    echo $DIVD$DIVD
    echo "Begin processing $PROGNM test runs $Start to $Stop"
    echo
    date
  fi

  while [ $Test -ge $Start -a $Test -le $Stop ]
  do
    echo
    echo
    echo $DIVD$DIVD
    echo "Test run number $Test"
    rm -f go
    if [ $Test -eq 1 ] ; then 
      Name=zbtest 
      echo $Name.lst > go
      echo $DATA/$Name.bud >> go
      echo Test data for running ZONEBUDGET >> go
      echo $DATA/$Name.zon >> go
      echo A >> go
    fi
    $PROG < go > $Name.log
    Test=`expr $Test + 1`
  done
  Test=`expr $Test - 1`
#
  if [ $Test -ge $Start -a $Test -le $Stop ]
  then
    echo
    echo
    echo $DIVD$DIVD
    echo "Completed $PROGNM test runs $Start to $Test"
    echo
  fi

# check output against original output in $DATA directory
  $CHECK zbtest
