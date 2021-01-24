#!/bin/sh
# test.sh -- run dr3m test data sets
#
# Usage: test.sh [start [stop]]
#        test.sh [start [stop]] | tee test.log
#
#        where: start = starting test number
#                stop = ending test number (may be same as start)
# 
# History: 95/12/08 kmflynn

# pathname
  WrdA=/usr/opt/wrdapp

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

  Prgm=$WrdA/dr3m2.1
  Data=$Prgm/data
  Anne=$WrdA/annie2.2
  Chck=$WrdA/dr3m2.1/test

  exec 2>&1                                # stderr shows up in log file
  Start=${1-1}                             # by default, start at 1
  Stop=${2-5}                              # by default, stop at 5
                                           # 1 - annie to build and fill wdm
                                           # 2 - optimization, no routing
                                           # 3 - routing, saved to wdm
                                           # 4 - routing saved to old format
                                           # 5 - routing, not saved
# 
# Begin test runs
  echo
  echo _________________________________________________________________
  echo " Begin processing DR3M test runs $Start to $Stop"
  echo "  dr3m program from: $Dr3m"
  echo "     test data from: $Data"
  echo " annie program from: $Anne"
  echo `date`
#
  Test=$Start
  if [ $Test -eq 1 ] ; then
#   build the wdm file
    echo _________________________________________________________________
    echo Test run number 1
#   remove any old files
    for File in ERROR.FIL ANNIE.LOG test1.out sand.wdm sand.xpt ; do
       if [ -f $File ] ; then rm $File ; fi
    done
#   link in input files
    ln -s $Data/sand.xpt sand.xpt

    $Anne/bin/annie <<-EOT
	@$Data/test1.log
	EOT
#   remove unneeded files
    rm ERROR.FIL ANNIE.LOG sand.xpt

    if [ $Test -ne $Stop ] ; then Test=2 ; fi

  fi

  while [ $Test -gt 1 -a $Test -le $Stop ] ; do
#   run dr3m tests
    echo 
    echo _________________________________________________________________
    echo "Test run number $Test"
    echo
#   remove old output files
    for Sufx in out dmp unt pks sgs ; do
      if [ -f test$Test.$Sufx ] ; then rm test$Test.$Sufx ; fi
    done
#   link in input file
    ln -s $Data/test$Test.inp test$Test.inp 

    $Prgm/bin/dr3m <<-EOT
	$Data/test$Test.mtr
	EOT

#   remove linked files
    rm test$Test.inp

    Test=`expr $Test + 1`

  done

  echo
  echo
  echo _________________________________________________________________
  echo "Completed DR3M test runs $Start to $Stop"
  echo
# check output against original output in data directory
  $Chck/check.sh $Data

# end of shell
