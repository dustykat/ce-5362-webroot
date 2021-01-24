#!/bin/sh
# clean.sh - clean up files after test runs
#
# Usage:  clean.sh
#
# History:  95/12/07  kmflynn
#
# remove check output and miscellaneous other output files
  for Fil in check.out ANNIE.LOG ERROR.FIL XPAD.DAT ; do
    if [ -f $Fil ] ; then rm $Fil ; fi
  done
#
# remove wdm
  if [ -f sand.wdm ] ; then rm sand.wdm ; fi
#
# remove output from each test
  for Num in 1 2 3 4 5 6 ; do
    for Sfx in inp out dmp pks unt sgs ; do
      if [ -f test$Num.$Sfx ] ; then rm test$Num.$Sfx ; fi
    done
  done

# end of shell
