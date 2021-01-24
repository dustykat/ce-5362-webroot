#!/bin/sh
#
#    check.sh:  Compare (using diff) 'original' versions of files to
#               files in current directory.  Used most often to compare
#               expected files to actual test output.
#   arguments:  DirO - directory containing original, expected versions
#                      defaults to ../data
#               Name - prefix of file names
#                      defaults to test
#  file names:  naming convention is "nameN.sfx", where:
#               name - "test" by default, or supplied as second argument
#                  N - test numbers, see Test below
#                sfx - suffix for file names, see Sufx below
#
#     history:  3/14/94  kmflynn  modified version of check.sh

  DirO=${1-../data}             # by default, data in parallel directory
  Name=${2-test}                # by default, file prefix is test

# define range of tests and sufixes to be considered
  Tests='2 3 4 5'
  Sufix='out dmp pks unt'

# delete old file and write heading to file
  if [ -f check.out ] ; then rm check.out ; fi
  echo "Comparing original output files and new output files" | tee check.out
  echo `date`                                                 | tee -a check.out
  echo "Original files in " $DirO                             | tee -a check.out
  echo "  "                                                   | tee -a check.out

  for Test in $Tests ; do
     for Sufx in $Sufix ; do
        if [ -f $DirO/$Name$Test.$Sufx -a -f $Name$Test.$Sufx ] ; then
           # both original and new output files exist, compare
           echo "_________________________________________________________ " \
                $Name$Test.$Sufx | tee -a check.out
           if diff $DirO/$Name$Test.$Sufx $Name$Test.$Sufx >> check.out
              then
              echo FILES ARE IDENTICAL | tee -a check.out
           else
              echo FILES DIFFER:  see file check.out for differences
           fi
        fi
     done
  done

# end of shell
