      program time2drain
c====time to drain a tank model
c
c Author: T.G. Cleveland
c Date:   2009_0105
c Purpose: Pedagogical tool for CE 5362 Surface Water Modeling
c
c====variable names:
c    depth = tank depth (an array)
c    time = elapsed time (an array)
c    tankArea = tank surface area (a constant)
c    pipeArea = pipe area (a constant)
c    deltaTime = time step length (a constant)
c
c==== Program testing
c    Works as written using Windows MS Powerstation Fortran
c    Works as written using Windows GNU FORTRAN
c    Works as written using Linux/UNIX g77 FORTRAN
c    Works as written using GNU Fortran 95 (Mac OS X) 
c=== Spacing guide  
c23456789012345678901234567890
c        1         2         3
c====declare variables====
      real*8 depth
      real*8 tankArea,pipeArea,alpha
      real*8 time
      real*8 deltaTime
c====size arrays====
      dimension depth(500),time(500)
c====file name for output====
      character*14 fileout
	  character*14 fileinp
      fileinp='input.txt'
      fileout='output.txt'
c==== attach files to the program ====
      open(unit=11,file=fileinp,status='old')
      rewind(11)
      open(unit=12,file=fileout,status='unknown')
      rewind(12)
c==== tank geometry, time step, and starting values ==== 
      read(11,*)tankArea
	  read(11,*)pipeArea
	  read(11,*)deltaTime
	  read(11,*)depth(1)
	  read(11,*)time(1)	  
c====characteristic geometry/acceleration term ====
      alpha=sqrt(2.0D0*9.8D0)*pipeArea/tankArea
c====begin update formula====
c====using count controlled repetition====
      do 1001 ir=2,500
       depth(ir)=depth(ir-1)-deltaTime*alpha*sqrt(depth(ir-1))
	   time(ir)=time(ir-1)+deltaTime
c====test for loop exit; crude control structure! Not recomended!
       if( depth(ir) .le. 0.001D0 )goto 1002
 1001 continue
c====write output to file====
 1002 continue
c====R expects header row====
      write(12,*)'time',' ','depth'
c====Now for the data====
      do 1003 ij=1,ir-1
       write(12,9001)time(ij),depth(ij)
 1003 continue
      stop
 9001 format(f10.3,2x,f10.3)
      end

