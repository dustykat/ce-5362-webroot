      program time2drain_analytical
c========
c Author: T.G. Cleveland
c Date:   2008_0104
c Purpose: Pedagogical tool for CE 5362
c
c====Time to drain by analytical expression====
c     produces two columns: time and depth
c    input:  depth1, areaT, areaO, tprt
c      depth1 = initial depth at time zero
c      areaT = tank area
c      areaO = outlet area
c      tprt = print frequency ( a time step )
c====Spacing guide
c23456789012345678901234567890
c        1         2         3
c====Declare Variables====
      real*8 depth1,areaT,areaO,tprt
      real*8 alpha
      real*8 time(1000),depth(1000)
c====Read from STDIO (redirection)====
      read(*,*)depth1,areaT,areaO,tprt
c====Compute constant====
      alpha=sqrt(2.0D0*9.8D0)*areaO/areaT
c====Initial Conditions====
      time(1)=0.0D0
      depth(1)=depth1
c====Compute time to drain====
      tdrain=2.0D0*sqrt(depth1)/alpha
c====Now make the time series, above is stopping value====
      do 1001 irow=2,1000
	   time(irow)=time(irow-1)+tprt
c==== Exit loop test, crude programming style====
	   if(time(irow) .ge. tdrain) goto 1002
	   depth(irow)=(sqrt(depth1) - 0.5D0*alpha*time(irow))**2
 1001 continue
c===If normal exit more time remains====
c===Would normally report in this block and do someting====
 1002 continue
c===Prepare and write the output to STDIO (redirection)====
      write(*,*)'time','  ','depth'
      do 1003 jrow=1,irow-1
       write(*,*)time(jrow),depth(jrow)
 1003 continue
      stop
      end
