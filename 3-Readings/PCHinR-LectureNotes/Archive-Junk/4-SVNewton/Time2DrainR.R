 # Time to Drain Model
 AreaTank<-987
 AreaPipe<-1
 alpha<-sqrt(2*9.8)*AreaPipe/AreaTank
 z<-numeric(0)  # define the depth array
 z[1]<-5.0 # initial depth
 dt<-5.0 # time step size
 dzdt<-function(alpha,depth){if(depth >= 0.001)alpha*sqrt(depth) else 0}
 for (i in 1:500){z[i+1]=z[i]-dt*dzdt(alpha,z[i])}
 time<-seq(0,500)*dt
 plot(time[1:197],z[1:197],xlab="Time (minutes)",ylab="Depth (meters)")