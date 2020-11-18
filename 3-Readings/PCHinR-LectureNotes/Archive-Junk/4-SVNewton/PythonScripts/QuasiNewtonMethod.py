# Python Script to Illustrate Newton's Method
# Script has three parts:
#  1)  Import f(x), and its derivative dfdx(x)
#  2)  Apply Newton Method
from WhatToSolve import func, dfdx
# Newton Method
yes=0 # get starting guess; trap for numeric
while yes == 0:  
    xnow = raw_input("Enter an initial guess for Newton method \n")
    try:
        xnow = float(xnow)
        yes =1
    except:
        print "Value should be numeric, try again \n"
yes=0 # Get number trials, use a simple error trap
while yes == 0:  
    HowMany = raw_input("Enter iteration maximum \n")
    try:
        HowMany = int(HowMany)
        yes =1
    except:
        print "Value should be numeric, try again \n"
yes=0 # Get stopping criterion
while yes == 0:
    HowSmall = raw_input("Enter a solution tolerance (e.g. 1e-06) \n")
    try:
        HowSmall= float(HowSmall)
        yes =1
    except:
        print "Value should be numeric, try again \n"
count = 0 # now we begin the process
for i in range(0,HowMany,1):
    xnew = xnow - func(xnow)/dfdx(xnow)
    if abs(xnew - xnow) < HowSmall:   # stopping criteria -- update not changing
        print "Update not changing \n"
        print("Function value =",func(xnew))
        print(" Root value    =",xnew)
        break
    else:
        xnow = xnew
        count = count +1
        continue
    if abs( func(xnew) ) < HowSmall:    # stopping criteria -- function close to zero
        print "Function value close to zero \n"
        print("Function value =",func(xnew))
        print(" Root value    =",xnew)
        break
    else:
        xnow = xnew
        count = count +1
        continue
if count == HowMany:   # next step, then have either broken from the loop or iteration counted out
    print(" Iteration Limit Reached ")
    print("Function value =",func(xnew))
    print(" Root value    =",xnew)
print("End of NewtonMethod.py ")



# DEBUG ##################################
##    print(" iteration     =",i)
##    print(" xnow          =",xnow)
##    print(" f(xnow)       =",func(xnow))
##    print(" dfdx(xnow)    =",dfdx(xnow))
##    print(" xnew          =",xnew)
##    print(" f(xnew)       =",func(xnew))
##    print(" dfdx(xnew)    =",dfdx(xnew))
##########################################           
        

