# Python Script to Illustrate Newton's Method
# Script has three parts:
#  1)  Define f(x) the function to be set to zero
#  2)  Define dfdx (x)  the derivative function of f
#  3)  Define Newton Method
#  import built in function for e^x, cosine
from math import exp, cos, sin
# Define the function
def func(x):
    func = exp(x) - 10*cos(x) - 100  #using the name as the temp var
    return func
# Define the derivative function
def dfdx(x):
    dfdx = exp(x) + 10*sin(x)
    return dfdx
# Now for the Newton Method Implementation
# Get initial guess, use a simple error trap
yes=0
while yes == 0:
    xnow = raw_input("Enter an initial guess for Newton method \n")
    try:
        xnow = float(xnow)
        yes =1
    except:
        print "Value should be numeric, try again \n"
# Get number trials, use a simple error trap
yes=0
while yes == 0:
    HowMany = raw_input("Enter iteration maximum \n")
    try:
        HowMany = int(HowMany)
        yes =1
    except:
        print "Value should be numeric, try again \n"
# Get stopping criterion
yes=0
while yes == 0:
    HowSmall = raw_input("Enter a solution tolerance (e.g. 1e-06) \n")
    try:
        HowSmall= float(HowSmall)
        yes =1
    except:
        print "Value should be numeric, try again \n"
# now we begin the process
count = 0
for i in range(0,HowMany,1):
    xnew = xnow - func(xnow)/dfdx(xnow)
# stopping criteria -- update not changing
    if abs(xnew - xnow) < HowSmall:
        print "Update not changing \n"
        print("Function value =",func(xnew))
        print(" Root value    =",xnew)
        break
    else:
        xnow = xnew
        count = count +1
        continue
# stopping criteria -- function close to zero
    if abs( func(xnew) ) < HowSmall:
        print "Function value close to zero \n"
        print("Function value =",func(xnew))
        print(" Root value    =",xnew)
        break
    else:
        xnow = xnew
        count = count +1
        continue
# next step, then have either broken from the loop or iteration counted out
if count == HowMany:
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
        

