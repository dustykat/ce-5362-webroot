# Example of user-defined function
import MyFunctionsModule 
yes = 0
while yes == 0:
    xvalue = raw_input("Enter a value for x \n")
    try:
        xvalue = float(xvalue)
        yes = 1
    except:
        print "value should be numeric, try again \n"
# call the function, get result and write to console
yvalue = MyFunctionsModule.dusty(xvalue)
print "f(",xvalue,") = ",yvalue
#  and we should be done!

