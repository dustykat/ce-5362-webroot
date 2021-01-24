# Example of user-defined function
def dusty(x):
    temp = x * ((1.0+x)**(0.5)) # should use math package
    return temp
# get an input value, trap simple errors
yes = 0
while yes == 0:
    xvalue = raw_input("Enter a value for x \n")
    try:
        xvalue = float(xvalue)
        yes = 1
    except:
        print "value should be numeric, try again \n"
# call the function, get result and write to console
yvalue = dusty(xvalue)
print "f(",xvalue,") = ",yvalue
#  and we should be done!

