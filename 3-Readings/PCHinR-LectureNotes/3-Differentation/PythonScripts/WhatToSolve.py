# external functions module name: WhatToSolve.py
#  import built in function for e^x, cosine
from math import exp, cos, sin
# Define the function
def func(x):
    func = exp(x) - 10*cos(x) - 100  #using the name as the temp var
    return func
# Define the derivative function
def dfdx(x):
#    dfdx = exp(x) + 10*sin(x)
    dfdx = (func(x + 1e-06) - func(x) )/ (1e-06)
    return dfdx
