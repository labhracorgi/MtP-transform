formula.mtop = number ~ -1 +
   	f(id, model="iid",
    hyper = list(
       prec = list(
       initial = log(0.000001),
       fixed = TRUE))) +
    f(resp2, model="iid",
        constr = FALSE,
        hyper = list(
        prec = list(
        initial = log(0.001),
        fixed = TRUE))) + (...)
        
#The (...) part will be where the console output from the mtop.inla(...) function should be copied. Unless it is desirable to write the formula yourself.
#The only parts that may need to be changed is the "id" which should correspond to the particular problem at hand.
