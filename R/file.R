Rcpp::sourceCpp('src/ballsd.cpp')
Rcpp::sourceCpp('src/SuppFuns.cpp')

ackley2D=function(x){
  ackley2D(x)
}

griewank2D=function(x){
  griewank2D(x)
}

ballsd=function(x0,objFunc, 
                sinc, sdec,pinc,pdec, 
                maxIter){
  ballsd(x0,objFunc, 
         sinc, sdec,pinc,pdec, 
         maxIter)
}