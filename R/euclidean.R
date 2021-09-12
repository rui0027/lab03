#'euclidean algorithm
#'
#'@description This function is a loop algorithm to find the greatest common
#'divisor (GCD) of two nonzero number x and y using euclidean algorithm. If the
#'lager number devided by the smaller number has a nonzero remainder,replace the
#'lager number with remainder.Reverse the step until the remainder is zero, then
#'the denominator is GCD of the original two numbers.
#'@param x A number
#'@param y A number
#'@return greatest common divisor (GCD) of x and y
#'@examples
#'euclidean(100,1000)
#'euclidean(1234,8888)
#'@references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean<-function(x,y){
  stopifnot(x!=0&y!=0)
  x<-abs(x)
  y<-abs(y)
  repeat{
    z<-max(x,y)%%min(x,y)
    if(z==0){
      break
    }else{
        x<-min(x,y)
        y<-z}
  }
  return(min(x,y))
}
