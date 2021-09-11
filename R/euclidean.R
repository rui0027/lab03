#'eulidean algorithms
#'
#'@description
#'The Euclidean algorithm calculates the greatest common divisor (GCD) of two
#'natural numbers a and b. The greatest common divisor g is the largest natural
#'number that divides both a and b without leaving a remainder. Synonyms for the
#'GCD include the greatest common factor (GCF), the highest common factor (HCF),
#'the highest common divisor (HCD), and the greatest common measure (GCM). The
#'greatest common divisor is often written as gcd(a, b) or, more simply, as (a,
#'b), although the latter notation is ambiguous, also used for concepts such as
#'an ideal in the ring of integers, which is closely related to GCD.If gcd(a, b)
#'= 1, then a and b are said to be coprime (or relatively prime).This property
#'does not imply that a or b are themselves prime numbers.Let g = gcd(a, b).
#'Since a and b are both multiples of g, they can be written a = mg and b = ng,
#'and there is no larger number G > g for which this is true. The natural
#'numbers m and n must be coprime, since any common factor could be factored out
#'of m and n to make g greater. Thus, any other number c that divides both a and
#'b must also divide g. The greatest common divisor g of a and b is the unique
#'(positive) common divisor of a and b that is divisible by any other common
#'divisor c.
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
  return(x)
}
