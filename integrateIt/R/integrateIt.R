#' Finding a definite integral under a curve
#'
#' Uses either Simpson's method or Trapezoidal method to approximate an integral
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x} where that is an evaluluation of \code{x} using the function.
#' @param a A number indicating the first point in \code{x}
#' @param b A number indicating the second point in \code{x}
#' @param rule A string taking on values "Trap" and "Simp" indicating which rule should be sued
#'
#' @return An object of class Trapezoid or Simpson containing
#'  \item{x}{The first object input corresponding to \code{x} in the call} 
#'  \item{y}{The second object input corresonding to \code{y} in the call}
#'  \item{answer}{The estimate of the integral}
#' @author Jacob M. Montgomery
#' @note Could optionally put some details about how the two methods estimate integrals here.
#' @examples
#' 
#' tryx<-seq(-5, 4.99, by=.95)
#' tryy<-dnorm(tryx)
#' exampSimp<-integrateIt(tryx, tryy, -5, 5, rule="Simp")
#' exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")
#' @seealso \code{\link{Trapezoid}}, \code{\link{Simpson}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
integrateIt<-function(x, y, a, b, rule="Trap"){
  ## Do the trapezoidal
  n<-length(x) # number of points
  if(rule=="Trap"){
    h<-(b-a)/(2*n) # calculate h. Put 2 in the denominator
    answer<-h*(2*sum(y[2:(n-1)])+y[1]+y[n]) # calculate the outcome
    out<-new("Trapezoid", x=x, y=y, answer=answer)
  }
  else {
    h<-(b-a)/(n*3) ## calculate h.  Put 3 in the denominator
    first<-y[1] ## first element
    last<-y[n] ## second element
    evens.sum<-sum(y[seq(2, n-1, by=2)]) ## every other elements excludign first and last
    odds.sum<-sum(y[seq(3, n-2, by=2)]) # the rest
    answer<-h*(first+4*evens.sum + 2*odds.sum + last) #calculate output
    out<-new("Simpson", x=x, y=y, answer=answer)
  }
}
