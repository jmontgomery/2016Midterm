#' A Trapezoid object
#' 
#' Object of class \code{Trapezoid} are created by the \code{integrateIt} function
#'
#' 
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{x} The potential values at which the function should be evaluated
#' \item \code{y} The value of the function evaluated at the corresponding values of x
#' \item \code{answer} The estimated integral using the Trapezoidal method

#' }
#'
#' @author Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases Trapezoid-class initialize,Trapezoid-method plot,Trapezoid-method print,Trapezoid-method
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid",
         representation=representation(
           x="numeric",
           y="numeric",
           answer="numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           answer=c()
         ))


setValidity("Trapezoid", function(object){
  sameLength<-length(object@x) == length(object@y)#X and Y should be of the same length
  allData<-all(!is.na(object@x)) & all(!is.na(object@y))
  if(!sameLength | !allData){return("Not appropriately set up")}
} )


#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, ...){
            value=callNextMethod()
            validObject(value)
            return(value)
          }
)



#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export
setMethod("print", "Trapezoid",
          function(x){
            cat("Integration using trapezoidal method \n")
            cat(x@answer)
          }
)

#' @export
setMethod("plot", "Trapezoid", 
          function(x, y=NULL){
            obj<-x
            plot(obj@x, obj@y, type="l")
            segments(obj@x, obj@y, obj@x, 0)
          }
)

