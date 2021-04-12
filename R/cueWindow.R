#' Create a 'cue window', for overlapping or continuous cues.
#' 
#' @export
#' @import stats
#' @param x A vector with numeric cues.
#' @param n Numeric value specifying the window size. If \code{n} has two 
#' values, the first value indicates the left window size, and the second 
#' value the size of the right window.
#' @param step Numeric value, indicating the difference between adjacent 
#' cues values. Set to 1 by default.
#' @param weights A vector with weights (round numbers) for multiplying the  
#' elements within the window. Defaults to NULL (which will give all cues 
#' the same weight).
#' @param min Numeric value specifying the lowest value on the scale. 
#' Defaults to 1.
#' @param max Numeric value specifying the maximum value on the scale. 
#' Defaults to 100.
#' @param round.values Logical, whether or not to round the values of 
#' \code{x} to multiples of \code{step} on the continuum between \code{min} 
#' and \code{max}. Defaults to TRUE.
#' @param split String, specifying the cue separator. Default value is "_".
#' @param premark String, specifying a character to add before each cue.
#' @param postmark String, specifying a character to add after each cue.
#' @param as.numeric Logical, whether or not to return the numeric values 
#' of the window as a list. Default is FALSE (return cue sets as a vector of 
#' strings).
#' @param dec Number of decimals for rounding. Defaults to NULL 
#' (automatically determined).
#' @return A vector of strings (default), or a list with vectors of numbers.
#' @author Jacolien van Rij
#' @examples
#' # generate random sample of cues on continuum of 1-10,
#' # with sep=1:
#' set.seed(123)
#' cues <- round(runif(20, min=0.5, max=10.5),1)
#' 
#' # Note that cues will be converted to rounded numbers
#' # as round.values=TRUE. With cue window of 3:
#' cueWindow(cues, n=3, max=10)
#' # step of 0.5 increases number of neighboring cues:
#' cueWindow(cues, n=3, max=10, step=.5)
#' # cue window of 5:
#' cueWindow(cues, n=5, max=10)
#' # asymmetrical window:
#' cueWindow(cues, n=c(2,1), max=10, step=.5)
#' 
#' # non-uniform weights:
#' cueWindow(cues, n=5, max=10, weights=c(1,2,3,2,1)) 
#' cueWindow(cues, n=2.5, max=10, step=.5, weights=c(1,2,3,2,1)) 
#' # left cues have stronger weights:
#' cueWindow(cues, n=5, max=10, weights=c(3,3,2,1,1))
#' # adjust weights, so that cue itself is not included:
#' cueWindow(cues, n=c(2,1), max=10, weights=c(1,1,0,1))
#' # premarking:
#' cueWindow(cues, n=2, max=10, weights=c(1,1,1), premark="stimulus")
#' # numeric output:
#' cueWindow(cues, n=2, max=10, weights=c(1,2,1), as.numeric=TRUE)
#' 

cueWindow <- function(x, n=1, step=1,
    weights=NULL, min=1, max=100, round.values=TRUE,
    split="_", premark="", postmark="", as.numeric=FALSE,
    dec=NULL){

    n.before <- n.after <- NULL
    if(length(n)==1){
        n.before <- n.after <- n/2
    }else if(length(n)>=2){
        n.before <- n[1]
        n.after  <- n[2]
    }
    # check whether window size is consistent:
    window <- unique(c(-1*rev(seq(from=0,to=n.before, by=step)),
            seq(from=0,to=n.after, by=step)))
    # check weights:
    if(is.null(weights)){
        weights <- rep(1, length(window))
    }else if(length(weights)!=length(window)){
        stop("Length of weights is different from the window specified by n.before and n.after.")
    }
    # round values:
    if(round.values){
        x <- round(x*(1/step))*step
    }

    out <- sapply(x, function(y){
        rep( window+y, weights)
    }, simplify = FALSE)

    if(as.numeric){
      out <- lapply(out, function(y){ 
          y <- y[y >= min & y <= max]
          return(aggregate(list(weight=y), list(x=y), length))
        })    
      return(out)
    }else{
      out <- lapply(out, function(y){ 
          y <- y[y >= min & y <= max] 
          if(is.null(dec)){
            dec <- ceiling(log10(1/step))
          }
          
        return( paste( sprintf(sprintf("%%s%%.%df%%s", dec), premark, y, postmark), collapse=split) )
        })
      return(unlist(out))
    }
}
