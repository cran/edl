#' Function implementing the Rescorla-Wagner learning equations without cue 
#' competition (for illustration purposes).
#'
#' @export
#' @import utils
#' @param data A data frame with columns \code{Cues} and \code{Outcomes}.
#' @param wm A weightmatrix of class matrix, or a list with weight matrices. 
#' If not provided a new 
#' weightmatrix is returned. Note that the cues and outcomes do not 
#' necessarily need to be available as cues and outcomes in the 
#' weightmatrix: if not present, they will be added.
#' @param eta Learning parameter, typically set to 0.01. 
#' If \code{eta} is not specified and set to the value NULL, 
#' the values of \code{alpha}, \code{beta1}, and \code{beta2} 
#' determine the learning rate. However, changing these settings 
#' is generally not very useful (see Hoppe et al, submitted). 
#' @param lambda Constant constraining the connection strength.
#' @param progress Logical: whether or not showing a progress bar 
#' (may slow down the process).
#' @param alpha Learning parameter (scaling both positive and negative 
#' evidence adjustments), typically set to 0.1. Only used if 
#' \code{eta=NULL}.
#' @param beta1 Learning parameter for positive evidence, typically set to 
#' 0.1. Only used if \code{eta=NULL}.
#' @param beta2 Learning parameter for negative evidence, typically set to 
#' 0.1. Only used if \code{eta=NULL}.
#' @param ... Parameters for the function \code{\link{getValues}}.
#' @return A list with weightmatrices for each learning event.
#' @seealso \code{\link[ndl]{RescorlaWagner}}, 
#' \code{\link{updateWeightsNoCueCompetition}}
#' @author Dorothee Hoppe
#' @family functions for explaining error-driven learning
#' @examples
#' # load example data:
#' data(dat)
#' 
#' # add obligatory columns Cues, Outcomes, and Frequency:
#' dat$Cues <- paste("BG", dat$Shape, dat$Color, sep="_")
#' dat$Outcomes <- dat$Category
#' dat$Frequency <- dat$Frequency1
#' head(dat)
#' dim(dat)
#' 
#' # now use createTrainingData to sample from the specified frequencies: 
#' train <- createTrainingData(dat)
#' 
#' # this training data can actually be used train network:
#' wm <- RWlearningNoCueCompetition(train)
#' 
#' # in R markdown or knitr reports the progress bar should be turned off:
#' wm <- RWlearningNoCueCompetition(train, progress=FALSE)
#' 
#' # Learning in steps is also possible:
#' wm <- RWlearningNoCueCompetition(train[1:20,])
#' getWM(wm)
#' length(wm)
#' 
#' train[21,c("Cues", "Outcomes")]
#' wm <- RWlearningNoCueCompetition(train[21,], wm=wm)
#' getWM(wm)
#' length(wm)
#' 

RWlearningNoCueCompetition <- function(data, wm=NULL, 
                       eta = 0.01, lambda = 1, 
                       alpha = 0.1, beta1 = 0.1, beta2 = 0.1,
                       progress=TRUE, ...){
  
  if(!all(c("Cues", "Outcomes") %in% names(data))){
    stop("Specify a column Cues and a column Outcomes in data.")
  }
  
  out <- list()
  lout <- 0
  
  if(!is.null(wm)){
    if(is.list(wm)){
      out <- wm
      lout <- length(wm)
      wm <- wm[[length(wm)]]
    }
    if(!is.matrix(wm)){
      stop(sprintf("Argument wm cannot be class %s: wm should specify weight matrix or list of weight matrices.", class(wm)[1]))
    }
  }
  
  if(progress & (nrow(data) > 2)){
    pb <- txtProgressBar(style=3, min=1, max=nrow(data))
    step <- min( c( max( c(1, round(.01*nrow(data)) )), nrow(data)) )
    
    for(i in 1:nrow(data)){
      if((i %% step == 0) | i==1 | i == nrow(data)){
        setTxtProgressBar(pb, i)
      }
      wm <- updateWeightsNoCueCompetition(cur.cues=getValues(data[i,]$Cues, ...), 
                          cur.outcomes=getValues(data[i,]$Outcomes, ...), 
                          wm=wm, eta=eta, lambda = lambda, 
                          alpha = alpha, beta1 = beta1, beta2 = beta2)
      out[[length(out)+1]] = wm
    }
    close(pb)
  }else{
    for(i in 1:nrow(data)){
      wm <- updateWeightsNoCueCompetition(cur.cues=getValues(data[i,]$Cues, ...), 
                          cur.outcomes=getValues(data[i,]$Outcomes, ...), 
                          wm=wm, eta=eta, lambda = lambda, 
                          alpha = alpha, beta1 = beta1, beta2 = beta2)
      out[[length(out)+1]] = wm
    }
  }
  
  return(out)
}
