#' Function implementing the Rescorla-Wagner learning.
#' 
#' @export
#' @import utils
#' @param data A data frame with columns \code{Cues} and \code{Outcomes}.
#' @param wm A weightmatrix of class matrix, or a list with weight matrices. 
#' If not provided a new 
#' weightmatrix is returned. Note that the cues and outcomes do not 
#' necessarily need to be available as cues and outcomes in the weightmatrix: 
#' if not present, they will be added.
#' @param alpha Learning parameter (scaling both positive and negative 
#' evidence adjustments), typically set to 0.1. 
#' @param beta1 Learning parameter for positive evidence, typically set to 
#' 0.1. 
#' @param beta2 Learning parameter for negative evidence, typically set to 
#' 0.1. 
#' @param lambda Constant constraining the connection strength.
#' @param progress Logical: whether or not showing a progress bar 
#' (may slow down the process).
#' @param ... Parameters for the function \code{\link{getValues}}.
#' @return A weightmatrix.
#' @seealso \code{\link[ndl]{RescorlaWagner}}, \code{\link{updateWeights}}
#' @author Jacolien van Rij
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
#' wm1 <- RWlearningMatrix(train)
#' # comparison with a list:
#' wm2 <- RWlearning(train)
#' length(wm2)
#' getWM(wm2)
#' 
#' # in R markdown or knitr reports the progress bar should be turned off:
#' wm <- RWlearningMatrix(train, progress=FALSE)
#' 
#' # Learning in steps is also possible:
#' wm <- RWlearningMatrix(train[1:20,])
#' 
#' train[21,c("Cues", "Outcomes")]
#' wm <- RWlearningMatrix(train[21,], wm=wm)
#' 

RWlearningMatrix <- function(data, wm=NULL, 
	alpha = 0.1, lambda = 1, beta1 = 0.1, beta2 = 0.1,
	progress=TRUE, ...){
	
	if(!all(c("Cues", "Outcomes") %in% names(data))){
		stop("Specify a column Cues and a column Outcomes in data.")
	}

	out <- NULL
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
				wm <- updateWeights(cur.cues=getValues(data[i,]$Cues, ...), 
					cur.outcomes=getValues(data[i,]$Outcomes, ...), 
					wm=wm, alpha = alpha, lambda = lambda, beta1 = beta1, beta2 = beta2)
				out = wm
			}
			close(pb)
	}else{
		for(i in 1:nrow(data)){
			wm <- updateWeights(cur.cues=getValues(data[i,]$Cues, ...), 
				cur.outcomes=getValues(data[i,]$Outcomes, ...), 
				wm=wm, alpha = alpha, lambda = lambda, beta1 = beta1, beta2 = beta2)
			out = wm
		}
	}

	return(out)
}
