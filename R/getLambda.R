#' Retrieve the lambda values for all or specific outcomes 
#' for each learning event.
#' 
#' @description For a given set of training data, 
#' the lambda values are returned for each or specific outcomes. 
#' The values are returned as data frame.
#' @export
#' @import data.table
#' @param data Data with columns \code{Cues} and \code{Outcomes},
#' as generated with \code{\link{createTrainingData}}.
#' @param lambda Numeric, value of lambda parameter. Defaults to 1.
#' @param split String, separator between cues or outcomes.
#' @param select.outcomes Optional selection of outcomes to limit the number of 
#' activations that are returned. The value of NULL (default) will 
#' return all activations. Note that specified values that are not in 
#' the weightmatrices will return the initial value without error or warning. 
#' Please use  \code{\link{getValues}} for returning all outcomes in the data.
#' @return Data frame.
#' @author Jacolien van Rij
#' @examples
#' # load example data:
#' data(dat)
#' 
#' # add obligatory columns Cues, Outcomes, and Frequency:
#' dat$Cues <- paste("BG", dat$Shape, dat$Color, sep="_")
#' dat$Outcomes <- dat$Category
#' head(dat)
#' dim(dat)
#' test <- getLambda(dat)
#' # only outcomes that do not occur in data results in 0:
#' test2 <- getLambda(dat, select.outcomes=c("a", "b", "C"))
#' 

getLambda <- function(data, 
	lambda=1, split="_",
	select.outcomes = NULL){

  	# check columns Cues, Outcomes
	if(!all(c("Cues", "Outcomes") %in% colnames(data))){
		stop("Data frame should contain columns 'Cues' and 'Outcomes'.")
	}
	out <- getValues(data$Outcomes, unique=TRUE)
	if(!is.null(select.outcomes)){
		out <- select.outcomes
	}

	l <- rep(0, length(out))
	names(l) <- out
	l <- data.frame(as.list(l))

	cur.out <- strsplit(as.character(data$Outcomes), split=split)
	cur.lambda <- lapply(cur.out, function(x){
	cur.l <- l
	if(any(x %in% names(cur.l))){
	  cur.l[which(names(cur.l) %in% x)] <- lambda
	}
	return(cur.l)
	})
  
	cur.lambda <- as.data.frame(rbindlist(cur.lambda))
	return(cur.lambda)
}