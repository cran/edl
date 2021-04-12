#' Retrieve all cues from a vector of text strings.
#' 
#' @export
#' @param text A vector with text strings containing cues or outcomes, 
#' separated by a symbol specified by \code{split}.
#' @param split separator between cues.
#' @param unique Logical: only return unique values (TRUE) or all values 
#' (FALSE, default). When unique values are bein returned, they are sorted.
#' @param decreasing Logical: sorting in alphabetical order (FALSE, default) or the reverse order (TRUE)? Only applies when \code{unique} is set to TRUE.
#' @return A vector with strings
#' @author Jacolien van Rij
#' @seealso \code{\link{strsplit}}, \code{\link{sort}}, \code{\link{unique}}, 
#' \code{\link{getOutcomes}}, \code{\link{getCues}}
#' @examples
#' 
#' # load example data:
#' data(dat)
#' # prepare training data:
#' dat$Cues <- paste("BG", dat$Shape, dat$Color, sep="_")
#' dat$Outcomes <- dat$Category
#' dat$Frequency <- dat$Frequency1
#' train <- createTrainingData(dat)
#' 
#' # find all cues in trainingdata:
#' cues <- getValues(train$Cues)
#' table(cues)
#' # find all outcomes in data:
#' out <- getValues(train$Outcomes)
#' table(out)
#' # find (sorted) unique cues and outcomes:
#' getValues(dat$Cues, unique=TRUE)
#' getValues(dat$Outcomes, unique=TRUE)
#' 

getValues <- function(text, split="_", unique=FALSE, decreasing=FALSE){
	output <- unlist(strsplit(as.character(text), split="_")) 
	output <- output[!is.na(output)]
	if(unique){
		return(sort(unique(output), decreasing=decreasing))
	}else{
		return(output)
	}
}