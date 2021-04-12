#' Remove empty cues and/or outcomes.
#' 
#' @export
#' @param data Data frame with columns \code{Cues} and \code{Outcomes}.
#' @param rm Logical: whether or not to remove empty strings. (Default TRUE).
#' @return data frame or numeric vector (see details)
#' @details
#' When \code{rm=FALSE} the function returns a code for each row of the data 
#' frame indicating whether an empty cue or outcome was detected. 
#' The function may return the following values:
#' \describe{
#'     \item{0}{No empty cues and outcomes were detected in this row.}
#'     \item{1}{Empty cue(s) but not empty outcomes were detected in this row.}
#'     \item{2}{Empty outcome(s) but not empty cues were detected in this row.}
#'     \item{3}{Empty cue(s) AND empty outcome(s) were detected in this row.}
#' }   
#' @author Jacolien van Rij
#' @examples
#' test1 <- c("a_b", "a__b", "_a_b", "a_b_", "_a__b_", "___")
#' \dontrun{
#'     # this returns an error:
#'     check(test1)
#' }
#' # data frame with cues and outcomes:
#' (dat <- data.frame(Cues=test1, Outcomes=sample(test1), stringsAsFactors=TRUE))
#' # remove empty:
#' check(dat)
#' # only indicating which rows contain empty cues/outcomes:
#' (test <- check(dat, rm=FALSE))
#' # check empty cues:
#' dat[test %in% c(1,3),]
#' # check empty outcomes:
#' dat[test %in% c(2,3),]
#' 

check <- function(data, rm=TRUE){
	if(!all(c("Cues", "Outcomes") %in% names(data))){
		stop("Specify a column Cues and a column Outcomes in data.")
	}
	rmEmpty <- function(input){
		out <- gsub("^(.*)([\\_]+)$", "\\1", input)
		out <- gsub("^([\\_]+)(.*)$", "\\2", out)
		out <- gsub("[\\_]+", "\\_", out)
		return(out)
	}
	findEmpty <- function(input){
		out <- grepl("^(.*)([\\_]+)$", input) | grepl(".*[\\_]{2}.*", input) | grepl("^([\\_]+)(.*)$", input)
		return(out)
	}
	if(rm){
		data$Cues <- rmEmpty(data$Cues)
		data$Outcomes <- rmEmpty(data$Outcomes)
		return(data)
	}else{
		out <- ifelse(findEmpty(data$Cues) & findEmpty(data$Outcomes), 3, 
			ifelse(findEmpty(data$Cues), 1, ifelse(findEmpty(data$Outcomes), 2, 0)))
		return(out)
	}
}