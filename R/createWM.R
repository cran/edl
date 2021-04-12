#' Create empty weight matrix based on a set of cues and outcomes.
#' 
#' @export
#' @param cues A vector with cues.
#' @param outcomes A vector with outcomes.
#' @param background A string specifying the background cue. Sets this as the 
#' value of the background cue for all functions in this R session. If NULL, 
#' the current value of the background cue will be used. 
#' @param init.value Initial value for all connections, typically set to 0.
#' @return A weightmatrix (matrix)
#' @author Jacolien van Rij
#' @seealso \code{link{RWlearning}}
#' @examples
#' 
#' # load example data:
#' data(dat)
#' 
#' # add obligatory columns Cues, Outcomes, and Frequency:
#' dat$Cues <- paste("BG", dat$Shape, dat$Color, sep="_")
#' dat$Outcomes <- dat$Category
#' dat$Frequency <- dat$Frequency1
#' head(dat)
#' 
#' # the function RWlearning uses createWM to construct a weight matrix: 
#' cues <- getValues(dat$Cues, unique=TRUE)
#' outcomes <- getValues(dat$Outcomes, unique=TRUE)
#' createWM(cues=cues, outcomes=outcomes)
#' # add background cue:
#' createWM(cues=cues, outcomes=outcomes, background=TRUE)
#' 


createWM <- function(cues, outcomes, background=NULL, init.value=0){
	if(!is.null(background)){
		setBackground(background[1])
	}
	cues <- sort(unique(cues))
	cues <- c(getOption('background'), cues)
	outcomes <- sort(unique(outcomes))

	wm <- matrix(rep(init.value, length(cues)*length(outcomes)), 
		ncol=length(outcomes), nrow=length(cues))
	colnames(wm) <- outcomes
	rownames(wm) <- cues
	return(wm)
}
