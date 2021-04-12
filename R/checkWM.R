#' Check whether cues and outcomes exist in a weight matrix and optionally add.
#' 
#' @export
#' @param cues A vector with cues.
#' @param outcomes A vector with outcomes.
#' @param wm A matrix with connection weights between cues and outcomes.
#' @return A weightmatrix (matrix)
#' @author Jacolien van Rij
#' @examples
#' data(dat)
#' # create training data:
#' dat$Cues <- paste("BG", dat$Shape, dat$Color, sep="_")
#' dat$Outcomes <- dat$Category
#' dat$Frequency <- 1
#' train <- createTrainingData(dat)
#' # train network:
#' wm <- RWlearning(train)
#' # inspect weight matrix:
#' wm[[1]]
#' # retrieve cues and outcomes from data:
#' c <- getCues(wm)
#' o <- getOutcomes(wm)
#' # add missing cues to initial weight matrix:
#' checkWM(c, o, wm=wm[[1]])
#' 

checkWM <- function(cues, outcomes, wm){
	cues <- sort(unique(cues))
	cues <- cues[!is.na(cues)]
	outcomes <- sort(unique(outcomes))
	outcomes <- outcomes[!is.na(outcomes)]

	if(! is.matrix(wm)){
		stop("wm is not a matrix.")
	}

	na.cues <- cues[!cues %in% rownames(wm)]
	if(length(na.cues) > 0){
		tmp <- matrix(rep(0, ncol(wm)*length(na.cues)), ncol=ncol(wm))
		rownames(tmp) <- na.cues
		wm <- rbind(wm, tmp)
	}

	na.out <- outcomes[!outcomes %in% colnames(wm)]
	if(length(na.out) > 0){
		tmp <- matrix(rep(0, nrow(wm)*length(na.out)), nrow=nrow(wm))
		colnames(tmp) <- na.out
		wm <- cbind(wm, tmp)
	}	
	return(wm)
}
