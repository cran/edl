#' Create event training data from a frequency data frame.
#' 
#' @export
#' @param data Data frame with columns \code{Cues} and \code{Outcomes},
#' and optionally \code{Frequency}.
#' @param nruns Numeric: number of times to run through the data.
#' @param random Logical: randomize the data or not (defaults to TRUE).
#' @param within.runs Logical: apply setting of \code{random} to the data 
#' _within_ each run (if set to TRUE) or over all data (if set to 
#' FALSE). Default setting is FALSE. 
#' Note that to randomize the data within seprate runs, both \code{random} 
#' and \code{within.runs} should be set to TRUE.
#' @param add.id Logical: whether or not to add columns that identify events 
#' (default is TRUE). The column \code{Item} is added to describe each type 
#' of event (unless this column already exists in \code{data}), the column 
#' \code{Run} is added when \code{within.runs=TRUE}, and the column 
#' \code{Trial} indicates the order of events within the data frame or 
#' within the run (when \code{within.runs=TRUE}).
#' @param check Logical: check for empty strings ("") or not (defaults 
#' to TRUE). If empty strings are found, they will be removed.
#' @return data frame
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
#' head(train)
#' dim(train)
#' # the rows should be equal to the sum of frequencies in dat:
#' sum(dat$Frequency)
#' 
#' # this training data can actually be used train network:
#' wm <- RWlearning(train)
#' # inspect weight matrix:
#' wm[[1]]
#' 
#' # retrieve cues and outcomes from data:
#' c <- getCues(wm)
#' o <- getOutcomes(wm)
#' # add missing cues to initial weight matrix:
#' checkWM(c, o, wm=wm[[1]])
#' 
#' # -------------------
#' # additional possibility for  
#' # simulating experimental designs:
#' # -------------------
#' dat$Frequency <- dat$Frequency2
#' train2 <- createTrainingData(dat, nruns=5)
#' head(train2)
#' # items are completely randomized, 
#' # and not equally distributed over the experiment:
#' train2$Run <- rep(1:5, each=(nrow(train2)/5))
#' table(train2$Run, train2$Item)
#' # in this way the items are randomized within each run: 
#' train3 <- createTrainingData(dat, nruns=5, within.runs=TRUE)
#' head(train3)
#' table(train3$Run, train3$Item)
#' # difference in learning (may take some time):
#' \dontrun{
#' wm2 <- RWlearning(train2)
#' plotCueWeights(wm2, cue="brown")	
#' wm3 <- RWlearning(train3)
#' plotCueWeights(wm3, cue="brown")	
#' plotOutcomeWeights(wm3, outcome="animal")	
#' }
#' @family Functions to prepare training data

createTrainingData <- function(data, nruns=1, random=TRUE, 
	within.runs=FALSE, add.id=TRUE, check=TRUE){
	# checks:
	if(!all(c("Cues", "Outcomes") %in% names(data))){
		stop("Specify a column Cues and a column Outcomes in data.")
	}
	if(! "Frequency" %in% names(data)){
		data$Frequency <- 1
	}
	if(check){
		data$Cues <- gsub("^([\\_]*)(.*)([\\_]*)", "\\2", data$Cues)
		data$Cues <- gsub("[\\_]+", "\\_", data$Cues)
	}
	if(add.id){
		if(!"Item" %in% names(data)){		
			if(nrow(data) < 99){
				data[, "Item"] <- sprintf("item_%02d", 1:nrow(data))
			}else if(nrow(data) < 999){
				data[, "Item"] <- sprintf("item_%03d", 1:nrow(data))
			}else{
				data[, "Item"] <- sprintf("item_%06d", 1:nrow(data))
			}			
		}
	}

	idx    <- rep(1:nrow(data), data$Frequency)
	idx    <- rep(idx, each=nruns[1])
	runs   <- rep(1:nruns[1], length(idx)/nruns[1])
	trials <- 1:length(idx)
	if(random){
		idx <- sample(idx, replace=FALSE)
	}

	if(within.runs){
		idx    <- rep(1:nrow(data), data$Frequency)
		idx    <- rep(idx, nruns[1])
		trials <- rep(1:(length(idx)/nruns[1]), nruns[1])
		runs   <- rep(1:nruns[1], each=length(idx)/nruns[1])

		if(random){
			for(i in 1:nruns[1]){
				idx[runs==i] <- sample(idx[runs==i], replace=FALSE)
			}
		}
	}

	data <- data[idx,]
	row.names(data) <- NULL
	data$Frequency <- 1

	if(add.id){
		if(within.runs){
			if("Run" %in% names(data)){
				warnings("Column 'Run' will be replaced.")
			}
			data[, "Run"]   <- runs
			data[, "Trial"] <- trials
		}else{
			data[, "Trial"] <- trials
		}
	}

	return(data)
}
