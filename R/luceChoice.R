#' Function implementing the Luce choice rule.
#'
#' @export
#' @import utils
#' @param value A positive value specifying a weight or activation 
#' (or comparable measure) of the choice option for which the choice probability 
#' is calculated
#' @param all A positive array of the weights or activations of all possible 
#' choice options, including \code{value}
#' @return A value between [0,1]
#' @author Dorothee Hoppe
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
#' wm <- RWlearning(train)
#' 
#' # caculate activations of outcomes given the cue set blue_car
#' red_rabbit <- getActivations(getWM(wm), cueset = "red_rabbit")$red_rabbit
#' 
#' # caculate choice probability of outcomes given the cue set blue_car after 
#' # normalizing with rectified linear unit
#' luceChoice(red_rabbit["vehicle"], red_rabbit)
#' luceChoice(red_rabbit["plant"], red_rabbit)
#' luceChoice(red_rabbit["animal"], red_rabbit)
#' 
#' # note that when some activations are negative, this rule either should not be 
#' # applied, or negative values have to be corrected for, e.g., with applying a 
#' # rectified linear unit (relu)
#' blue_car <- getActivations(getWM(wm), cueset = "blue_car")$blue_car
#' 
#' \dontrun{
#' # this is should not be done without correction
#' luceChoice(blue_car["vehicle"], blue_car)
#' # use, e.g., function relu() on the raw values
#' }
#' 



luceChoice <- function(value, all){

  if(!value %in% all){
    stop("value needs to be included in all")
  }
  
  if(length(all)<2){
    stop("Specify values of all choice possibilities in all")
  }
  
  if(any(all < 0)){
    stop("Don't apply this function to negative values")
  }
  
  return(value/sum(all))
}

