#' Function implementing the Rescorla-Wagner learning equations without cue 
#' competition for a single learning event.
#' 
#' @description Function implementing the Rescorla-Wagner learning equations 
#' without cue competition (for illustration purposes) for a single learning 
#' event. A set of cues and outcomes are provided, and a weightmatrix that 
#' needs to be updated.
#' @export
#' @param cur.cues A vector with cues.
#' @param cur.outcomes A vector with outcomes.
#' @param wm A weightmatrix of class matrix. If not provided a new 
#' weightmatrix is returned. Note that the cues and outcomes do not 
#' necessarily need to be available as cues and outcomes in the 
#' weightmatrix: if not present, they will be added.
#' @param eta Learning parameter, typically set to 0.01. 
#' If \code{eta} is not specified and set to the value NULL, 
#' the values of \code{alpha}, \code{beta1}, and \code{beta2} 
#' determine the learning rate. However, changing these settings 
#' is generally not very useful (see Hoppe et al, submitted). 
#' @param lambda Constant constraining the connection strength.
#' @param alpha Learning parameter (scaling both positive and negative 
#' evidence adjustments), typically set to 0.1. 
#' @param beta1 Learning parameter for positive evidence, typically set to 
#' 0.1. 
#' @param beta2 Learning parameter for negative evidence, typically set to 
#' 0.1. 
#' @return A weightmatrix (matrix)
#' @seealso \code{\link[ndl]{RescorlaWagner}}, \code{\link{RWlearning}}
#' @author Dorothee Hoppe, based on \code{\link[ndl]{RescorlaWagner}}
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
#' # retrieve trained network:
#' new <- getWM(wm)
#' 
#' train2 <- createTrainingData(dat)
#' updateWeightsNoCueCompetition(getValues(train2$Cues[1]), 
#'     getValues(train2$Outcomes[1]), wm=new)
#' 
#' # comparison between eta and alpha, beta1, beta2:
#' check.cues <- c("BG", "car", "red")
#' new[check.cues,]
#' tmp1 <- updateWeights(check.cues, 
#'     c("vehicle", "animal"), wm=new)
#' tmp2 <- updateWeights(check.cues, 
#'     c("vehicle", "animal"), wm=new, eta=NULL)
#' tmp3 <- updateWeights(check.cues, 
#'     c("vehicle", "animal"), wm=new, beta1=0.2)
#' tmp4 <- updateWeights(check.cues, 
#'     c("vehicle", "animal"), wm=new, eta=NULL, beta1=0.2)
#' # these two should be the same:
#' tmp1[check.cues,]
#' tmp2[check.cues,]
#' # now we change beta2, but this does not change anything,
#' # because eta is being used:
#' tmp3[check.cues,]
#' # when we turn eta off, beta2 changes the values:
#' tmp4[check.cues,]
#' 

updateWeightsNoCueCompetition <- function(cur.cues, cur.outcomes, 
                                          wm=NULL, eta=0.01, lambda = 1, 
                                          alpha = 0.1, beta1 = 0.1, beta2 = 0.1){
  
  bg <- getOption("background")
  cur.cues <- c(bg, cur.cues)
  
  # if no wm is specified, create new wm:
  if(is.null(wm)){
    wm <- createWM(cues=cur.cues, outcomes=cur.outcomes)
  }else{
    wm <- checkWM(cues=cur.cues, outcomes=cur.outcomes, wm=wm)
  }
  
  LambdaArray = rep(0, ncol(wm))
  LambdaArray[which(colnames(wm) %in% cur.outcomes)] <- lambda
  Lambda = matrix(rep(LambdaArray, length(cur.cues)), nrow=length(cur.cues), byrow=TRUE)
  
  # determine learning rate:
  lr = rep(eta, length(LambdaArray))
  if(is.null(eta)){
    lr = alpha * (beta1*LambdaArray + beta2*(lambda-LambdaArray))
  }
  LR = matrix(rep(lr, length(cur.cues)), nrow=length(cur.cues), byrow=TRUE)
  
  wm[cur.cues,] =  wm[cur.cues,] + LR * (Lambda - wm[cur.cues,])
  return(wm)
}
