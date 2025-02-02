% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RWlearning.R
\name{RWlearning}
\alias{RWlearning}
\title{Function implementing the Rescorla-Wagner learning.}
\usage{
RWlearning(
  data,
  wm = NULL,
  eta = 0.01,
  lambda = 1,
  alpha = 0.1,
  beta1 = 0.1,
  beta2 = 0.1,
  progress = TRUE,
  ...
)
}
\arguments{
\item{data}{A data frame with columns \code{Cues} and \code{Outcomes}.}

\item{wm}{A weightmatrix of class matrix, or a list with weight matrices. 
If not provided a new 
weightmatrix is returned. Note that the cues and outcomes do not 
necessarily need to be available as cues and outcomes in the weightmatrix: 
if not present, they will be added.}

\item{eta}{Learning parameter, typically set to 0.01. 
If \code{eta} is not specified and set to the value NULL, 
the values of \code{alpha}, \code{beta1}, and \code{beta2} 
determine the learning rate. However, changing these settings 
is generally not very useful (see Hoppe et al, submitted).}

\item{lambda}{Constant constraining the connection strength.}

\item{alpha}{Learning parameter (scaling both positive and negative 
evidence adjustments), typically set to 0.1. Only used if 
\code{eta=NULL}.}

\item{beta1}{Learning parameter for positive evidence, typically set to 
0.1. Only used if \code{eta=NULL}.}

\item{beta2}{Learning parameter for negative evidence, typically set to 
0.1. Only used if \code{eta=NULL}.}

\item{progress}{Logical: whether or not showing a progress bar 
(may slow down the process).}

\item{...}{Parameters for the function \code{\link{getValues}}.}
}
\value{
A list with weightmatrices for each learning event.
}
\description{
Function implementing the Rescorla-Wagner learning.
}
\examples{
# load example data:
data(dat)

# add obligatory columns Cues, Outcomes, and Frequency:
dat$Cues <- paste("BG", dat$Shape, dat$Color, sep="_")
dat$Outcomes <- dat$Category
dat$Frequency <- dat$Frequency1
head(dat)
dim(dat)

# now use createTrainingData to sample from the specified frequencies: 
train <- createTrainingData(dat)

# this training data can actually be used train network:
wm <- RWlearning(train)

# in R markdown or knitr reports the progress bar should be turned off:
wm <- RWlearning(train, progress=FALSE)

# Learning in steps is also possible:
wm <- RWlearning(train[1:20,])
getWM(wm)
length(wm)

train[21,c("Cues", "Outcomes")]
wm <- RWlearning(train[21,], wm=wm)
getWM(wm)
length(wm)

}
\seealso{
\code{\link[ndl]{RescorlaWagner}}, \code{\link{updateWeights}}
}
\author{
Jacolien van Rij
}
