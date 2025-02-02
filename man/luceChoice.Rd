% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luceChoice.R
\name{luceChoice}
\alias{luceChoice}
\title{Function implementing the Luce choice rule.}
\usage{
luceChoice(value, all)
}
\arguments{
\item{value}{A positive value specifying a weight or activation 
(or comparable measure) of the choice option for which the choice probability 
is calculated}

\item{all}{A positive array of the weights or activations of all possible 
choice options, including \code{value}}
}
\value{
A value between [0,1]
}
\description{
Function implementing the Luce choice rule.
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

# caculate activations of outcomes given the cue set blue_car
red_rabbit <- getActivations(getWM(wm), cueset = "red_rabbit")$red_rabbit

# caculate choice probability of outcomes given the cue set blue_car after 
# normalizing with rectified linear unit
luceChoice(red_rabbit["vehicle"], red_rabbit)
luceChoice(red_rabbit["plant"], red_rabbit)
luceChoice(red_rabbit["animal"], red_rabbit)

# note that when some activations are negative, this rule either should not be 
# applied, or negative values have to be corrected for, e.g., with applying a 
# rectified linear unit (relu)
blue_car <- getActivations(getWM(wm), cueset = "blue_car")$blue_car

\dontrun{
# this is should not be done without correction
luceChoice(blue_car["vehicle"], blue_car)
# use, e.g., function relu() on the raw values
}

}
\author{
Dorothee Hoppe
}
