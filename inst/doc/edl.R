## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 4, fig.height = 4)

## ----load-packages,message=FALSE----------------------------------------------
library(edl)

## -----------------------------------------------------------------------------
data(dat)
head(dat)

## -----------------------------------------------------------------------------
dat$Cues      <- paste("BG", dat$Shape, dat$Color, sep="_")
dat$Outcomes  <- paste(dat$Category)
dat$Frequency <- dat$Frequency2
# remove remaining columns to simplify this example:
dat <- dat[, c("Cues", "Outcomes", "Frequency")]
# add ID for learning events:
dat$ID <- 1:nrow(dat)
head(dat)

## -----------------------------------------------------------------------------
table(dat$Outcomes)

## -----------------------------------------------------------------------------
# by default 1 run, with tokens randomized:
train <- createTrainingData(dat)
head(train)
# Frequency is always 1:
unique(train$Frequency)
# total counts per outcome match original frequencies:
table(train$Outcomes)
table(train$ID)

## ---- eval=FALSE--------------------------------------------------------------
#  wm <- RWlearning(train)

## ---- include=FALSE-----------------------------------------------------------
wm <- RWlearning(train, progress = FALSE)

## -----------------------------------------------------------------------------
length(wm)
# ... which is the same as the number of rows in the training data:
nrow(train)

## -----------------------------------------------------------------------------
# after the first learning event:
wm[[1]]
# the final state of the network:
wm[[length(wm)]]

## -----------------------------------------------------------------------------
# after the first learning event:
getWM(wm,1)

## -----------------------------------------------------------------------------
wm2 <- sapply(1:length(wm), function(x){getWM(wm,x)}, simplify = FALSE)
# inspect the list of states:
length(wm2)
wm2[[1]]

## -----------------------------------------------------------------------------
# weights for outcome "plant"
weights <- getWeightsByOutcome(wm, outcome="plant")
head(weights)
tail(weights)

## -----------------------------------------------------------------------------
# weights for cue "red"
weights <- getWeightsByCue(wm, cue="red")
head(weights)
tail(weights)

## -----------------------------------------------------------------------------
act <- getActivations(wm, data=train)
head(act)

## -----------------------------------------------------------------------------
act <- getActivations(wm, data=train, select.outcomes = TRUE)
head(act)

## -----------------------------------------------------------------------------
act$Activation <- apply(act, 1, function(x){
  out <- x['Outcomes']
  return(as.numeric(x[out]))
})
head(act)

## ----plots-1, fig.width=8-----------------------------------------------------
oldpar <- par(mfrow=c(1,2), cex=1.1)

# plot left:
plotCueWeights(wm, cue="brown")

# plot right:
plotOutcomeWeights(wm, outcome="animal")

par(oldpar)

## ----plots-2, fig.width=8-----------------------------------------------------
oldpar <- par(mfrow=c(1,2), cex=1.1)

# plot left:
# 1. get outcome values:
out <- getValues(train$Outcomes, unique=TRUE)
out <- out[out != "animal"]
# 2. plot all outcomes, except 'plural':
lab <- plotCueWeights(wm, cue="brown", select.outcomes = out, 
                      col=1, add.labels=FALSE, xlab='', ylim=range(getWM(wm)))
# 3. add plural:
lab2 <- plotCueWeights(wm, cue="brown", select.outcomes = "animal", col=2, lwd=2, adj=0, add=TRUE, font=2)
# 4. add legend:
legend_margin('bottom', ncol=4, 
              legend=c(lab2$labels, lab$labels), 
              col=c(lab2$col, lab$col), lty=c(lab2$lty, lab$lty), 
              lwd=c(lab2$lwd, lab$lwd), bty='n', cex=.85)


# plot right, different layout variant:
out <- getValues(dat$Cues, unique=TRUE)
out <- out[out != "animal"]
lab <- plotOutcomeWeights(wm, outcome="animal", select.cues = out, 
                          col=alpha(1, f=.25), lty=1, pos=4, ylim=c(-.02,.2), font=2, ylim=range(getWM(wm)))
lab2 <- plotOutcomeWeights(wm, outcome="animal", select.cues = "brown", col='red', lwd=2, pos=4, add=TRUE, font=2)

par(oldpar)

## ----getWeights-1-------------------------------------------------------------
weights <- getWeightsByCue(wm, cue="brown")
head(weights)

## ---- fig.width=8, results='hold'---------------------------------------------
oldpar <- par(mfrow=c(1,2), cex=1.1)

# an observed cueset:
plotActivations(wm, cueset="BG_cat_brown")
# an un-observed cueset:
plotActivations(wm, cueset="BG_cat_yellow")

par(oldpar)

## ----continue-1---------------------------------------------------------------
# create a second data set with different frequencies:
data(dat)
head(dat)

## -----------------------------------------------------------------------------
dat$Cues      <- paste("BG", dat$Shape, dat$Color, sep="_")
dat$Outcomes  <- paste(dat$Category)
dat$Frequency <- dat$Frequency1
# remove remaining columns to simplify this example:
dat <- dat[, c("Cues", "Outcomes", "Frequency")]
# add ID for learning events:
dat$ID <- 1:nrow(dat)
head(dat)

# create training data:
train2 <- createTrainingData(dat)

## -----------------------------------------------------------------------------
# continue learning from last weight matrix:
wm2 <- RWlearning(train2, wm=getWM(wm), progress = FALSE)
# number of learned event matches rows in dat2:
nrow(train2)
length(wm2)

# Alternatively, add the learning events to the existing output list wm1:
wm3 <- RWlearning(train2, wm=wm, progress = FALSE)
# number of learned event are now added to wm1:
length(wm3)

## -----------------------------------------------------------------------------
out <- getValues(dat$Cues, unique=TRUE)
out <- out[out != "animal"]
lab <- plotOutcomeWeights(wm3, outcome="animal", 
                          select.cues = out, 
                          col=alpha(1, f=.25), lty=1, pos=4, 
                          ylim=c(-.02,.2), font=2, ylim=range(getWM(wm3)),
                          xmark=TRUE, ymark=TRUE, las=1)
lab2 <- plotOutcomeWeights(wm3, outcome="animal", 
                           select.cues = "brown", col='red', 
                           lwd=2, pos=4, add=TRUE, font=2)
abline(v=length(wm), lty=3)

## -----------------------------------------------------------------------------
# select weight matrix:
mat <- getWM(wm)
# for a cueset:
activationsMatrix(mat,cues="BG_cat_brown")
# for a specific outcome:
activationsMatrix(mat,cues="BG_cat_brown", select.outcomes = "animal")
# for a group of cuesets (all connection weights will be added):
activationsMatrix(mat,cues=c("BG_cat_brown", "BG_cat_blue"))

## -----------------------------------------------------------------------------
# new dummy data:
dat <- data.frame(Cues = c("noise", "noise", "light"),
                  Outcomes = c("food", "other", "food_other"),
                  Frequency = c(5, 10, 15) )
dat$Cues <- paste("BG", dat$Cues, sep="_")                  
train <- createTrainingData(dat)
wm <- RWlearning(train, progress = FALSE)

# list with activations for observed outcomes:
act <- activationsEvents(wm, data=train)
head(act)
# calculate max activation:
maxact <- lapply(act, function(x){ return(max(x, na.rm=TRUE)) }) 
unlist(maxact)

# Using argument 'fun':
act <- activationsEvents(wm, data=train, fun="max")
head(act)

## -----------------------------------------------------------------------------
# list with activations for observed outcomes:
act <- activationsCueSet(wm, cueset=c("BG_noise", "BG_light", "BG_somethingelse"))
names(act)
head(act[[1]])
# also activations for non-trained connections:
head(act[[3]])

## -----------------------------------------------------------------------------
# list with activations for observed outcomes:
act <- activationsOutcomes(wm, data=train)
head(act)

