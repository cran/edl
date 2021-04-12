#' Simulated learning data.
#'
#' Data set for illustrating discrimination learning. 
#' 
#' @format A data frame with 36 rows and 5 variables:
#' \describe{
#'   \item{\code{Shape}}{Shape is the discriminative cue. 6 shapes: 
#' cat, rabbit, flower, tree, car, bicycle.}
#'   \item{\code{Color}}{Color is the nondiscriminative cue. 6 colors: 
#' brown, gray, white, yellow, red, blue.}
#'   \item{\code{Category}}{Three categories: animal, plant, vehicle.}
#'   \item{\code{Frequency1}}{Different frequency values assigned 
#' to the shapes, no difference between colors.}
#'   \item{\code{Frequency2}}{Different frequency values assigned 
#' to the color-shape combinations, no difference between categories.}
#' }
#' @author Jacolien van Rij
"dat"
