#' Set value background cue.
#' 
#' @export
#' @param value A string specifying the background cue. 
#' NULL or FALSE or a number < 0 indicates to remove the background cue, 
#' whereas the values TRUE or a number > 0 set the value to "***", and a 
#' string can specify the value for the background cue.
#' @return No return value
#' 

setBackground <- function(value){
	if(is.logical(value)){
		if(value == TRUE){
			options(background="***")
		}else{
			options(background=NULL)
		}
	}else if(is.numeric(value)){
		options(background=ifelse(value<=0, NULL, "***"))
	}else if(is.character(value)){
		options(background=value)
	}else{
		stop(sprintf("Cannot interpret input value %s. Try to use logical values TRUE or FALSE.", value))
	}
}
