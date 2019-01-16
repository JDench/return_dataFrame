# This is a general format for creating a data.frame returning function that can accept any number of named arguments
### CAUTION: As this will silently recycle arguments as required to match the longest vector passed
return_dataFrame <- function(...,func_sepString = ref_sepString){
	# This uses the match.call() to find the called expression, and we suppress the 
	# first value returned which would be the call name itself
	func_argList <-  as.list(match.call(expand.dots = TRUE)[-1])
	# If there nothing we return NULL
	if(length(func_argList) == 0){ return( NULL ) }
	# Now, names must have been supplied to the arguments
	if(length(names(func_argList)) == 0){ 
		stopError(paste("Did not supply names to arguments passed in ",match.call(expand.dots = TRUE)[1],sep="")) 
	}
	# If only one thing was passed we simply return that data.frame
	if(length(func_argList) == 1){
		func_tmpReturn <- data.frame(eval(func_argList[[1]]),stringsAsFactors = default.stringsAsFactors())
		colnames(func_tmpReturn) <- names(func_argList)
		return( func_tmpReturn ) 
	}
	# If we have more then we build our tmpReturn object, 
	func_tmpReturn <- data.frame("tmpRemove"=rep(NA,max(sapply(func_argList,function(x){ length(eval(x)) }))),
								stringsAsFactors = default.stringsAsFactors())
	# Now we add our columns
	for(thisCol in 1:length(func_argList)){
		# We create a named vector of the information to add
		func_tmpAdd <- eval(func_argList[[thisCol]])
		# Here we adjust the length if required
		if(length(func_tmpAdd) < nrow(func_tmpReturn)){
			func_tmpAdd <- rep(func_tmpAdd,ceiling(nrow(func_tmpReturn)/length(func_tmpAdd)))
		}
		# Now we add this into our growing frame
		func_tmpReturn <- cbind(func_tmpReturn,
								func_tmpAdd[1:nrow(func_tmpReturn)])
	}
	# We now remove the initialising columns
	func_tmpReturn <- func_tmpReturn[,-1]
	colnames(func_tmpReturn) <- names(func_argList)
	return( func_tmpReturn )
}
