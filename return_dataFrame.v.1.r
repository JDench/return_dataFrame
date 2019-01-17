# This is a general format for creating a data.frame returning function that can accept any number of named arguments
### CAUTION: As this will silently recycle arguments as required to match the longest vector passed
### NOTE: This depends upon my general stopError() function, which is really a special use of error()
return_dataFrame <- function(...,func_sepString = ref_sepString){
	# We want the formal arguments and those supplied. 
	# This uses the match.call() to find the called expression, and we suppress the 
	# first value returned which would be the call name itself
	func_argList <- list("formal" = as.list(args(return_dataFrame)),
						"supplied" = as.list(match.call(expand.dots = TRUE)[-1]))
	# We keep the uniquely named arguments taking supplied values first and then
	# adding in any formal ones that were no directly define
	# We tidy the names for matching by ignoring any leading or trailing punctuation
	func_workingArgs <- c(func_argList[["supplied"]],
							func_argList[["formal"]][which(!is.element(names(func_argList[["formal"]]),
																		names(func_argList[["supplied"]])))])
	# We remove the NULL "", "...", and func_sepString from argument list
	func_ignoreArgs <- c("...","func_sepString","")
	func_workingArgs <- func_workingArgs[which(!is.element(names(func_workingArgs),func_ignoreArgs))]
	# If there nothing we return NULL
	if(length(func_workingArgs) == 0){ return( NULL ) }
	# Now, names must have been supplied to the arguments
	if(length(names(func_workingArgs)) == 0){ 
		stopError(paste("Did not supply names to arguments passed in ",match.call(expand.dots = TRUE)[1],sep="")) 
	}
	# If only one thing was passed we simply return that data.frame
	if(length(func_workingArgs) == 1){
		func_tmpReturn <- data.frame(eval(func_workingArgs[[1]]),stringsAsFactors = default.stringsAsFactors())
		colnames(func_tmpReturn) <- names(func_workingArgs)
		return( func_tmpReturn ) 
	}
	# If we have more then we build our tmpReturn object, 
	func_tmpReturn <- data.frame("tmpRemove"=rep(NA,max(sapply(func_workingArgs,function(x){ length(eval(x)) }))),
								stringsAsFactors = default.stringsAsFactors())
	# Now we add our columns
	for(thisCol in 1:length(func_workingArgs)){
		# We create a named vector of the information to add
		func_tmpAdd <- eval(func_workingArgs[[thisCol]])
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
	colnames(func_tmpReturn) <- names(func_workingArgs)
	return( func_tmpReturn )
}

