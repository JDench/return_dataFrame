library(foreach)
library(doParallel)

# This is a generalised framework for building a data.frame, 
# obviously some of the specifics need to be updated based on what you're building.
basalData <- workReviewers
tmpCategories <- c("PRC_CD","A_OFFICIAL_LANGUAGE")
# Now this builds the table to select from and accounts for the length of tmpCategories.
tmpSets <- if(length(tmpCategories) > 1){
				expand.grid(sapply(tmpCategories,function(x){ unique(basalData[[x]]) }))
			} else if(length(tmpCategories) == 1){
				tmpReturn <- expand.grid(sapply(tmpCategories,function(x){ unique(basalData[[x]]) }))
				names(tmpReturn) <- tmpCategories
				tmpReturn
			} else {
				stopError("There are not categories with which to build a frame, please review")
			}

# We register a cluster of cores equal to one less than this machine has
tmpCluster <- makeCluster(detectCores()-1, type='PSOCK')
registerDoParallel(tmpCluster)
plotData <- foreach(thisRow = 1:nrow(tmpSets),.combine="rbind") %dopar% {
				# We define the rows that fit this set of parameters
				tmpRows <- sapply(names(tmpSets),function(x){ 
								which(basalData[[x]] == tmpSets[[x]][thisRow]) 
							},simplify=FALSE)
				# We find the intersect of these rows.
				tmpRows <- if(length(tmpRows) > 1){
								recursiveIntersect(tmpRows)
							} else {
								unlist(tmpRows,use.names=FALSE)
							}
				# we return a data.frame
				return( cbind(tmpSets[thisRow,],
											"numApps"=if(length(tmpRows) > 0){ 
														length(unique(basalData[["APPL_NUMBER"]][tmpRows]))
													} else {
														0
													}) )
			}
stopCluster(tmpCluster)
