complete <- function(directory, id = 1:332) {
	fullDir <- paste(getwd(), "/", directory, "/", sep = "")
	fileNames <- formatC(c(id), width = 3, flag = '0')
	for (i in 1:length(fileNames)) {
		fileNames[i] <- paste(fullDir, fileNames[i], ".csv", sep = "")
	}
	nobs <- id
	for (i in 1:length(fileNames)) {
		temp <- read.csv(fileNames[i])
		size <- dim(temp)
		temp1 <- temp[1:size[1], "sulfate"]
		temp2 <- temp[1:size[1], "nitrate"]
		completeCheck <- !is.na(temp1) & !is.na(temp2)
		nobs[i] = sum(completeCheck)
	}
	data.frame(id, nobs)
}