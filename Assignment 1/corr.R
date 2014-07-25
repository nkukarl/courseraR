corr <- function(directory, threshold = 0) {
	idNobs <- complete(directory)
	size <- dim(idNobs)
	id <- idNobs[1:size[1], "id"]
	nobs <- idNobs[1:size[1], "nobs"]
	id <- id[nobs > threshold]
	if (length(id) != 0) {
		fileNames <- formatC(c(id), width = 3, flag = '0')
		fullDir <- paste(getwd(), "/", directory, "/", sep = "")
		cors <- id
		for (i in 1:length(id)) {
			fileNames[i] <- paste(fullDir, fileNames[i], ".csv", sep = "")
			temp <- read.csv(fileNames[i])
			size <- dim(temp)
			temp1 <- temp[1:size[1], "sulfate"]
			temp2 <- temp[1:size[1], "nitrate"]
			completeCheck <- !is.na(temp1) & !is.na(temp2)
			temp1 = temp1[completeCheck]
			temp2 = temp2[completeCheck]
			cors[i] = round(cor(temp1, temp2), digits = 4)
		}
		cors
	} else {
	id
	}
}
		
		