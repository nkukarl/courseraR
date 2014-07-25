pollutantmean <- function(directory, pollutant, id = 1:332){
	fullDir <- paste(getwd(), "/", directory, "/", sep = "")
	fileNames = formatC(c(id), width = 3, flag = '0')
	for (i in 1:length(fileNames)) {
		fileNames[i] <- paste(fullDir, fileNames[i], ".csv", sep = "")
	}
	cnt <- 0
	tot <- 0
	for (i in 1:length(fileNames)) {
		temp <- read.csv(fileNames[i])
		size <- dim(temp)
		temp <- temp[1:size[1], pollutant]
		temp <- temp[!is.na(temp)]
		cnt <- cnt + length(temp)
		tot <- tot + sum(temp)
	}
	round(tot / cnt, digits = 3)
}