best <- function(state, outcome) {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	colID <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	if(nrow(data[data[, 7] == state, ]) == 0) {
		stop("invalid state")
	} else if(is.na(colID[outcome]) == TRUE) {
		stop("invalid outcome")
	} else {
		temp <- data[data [, 7] == state, c(2, colID[outcome])]
		temp <- temp[temp[, 2] != "Not Available", ]
		temp[, 2] <- as.numeric(temp[, 2])
		res <- temp[temp[, 2] == min(temp[, 2]), ]
		res <- sort(res[, 1])
		res[1]
	}
}