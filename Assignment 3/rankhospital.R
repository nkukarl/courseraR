rankhospital <- function(state, outcome, num = "best") {
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
		seq <- order(temp[, 2], temp[, 1])
		temp <- temp[seq, 1]
		if(num == "best") {
			temp[1]
		} else if(num == "worst") {
			temp[length(temp)]
		} else {
			temp[num]
		}
	}
}