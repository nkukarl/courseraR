rankall <- function(outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	colID <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	if(is.na(colID[outcome]) == TRUE) {
		stop("invalid outcome")
	} else {
		states <- unique(data[, 7])
		states <- sort(states)
		res <- cbind(states, states)
		for(i in 1:length(states)) {
			state <- states[i]
			temp <- data[data[, 7] == state, c(2, colID[outcome])]
			temp <- temp[temp[, 2] != "Not Available", ]
			temp[, 2] <- as.numeric(temp[, 2])
			seq <- order(temp[, 2], temp[, 1])
			temp <- temp[seq, 1]
			if(num == "best") {
				res[i, 1] <- temp[1]
			} else if(num == "worst") {
				res[i, 1] <- temp[length(temp)]
			} else {
				res[i, 1] <- temp[num]
			}
		}
		res <- data.frame(hospital = res[, 1], state = res[, 2], row.names = states)
		res
	}
}