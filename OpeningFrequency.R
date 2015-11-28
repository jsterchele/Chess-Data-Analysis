###############################################
##this file will be used for creating two histograms
#that contain the most popular openings in chess
#############################################


load("chessDF.rdata")
#this set of lines makes a table and sorts so we could grab the most popular openings
whitetable <- table(chess$whiteOpen)
whitetable <- as.table(sort(whitetable))
whitetable10 <- as.table(tail(whitetable, 10))

#this set of lines makes a table and sorts so we could grab the most popular openings
blacktable <- table(chess$blackOpen)
blacktable <- as.table(sort(blacktable))
blacktable10 <- as.table(tail(blacktable, 10))

barplot(whitetable10, ylim=c(0, 8000), cex.names= 0.8, col=c("green", "brown","coral",
                                                             "yellow", "red", "blue", "palegreen", "purple", "orange",
                                                             "snow3", "chocolate"),
        main="Frequencies of Opening 3 Moves For White",
        space=.06, xlab="Moves", ylab="Frequency")

barplot(blacktable10, ylim=c(0, 8000), cex.names= 0.8, col=c("green", "brown","coral", 
                                                             "yellow", "red", "blue", "palegreen", "purple", "orange",
                                                             "snow3", "chocolate"),
        main="Frequencies of Opening 3 Moves For Black",
        space=.06, xlab="Moves", ylab="Frequency")