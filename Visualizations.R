#This file will be used for getting information 
#regarding opening moves and their respected win/loss ratios


##This file is dependant on having the chessDF.rdata
openingMoves <- findOpenings()
chess <- data.frame(chess, openingMoves)

combTable <- table(chess$openingMoves)
combTable <- sort(combTable)
topComb <- as.table(tail(combTable, 15))
combResults <- chess$result

#coding each variable for white wins
whiteWin<-gsub("w", "1", combResults)
whiteWin<-gsub("b", "0", whiteWin)
whiteWin<-gsub("d", "0", whiteWin)
whiteWin <- as.numeric(whiteWin)

#coding each variable for draws
combDraw<-gsub("w", "0", combResults)
combDraw<-gsub("b", "0", combDraw)
combDraw<-gsub("d", "1", combDraw)
combDraw <- as.numeric(combDraw)


combWhite <- numeric(length(topComb))
combD <- numeric(length(topComb))

#this loop goes through each the white wins, and draws and counts them
y<-1
for (i in topComb)
{
  WinLoc<- grep(names(topComb[y]), chess$openingMoves)
  whiteWins <- character(length(WinLoc))
  combDraws <- character(length(WinLoc))
  x<-1
  for (i in WinLoc) 
  {
    combDraws[x] <- combDraw[i]
    combDraws <- as.numeric(combDraws)
    whiteWins[x] <- whiteWin[i]
    whiteWins <- as.numeric (whiteWins)
    combWhite[y]  <-mean(whiteWins, na.rm=TRUE)
    combD[y] <- mean(combDraws, na.rm=TRUE)
    x<-x+1
  }
  y<-y+1
}
#this is for the resulting black 
combLoss <- (1-combD-combWhite)

#creates a new data frame
combWinRates <- data.frame(names(topComb), combWhite, combD, combLoss)
#saves this dataframe


#save(combWinRates, file="combWinRates.rdata")
#creating a data frame that will hold the win/loss/draw raitos for each set of opening moves
forGraph <- data.frame(row.names=names(topComb),combWhite, combD, combLoss)
toGraph<-do.call(rbind,forGraph)
#legend
LegendNames<- c("White Victory", "Draw", "Black Victory")
locs <- c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58)
par(mar=c(8,4,2,2) + 0.1)

####Drawing the graph####
barplot(toGraph, beside=TRUE, legend=LegendNames ,args.legend = list(x="top",  bty="n",cex=.7), col=c("green3", "black", "red"),ylab="Percentages")
axis(1, at=locs, labels= names(topComb), cex.axis=.65, las=2)



