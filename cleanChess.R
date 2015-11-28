####This file is used for cleaning the chess.csv file into a data frame.####
############################################################################
source("funky.R")
#reads in the text file
els <- readLines("chess.csv")

#vector that contains the lines that contain "Event"
#This will be used to seperate games.
eventLine <- grep("\"\\[Event", els)
whiteElo <- vector(mode="integer", length=length(eventLine))
blackElo <- vector(mode="integer", length=length(eventLine))
result <- vector(mode="character", length=length(eventLine))
w1 <- vector(mode="character", length=length(eventLine))
w2 <- vector(mode="character", length=length(eventLine))
w3 <- vector(mode="character", length=length(eventLine))
b1 <- vector(mode="character", length=length(eventLine))
b2 <- vector(mode="character", length=length(eventLine))
b3 <- vector(mode="character", length=length(eventLine))

for (i in seq_along(eventLine))
{
  #line of the text file that contains the current game
  game <- els[eventLine[i]:eventLine[i+1]-1]
  
  #searches for the line that contains white elo
  wElo <- grep("WhiteElo", game)
  if(length(wElo) == 0){
    we <- NA
  }
  else
    {
    w <- gsub("[[:punct:]]", "", game[wElo])
    w <- gsub("whiteelo", "", w, ignore.case = TRUE)
    we <- as.numeric(w)
    }
  #assigns the white elo for this game
  whiteElo[i] <- we
  ######################################################
  bElo <- grep("BlackElo", game)
  if(length(bElo) == 0){
    be <- NA
  }
  else
  {
    b <- gsub("[[:punct:]]", "", game[bElo])
    b <- gsub("blackelo", "", b, ignore.case = TRUE)
    be <- as.numeric(b)
  }
  #assigns the black elo for this game
  blackElo[i] <- be
  #######################################################
  #gets the reuslts line
  res <- grep("Result", game)
  #willl strip the line for only the result
  resInd <- gsub("\\D", "", game[res])
  if(length(resInd == 0))
  {
    r <- NA
  }
  #if it has a result
  if(length(resInd > 0)){
    if(resInd == "01"){
      r <- "b"
    }
    else if (resInd == "10"){
      r <- "w"
    }
    else{
      r <- "d"
    }
  }
  #assigns the result of the game into the vector result
  result[i] <- r
  ######################################################
  #gets the line that holds the first moves
  moves <- grep("^1\\.", game)
  if(length(moves) == 0){
    w1[i] <- NA
    b1[i] <- NA
    w2[i] <- NA
    b2[i] <- NA
    w3[i] <- NA
    b3[i] <- NA
  }
  else{
  #seperates on blank space
  movez <- unlist(strsplit(game[moves], " "))
  #saves the first move for each white and black
  w1[i] <- movez[2]
  b1[i] <- movez[3]
  w2[i] <- movez[5]
  b2[i] <- movez[6]
  w3[i] <- movez[8]
  b3[i] <- movez[9]
  }
}
#saves everything into a data frame
chess <- data.frame(whiteElo, blackElo, result, w1, w2, w3, b1, b2, b3)

difference <- vector(length=nrow(chess), mode="integer")
#loop for finding the diffrence in either players Elo score
for (i in seq_along(eventLine))
{
  if(is.na(chess$whiteElo[i]) | is.na(chess$blackElo[i]))
    difference[i] <- NA
  else
    difference[i] <- chess$whiteElo[i] - chess$blackElo[i]
}
#adds the diffrences to the data frame
chess <- data.frame(chess, difference)
#concatinates all three opening moves together for white. then black
whiteOpen <- vector(length= nrow(chess), mode="character")
blackOpen <- vector(length= nrow(chess), mode="character")
for (i in seq_along(whiteOpen))
{
  if (is.na(w1[i]))
  {
    whiteOpen[i] <- NA
    blackOpen[i] <- NA
  }
  else{
  whiteOpen[i] <- paste(w1[i], w2[i], w3[i], sep="-")
  blackOpen[i] <- paste(b1[i], b2[i], b3[i], sep="-")
  }
}

#creates a data frame with all 12 variables
chess <- data.frame(chess, whiteOpen, blackOpen)
save(chess, file = "chessDF.rdata")

