#returns what each category should be limimted at when constructing the bins
#@PARAM X-> X is the vector that will be limited
#@RETURN hi -> Vector that will hold the bounds for each bin
catCreate <- function(x)
{
  #hi vector will be the top bound for each category
  hi <- vector(mode="integer", length=50)
  #sorts the vector
  x <- sort(x)
  for (i in seq_along(hi))
  { #sets the mimimum value
    if (i == 1)
    {
      hi[i] <- -500
    }
    else{
      xhi <- (i * 146)
      hi[i] <- x[xhi]
    }
  }
  return(hi)
}

#returns the diffrence between the actual win % and the predicted win  precentage
#@PARAM cat -> categorical bins as a vector
#@PARAM lm -> a logistic model
#@RETURN resid ->the vector of residuals
residu <- function(cat, lm)
{ 
  resid <- vector(mode="double", length=length(cat)-1)
  for(i in seq_along (cat)-1)
  {
    #creates bins
    lo <- cat[i]
    hi <- cat[i+1]
    #creates a new df for the bind
    thisCat <- newElo[newElo$difference >= lo & newElo$difference < hi, ]
    #gets the mean difference & mean result
    meanElo <- mean(thisCat$difference)
    
    meanSucc <- mean(as.numeric(thisCat$resVec))
    
    #looks at the linear model and gets gets the predicted value for the mean Elo
    b0 <- coef(lm)[1]
    b1 <- coef(lm)[2]
    preSucc <- exp(b0+b1*meanElo)/(1+exp(b0+b1*meanElo))
    #calculates the "Residual"
    resi <- meanSucc - preSucc
    resid[i] = resi
  }
  return(resid)
}

#returns the mean EloDiffrence for a bin
#@param cat the bins as a vector
meanElo <- function(cat)
{
  means <- vector(mode="double", length=length(cat)-1)
  for(i in seq_along (cat)-1)
  {
    #creates bins
    lo <- cat[i]
    hi <- cat[i+1]
    #creates a new df for the bind
    thisCat <- newElo[newElo$difference >= lo & newElo$difference < hi, ]
    #gets the mean difference & mean result
    meanElo <- mean(thisCat$difference)
    means[i] <- meanElo
  }
  return(means)
}

#this function gathers the mean win/loss rate for each bin and returns them as a vector
#@PARAM cat-> a vector that has  been broken down into categories
#@RETURN succ -> vector of all the mean win percentages
meanSucc <- function(cat)
{ 
  succ <- vector(mode="double", length=length(cat)-1)
  for(i in seq_along (cat)-1)
  {
    #creates bins
    lo <- cat[i]
    hi <- cat[i+1]
    #creates a new df for the bind
    thisCat <- newElo[newElo$difference >= lo & newElo$difference < hi, ]
    #gets the mean difference & mean result
    meanElo <- mean(thisCat$difference)
    meanSucc <- mean(as.numeric(thisCat$resVec))
    succ[i] <- meanSucc
  }
  return(succ)
}


#results vector with  values equal to 1 if white won / 0 if black won.
#@PARAM x-> vector of the win and loss coded as "b" or "w"
createResultVec <- function(x)
{
  resVec <- vector(length=nrow(EloDF), mode="integer")
  for (i in seq_along(x))
  {
    #if white won, change it to 1 --> success
    if(as.character(x[i]) == "w")
    resVec[i] <- 1
    #if black won, code it as a zero --> failure
    else
    resVec[i] <- 0
  }
  return(resVec)
}

##combines the first three opening moves for both players
findOpenings <-function()
{
  combOpen <- vector(length=nrow(chess), mode="character")
  for (i in seq_along(combOpen))
  {
    combOpen[i]  <- paste("1.", chess$w1[i], chess$b1[i], "2.", chess$w2[i], chess$b2[i], "3.", chess$w3[i], chess$b3[i], sep=" ")
  }
return(combOpen)
}
