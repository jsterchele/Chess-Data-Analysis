####################
## This file with run a logistic regression on the ELO scores and produce a logistic plot as well as a residual plot
## Logistic Regression
####################
#load("H:/csc 255 big data/chessDF.rdata")
source("funky.R")
require(ggplot2)

#subsets the chess DF for only gamers that both players have data Frames
EloDF <- chess[!is.na(chess$difference) & chess$result != "d", ]
##this data frame contains only games that both players have Elo scores & games that did not end in draw.

#creates results vector with vaules equal to 1 if white won / 0 if black won.
resVec <- createResultVec(as.character(EloDF$result))

#adds the result Vector to the data Frame,  we must use this when we do regression
EloDF <- data.frame(EloDF, resVec)

#creates the bins for the diffrence of Elo scores
b <- catCreate(EloDF$difference)

#creates a new data frame containing the only two variables, difference and result
newElo <- EloDF[c(10,13)]

#the mean elo score for each bin
meanElos <- meanElo(b)
#the actual winpercentage per each mean Elo score
meanWin  <- meanSucc(b)

# Finding the logistic model
Elo.glm <- glm(EloDF$resVec~EloDF$difference, family=binomial)
#####################################
###   GRAPH   ####
#gathering the inercepts of the graph --> for use with regression
b0 <- coef(Elo.glm)[1]
b1 <- coef(Elo.glm)[2]
range <- -750:1600
xrange <- c(-750,1600)
yrange <- c(0,1)

regline <- function(x) {exp(b0+b1*x)/(1+exp(b0+b1*x))}
resid <- data.frame(meanElos,meanWin)

board <- data.frame(x1 <- c(-1000, -1000, -500, -500, 0, 0, 500, 500, 
                            1000, 1000, 1500, 1500), 
                    x2 <- c(-500, -500, 0, 0, 500, 500, 1000, 1000, 
                            1500, 1500, 2000, 2000), 
                    y1 <- c(0, .5, .25, .75, 0, .5, .25, .75, 0, .5, .25, .75), 
                    y2 <- c(.25, .75, .5, 1, .25, .75, .5, 1, .25, .75, .5, 1))


EloRegPlot <- ggplot() +
  scale_x_continuous("Difference of Elo Scores by White compared to Black") + 
  scale_y_continuous("White Results") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  layer(data = board, 
        mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), 
        stat = "identity", geom = "rect", 
        geom_params = list(color = NA, alpha = 0.5)) +
  layer(data = EloDF, 
        mapping = aes(x = EloDF$difference, y = as.numeric(resVec)), 
        stat = "identity", geom = "point", 
        geom_params = list(color = "red3", size = 6)) +
  layer(data = EloDF,
        stat = "function", stat_params = list(fun = regline), 
        geom = "smooth", geom_params = list(color = "gold2", size = 2)) + 
  layer(data = resid, mapping = aes(x = meanElos, y = meanWin), 
        stat = "identity", geom = "point", 
        geom_params = list(color = "dodgerblue3", size = 14, shape = "*"))   


##prints our the graph
print(EloRegPlot)

####################################
### ScatterPlot for residuals ###
residuals <- residu(b, Elo.glm)

board2 <- data.frame(x1 <- c(-400, -400, -200, -200, 0, 0, 200, 200), 
                     x2 <- c(-200, -200, 0, 0, 200, 200, 400, 400), 
                     y1 <- c(-0.1, 0, -0.05, 0.05, -0.1, 0, -0.05, 0.05), 
                     y2 <- c(-0.05, 0.05, 0, .1, -0.05, 0.05, 0, .1))


ResidPlot <- ggplot() + 
  scale_x_continuous("Mean Elos") + 
  scale_y_continuous("Residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  layer(data = board2, 
        mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), 
        stat = "identity", geom = "rect", 
        geom_params = list(color = NA, alpha = 0.5)) +
  layer(data = resid, mapping = aes(x = meanElos, y = residuals), 
        stat = "identity", geom = "point", 
        geom_params = list(color = "dodgerblue3", size = 6)) + 
  geom_hline(yintercept = 0, color = "red3", size = 1.5)


print(ResidPlot)



