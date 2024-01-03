# influence of ratios on alpha A/B, benefit, thresholds 

source("scripts/multiSpOptFuncs.R")

# What is this script for?
# Making graphs showing the influence of each key variable on the optimal proportions, the benefits and the thresholds for optimising.

## vars ----

muRvect <- c(1, 2, 5, 10, 20) #mu R
ERvect <- c(seq(from = 0.001, to = 0.99, length.out = 25), seq(from = 1, to = 100, length.out = 25) ) #ER

RcVect <- c(0.001, 0.01, 0.05, 0.2, 0.5, 2, 5)

linetypes <- c("dashed", "longdash", "dotdash", "dotted", "twodash", "longdash", "solid")
myColours <- c("lightgrey", "grey", "darkgrey", rep("black", times = 4))
  
mylines <- data.frame("linetype" = linetypes, 
                                    "colours" = myColours)

leglabs1 <- expression(italic(R)[cA] == 0.001)
leglabs2 <- expression(italic(R)[cA]==0.01)
leglabs3 <- expression(italic(R)[cA]==0.05)
leglabs4 <- expression(italic(R)[cA]==0.2)
leglabs5 <- expression(italic(R)[cA]==0.5)
leglabs6 <- expression(italic(R)[cA]==2)
leglabs7 <- expression(italic(R)[cA]==5)

leglabs <- c(leglabs1, leglabs2, leglabs3, leglabs4, leglabs5, leglabs6, leglabs7)
# alpha A and B ----

# 5 sets of A and B, each set at a different muR

#function to make the graphs:

AB_panels <- function(muRvect, ERvect, RcVect){
  #make dataframes
  
  #one df for each value of muR
  #within each df, one col for each value of Rc
  myDF <- data.frame(matrix(nrow = 50, ncol = length(RcVect)+1))
  
  myNamesA <- character(length(RcVect))
  myNamesB <- character(length(RcVect))
   
  for(a in 1:length(RcVect)){
    myNamesA[a] <- paste0("alphaA_", RcVect[a])
    myNamesB[a] <- paste0("alphaB_", RcVect[a])
  }
  
  results <- list()
  
for(p in 1:length(muRvect)){
  muRdfA <- myDF
  muRdfB <- myDF

  colnames(muRdfA) <-   c("logER", myNamesA)
  colnames(muRdfB) <-   c("logER", myNamesB)
  
  muRdfA$logER <- log(ERvect)
  muRdfB$logER <- log(ERvect)
  
  for(i in 1:length(RcVect)){
   muRdfA[,(i+1)] <- ifelse(RcVect[i]>oneToTwoAlpha_Rc(muRat = muRvect[p], Erat = ERvect),
                optAB_Rc(Rc = RcVect[i], muRat = muRvect[p], Erat = ERvect)[[1]], 1)
   muRdfB[,(i+1)] <- ifelse(RcVect[i]>oneToTwoAlpha_Rc(muRat = muRvect[p], Erat = ERvect),
                optAB_Rc(Rc = RcVect[i], muRat = muRvect[p], Erat = ERvect)[[2]], NA)
  
   muRdfB[is.na(muRdfB[,(i+1)]),(i+1)] <- ifelse(RcVect[i]> alphBToLTS_Rc(muRat = muRvect[p], Erat = ERvect[is.na(muRdfB[,(i+1)])]),
                                        optB_Rc(Rc = RcVect[i], muRat = muRvect[p], Erat = ERvect[is.na(muRdfB[,(i+1)])]), 1 )
   
  }
  
results[[p]] <- assign(paste0("ABdf_muR_", muRvect[p]), merge(muRdfA, muRdfB, by = "logER"))

   
  }
  
  return(results)
}

things <- AB_panels(muRvect = muRvect, ERvect = ERvect, RcVect = RcVect)

#make the graphs:

# graph 1 ----
# five rows of two, labelled 1a, 1b, 2a, 2b, etc.
png("graphs/ABpanels1_3_formatted.png", height = 1400, width = 900)
layout(matrix(c(1,2,3,4,5,6,7,7), ncol = 2, byrow = TRUE), heights = c(1,1,1,0.4))
par(pty = "s", cex = 1.3, xpd = TRUE, mar = c(4,4,2,1))
plot(x = things[[1]]$logER, y = things[[1]]$alphaA_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5,
     xlab = "", ylab = "", lty = mylines$linetype[1], col = "lightgrey")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[A], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[1]]$logER, y = things[[1]]$alphaA_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaA_0.05, lty = mylines$linetype[3], col = "darkgrey", lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaA_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaA_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaA_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaA_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(1, italic(a),":", ~mu[R] == 1)), side = 3, line = 0, cex = 1.5)

plot(x = things[[1]]$logER, y = things[[1]]$alphaB_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5,
     xlab = "", ylab = "", , lty = mylines$linetype[1], col = "lightgrey")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[B], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[1]]$logER, y = things[[1]]$alphaB_0.01, lty = mylines$linetype[2], lwd = 1.5, col = "grey")
lines(x = things[[1]]$logER, y = things[[1]]$alphaB_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaB_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaB_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaB_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[1]]$logER, y = things[[1]]$alphaB_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(1, italic(b),":", ~mu[R] == 1)), side = 3, line = 0, cex = 1.5)

plot(x = things[[2]]$logER, y = things[[2]]$alphaA_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5,
          xlab = "", ylab = "", lty = mylines$linetype[1], col = "lightgrey")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[A], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[2]]$logER, y = things[[2]]$alphaA_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaA_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaA_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaA_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaA_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaA_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(2, italic(a),":", ~mu[R] == 2)), side = 3, line = 0, cex = 1.5)

plot(x = things[[2]]$logER, y = things[[2]]$alphaB_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5,
          xlab = "", ylab = "",lty = mylines$linetype[1], col = "lightgrey")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[B], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[2]]$logER, y = things[[2]]$alphaB_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaB_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaB_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaB_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaB_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[2]]$logER, y = things[[2]]$alphaB_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(2, italic(b),":", ~mu[R] == 2)), side = 3, line = 0, cex = 1.5)

plot(x = things[[3]]$logER, y = things[[3]]$alphaA_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5,
          xlab = "", ylab = "", lty = mylines$linetype[1], col = "lightgrey")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[A], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[3]]$logER, y = things[[3]]$alphaA_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaA_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaA_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaA_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaA_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaA_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(3, italic(a),":", ~mu[R] == 5)), side = 3, line = 0, cex = 1.5)

plot(x = things[[3]]$logER, y = things[[3]]$alphaB_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5,
          xlab = "", ylab = "", lty = mylines$linetype[1], col = "lightgrey")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[B], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[3]]$logER, y = things[[3]]$alphaB_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaB_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaB_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaB_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaB_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[3]]$logER, y = things[[3]]$alphaB_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(3, italic(b),":", ~mu[R] == 5)), side = 3, line = 0, cex = 1.5)

plot(0, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = 1, xjust = 0.5, yjust = 1, legend = leglabs[1:4], lty = mylines$linetype[1:4], 
       col = mylines$colours[1:4], bty = "n",
       horiz = TRUE, cex = 1.5)
legend(x = -1, xjust = 0.5, yjust = 1, legend = leglabs[5:7], lty = mylines$linetype[5:7], 
       col = mylines$colours[5:7], bty = "n",
       horiz = TRUE, cex = 1.5)
dev.off()

png("graphs/ABpanels4_5.png", height = 900, width = 800)
layout(matrix(c(1,2,3,4,5,5), ncol = 2, byrow = TRUE), heights = c(1,1,0.4))
par(pty = "s", cex = 1.3, xpd = TRUE, mar = c(4,4.5,2,1))
plot(x = things[[4]]$logER, y = things[[4]]$alphaA_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5, lty = mylines$linetype[1], col = "lightgrey",
          xlab = "", ylab = "")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[A], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[4]]$logER, y = things[[4]]$alphaA_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaA_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaA_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaA_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaA_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaA_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(4, italic(a),":", ~mu[R] == 10)), side = 3, line = 0, cex = 1.5)

plot(x = things[[4]]$logER, y = things[[4]]$alphaB_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5, lty = mylines$linetype[1], col = "lightgrey",
          xlab = "", ylab = "")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[B], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[4]]$logER, y = things[[4]]$alphaB_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaB_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaB_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaB_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaB_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[4]]$logER, y = things[[4]]$alphaB_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(4, italic(b),":", ~mu[R] == 10)), side = 3, line = 0, cex = 1.5)

plot(x = things[[5]]$logER, y = things[[5]]$alphaA_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5, lty = mylines$linetype[1], col = "lightgrey",
          xlab = "", ylab = "")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[A], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[5]]$logER, y = things[[5]]$alphaA_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaA_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaA_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaA_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaA_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaA_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(5, italic(a),":", ~mu[R] == 20)), side = 3, line = 0, cex = 1.5)

plot(x = things[[5]]$logER, y = things[[5]]$alphaB_0.001, type = "l", 
     ylim = c(0,1), lwd = 1.5, lty = mylines$linetype[1], col = "lightgrey",
          xlab = "", ylab = "")
mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.5)
mtext(text = expression(paste(alpha[B], "*")), side = 2, line = 3, cex = 2, las = 2)
lines(x = things[[5]]$logER, y = things[[5]]$alphaB_0.01, lty = mylines$linetype[2], col = "grey", lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaB_0.05, lty = mylines$linetype[3], col = "darkgrey",lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaB_0.2, lty = mylines$linetype[4], lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaB_0.5, lty = mylines$linetype[5], lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaB_2, lty = mylines$linetype[6], lwd = 1.5)
lines(x = things[[5]]$logER, y = things[[5]]$alphaB_5, lty = mylines$linetype[7], lwd = 1.5)
mtext(text = expression(paste(5, italic(b),":", ~mu[R] == 20)), side = 3, line = 0, cex = 1.5)

plot(0, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = 1, xjust = 0.5, yjust = 1, legend = leglabs[1:4], lty = mylines$linetype[1:4], 
       col = mylines$colours[1:4], bty = "n",
       horiz = TRUE, cex = 1)
legend(x = -1, xjust = 0.5, yjust = 1, legend = leglabs[5:7], lty = mylines$linetype[5:7], 
       col = mylines$colours[5:7], bty = "n",
       horiz = TRUE, cex = 1)
dev.off()


# myGraph <- function(df, ylim = c(0,1), i = 2, A = "A", B = "B", RcVect, labeln = 1, muR, mylines){
#   
#   
#   plot(x = df$logER, y = df[,i], xlim = c(min(df$logER), max(df$logER)), ylim = ylim,
#        xlab = "", ylab = paste0("alpha ", A), type = "l", lwd = 1.5)
#   mtext(text = paste0(labeln, "a: ", "muR = ", muR), side = 3, line = 0, cex = 1.2)
#   a <- 0
#   for(a in 1:(length(RcVect)-1)){
#    lines(x = df$logER, y = df[,(a+2)], lwd = 1.5, lty = mylines$linetype[a], col = mylines$colours[a]) 
#   }
# 
#   plot(x = df$logER, y = df[,i], xlim = c(min(df$logER), max(df$logER)), ylim = ylim,
#        xlab = "", ylab = paste0("alpha ", B), type = "l", lwd = 1.5)
#   mtext(text = paste0(labeln, "b: ", "muR = ", muR), side = 3, line = 0, cex = 1.2)
#   b <- 0
#   for(b in 1:(length(RcVect)-1)){
#    lines(x = df$logER, y = df[,(length(RcVect)+b+1)], lwd = 1.5, lty = mylines$linetype[b], col = mylines$colour[b]) 
#   }
# }


# png("graphs/ABpanels1_3.png", height = 1200, width = 800)
# layout(matrix(c(1,2,3,4,5,6,7,7), ncol = 2, byrow = TRUE), heights = c(1,1,1,0.3))
# par(pty = "s", cex = 1.3, xpd = TRUE, mar = c(4,3,1,1))
# for(x in 1:3){
#  testplots <- myGraph(df = things[[x]], RcVect = RcVect, labeln = x, muR = muRvect[x], mylines = mylines)
#  
# }
# mtext(text = "log ER", side = 1, line = 3, at = -11, cex = 1.3)
# plot(0, type = "n", axes=FALSE, xlab="", ylab="")
# legend(x = "center", legend = leglabs, lty = c("solid", mylines$linetype), col = c("black", mylines$colours), bty = "n",
#        horiz = TRUE, cex = 0.9, inset = c(0, -2), text.width = 3.2, seg.len = 2)
# dev.off()
# 
# png("graphs/ABpanels4_5.png", height = 800, width = 800)
# layout(matrix(c(1,2,3,4,5,5), ncol = 2, byrow = TRUE), heights = c(1,1,0.3))
# par(pty = "s", cex = 1.3, xpd = TRUE, mar = c(4,3,1,1))
# 
# for(x in 4:5){
#  testplots <- myGraph(df = things[[x]], RcVect = RcVect, labeln = x, muR = muRvect[x], mylines = mylines)
#  
# }
# mtext(text = "log ER", side = 1, line = 3, at = -11, cex = 1.3)
# plot(0, type = "n", axes=FALSE, xlab="", ylab="")
# legend(x = "bottom", legend = leglabs, lty = c("solid", mylines$linetype), col = c("black", mylines$colours), bty = "n",
#        horiz = TRUE, cex = 0.9, inset = c(0, -2),text.width = 4.7, seg.len = 1.5)
# dev.off()

# same range of muR and Rc for benefit, then thresholds - latter might be tough to visualise, so many lines on the same plot (be artful, e.g. solid black to faded)

# benefit----

benPanels <- function(muRvect, ERvect, RcVect){
  #make dataframe, one col for logER, several cols for value of benefit at different values of Rc
  #different dataframe for each value of muR
  results <- list()
  
  for(x in 1:length(muRvect)){
   myDF <- data.frame(matrix(nrow = length(ERvect), ncol = length(RcVect)+1))

  myNamesBen <- character(length(RcVect))
  
  for(a in 1:length(RcVect)){
    myNamesBen[a] <- paste0("ben_", RcVect[a])
    
  }
  colnames(myDF) <-   c("logER", myNamesBen)
 myDF$logER <- log(ERvect)

  for(i in 1:length(RcVect)){
   myDF[,(i+1)] <- ifelse(RcVect[i]>oneToTwoAlpha_Rc(muRat = muRvect[x], Erat = ERvect),
                b3(Rc = RcVect[i], muR = muRvect[x], ER = ERvect), NA)
  
   myDF[is.na(myDF[,(i+1)]),(i+1)] <- ifelse(RcVect[i]> alphBToLTS_Rc(muRat = muRvect[x], Erat = ERvect[is.na(myDF[,(i+1)])]),
                                             b2(Rc = RcVect[i], muR = muRvect[x], ER= ERvect[is.na(myDF[,(i+1)])]), NA)
   
  }
  
results[[x]] <- assign(paste0("bendf_muR_", muRvect[x]), myDF)
  }
  return(results)
  }

test <- benPanels(muRvect = muRvect, ERvect = ERvect, RcVect = RcVect)

myLabels1 <- expression(paste(italic(a),":", ~mu[R] == 1))
myLabels2 <- expression(paste(italic(b),":", ~mu[R] == 2))
myLabels3 <- expression(paste(italic(c),":", ~mu[R] == 5))
myLabels4 <- expression(paste(italic(d),":", ~mu[R] == 10))
myLabels5 <- expression(paste(italic(e),":", ~mu[R] == 20))

myLabels <- c(myLabels1, myLabels2, myLabels3, myLabels4, myLabels5)

myGraph1 <- function(df, ylim = c(1,3), i = 2, RcVect, myLabels = myLabels[1], muR, mylines){
  
  plot(x = df$logER, y = df[,i], xlim = c(min(df$logER), max(df$logER)), ylim = ylim,
       xlab = "", ylab = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), type = "l", lwd = 1.5, lty = mylines$linetype[1], col = mylines$colours[1])
  mtext(text = myLabels, side = 3, line = 0, cex = 1.2)
  mtext(text = expression(log(italic(E)[R])), side = 1, line = 3, cex = 1.2)
  a <- 1
  for(a in 1:(length(RcVect)-1)){
   lines(x = df$logER, y = df[,(a+2)], lwd = 1.5, lty = mylines$linetype[a+1], col = mylines$colours[a+1], xpd = FALSE) 
  }
}

# leglabs <- character(length(RcVect))
# for(c in 1:(length(leglabs))){
#   leglabs[c] <- paste0("Rc = ", RcVect[c])
# }

newtest <- myGraph1(df = test[[5]], RcVect = RcVect, myLabels = myLabels[5], muR = muRvect[5], mylines = mylines)

#make panel of graphs
png("graphs/benefitPanels.png", height = 1200, width = 800)
layout(matrix(c(1,2,3,4,5,0,6,6), ncol = 2, byrow = TRUE), heights = c(1,1,1,0.4))
par(pty = "s", cex = 1.4, xpd = TRUE, mar = c(4,3,2,1))
for(x in 1:length(muRvect)){
 testplots <- myGraph1(df = test[[x]], ylim = c(0, 11), RcVect = RcVect, myLabels = myLabels[x], muR = muRvect[x], mylines = mylines)
 
}

plot(0, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = 1, xjust = 0.5, yjust = 0.5, legend = leglabs[1:4], lty = mylines$linetype[1:4], col =  mylines$colours[1:4], bty = "n",
       horiz = TRUE, cex = 1, inset = c(0, -2))
legend(x = -2, xjust = 0.5, yjust = 0.5, legend = leglabs[5:7], lty = mylines$linetype[5:7], col =  mylines$colours[5:7], bty = "n",
       horiz = TRUE, cex = 1, inset = c(0, -2))

dev.off()

## single sp alpha ----

alph1 <- sqrt(1/(2*RcVect))

#what about species B? don't have original Cm and Cw to return to... Can calculate from encounter rate?

alph1b <- sqrt(1/(2*(RcVect*2^2)))
# alpha a is alpha b*mur so alpha b is alpha a/mur - square it because it's going inside the sqrt

(alph1b_muR5 <- sqrt(1/(2*(RcVect*5^2))))
(alph1b_muR10 <- sqrt(1/(2*(RcVect*10^2)))) 
(alph1b_muR20 <- sqrt(1/(2*(RcVect*20^2)))) 

#but what is the single species alpha b?

#benefit single species based on Rc (which is relative to species A)
predBen(RcVect) #Rc has to be above 0.5?? Check this.

#yup, the asymptote of the benefit as ER approaches 0 is the single species benefit of species

