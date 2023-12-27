#Varying one of the key variables (encounter rate ratio, ratio of detectabilities and cost ratio) keeping all others constant, how does each key variable affect the optimal proportions?

library(plotmath)

source("scripts/multiSpOptFuncs.R") # loads functions that calculate thresholds between optimal strategies, and calculate the optimal proportions for the various strategies.
source("scripts/makeDfFunctions_varEffectsGraphs.R") # loads functions for creating the dataframes that the graphs use.
source("scripts/caseStudyVars.R") # loads values from Decky's dataset for C. auranticum and C. purpureum

#What goes in?
#muRat, Erat, Rc

#what comes out?
# same graphs for alphas A and B: how alphas A and B changes with change in detectability ratio, change in cost ratio, change in encounter rate ratio. (a and b are one set, just b is another set)

# all graphs need to only show ranges that are within the relevant optimal strategy range

opar <- par()
dflength  <-  100
# Graphs ----

## Using Decky data ----

### alpha AB <1 ----

# make dataframes:

#case study 1 is varying Rc from the threshold where the optimal strategy goes from alpha A = 1 and alpha B <1 to both alphas <1. Top end of the range is 5* the Rc of the case study.

caseStudy1 <- makeRcABdf(dflength = dflength, 
                         Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
                         muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                         RcTop = 5)

#case study 2 is varying Encounter rate ratio, from the threshold between strategies to 5* the Erat of the case study.

caseStudy2 <- makeEratABdf(dflength = dflength, 
                           Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = (caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])*20)

caseStudy3 <- makeDF_AB_XmuR(dflength = dflength, muRto = 1.112,
                            Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1],
                            Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
#single sp benefits

# checking benefits: single and multispecies compare

benA <- predBen(Rc=caseStudyVals$cm[1]/caseStudyVals$cw[1])

########
#This doesn't make sense... The cost ratio is really low, so how is the single species benefit 1.25? 

benB <- predBen(Rc = caseStudyVals$cm[2]/caseStudyVals$cw_EB[2])

benMult <- b3(Rc = caseStudyVals$cm[1]/caseStudyVals$cw[1], ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = 1)

# adding benefits to the dataframes

caseStudy1$b3 <- b3(Rc = caseStudy1$Rc, muR = 1, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
#generate data for lines with different encounter rates
# Different E ratio, same mu ratio, looking at change over a range of Rc
(mySp1_T2 <- oneToTwoAlpha_Rc(Erat = 1, muRat = 1))

mySpecies1 <- makeRcABdf(Erat = 1, muRat = 1, RcTop = 5)
mySpecies1$b3 <- b3(Rc = mySpecies1$Rc, muR = 1, ER = 1)

mySpecies2 <- makeRcABdf(Erat = 4, muRat = 1, RcTop = 5)
mySpecies2$b3 <- b3(Rc = mySpecies2$Rc, muR = 1, ER = 4)

mySpecies3 <- makeRcABdf(Erat = 16, muRat = 1, RcTop = 5)
mySpecies3$b3 <- b3(Rc = mySpecies3$Rc, muR = 1, ER = 16)

#change this one to 100
(mySp2_T2 <- oneToTwoAlpha_Rc(Erat = 50, muRat = 1))
mySpecies4 <- makeRcABdf(Erat = 100, muRat = 1, RcTop = 5)
mySpecies4$b3 <- b3(Rc = mySpecies4$Rc, muR = 1, ER = 100)

# legend labels
leg1 <- expression(italic(E)[R]==1)
leg2 <- expression(italic(E)[R]==4)
leg3 <- expression(italic(E)[R]==16)
leg4 <- expression(italic(E)[R]==26.6)
leg5 <- expression(italic(E)[R]==100)

# plot the data

#plot 1: a) alpha a, and b) alpha b as a function of Rc, with multiple lines showing the effect of changing the E ratio

png("graphs/alphaAB_caseStudy_Rc.png", width = 700, height = 500)

layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE), heights = c(0.75,0.25))
par(xpd = TRUE, mar = c(2,4,1,2), cex = 1.3, pty = "s") 

# varying Rc: 
plot(x = caseStudy1$Rc, y = caseStudy1$alphaA, xlab = "", ylab = "",
     type = "l", ylim = c(0, 1))
lines(x = mySpecies1$Rc, y = mySpecies1$alphaA, xlab = "", ylab = "",
     lty = "dashed", ylim = c(0, 1))
lines(x = mySpecies2$Rc, y = mySpecies2$alphaA, xlab = "", ylab = "",
     lty = "dotted", ylim = c(0, 1))
lines(x = mySpecies3$Rc, y = mySpecies3$alphaA, xlab = "", ylab = "",
     lty = "longdash", ylim = c(0, 1))
lines(x = mySpecies4$Rc, y = mySpecies4$alphaA, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(0, 1))
points(x = 0.0222, y = 0.90, pch = 4, cex = 1.5)

mtext(text =expression(displaystyle(R)[c]),  side = 1, line = 2.5, at = 2.5)
mtext(text = expression(paste(alpha[A]^"*")), side = 2, line = 2, at = 0.5, cex = 1.4)

plot(x = caseStudy1$Rc, y = caseStudy1$alphaB, xlab = "", ylab = "",
     type = "l", ylim = c(0, 1))
lines(x = mySpecies1$Rc, y = mySpecies1$alphaB, xlab = "", ylab = "",
     lty = "dashed", ylim = c(0, 1))
lines(x = mySpecies2$Rc, y = mySpecies2$alphaB, xlab = "", ylab = "",
     lty = "dotted", ylim = c(0, 1))
lines(x = mySpecies3$Rc, y = mySpecies3$alphaB, xlab = "", ylab = "",
     lty = "longdash", ylim = c(0, 1))
lines(x = mySpecies4$Rc, y = mySpecies4$alphaB, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(0, 1))
points(x = 0.0222, y = 0.90, pch = 4, cex = 1.5)

mtext(text =expression(displaystyle(R)[c]),  side = 1, line = 2.5, at = 2.5)
mtext(text = expression(paste(alpha[B]^"*")), side = 2, line = 2, at = 0.5, cex = 1.4)

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", xjust = 0, legend = c(leg1, leg2, leg3, leg4, leg5), lty = c("dashed", "dotted", "longdash", "solid", "dotdash"), bty = "n", cex = 1, horiz = TRUE, xpd = TRUE)
legend(x = "bottom", y = "center", legend = expression(italic(Cestrum)~survey), pch = 4, bty = "n", cex = 1)

dev.off()

#trying to see what happens to the proportions over a range of mu ratio, but with an ER closer to 1.

#checking relevant thresholds:

#Rc where strategies change (one to two opt proportions):
(oneToTwoAlpha_Rc(muRat = 1, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"]))
# at case study muR and Er: 0.01811594

# when Er = 1
(oneToTwoAlpha_Rc(muRat = 1, Erat = 1)) #0.25

#alpha a and alpha b
(optAB_Rc(Rc = (caseStudyVals$cm[1]/caseStudyVals$cw[1])*5, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], muRat = 1))

# when Er=1
(optAB_Rc(Rc = (caseStudyVals$cm[1]/caseStudyVals$cw[1])*5, Erat =1, muRat = 1))
# not optimisable with case study Rc - needs to be at least 0.25

# so, need to show how proportions change with muR for at least one value of Rc: try 0.25 and 0.5

ERtest.25 <- makeDF_AB_XmuR(muRto = 10, Rc = 0.25, Erat = 1)
ERtest.5 <- makeDF_AB_XmuR(muRto = 10, Rc = 0.5, Erat = 1)
(sqrt(1/(2*0.5))) #single sp opt prop is 1
#make dataframes 

#add benefit to caseStudy3

caseStudy3$b3 <- b3(muR = caseStudy3$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1])

myMu1 <- makeDF_AB_XmuR(muRto = 2.03, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
myMu1$b3 <- b3(muR = myMu1$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3)

myMu2 <- makeDF_AB_XmuR(muRto = 2.76, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
myMu2$b3 <- b3(muR = myMu2$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5)

myMu3 <- makeDF_AB_XmuR(muRto = 4.64, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
myMu3$b3 <- b3(muR = myMu3$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10)

alphB_muRat <- makeDF_B_XmuR(muRfrom = max(caseStudy3$muRat), muRto = 5, Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
alphB_muRat$b2 <- b2(Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat$muRat)

alphB_muRat1 <- makeDF_B_XmuR(muRfrom = max(myMu1$muRat), muRto = 5, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3, Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])  
alphB_muRat1$b2 <- b2(Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat1$muRat)

alphB_muRat2 <- makeDF_B_XmuR(muRfrom = max(myMu2$muRat), muRto = 5, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5, Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])  
alphB_muRat2$b2 <- b2(Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat2$muRat)

alphB_muRat3 <- makeDF_B_XmuR(muRfrom = max(myMu3$muRat), muRto = 5, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10, Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
alphB_muRat3$b2 <- b2(Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat3$muRat)


rc1 <- expression(italic(R)[c]==0.022)
rc2 <- expression(italic(R)[c]==0.066)
rc3 <- expression(italic(R)[c]==0.11)
rc4 <- expression(italic(R)[c]==0.22)

png("graphs/alphAB_caseStudy_muR.png", width = 700, height = 550)
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE), heights = c(0.75,0.25))
par(xpd = TRUE, mar = c(2,4,1,2), cex = 1.3, pty = "s") 
# varying mu ratio:

plot(x = caseStudy3$muRat, y = caseStudy3$alphaA, type = "l", xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1,5))
mtext(text = expression(mu[R]), side = 1, line = 2.5, at = 3, cex = 1.4)
mtext(text = expression(paste(alpha[A]^"*")), side = 2, line = 2.2, cex = 1.4)

lines(x = myMu1$muRat, y = myMu1$alphaA, xlab = "", ylab = "",
     ylim = c(0, 1), xlim = c(1,5),
     lty = "dashed")
lines(x = myMu2$muRat, y = myMu2$alphaA, xlab = "", ylab = "",
     ylim = c(0, 1), xlim = c(1,5),
     lty = "dotted")
lines(x = myMu3$muRat, y = myMu3$alphaA, xlab = "", ylab = "",
     ylim = c(0, 1), xlim = c(1,5),
     lty = "longdash")
points(x = 1, y = 0.9, pch = 4, cex = 1.3)

plot(x = caseStudy3$muRat, y = caseStudy3$alphaB, type = "l", xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1,5))
mtext(text = expression(mu[R]), side = 1, line = 2.5, at = 3, cex = 1.4)
mtext(text = expression(paste(alpha[B]^"*")), side = 2, line = 2.2, cex = 1.4)

lines(x= myMu1$muRat, y = myMu1$alphaB, lty = "dashed", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")
lines(x= myMu2$muRat, y = myMu2$alphaB, lty = "dotted", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")
lines(x= myMu3$muRat, y = myMu3$alphaB, lty = "longdash", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")

lines(x = alphB_muRat$muRat, y = alphB_muRat$alphaB, 
      xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5), lty = "solid", col = "grey")
lines(x = alphB_muRat1$muRat, y = alphB_muRat1$alphaB, 
     xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5), lty = "dashed", col = "grey")
lines(x = alphB_muRat2$muRat, y = alphB_muRat2$alphaB, 
     xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5),, lty = "dotted", col = "grey")
lines(x = alphB_muRat3$muRat, y = alphB_muRat3$alphaB, 
     xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5),, lty = "longdash", col = "grey")
points(x = 1, y = 0.9, pch = 4, cex = 1.3)

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", legend = c(rc1, rc2, rc3, rc4), lty = c("solid", "dashed", "dotted", "longdash"), bty = "n", horiz = TRUE, cex = 1, inset = -0.15)
legend("center", legend = c("Strategy 1", "Strategy 2", expression(italic(Cestrum)~survey)), pch = c(16, 16, 4), col = c("grey", "black", "black"), bty = "n", cex = 1, horiz = TRUE)
dev.off()

par(opar)
png("graphs/benefit_caseStudy_Rc.png", height = 550)

par(xpd = TRUE, mar = c(8,4,1,2), cex = 1.3, pty = "s") 

plot(x = caseStudy1$Rc, y = caseStudy1$b3, type = "l", xlim = c(0,5), ylim = c(1, 2.7), xlab = "", ylab = "")

mtext(text = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), side = 2, line = 2.2, cex = 1.5)
mtext(text = expression(italic(R)[c]), side = 1, line = 2.3, cex = 1.5)

lines(x = mySpecies1$Rc, y = mySpecies1$b3, xlab = "", ylab = "",
     lty = "dashed", ylim = c(1, 2.7))
lines(x = mySpecies2$Rc, y = mySpecies2$b3, xlab = "", ylab = "",
     lty = "dotted", ylim = c(1, 2.7))
lines(x = mySpecies3$Rc, y = mySpecies3$b3, xlab = "", ylab = "",
     lty = "longdash", ylim = c(1, 2.7))
lines(x = mySpecies4$Rc, y = mySpecies4$b3, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(1, 2.7))
points(x = 0.0222, y = 1.002, pch = 4, cex = 1.5)

legend(x = -1.2, y = 0.5, legend = c(leg1, leg2, leg3,leg4, leg5), lty = c("dashed", "dotted", "longdash","solid", "dotdash"), bty = "n", cex = 1, horiz = TRUE, seg.len = 1.5)

legend(x = 0.2, y = 0.3, legend = c(leg5), lty = "dotdash", bty = "n", cex = 1, horiz = TRUE, seg.len = 1.5)

legend(x = 2.2, y = 0.3, legend = c(expression(italic(Cestrum)~survey)), pch = c(4), bty = "n", cex = 1)

dev.off()

png("graphs/benefit_caseStudy_muR.png", height = 550)

par(xpd = TRUE, mar = c(8,4,1,2), cex = 1.3, pty = "s") 

plot(x = caseStudy3$muRat, y = caseStudy3$b3, type = "l", xlim = c(1,5), ylim = c(1, 1.8), xlab = "", ylab = "")

mtext(text = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), side = 2, line = 2.2, cex = 1.5)
mtext(text = expression(mu[R]), side = 1, line = 2.3, cex = 1.5)

lines(x = myMu1$muRat, y = myMu1$b3, xlab = "", ylab = "",
     lty = "dashed", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
lines(x = myMu2$muRat, y = myMu2$b3, xlab = "", ylab = "",
     lty = "dotted", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
lines(x = myMu3$muRat, y = myMu3$b3, xlab = "", ylab = "",
     lty = "longdash", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
points(x = 1, y = 1.002, pch = 4, cex = 1.5)
lines(x = alphB_muRat$muRat, y = alphB_muRat$b2, 
      xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "solid", col = "grey", lwd = 1.5)
lines(x = alphB_muRat1$muRat, y = alphB_muRat1$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "dashed", col = "grey", lwd = 1.5)
lines(x = alphB_muRat2$muRat, y = alphB_muRat2$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "dotted", col = "grey", lwd = 1.5)
lines(x = alphB_muRat3$muRat, y = alphB_muRat3$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "longdash", col = "grey", lwd = 1.5)


legend(x = -0.1, y = 0.78, legend = c(rc1, rc2, rc3, rc4), lty = c("solid", "dashed", "dotted", "longdash"),bty = "n", cex = 1, horiz = TRUE, seg.len = 1.2)

legend(x = 0.15, y = 0.69, legend = c("Strategy 1", "Strategy 2", expression(italic(Cestrum)~survey)), pch = c(16, 16, 4), col = c("grey", "black", "black"), bty = "n", cex = 1, horiz = TRUE)

dev.off()

## Same except for encounter rates ---- 

png("graphs/alphAB_ER1_muR.png", width = 700, height = 550)
layout(matrix(c(1,2, 3, 3), ncol = 2, byrow = TRUE), heights = c(0.75,0.25))
par(xpd = TRUE, mar = c(2,4,1,2), cex = 1.3, pty = "s") 
# varying mu ratio:

plot(x = ERtest.5$muRat, y = ERtest.5$alphaA, type = "l", , lty = "dashed", xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1,10))
lines(x= ERtest.25$muRat, y = ERtest.25$alphaA, lty = "solid", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")
mtext(text = "mu ratio (muB/muA)", side = 1, line = 2, at = 5, cex = 1.2)
mtext(text = "alpha A *", side = 2, line = 2.2, cex = 1.2)
#single species with same Rc


plot(x = ERtest.25$muRat, y = ERtest.25$alphaB, type = "l", xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1,10))
mtext(text = "mu ratio (muB/muA)", side = 1, line = 2, at = 5, cex = 1.2)
mtext(text = "alpha B *", side = 2, line = 2.2, cex = 1.2)
lines(x= ERtest.5$muRat, y = ERtest.5$alphaB, lty = "dashed", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", legend = c("Rc = 0.25", "Rc = 0.5"), lty = c("solid", "dashed"), bty = "n", horiz = TRUE, cex = 0.8)

dev.off()


