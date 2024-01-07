#Varying one of the key variables (encounter rate ratio, ratio of detectabilities and cost ratio) keeping all others constant, how does each key variable affect the optimal proportions?
# all graphs need to only show ranges that are within the relevant optimal strategy range

source("scripts/multiSpOptFuncs.R") # loads functions that calculate thresholds between optimal strategies, and calculate the optimal proportions for the various strategies.
source("scripts/makeDfFunctions_varEffectsGraphs.R") # loads functions for creating the dataframes that the graphs use.
source("scripts/caseStudyVars.R") # loads values from Decky's dataset for C. auranticum and C. purpureum

opar <- par()
dflength  <-  100

# Graphs ----

## Using Decky data

# make dataframes:

#case study 1 is varying Rc from the threshold where the optimal strategy goes from alpha A = 1 and alpha B <1 to both alphas <1. Top end of the range is Rc=5.

caseStudy1 <- makeRcABdf(dflength = dflength, 
                         Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
                         muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                         RcTop = 5)

#case study 2 is varying Encounter rate ratio, from the threshold between strategies to Er=100.

caseStudy2 <- makeEratABdf(dflength = dflength, 
                           Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = 100)

#case study 3 is varying mu ratio, from muR = 1 to threshold between strategies (in this case when alpha A reaches 1)
caseStudy3 <- makeDF_AB_XmuR(dflength = dflength, muRto = 1.112,
                            Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1],
                            Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])

#Graph 1: y is alpha A and B (they are the same), x is ER. Different lines for different values of Rc

vectRc <- c(0.01, 0.05, 0.2, 0.5, 2, 5)

#graph legend labels
gr1leg1 <- expression(italic(R)[cA]==0.01)
gr1leg2 <- expression(italic(R)[cA]==0.022)
gr1leg3 <- expression(italic(R)[cA]==0.05)
gr1leg4 <- expression(italic(R)[cA]==0.2)
gr1leg5 <- expression(italic(R)[cA]==2)
gr1leg6 <- expression(italic(R)[cA]==5)

#Dataframes for the lines (case study line is in caseStudy2 DF)
graph1_Rc1 <- makeEratABdf(dflength = dflength, 
                           Rc = vectRc[1], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = 100)

graph1_Rc2 <- makeEratABdf(dflength = dflength, 
                           Rc = vectRc[2], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = 100)

graph1_Rc3 <- makeEratABdf(dflength = dflength, 
                           Rc = vectRc[3], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = 100)

#checking where the one to two alpha threshold is for this Rc:
(oneToTwoAlpha_Erat(Rc = vectRc[4], muRat = 1))

graph1_Rc4 <- makeEratABdf(dflength = dflength, 
                           Rc = vectRc[4], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = 100)

graph1_Rc5 <- makeEratABdf(dflength = dflength, 
                           Rc = vectRc[5], 
                           muRat = caseStudyVals$mu[2]/caseStudyVals$mu[1], 
                           EratTop = 100)
# Graph 1 ----
png("graphs/alphaAB_caseStudy_ER.png", height = 550)

par(xpd = TRUE, mar = c(7,6,1,4), cex = 1.3, pty = "s")

plot(x = caseStudy2$logER, y = caseStudy2$alphaA, xlab = "", ylab = "",
     type = "l", ylim = c(0, 1), xlim = c(0,4.65), xaxp = c(0,4.5,9), lty = "longdash", col = "darkgrey")
lines(x = graph1_Rc1$logER, y = graph1_Rc1$alphaA, xlab = "", ylab = "",
     lty = "dashed", ylim = c(0, 1), col = "lightgrey")
lines(x = graph1_Rc2$logER, y = graph1_Rc2$alphaA, xlab = "", ylab = "",
     lty = "dotted", ylim = c(0, 1))
lines(x = graph1_Rc3$logER, y = graph1_Rc3$alphaA, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(0, 1))
lines(x = graph1_Rc4$logER, y = graph1_Rc4$alphaA, xlab = "", ylab = "",
     lty = "longdash", ylim = c(0, 1))
lines(x = graph1_Rc5$logER, y = graph1_Rc5$alphaA, xlab = "", ylab = "",
     lty = "solid", ylim = c(0, 1))
points(x = log(26.6), y = 0.90, pch = 4, cex = 2)

mtext(text =expression(log(italic(E)[R])),  side = 1, line = 2.8, at = 2.3, cex = 1.6)
mtext(text = expression(paste(alpha[A],"*"~"&")), side = 2, line = 2.5, at = 0.5, cex = 1.6, las = 2)
mtext(text = expression(paste(alpha[B],"*")), side = 2, line = 3, at = 0.4, cex = 1.6, las = 2)

legend(x = -1.25, y = -0.37,legend = c(gr1leg1, gr1leg2, gr1leg3), lty = c("dashed", "longdash", "dotted"), col = c("lightgrey", "darkgrey", "black"), bty = "n", cex = 0.95, horiz = TRUE, xpd = TRUE)
legend(x = -1, y = -0.5, legend = c(gr1leg4, gr1leg5, gr1leg6), lty = c("dotdash", "longdash", "solid"), col = c("black", "black", "black"), bty = "n", cex = 0.95, horiz = TRUE, xpd = TRUE)
legend(x = 1, y = -0.63, legend = expression(italic(Cestrum)~survey), pch = 4, bty = "n", cex = 0.95, xpd = TRUE)

dev.off()

# Alternative graph 1: alpha A and B with x=RcA
mySpecies1 <- makeRcABdf(Erat = 1, muRat = 1, RcTop = 5)
mySpecies1$b3 <- b3(Rc = mySpecies1$Rc, muR = 1, ER = 1)

mySpecies2 <- makeRcABdf(Erat = 4, muRat = 1, RcTop = 5)
mySpecies2$b3 <- b3(Rc = mySpecies2$Rc, muR = 1, ER = 4)

mySpecies3 <- makeRcABdf(Erat = 16, muRat = 1, RcTop = 5)
mySpecies3$b3 <- b3(Rc = mySpecies3$Rc, muR = 1, ER = 16)

mySpecies4 <- makeRcABdf(Erat = 100, muRat = 1, RcTop = 5)
mySpecies4$b3 <- b3(Rc = mySpecies4$Rc, muR = 1, ER = 100)

# legend labels
leg1 <- expression(italic(E)[R]==1)
leg2 <- expression(italic(E)[R]==4)
leg3 <- expression(italic(E)[R]==16)
leg4 <- expression(italic(E)[R]==26.6)
leg5 <- expression(italic(E)[R]==100)

## Alternative Graph 1 ----
#plot 1: alpha a and b as a function of Rc, with multiple lines showing the effect of changing the E ratio

png("graphs/alphaAB_caseStudy_Rc1.png", height = 550)
# 

par(xpd = TRUE, mar = c(7,6,1,4), cex = 1.3, pty = "s")

# varying Rc:
plot(x = caseStudy1$Rc, y = caseStudy1$alphaA, xlab = "", ylab = "",
     type = "l", ylim = c(0, 1), lty = "longdash")
lines(x = mySpecies1$Rc, y = mySpecies1$alphaA, xlab = "", ylab = "",
     lty = "dashed", ylim = c(0, 1), col = "darkgrey")
lines(x = mySpecies2$Rc, y = mySpecies2$alphaA, xlab = "", ylab = "",
     lty = "dotted", ylim = c(0, 1))
lines(x = mySpecies3$Rc, y = mySpecies3$alphaA, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(0, 1))
lines(x = mySpecies4$Rc, y = mySpecies4$alphaA, xlab = "", ylab = "",
     lty = "solid", ylim = c(0, 1))
points(x = 0.0222, y = 0.90, pch = 4, cex = 2.5)

mtext(text =expression(italic(R)[cA]),  side = 1, line = 2.5, at = 2.5, cex = 1.6)
mtext(text = expression(paste(alpha[A],"*"~"&")), side = 2, line = 2.5, at = 0.5, cex = 1.6, las = 2)
mtext(text = expression(paste(alpha[B],"*")), side = 2, line = 3, at = 0.4, cex = 1.6, las = 2)

# plot(x = caseStudy1$Rc, y = caseStudy1$alphaB, xlab = "", ylab = "",
#      type = "l", ylim = c(0, 1))
# lines(x = mySpecies1$Rc, y = mySpecies1$alphaB, xlab = "", ylab = "",
#      lty = "dashed", ylim = c(0, 1))
# lines(x = mySpecies2$Rc, y = mySpecies2$alphaB, xlab = "", ylab = "",
#      lty = "dotted", ylim = c(0, 1))
# lines(x = mySpecies3$Rc, y = mySpecies3$alphaB, xlab = "", ylab = "",
#      lty = "longdash", ylim = c(0, 1))
# lines(x = mySpecies4$Rc, y = mySpecies4$alphaB, xlab = "", ylab = "",
#      lty = "dotdash", ylim = c(0, 1))
# points(x = 0.0222, y = 0.90, pch = 4, cex = 1.5)
# 
# mtext(text =expression(displaystyle(R)[c]),  side = 1, line = 2.5, at = 2.5)
# mtext(text = expression(paste(alpha[B]^"*")), side = 2, line = 2, at = 0.5, cex = 1.4)

legend(x = -0.5, y = -0.37, legend = c(leg1, leg2, leg3), lty = c("dashed", "dotted", "dotdash"), col = c("darkgrey", "black", "black"), bty = "n", cex = 0.95, horiz = TRUE, xpd = TRUE)
legend(x = 0.3, y = -0.5, legend = c(leg4,leg5), lty = c("longdash", "solid"), bty = "n", cex = 0.95, horiz = TRUE, xpd = TRUE)
legend(x = 1.3, y = -0.63, legend = expression(italic(Cestrum)~survey), pch = 4, bty = "n", cex = 0.95)

dev.off()

# Graph 2: change in alpha A and alpha B (y axis) over a range of muR (1 to 10) with the same values of Rc (same values and line types as above)

#add benefit to caseStudy3 - modified so that ER is the x variable

caseStudy3$b3 <- b3(muR = caseStudy3$muRat, 
                    ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
                    Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1])

#Make dataframes 
# myMu1 <- makeDF_AB_XmuR(muRto = 1.7, 
#                         Rc = 0.05, 
#                         Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
# myMu1$b3 <- b3(muR = myMu2$muRat, 
#                ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
#                Rc = 0.05)
# 
# myMu2 <- makeDF_AB_XmuR(muRto = 4.1818, 
#                         Rc = 0.2, 
#                         Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
# myMu2$b3 <- b3(muR = myMu1$muRat, 
#                ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
#                Rc = 0.2)
# 
# myMu3 <- makeDF_AB_XmuR(muRto = 10, 
#                         Rc = 2, 
#                         Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
# myMu3$b3 <- b3(muR = myMu3$muRat, 
#                ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
#                Rc =2)
# 
# myMu4 <- makeDF_AB_XmuR(muRto = 10, 
#                         Rc = 5, 
#                         Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
# myMu3$b3 <- b3(muR = myMu3$muRat, 
#                ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], 
#                Rc =5)


myMu1 <- makeDF_AB_XmuR(muRto = 2.03, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
myMu1$b3 <- b3(muR = myMu1$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3)

 myMu2 <- makeDF_AB_XmuR(muRto = 2.76, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
 myMu2$b3 <- b3(muR = myMu2$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5)

 myMu3 <- makeDF_AB_XmuR(muRto = 4.64, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10, Erat = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"])
 myMu3$b3 <- b3(muR = myMu3$muRat, ER = caseStudyVals$Erates[caseStudyVals$species=="C_auranticum"]/caseStudyVals$Erates[caseStudyVals$species=="C_purpureum"], Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10)

# alphB_muRat <- makeDF_B_XmuR(muRfrom = max(caseStudy3$muRat), muRto = 10, 
#                              Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], 
#                              Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
# alphB_muRat$b2 <- b2(Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], 
#                      ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], 
#                      muR = alphB_muRat$muRat)
# 
# alphB_muRat1 <- makeDF_B_XmuR(muRfrom = max(myMu1$muRat), muRto = 10, 
#                               Rc = 0.05, 
#                               Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])  
# alphB_muRat1$b2 <- b2(Rc = 0.05, 
#                       ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], 
#                       muR = alphB_muRat1$muRat)
# 
# alphB_muRat2 <- makeDF_B_XmuR(muRfrom = max(myMu2$muRat), muRto = 10, 
#                               Rc = 0.2, 
#                               Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])  
# alphB_muRat2$b2 <- b2(Rc = 0.2, 
#                       ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1],
#                       muR = alphB_muRat2$muRat)


 alphB_muRat <- makeDF_B_XmuR(muRfrom = max(caseStudy3$muRat), muRto = 5, Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
 alphB_muRat$b2 <- b2(Rc = caseStudyVals$cm[2]/caseStudyVals$cw[1], ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat$muRat)

 alphB_muRat1 <- makeDF_B_XmuR(muRfrom = max(myMu1$muRat), muRto = 5, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3, Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
 alphB_muRat1$b2 <- b2(Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*3, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat1$muRat)

 alphB_muRat2 <- makeDF_B_XmuR(muRfrom = max(myMu2$muRat), muRto = 5, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5, Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
 alphB_muRat2$b2 <- b2(Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*5, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat2$muRat)

 alphB_muRat3 <- makeDF_B_XmuR(muRfrom = max(myMu3$muRat), muRto = 5, Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10, Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1])
 alphB_muRat3$b2 <- b2(Rc = (caseStudyVals$cm[2]/caseStudyVals$cw[1])*10, ER = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muR = alphB_muRat3$muRat)

# legend labels
rc1 <- expression(italic(R)[cA]==0.022)
rc2 <- expression(italic(R)[cA]==0.066)
rc3 <- expression(italic(R)[cA]==0.11)
rc4 <- expression(italic(R)[cA]==0.22)

# Graph 2 ----
png("graphs/alphAB_caseStudy_muR.png", width = 700, height = 550)
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE), heights = c(0.75,0.25))
par(xpd = TRUE, mar = c(2,4,1,2), cex = 1.3, pty = "s") 
# varying mu ratio:

plot(x = caseStudy3$muRat, y = caseStudy3$alphaA, type = "l", xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1,5), lty = "dotted")
mtext(text = expression(italic(mu)[R]), side = 1, line = 2.5, at = 6, cex = 1.6)
mtext(text = expression(paste(alpha[A]^"*")), side = 2, line = 2.4, cex = 1.6, las = 2)

lines(x = myMu1$muRat, y = myMu1$alphaA, xlab = "", ylab = "",
     ylim = c(0, 1), xlim = c(1,5),
     lty = "dotdash")
lines(x = myMu2$muRat, y = myMu2$alphaA, xlab = "", ylab = "",
     ylim = c(0, 1), xlim = c(1,5),
     lty = "longdash")
lines(x = myMu3$muRat, y = myMu3$alphaA, xlab = "", ylab = "",
     ylim = c(0, 1), xlim = c(1,5),
     lty = "solid")

points(x = 1, y = 0.9, pch = 4, cex = 1.3)

plot(x = caseStudy3$muRat, y = caseStudy3$alphaB, type = "l", xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1,5), lty = "dotted")
mtext(text = expression(italic(mu)[R]), side = 1, line = 2.5, at = 6, cex = 1.6)
mtext(text = expression(paste(alpha[B]^"*")), side = 2, line = 2.4, cex = 1.6, las = 2)

lines(x= myMu1$muRat, y = myMu1$alphaB, lty = "dotdash", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")
lines(x= myMu2$muRat, y = myMu2$alphaB, lty = "longdash", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")
lines(x= myMu3$muRat, y = myMu3$alphaB, lty = "solid", ylim = c(0, 1), xlim = c(1,5), xlab = "", ylab = "")

lines(x = alphB_muRat$muRat, y = alphB_muRat$alphaB, 
      xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5), lty = "dotted", col = "grey")
lines(x = alphB_muRat1$muRat, y = alphB_muRat1$alphaB, 
     xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5), lty = "dotdash", col = "grey")
lines(x = alphB_muRat2$muRat, y = alphB_muRat2$alphaB, 
     xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5), lty = "longdash", col = "grey")
lines(x = alphB_muRat3$muRat, y = alphB_muRat3$alphaB, 
     xlab = "", ylab = "",  ylim = c(0, 1), xlim = c(1,5), lty = "solid", col = "grey")

points(x = 1, y = 0.9, pch = 4, cex = 1.3)

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", legend = c(rc1, rc2, rc3, rc4), lty = c("dotted", "dotdash", "longdash", "solid"), bty = "n", horiz = TRUE, cex = 1, inset = -0.15)
legend("center", legend = c("Strategy 1", "Strategy 2", expression(italic(Cestrum)~survey)), pch = c(16, 16, 4), col = c("grey", "black", "black"), bty = "n", cex = 1, horiz = TRUE)
dev.off()


# Graph 3: y is benefit, x is ER, with different lines for Rc (same values and line types as above)
# adding benefits to the dataframes
par(opar)
 caseStudy1$b3 <- b3(ER = 26.6, muR = 1, Rc = caseStudy1$Rc)
# 
# graph1_Rc1$b3 <- b3(Rc = 0.01, muR = 1, ER = graph1_Rc1$Erat)
# graph1_Rc2$b3 <- b3(Rc = 0.05, muR = 1, ER = graph1_Rc2$Erat)
# graph1_Rc3$b3 <- b3(Rc = 0.2, muR = 1, ER = graph1_Rc3$Erat)
# graph1_Rc4$b3 <- b3(Rc = 2, muR = 1, ER = graph1_Rc4$Erat)
# graph1_Rc5$b3 <- b3(Rc = 5, muR = 1, ER = graph1_Rc5$Erat)
# 

 # Graph 3 ----
png("graphs/benefit_caseStudy_Rc.png", height = 550)

par(xpd = TRUE, mar = c(8,4,1,2), cex = 1.3, pty = "s")

plot(x = caseStudy1$Rc, y = caseStudy1$b3, type = "l", xlim = c(0,5), ylim = c(1, 2.7), xlab = "", ylab = "", lty = "longdash")

mtext(text = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), side = 2, line = 2.2, cex = 1.5)
mtext(text = expression(italic(R)[cA]), side = 1, line = 2.3, cex = 1.5)

lines(x = mySpecies1$Rc, y = mySpecies1$b3, xlab = "", ylab = "",
     lty = "dashed", ylim = c(1, 2.7), col = "darkgrey")
lines(x = mySpecies2$Rc, y = mySpecies2$b3, xlab = "", ylab = "",
     lty = "dotted", ylim = c(1, 2.7))
lines(x = mySpecies3$Rc, y = mySpecies3$b3, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(1, 2.7))
lines(x = mySpecies4$Rc, y = mySpecies4$b3, xlab = "", ylab = "",
     lty = "solid", ylim = c(1, 2.7))
points(x = 0.0222, y = 1.002, pch = 4, cex = 1.5)

legend(x = -1.2, y = 0.5, legend = c(leg1, leg2, leg3,leg4), lty = c("dashed", "dotted", "dotdash","longdash"), col = c("darkgrey", "black","black","black"), bty = "n", cex = 1, horiz = TRUE, seg.len = 1.5)

legend(x = 0.2, y = 0.3, legend = c(leg5), lty = "solid", bty = "n", cex = 1, horiz = TRUE, seg.len = 1.5)

legend(x = 2.2, y = 0.3, legend = c(expression(italic(Cestrum)~survey)), pch = c(4), bty = "n", cex = 1)

dev.off()

rc1 <- expression(italic(R)[cA]==0.022)
rc2 <- expression(italic(R)[cA]==0.066)
rc3 <- expression(italic(R)[cA]==0.11)
rc4 <- expression(italic(R)[cA]==0.22)


# Graph 4 ----
png("graphs/benefit_caseStudy_muR.png", height = 550)

par(xpd = TRUE, mar = c(8,4,1,2.2), cex = 1.3, pty = "s") 

plot(x = caseStudy3$muRat, y = caseStudy3$b3, type = "l", xlim = c(1,5), ylim = c(1, 1.8), xlab = "", ylab = "")

mtext(text = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), side = 2, line = 2.2, cex = 1.5)
mtext(text = expression(mu[R]), side = 1, line = 2.3, cex = 1.5)

lines(x = myMu1$muRat, y = myMu1$b3, xlab = "", ylab = "",
     lty = "longdash", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
lines(x = myMu2$muRat, y = myMu2$b3, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
lines(x = myMu3$muRat, y = myMu3$b3, xlab = "", ylab = "",
     lty = "dotted", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
points(x = 1, y = 1.002, pch = 4, cex = 1.5)
lines(x = alphB_muRat$muRat, y = alphB_muRat$b2, 
      xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "solid", col = "grey", lwd = 1.5)
lines(x = alphB_muRat1$muRat, y = alphB_muRat1$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "longdash", col = "grey", lwd = 1.5)
lines(x = alphB_muRat2$muRat, y = alphB_muRat2$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "dotdash", col = "grey", lwd = 1.5)
lines(x = alphB_muRat3$muRat, y = alphB_muRat3$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "dotted", col = "grey", lwd = 1.5)


legend(x = 0, y = 0.78, legend = c(rc1, rc2, rc3, rc4), lty = c("solid", "longdash", "dotdash", "dotted"),bty = "n", cex = 0.95, horiz = TRUE, seg.len = 1.2)

legend(x = 0.15, y = 0.69, legend = c("Strategy 1", "Strategy 2", expression(italic(Cestrum)~survey)), pch = c(16, 16, 4), col = c("grey", "black", "black"), bty = "n", cex = 1, horiz = TRUE)

dev.off()

# graph 5 ----
# putting 3 and 4 on the same graph

png("graphs/benefit_caseStudy2.png", width = 700, height = 550)
layout(matrix(c(1,2,3,4), ncol = 2, byrow = TRUE), heights = c(0.75,0.25))
par(mar = c(2.5,4,1,1), cex = 1.3, pty = "s") 

plot(x = caseStudy1$Rc, y = caseStudy1$b3, type = "l", xlim = c(0,5), ylim = c(1, 2.7), xlab = "", ylab = "", lty = "longdash")

mtext(text = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), side = 2, line = 2.2, cex = 1.5)
mtext(text = expression(italic(R)[cA]), side = 1, line = 2.3, cex = 1.5)

lines(x = mySpecies1$Rc, y = mySpecies1$b3, xlab = "", ylab = "",
     lty = "dashed", ylim = c(1, 2.7), col = "darkgrey")
lines(x = mySpecies2$Rc, y = mySpecies2$b3, xlab = "", ylab = "",
     lty = "dotted", ylim = c(1, 2.7))
lines(x = mySpecies3$Rc, y = mySpecies3$b3, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(1, 2.7))
lines(x = mySpecies4$Rc, y = mySpecies4$b3, xlab = "", ylab = "",
     lty = "solid", ylim = c(1, 2.7))
points(x = 0.0222, y = 1.002, pch = 4, cex = 2.5)


plot(x = caseStudy3$muRat, y = caseStudy3$b3, type = "l", xlim = c(1,5), ylim = c(1, 1.8), xlab = "", ylab = "")

mtext(text = expression(italic(b)[multisp.]~(scriptstyle(italic(var)(italic(D)[LTS])/italic(var)(italic(D)[opt])))), side = 2, line = 2.2, cex = 1.5)
mtext(text = expression(mu[R]), side = 1, line = 2.3, cex = 1.5)

lines(x = myMu1$muRat, y = myMu1$b3, xlab = "", ylab = "",
     lty = "longdash", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
lines(x = myMu2$muRat, y = myMu2$b3, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
lines(x = myMu3$muRat, y = myMu3$b3, xlab = "", ylab = "",
     lty = "dotted", ylim = c(1, 4.1), xlim = c(1,5), lwd = 1.5)
points(x = 1, y = 1.002, pch = 4, cex = 2.5)

lines(x = alphB_muRat$muRat, y = alphB_muRat$b2, 
      xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "solid", col = "grey", lwd = 1.5)
lines(x = alphB_muRat1$muRat, y = alphB_muRat1$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "longdash", col = "grey", lwd = 1.5)
lines(x = alphB_muRat2$muRat, y = alphB_muRat2$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "dotdash", col = "grey", lwd = 1.5)
lines(x = alphB_muRat3$muRat, y = alphB_muRat3$b2, 
     xlab = "", ylab = "", ylim = c(1, 4.1), xlim = c(1,5), lty = "dotted", col = "grey", lwd = 1.5)

par(mar = c(2.5,1,1,1), cex = 1.3, pty = "s") 
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", legend = c(leg1, leg2, leg3), lty = c("dashed", "dotted", "dotdash","longdash"), col = c("darkgrey", "black","black"),bty = "n", horiz = TRUE, cex = 0.95, inset = -0.15, xpd = TRUE)

legend("center", legend = c(leg4, leg5), lty = c("longdash", "solid"), bty = "n", cex = 0.95, horiz = TRUE, xpd = TRUE)

legend("bottom", legend = c(expression(italic(Cestrum)~survey)), pch = c(4), bty = "n", cex = 0.95, inset = -0.15, xpd = TRUE)

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", legend = c(rc1, rc2), lty = c("solid", "longdash"), bty = "n", horiz = TRUE, cex = 0.95, inset = -0.15, xpd = TRUE)

legend("center", legend = c(rc3, rc4), lty = c("dotdash", "dotted"), bty = "n", cex = 0.95, horiz = TRUE, xpd = TRUE)

legend("bottom", legend = c("Strategy 1", "Strategy 2"), pch = c(16, 16), col = c("grey", "black"), bty = "n", cex = 0.95, horiz = TRUE, inset = -0.15, xpd = TRUE)

legend("bottom", legend = c(expression(italic(Cestrum)~survey)), pch = 4, col = "black", bty = "n", cex = 0.95, horiz = TRUE, inset = -0.6, xpd = TRUE)


dev.off()

# Graph 6 ----
# Putting the two versions of graph 1 on the same png file

png("graphs/alphaAB_caseStudyPanel.png", width = 700, height = 550)


layout(matrix(c(1,2,3,4), ncol = 2, byrow = TRUE), heights = c(0.75,0.25))
par(mar = c(2,5,1,1), cex = 1.3, pty = "s") 

# alpha a and b against Rc with different lines for ER
plot(x = caseStudy1$Rc, y = caseStudy1$alphaA, xlab = "", ylab = "",
     type = "l", ylim = c(0, 1), lty = "longdash")
lines(x = mySpecies1$Rc, y = mySpecies1$alphaA, xlab = "", ylab = "",
     lty = "dashed", ylim = c(0, 1), col = "darkgrey")
lines(x = mySpecies2$Rc, y = mySpecies2$alphaA, xlab = "", ylab = "",
     lty = "dotted", ylim = c(0, 1))
lines(x = mySpecies3$Rc, y = mySpecies3$alphaA, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(0, 1))
lines(x = mySpecies4$Rc, y = mySpecies4$alphaA, xlab = "", ylab = "",
     lty = "solid", ylim = c(0, 1))
points(x = 0.0222, y = 0.90, pch = 4, cex = 2.5)

mtext(text =expression(italic(R)[cA]),  side = 1, line = 2.5, at = 2.5, cex = 1.6)
mtext(text = expression(paste(alpha[A],"*"~"&")), side = 2, line = 2.5, at = 0.5, cex = 1.6, las = 2)
mtext(text = expression(paste(alpha[B],"*")), side = 2, line = 3, at = 0.4, cex = 1.6, las = 2)

# alpha a and b against log ER with different lines for Rc

plot(x = caseStudy2$logER, y = caseStudy2$alphaA, xlab = "", ylab = "",
     type = "l", ylim = c(0, 1), xlim = c(0,4.65), xaxp = c(0,4.5,9), lty = "longdash", col = "darkgrey")
lines(x = graph1_Rc1$logER, y = graph1_Rc1$alphaA, xlab = "", ylab = "",
     lty = "dashed", ylim = c(0, 1), col = "lightgrey")
lines(x = graph1_Rc2$logER, y = graph1_Rc2$alphaA, xlab = "", ylab = "",
     lty = "dotted", ylim = c(0, 1))
lines(x = graph1_Rc3$logER, y = graph1_Rc3$alphaA, xlab = "", ylab = "",
     lty = "dotdash", ylim = c(0, 1))
lines(x = graph1_Rc4$logER, y = graph1_Rc4$alphaA, xlab = "", ylab = "",
     lty = "longdash", ylim = c(0, 1))
lines(x = graph1_Rc5$logER, y = graph1_Rc5$alphaA, xlab = "", ylab = "",
     lty = "solid", ylim = c(0, 1))
points(x = log(26.6), y = 0.90, pch = 4, cex = 2)

mtext(text =expression(log(italic(E)[R])),  side = 1, line = 2.8, at = 2.3, cex = 1.6)
mtext(text = expression(paste(alpha[A],"*"~"&")), side = 2, line = 2.5, at = 0.5, cex = 1.6, las = 2)
mtext(text = expression(paste(alpha[B],"*")), side = 2, line = 3, at = 0.4, cex = 1.6, las = 2)

# legend panels

par(mar = c(2,1,1,1), cex = 1.3, pty = "s") 
plot(1, type = "n", axes=FALSE, xlab="", ylab="")

legend(x = "top", legend = c(leg1, leg2, leg3), lty = c("dashed", "dotted", "dotdash","longdash"), col = c("darkgrey", "black","black"),bty = "n", horiz = TRUE, cex = 0.90, inset = -0.15, xpd = TRUE)

legend("center", legend = c(leg4, leg5), lty = c("longdash", "solid"), bty = "n", cex = 0.90, horiz = TRUE, xpd = TRUE)

legend("bottom", legend = c(expression(italic(Cestrum)~survey)), pch = c(4), bty = "n", cex = 0.95, inset = -0.15, xpd = TRUE)

plot(1, type = "n", axes=FALSE, xlab="", ylab="")

legend(x = "top", legend = c(gr1leg1, gr1leg2, gr1leg3), lty = c("dashed", "longdash", "dotted"), col = c("lightgrey", "darkgrey", "black"), bty = "n", horiz = TRUE, cex = 0.90, inset = -0.15, xpd = TRUE)

legend("center", legend = c(gr1leg4, gr1leg5, gr1leg6), lty = c("dotdash", "longdash", "solid"), col = c("black", "black", "black"), bty = "n", cex = 0.90, horiz = TRUE, xpd = TRUE)

legend("bottom", legend = c(expression(italic(Cestrum)~survey)), pch = 4, col = "black", bty = "n", cex = 0.95, horiz = TRUE, inset = -0.15, xpd = TRUE)


dev.off()