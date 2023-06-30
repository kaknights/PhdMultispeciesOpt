#Varying one of the key variables (encounter rates, ratio of detectabilities and cost ratio) keeping all others constant, how does each key variable affect the optimal proportions?

#This version of this script = between using separate variables for sp A and B vs ratios. Commented out sections are the separate variables (functions to create dfs) and code below uses separate variables to make graphs.

source("scripts/multiSpOptFuncs.R") # loads functions that calculate thresholds between optimal strategies, and calculate the optimal proportions for the various strategies.
source("scripts/caseStudyVars.R") # loads values from Decky's dataset for C. auranticum and C. purpureum

#What goes in?
#muRat, Erat, Rc

#what comes out?
#three graphs: how alpha B changes with a) change in detectability ratio, b) change in cost ratio, c) change in encounter rate ratio.
# same graphs for alphas A and B
# all graphs need to only show ranges that are within the relevant optimal strategy range

opar <- par()
dflength <- 100
#Make dataframes for plots

# alpha A and alpha B both <1
  
makeRcABdf <- function(dflength = 100, Erat, muRat, RcTop){
  Rc_ABdf <- data.frame("Rc" = numeric(dflength), "alphaA" = numeric(dflength), "alphaB" = numeric(dflength))
  Rc_ABdf$Rc <- seq(from = oneToTwoAlpha_Rc(Erat = Erat, muRat = muRat), to = RcTop, length.out = dflength)
  Rc_ABdf$alphaA <- optAB_Rc(Rc = Rc_ABdf$Rc, muRat = muRat, Erat = Erat)[[1]]
  Rc_ABdf$alphaB <- optAB_Rc(Rc = Rc_ABdf$Rc, muRat = muRat, Erat = Erat)[[2]]
  
  return(Rc_ABdf)
}  

makeEratABdf <- function(dflength = 100, Rc, muRat, EratTop){
 Erat_ABdf <- data.frame("Erat" = numeric(dflength), "alphaA" = numeric(dflength), "alphaB" = numeric(dflength))
 EratBottom <- ifelse(oneToTwoAlpha_Erat(Rc = Rc, muRat = muRat)<0.0001, 0.0001, 
                      oneToTwoAlpha_Erat(Rc = Rc, muRat = muRat))
  Erat_ABdf$Erat <- seq(from = EratBottom, to = EratTop, length.out = dflength)
  Erat_ABdf$alphaA <- optAB_Rc(Rc = Rc, muRat = muRat, Erat = Erat_ABdf$Erat)[[1]]
  Erat_ABdf$alphaB <- optAB_Rc(Rc = Rc, muRat = muRat, Erat = Erat_ABdf$Erat)[[2]]
  
  return(Erat_ABdf)
}
  
makeMuRatABdf <- function(dflength = 100, Rc, Erat, muRatTop){
muRat_ABdf <- data.frame("muRat" = numeric(dflength), "alphaA" = numeric(dflength), "alphaB" = numeric(dflength))
  muRat_ABdf$muRat <- seq(from = 1, to = muRatTop, length.out = dflength)
  muRat_ABdf$alphaA <- optAB_Rc(Rc = Rc, Erat = Erat, muRat = muRat_ABdf$muRat)[[1]]
  muRat_ABdf$alphaB <- optAB_Rc(Rc = Rc, Erat = Erat, muRat = muRat_ABdf$muRat)[[2]]  
  
  return(muRat_ABdf)
}


# alpha A = 1, alpha B<1

#effect of changing Rc

makeRc_Bdf <- function(dflength = 100, muRat, Erat){
  alphB_Rc <- data.frame("Rc" = numeric(dflength),
                         "alphaB" = numeric(dflength))
  alphB_Rc$Rc <- seq(from = alphBToLTS_Rc(Erat = Erat, muRat = muRat), 
                     to = oneToTwoAlpha_Rc(Erat = Erat, muRat = muRat), 
                     length.out = dflength)
  alphB_Rc$alphaB <- optB_Rc(alphB_Rc$Rc,muRat,Erat)
  
  return(alphB_Rc)
}

test <- makeRc_Bdf(dflength = dflength, muRat = 2, Erat = Erat)

makeMuRat_Bdf <- function(dflength = 100, Rc, Erat){
  #effect of changing muRat
  alphB_muRat <- data.frame("muRat" = numeric(dflength),
                            "alphaB" = numeric(dflength))
  muRatTop <- ifelse(oneToTwoAlpha_muRatSq(Rc = Rc, Erat = Erat)<=1, NA,
                     oneToTwoAlpha_muRatSq(Rc = Rc, Erat = Erat))
  if (is.na(muRatTop)){
    print(paste0("The ratio muB/muA would need to be <1 to fall within the range of this optimal strategy, where alpha A = 1 and alpha B < 1, with the current values of Rc = ", round(Rc, 3), " and E ratio = ", round(Erat, 3), ". This ratio must be > 1 for the expressions used here to apply."))
  } else {
    alphB_muRat$muRat <- seq(from = 1, to = sqrt(muRatTop), length.out = dflength)
  alphB_muRat$alphaB <- optB_Rc(Rc = Rc, Erat = Erat, muRat = alphB_muRat$muRat)
  return(alphB_muRat)
    }
}
  
test1 <- makeMuRat_Bdf(Rc = 0.05, Erat = Erat) 

makeErat_Bdf <- function(dflength = 100, Rc, muRat){
  #effect of changing Erat
  alphB_Erat <- data.frame("Erat" = numeric(dflength),
                           "alphaB" = numeric(dflength))
  alphB_Erat$Erat <- seq(from = alphBtoLTS_Erat(Rc = Rc, muRat = muRat), 
                         to = oneToTwoAlpha_Erat(Rc = Rc, muRat = muRat), length.out = dflength)
  alphB_Erat$alphaB <- optB_Rc(Rc = Rc, muRat = muRat, Erat = alphB_Erat$Erat)
  
  return(alphB_Erat)
}  

test2 <- makeErat_Bdf(dflength = dflength, Rc = Rc_calc, muRat = muRat)

#it looks like the function to make the dataframes works fine, but very few of the rows actually result in an alpha B opt scenario. Need to think through how to narrow down the range of the variables so that the graphs are useful.

# changing the ratio of detectabilities
# ABdata <- data.frame("muA" = seq(from_muA, muB, length.out = dflength), "muB" = muB, "alphaA" = numeric(dflength), "alphaB" = numeric(dflength))
# ABdata$ratio <- ABdata$muA/ABdata$muB
# ABdata$alphaA <- ifelse(new_optAB(muA = ABdata$muA, muB = muB, Ea = Ea, Eb = Eb, C = C)[[1]]<=1, 
#                         new_optAB(muA = ABdata$muA, muB = muB, Ea = Ea, Eb = Eb, C = C)[[1]], 1)
# ABdata$alphaB <- new_optAB(muA = ABdata$muA, muB = muB, Ea = Ea, Eb = Eb, C = C)[[2]]
# 
# # changing cost ratio
# costRat <- data.frame("C" = seq(range_C[1], range_C[2], length.out = dflength), "alphaA" = numeric(dflength), "alphaB" = numeric(20))
# costRat$alphaA <- new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = Eb, C = costRat$C)[[1]]
# costRat$alphaB <- new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = Eb, C = costRat$C)[[2]]
#   
# #changing encounter rate of species A (the rare one/less detectable)
# AB_A_data <- data.frame("EA" = seq(range_Ea[1], range_Ea[2], length.out = dflength), "alphaA" = numeric(dflength), "alphaB" = numeric(dflength))
# AB_A_data$alphaA <- ifelse(new_optAB(muA = muA, muB = muB, Ea = AB_A_data$EA, Eb = Eb, C = C)[[1]]<=1,
#                            new_optAB(muA = muA, muB = muB, Ea = AB_A_data$EA, Eb = Eb, C = C)[[1]], 1)
# AB_A_data$alphaB <- ifelse(new_optAB(muA = muA, muB = muB, Ea = AB_A_data$EA, Eb = Eb, C = C)[[2]]<=1,
#                            new_optAB(muA = muA, muB = muB, Ea = AB_A_data$EA, Eb = Eb, C = C)[[2]], 1)
# 
# #changing encounter rate of species B (the abundant one/more detectable)
# AB_B_data <- data.frame("EB" = seq(range_Eb[1], range_Eb[2], length.out = dflength), "alphaA" = numeric(dflength), "alphaB" = numeric(dflength))
# AB_B_data$alphaA <- ifelse(new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = AB_B_data$EB, C = C)[[1]]<=1,
#                            new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = AB_B_data$EB, C = C)[[1]], 1)
# AB_B_data$alphaB <- ifelse(new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = AB_B_data$EB, C = C)[[2]]<=1,
#                            new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = AB_B_data$EB, C = C)[[2]], 1)
# results_dfs <- list(ABdata, costRat, AB_A_data, AB_B_data)
# }


# commented out version of function to make dataframes that uses mu A and B and E A and B separately.
# make_alphB_df <- function(muA, muB, Ea, Eb, C, from_muA, range_C, range_Ea, range_Eb, dflength){
#   # changing the ratio of detectabilities
#   detect <- data.frame("muA" = seq(from_muA, muB, length.out = dflength), "muB"=muB)
# detect$ratio <- detect$muA/detect$muB
# detect$alphaB <- ifelse(opt_alpha_B(Ea = Ea, Eb = Eb, muA = detect$muA, muB = detect$muB, C = C)<1,
#                         opt_alpha_B(Ea = Ea, Eb = Eb, muA = detect$muA, muB = detect$muB, C = C), 1)
# 
# #changing the cost ratio
# costRat <- data.frame("C" = seq(range_C[1], range_C[2], length.out = dflength), "alphaB" = numeric(dflength))
# costRat$alphaB <- ifelse(opt_alpha_B(Ea=Ea, Eb = Eb, muA = muA, muB = muB, C = costRat$C)<=1, 
#                          opt_alpha_B(Ea=Ea, Eb = Eb, muA = muA, muB = muB, C = costRat$C), 1)
# 
# #changing encounter rate of species A (the rare one)
# EAdata <- data.frame("EA" = seq(range_Ea[1], range_Ea[2], length.out = dflength), "alphaB" = numeric(dflength))
# EAdata$alphaB <- opt_alpha_B(Ea = EAdata$EA, Eb = Eb, muA = muA, muB = muB, C=C)
# 
# #changing encounter rate of species B (the common one)
# EBdata <- data.frame("EB" = seq(range_Eb[1], range_Eb[2], length.out = dflength), "alphaB" = numeric(dflength))
# EBdata$alphaB <- opt_alpha_B(Ea = Ea, Eb = EBdata$EB, muA = muA, muB = muB, C=C)
# 
# myDataframes <- list(detect, costRat, EAdata, EBdata)
# }

# at these values alpha A = 1 for C = 0.5
muA  <-  1.5; muB <- 3; Ea <- 0.01; Eb <- 0.3

# what C gives alpha A < 1 for these values? 
# muA  <-  1.5; muB <- 3; Ea <- 0.01; Eb <- 0.3 upper = 0.68; lower = 0.17
# muA <- 1.2; upper = 0.68; lower = 0.116

upperT_C(EA = Ea, EB = Eb, muA = muA, muB = muB) 
lowerT_C(EA = Ea, EB = Eb, muA = muA, muB = muB) 

# make dfs for the alpha A = 1 solution
alphBdfs <- make_alphB_df(muA = 1.5, muB = 3, Ea = 0.01, Eb = 0.3, C = 0.68-((0.68-0.17)/2), from_muA = 0.1, range_C = c(0.17, 0.68), range_Ea = c(0.001, 0.1), range_Eb = c(0.2, 1), dflength = 20)

# opt B
alphB <- opt_alpha_B(Ea = Ea, Eb = Eb, muA = muA, muB = muB, C = 0.17)

#make dfs for the two alpha solution 

AB_dfs <- make_dfs_alphAB(muA = 1.5, muB = 3, Ea = 0.01, Eb = 0.3, C = 0.17-((0.17-0.01)/2), from_muA = 0.1, range_C = c(0.01, 0.17), range_Ea = c(0.001, 0.1), range_Eb = c(0.2, 1), dflength = 20)

# opt A and B
alphAB <- new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = Eb, C = 0.17)

C <- round(0.68-((0.68-0.17)/2), 2) #for plotting in alpha B graphs
C2 <- round(0.17-((0.17-0.01)/2), 2) #for plotting in alpha AB graphs


par(opar)
#png("graphs/bivar_alphaB.png", width = 700, height = 700)
par(mfrow = c(2,2), cex = 1.2)

#Panel 1: how alpha B changes with change in ratio muA/muB

plot(x = alphBdfs[[1]]$ratio, y = alphBdfs[[1]]$alphaB, 
     xlab = "ratio muA/muB", ylab = "alpha B *", 
     ylim = c(0,1),
     type = "l", main = paste0("EA = ", round(Ea, 2), "; EB = ", round(Eb, 2), "; C = ", C))


#Panel 2: how alpha B changes with change in the cost ratio
plot(x = alphBdfs[[2]]$C, y = alphBdfs[[2]]$alphaB, 
     xlab = "cost ratio Cl/Cm", ylab = "alpha B *", 
     ylim = c(0,1),
     type = "l", main = paste0("EA = ", round(Ea, 2), "; EB = ", round(Eb, 2), "; muA/muB = ", round(muA/muB, 2)))
abline(h = alphB, col = "red", xpd = FALSE)


#Panel 3: how alpha B changes with change in encounter rate of species A
plot(x = alphBdfs[[3]]$EA, y = alphBdfs[[3]]$alphaB, 
     xlab = "encounter rate species A (less detectable)", ylab = "alpha B *",
     ylim = c(0,1),
     type = "l", main = paste0("EB = ", round(Eb, 2), "; muA/muB = ", round(muA/muB, 2), "; C = ", C))

#Panel 4: how alpha B changes with change in encounter rate of species B
plot(x = alphBdfs[[4]]$EB, y = alphBdfs[[4]]$alphaB, 
     xlab = "encounter rate species B (more detectable)", ylab = "alpha B *",
     ylim = c(0,1),
     type = "l", main = paste0("EA = ", round(Ea, 2), "; muA/muB = ", round(muA/muB, 2), "; C = ", C))
#dev.off()

#page 1: varying ratio of detectabilities and cost ratio
#png("graphs/bivar_alphaAB_muAndC.png", width = 700, height = 700)
par(mfrow = c(2,2), xpd = TRUE, mar = c(3.5,4,4,2), cex = 1.3) 
plot(x = AB_dfs[[1]]$ratio, y = AB_dfs[[1]]$alphaA, xlab = "", ylab = "alpha A *",
     type = "l", ylim = c(0, 1))
mtext(text = "ratio muA/muB", side = 1, line = 1.8, at = 0.5, cex = 1.3)
plot(x = AB_dfs[[1]]$ratio, y = AB_dfs[[1]]$alphaB, xlab = "", ylab = "alpha B *",
     type = "l", ylim = c(0, 1))
mtext(text = "ratio muA/muB", side = 1, line = 1.8, at = 0.5, cex = 1.3)
mtext(text = paste0("fixed encounter rates and cost ratio\nspA - E=", round(Ea, 2), ", spB - E=", round(Eb,2),"; cost ratio = ", C2), side = 3, line = 1, at = -0.25, cex = 1.3)
plot(x = AB_dfs[[2]]$C, y = AB_dfs[[2]]$alphaA, xlab = "", ylab = "alpha A *",
     ylim = c(0, 1),
     type = "l")
mtext(text = "cost ratio cl/cm", side = 1, line = 1.8, at = C2, cex = 1.3)
plot(x = AB_dfs[[2]]$C, y = AB_dfs[[2]]$alphaB, xlab = "", ylab = "alpha B *",
     ylim = c(0, 1),
     type = "l")
abline(h = alphAB[[2]], col = "red", xpd = FALSE)
mtext(text = "cost ratio cl/cm", side = 1, line = 1.8, at = C2, cex = 1.3)
mtext(text = paste0("fixed encounter rates and ratio of detectabilities\nspA - E=", round(Ea, 2), ", spB - E=", round(Eb,2), "; muA/muB = ", round(muA/muB, 2)), side = 3, line = 1, at = -0.03, cex = 1.3)
#dev.off()

#page 2: varying encounter rates of sp A and B
#png("graphs/bivar_alphaAB_E.png", width = 700, height = 700)
par(mfrow = c(2,2), xpd = TRUE, mar = c(3.5,4,4,2), cex = 1.2) 
plot(x = AB_dfs[[3]]$EA, y = AB_dfs[[3]]$alphaA, type = "l", xlab = "", ylab = "alpha A *", ylim = c(0, 1))
mtext(text = "encounter rate species A (less detectable)", side = 1, line = 2.1, at = 0.05, cex = 1.2)
plot(x = AB_dfs[[3]]$EA, y = AB_dfs[[3]]$alphaB, type = "l", xlab = "", ylab = "alpha B *", ylim = c(0, 1))
mtext(text = "encounter rate species A (less detectable)", side = 1, line = 2.1, at = 0.05, cex = 1.2)
mtext(text = paste0("fixed encounter rate spB, cost ratio and ratio of detectabilities\nspB - E=", round(Ea,2), "; muA/muB = ", round(muA/muB, 2), "; C = ", C2), side = 3, line = 1, at = -0.025, cex = 1.2)
plot(x = AB_dfs[[4]]$EB, y = AB_dfs[[4]]$alphaA, type = "l", xlab = "", ylab = "alpha A *", ylim = c(0, 1))
mtext(text = "encounter rate species B (more detectable)", side = 1, line = 2.1, at = 0.6, cex = 1.2)
plot(x = AB_dfs[[4]]$EB, y = AB_dfs[[4]]$alphaB, type = "l", xlab = "", ylab = "alpha B *", ylim = c(0, 1))
mtext(text = "encounter rate species B (more detectable)", side = 1, line = 2.1, at = 0.6, cex = 1.2)
mtext(text = paste0("fixed encounter rate spA, cost ratio and ratio of detectabilities\nspA - E=", round(Eb,2), "; muA/muB = ", round(muA/muB, 2), "; C = ", C2), side = 3, line = 1, at = 0.03, cex = 1.2)
#dev.off()