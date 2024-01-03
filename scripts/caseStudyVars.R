# %Notes from Decky:
# %\begin{itemize}
# %    \item CI : The minimum value was 5 seconds and the maximum value was roughly 90 seconds. My best guess for the average (CI) is 20 seconds 
#  %   \item Cm : The shortest time I remembered for this measurement activity was 10 seconds and the longest time needed was 150 seconds. The estimate for the average value (Cm) is 40 seconds
# %    \item Cw : The estimated smallest value is 30 seconds, the estimated largest value is 80 seconds. The estimated average value (Cw) is 45 seconds
# %\end{itemize}

#requires 
source("scripts/multiSpOptFuncs.R")

# Data ----
decky <- read.csv("dataRaw/distance_Kath_2018_DataDecky.csv")
# Only at a single site: CBG
cbgInfo <- decky[decky$Location=="CBG", ]

#calculate encounter rates for both spp
# 3 transects totaling 1350m (thesis p 68)
EB <- nrow(cbgInfo[cbgInfo$spab=="CEAU",])/1350
#0.296
EA <- nrow(cbgInfo[cbgInfo$spab=="CEEL",])/1350
#0.011

# vars ----
cl <- 0.33 #mins per m
cm <- 0.66 #mins per detection
cw_est <- 0.75 #mins per detection
cw_calc <- cl/EA
cw_EB <- cl/EB

#using these encounter rates and cw gives Cl = 0.23 
#cl_1 <- cw * (E_a + E_p)
#C_2 <- cl_1/cm

#no actual numbers for sigma or density - will have to calculate 
sigm <- 2.5 #eyeballing the graph on thesis p 134
mu <- sqrt(pi*(sigm^2)/2)  

caseStudyVals <- data.frame("species" = c("C_purpureum", "C_auranticum"), 
                            "Erates" = c(EA, EB), 
                            "sigm" = c(sigm, sigm),
                            "mu" = c(mu, mu),
                            "cl" = cl,
                            "cm" = cm,
                            "cw" = c(cw_calc, NA),
                            "alt_cw" = cw_est,
                            "cw_EB" = c(NA, cw_EB))


# w <- max(cbgInfo$distance)#use max recorded distance
# p <- mu/w
# area <- 2*w*1350
# w_pu <- max(cbgInfo$distance[cbgInfo$spab=="CEEL"])
# w_au <- max(cbgInfo$distance[cbgInfo$spab=="CEAU"])
# 
# D_au <- nrow(cbgInfo[cbgInfo$spab=="CEAU", ])/p/area
# D_pu <- nrow(cbgInfo[cbgInfo$spab=="CEEL", ])/p/area

# Thresholds ----

# Which strategy is optimal?

twoTo1alpha_Cestrum <- oneToTwoAlpha_Rc(Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muRat = 1)
optAB_Cestrum <- optAB_Rc(Rc = (caseStudyVals$cm[1]/caseStudyVals$cw[1]), Erat = caseStudyVals$Erates[2]/caseStudyVals$Erates[1], muRat = 1)
singleSpOptA_CestrumP <- optA(Cm = caseStudyVals$cm[1], Cw = caseStudyVals$cw[1])
singleSpOptA_CestrumA <- optA(Cm = caseStudyVals$cm[2], Cw = caseStudyVals$cw_EB[2])
singleSpOptA_CestCombined <- optA(Cm = caseStudyVals$cm[2], Cw = cl/(caseStudyVals$Erates[1]+caseStudyVals$Erates[2]))

rm(decky, cbgInfo, EB, EA, cl, cm, cw_calc, cw_est, sigm, mu, cw_EB)