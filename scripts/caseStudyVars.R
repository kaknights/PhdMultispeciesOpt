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

Erat <- EB/EA

# vars ----
cl <- 0.33 #mins per m
cm <- 0.66 #mins per detection
cw_est <- 0.75 #mins per detection
cw_calc <- cl/(EA)
C <- cl/cm
Rc_calc <- cw_calc/cm

#using these encounter rates and cw gives Cl = 0.23 
#cl_1 <- cw * (E_a + E_p)
#C_2 <- cl_1/cm

#no actual numbers for sigma or density - will have to calculate 
sigm <- 2.5 #eyeballing the graph on thesis p 134
mu <- sqrt(pi*(sigm^2)/2)  

muRat <- 1

# w <- max(cbgInfo$distance)#use max recorded distance
# p <- mu/w
# area <- 2*w*1350
# w_pu <- max(cbgInfo$distance[cbgInfo$spab=="CEEL"])
# w_au <- max(cbgInfo$distance[cbgInfo$spab=="CEAU"])
# 
# D_au <- nrow(cbgInfo[cbgInfo$spab=="CEAU", ])/p/area
# D_pu <- nrow(cbgInfo[cbgInfo$spab=="CEEL", ])/p/area

