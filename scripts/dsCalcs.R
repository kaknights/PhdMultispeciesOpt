# General distance calculations for reference
Atot <- 250000 #survey area, in m^2 - distinct from covered area (below)
B <- 10000; #budget, in generic units (time to travel 1 unit length of transect)

D <- 0.5 #population density, indivs per m^2
Cm <- 0.5 #cost to measure distance to one detected target, in mins
Cl <- 0.1 #cost per unit length of transect (walking, not measuring), in mins 
#Cl <- E*Cw
sigm <- 1:10 #parameter for half-normal det func
w <- 2*sigm #strip half-width, user defined, can be set in m
mu <- sqrt(pi*(sigm^2)/2)
E <-  2*mu*D #encounter rate in indivs per m
optA <-  ifelse(sqrt(Cw/(2*E*Cm))< 1, sqrt(Cw/(2*E*Cm)), 1) #optimal alpha
optL <-  B/(Cw + E*optA*Cm) #corresponding optimal length of transect
optArea <- optL*2*w #covered area under optimisation
fullL <- B/(Cw + (E*Cm)) #length of transect under conventional ds (LTS)
fullA  <-  fullL*2*w #covered area under LTS
Cw <- Cl/E #cost of walking per detection
Rc <- Cm/Cw #cost ratio

#-----------------------------------------------------------------------------
#running example scenario to test preliminary multispecies opt calculations

Da <- 0.5; Db <- 0.1 #make into a matrix? or dataframe?
#if detectabilities are the same, sigm should be the same (keep it simple)
sigm <- 10
w <- 25 #so the sample strip is 50m wide
mu <- sqrt(pi*(sigm^2)/2)
Ea <- 2*mu*Da
Eb <- 2*mu*Db
#costs - Cl is length related, Cm we consider the same for both species
Cm <- 0.5
Cl <- 0.05

#exact (I think) recreation of the expression from mathematica
#not super sure if the Da outside the radical is a multiplier or root
alphaA <- (Da*sqrt((Cl*Eb)/((Cm*Db^2*Ea^2)+(Cm*Da^2*Ea*Eb))))/sqrt(2)

#collective intermediate version
alphaAmod <- sqrt((Cl/Cm)*((Eb/Ea)/((Db^2/Da^2)*Ea + Eb)))/sqrt(2)
#my attempt at modification
alphaAmodme <- sqrt((Cl/Cm)*((Eb/Ea)/((Db^2*Ea/Da^2) + Eb)))/sqrt(2)

####
# Currently (25.02.2021) all give same answer!!!!

alphaAsimp <- sqrt((Cl/Cm)*())
