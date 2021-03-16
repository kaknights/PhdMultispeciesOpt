##Distance sampling optimisation functions

#function to calculate the optimal proportion to measure 
#when surveying 2 species, species A and species B
 #arguments are:
# Cl = cost per unit length of transect (walking, not measuring)
# Cm = cost to measure one detected target's distance
# sigmA = sigma for species A (in same units as w, strip half width)
# sigmB = sigma for species B
# Da = density of species A
# Db = density of species B

optAoptB <- function(Cl, Cm, sigmA, sigmB, Da, Db){
 
  muA <- sqrt(pi*(sigmA^2)/2)
  muB <- sqrt(pi*(sigmB^2)/2)

  alphA <- sqrt(Cl/Cm*((2*muB)/(2*muA)/((2*muA*Db)+(2*muB*Da))))/sqrt(2)
  alphB <- sqrt(Cl/Cm*((2*muA)/(2*muB)/((2*muA*Db)+(2*muB*Da))))/sqrt(2)
  myOptProps <- data.frame(alphA, alphB)
  return(myOptProps)
}

#myTest <- optAoptB(Cl = 0.05, Cm = 0.5, sigmA = 10, sigmB = 8, 
#Da = 0.5, Db = 0.1)
