#functions for the expressions in the multiple species solutions;

# FUNCTIONS ----

## Optimal proportions ----

### Alphas A and B ----
#alphas A and B when there are two optimal alphas:

opt_alpha_AB <- function(muA, muB, Da, Db, C){
  
alphA <- sqrt(C*((muB/muA)/(muA*Db + muB*Da)))/2
alphB <- sqrt(C*((muA/muB)/(muB*Da + muA*Db)))/2

myStuff <- list(alphA, alphB)

return(myStuff)

}

#new rearranged expressions using symbolab:
new_optAB <- function(muA, muB, Ea, Eb, C){
  alphaA <- sqrt(C*(2*muB^2/(muA^2*Eb+muB^2*Ea)))/2
  alphaB <- sqrt(C*(2*muA^2/(muB^2*Ea+muA^2*Eb)))/2
  
  things <- list(alphaA, alphaB)
  
  return(things)
}

#expressed using three variables instead of four:
v3_alphaAB <- function(muRat, Erat, Cr){
  alphaA <- sqrt(Cr*((2*muRat^2)/(Erat+muRat^2)))/2
  alphaB <- sqrt(Cr*((2*(1/muRat^2)/(1+(1/muRat^2*Erat)))))/2
  alt_alphaB <- sqrt(Cr*(2/(muRat^2+Erat)))/2
  
  myThings <- c("alphA" = round(alphaA, 3), 
                "alphB" = round(alphaB, 3), 
                "alt_alphB" = round(alt_alphaB, 3))
  
  return(myThings)
}

#using Rc instead of Cr:
optAB_Rc <- function(Rc, muRat, Erat){
  alphaA <- muRat*(sqrt(2/(Rc*(Erat+muRat^2)))/2)
  alphaB <- sqrt(2/(Rc*(Erat+muRat^2)))/2
  
  myAlphas <- list("alphaA" = round(alphaA, 3),
                "alphaB" = round(alphaB, 3))
}


### Alpha B ----

#alpha B when alpha A = 1:
opt_alpha_B <- function(Ea, Eb, muA, muB, C){
  alphaB <- sqrt((C+Ea)/(2*Eb+(3*Ea*(muB/muA)^2)))
  return(alphaB)

}


# alternative using three variables instead of four:
altOpt_alpha_B <- function(muRat, Erat, Cr){
  alphaB <- sqrt((Cr+1)/((2*Erat) + (3*muRat^2)))
  return(alphaB)
}

# using three variables and Rc instead of Cr
optB_Rc <- function(Rc, muRat, Erat){
  alphaB <- sqrt((1/Rc +1)/((2*Erat)+(3*muRat^2)))
  return(alphaB)
}

### Single sp alpha ----

#opt Alpha (single species):
optA <- function(Cm, Cw){
  opt <- sqrt(Cw/(2*Cm))
  return(opt)
}


## Thresholds ----

# value of C where optimal goes from single optimal <1 to both = 1
alphBToLTS_C <- function(EA, EB, muA, muB){
  myThreshold <- 3*(muB^2*EA/muA^2)+(2*EB)-EA
  return(myThreshold)
}

# value of C where optimal goes from two optimal props to 1
oneToTwoAlpha_C <- function(EA, EB, muA, muB){
  myThreshold <- 2*(muA^2*EB/muB^2)+(2*EA)
  return(myThreshold)
}


# Thresholds using three variables instead of four:

# Going from LTS to alpha B<1 (alpha A = 1)
alphBToLTS_Rc <- function(Erat, muRat){
  myThresh <- 1/((2*Erat)+(3*muRat^2)-1)
}
# rearranged for muRat - not tested
# alphBToLTS_muRat_sq <- function(Rc, Erat){
#   muRat_sq <- ((1/Rc+1)/(2*Erat)-1)/3
#   return(muRat_sq)
# }


# from above to both alphas <1
oneToTwoAlpha_Rc <- function(Erat, muRat){
  myThresh <- muRat^2/(2*(Erat+muRat^2))
}

test3 <- oneToTwoAlpha_Rc(Erat = 50, muRat = 1)

# rearranged for Erat (using symbolab)
oneToTwoAlpha_Erat <- function(Rc, muRat){
  myThreshold <- (muRat^2 - (2*muRat^2*Rc))/(2*Rc)
}

test4 <- oneToTwoAlpha_Erat(Rc = Rc, muRat = muRat)

### Find upper threshold ----

#this will be the highest possible C threshold (where detectabilities are equal)
T1_test_equalmu <- function(Ea,Eb,C){
  value <- 2*Eb +(2*Ea)
  result <- ifelse(C>= (2*Ea +(2*Eb)), "LTS", "opt")
  
  myList <- list(result, value)
  return(myList)
}

#this will be the highest possible C threshold (where detectabilities are not equal)
T1_test_diffmu <- function(Ea, Eb, C, muRat){
  value <- (2*muRat*Eb)+(2*Ea)
  result <- ifelse(C>= value, "LTS", "opt")
  myList <- list(result, value)
  return(myList)
}

### Benefits ----

predBen <- function(Rc){
  3/2*((1+Rc)/(1+sqrt(Rc/2))^2)
}
