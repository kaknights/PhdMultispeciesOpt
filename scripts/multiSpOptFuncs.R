#functions for the expressions in the multiple species solutions;

# FUNCTIONS ----

## Optimal proportions ----

### Alphas A and B ----
#alphas A and B when there are two optimal alphas:

# opt_alpha_AB <- function(muA, muB, Da, Db, C){
#   
# alphA <- sqrt(C*((muB/muA)/(muA*Db + muB*Da)))/2
# alphB <- sqrt(C*((muA/muB)/(muB*Da + muA*Db)))/2
# 
# myStuff <- list(alphA, alphB)
# 
# return(myStuff)
# 
# }

# #new rearranged expressions using symbolab:
# new_optAB <- function(muA, muB, Ea, Eb, C){
#   alphaA <- sqrt(C*(2*muB^2/(muA^2*Eb+muB^2*Ea)))/2
#   alphaB <- sqrt(C*(2*muA^2/(muB^2*Ea+muA^2*Eb)))/2
#   
#   things <- list(alphaA, alphaB)
#   
#   return(things)
# }
# 
# #expressed using three variables instead of four:
# v3_alphaAB <- function(muRat, Erat, Cr){
#   alphaA <- sqrt(Cr*((2*muRat^2)/(Erat+muRat^2)))/2
#   alphaB <- sqrt(Cr*((2*(1/muRat^2)/(1+(1/muRat^2*Erat)))))/2
#   alt_alphaB <- sqrt(Cr*(2/(muRat^2+Erat)))/2
#   
#   myThings <- c("alphA" = round(alphaA, 3), 
#                 "alphB" = round(alphaB, 3), 
#                 "alt_alphB" = round(alt_alphaB, 3))
#   
#   return(myThings)
# }

#using Rc instead of Cr:
optAB_Rc <- function(Rc, muRat, Erat){
  alphaA <- muRat*(sqrt(2/(Rc*(Erat+muRat^2)))/2)
  alphaB <- sqrt(2/(Rc*(Erat+muRat^2)))/2
  
  myAlphas <- list("alphaA" = alphaA,
                "alphaB" = alphaB)
  return(myAlphas)
}

### Alpha B ----

#alpha B when alpha A = 1:
# opt_alpha_B <- function(Ea, Eb, muA, muB, C){
#   alphaB <- sqrt((C+Ea)/(2*Eb+(3*Ea*(muB/muA)^2)))
#   return(alphaB)
# 
# }
# 
# # alternative using three variables instead of four:
# altOpt_alpha_B <- function(muRat, Erat, Cr){
#   alphaB <- sqrt((Cr+1)/((2*Erat) + (3*muRat^2)))
#   return(alphaB)
# }

# using three variables and Rc instead of Cr
optB_Rc <- function(Rc, muRat, Erat){
  alphaB <- sqrt((1/Rc +1)/((2*Erat)+(3*muRat^2)))
  return(alphaB)
}

### Alpha A ----

#when alpha B=1 and alpha A <1

# optA_Rc <- function(Rc, muRat, Erat){
#   alphaA <- sqrt(((1/(Rc*Erat))+1)/((3/muRat^2)+(2/Erat)))
#   return(alphaA)
# }


### Single sp alpha ----

#opt Alpha (single species):
optA <- function(Cm, Cw){
  opt <- sqrt(Cw/(2*Cm))
  return(opt)
}

## Thresholds ----

### LTS to one opt. prop. ----
# value of C where optimal goes from single optimal <1 to both = 1
# alphBToLTS_C <- function(EA, EB, muA, muB){
#   myThreshold <- 3*(muB^2*EA/muA^2)+(2*EB)-EA
#   return(myThreshold)
# }
# 
# # value of C where optimal goes from two optimal props to 1
# oneToTwoAlpha_C <- function(EA, EB, muA, muB){
#   myThreshold <- 2*(muA^2*EB/muB^2)+(2*EA)
#   return(myThreshold)
# }

# Thresholds using three variables instead of four:

# Going from LTS to alpha B<1 (alpha A = 1) when muA<muB
alphBToLTS_Rc <- function(Erat, muRat){
  myThresh <- 1/((2*Erat)+(3*muRat^2)-1)
  return(myThresh)
}

alphBtoLTS_Erat <- function(Rc, muRat){
  myThreshold <- ((1/Rc)-(3*muRat^2)+1)/2
  return(myThreshold)
}

alphBtoLTS_muRatSq <- function(Rc, Erat){
  threshold <- ((1/Rc)-(2*Erat)+1)/3
  return(threshold)
}

# same thresholds when muA>muB

# alphAToLTS_Rc <- function(Erat, muRat){
#   threshold <- 1/(((3*Erat)/(muRat^2))+2-Erat)
#   return(threshold)
# }
# 
# alphAToLTS_muRatSq <- function(Rc, Erat){
#   Threshold <- 3/((1/(Rc*Erat))-(2/Erat)+1)
#   return(Threshold)
# }
# 
# alphAToLTS_Erat <- function(Rc, muRat){
#   
# }

### one to both alphas <1 ----

# if muA<muB:

# Rc is the subject
oneToTwoAlpha_Rc <- function(Erat, muRat){
  myThresh <- muRat^2/(2*(Erat+muRat^2))
  return(myThresh)
}

# rearranged for Erat (using symbolab)
oneToTwoAlpha_Erat <- function(Rc, muRat){
  myThreshold <- (muRat^2 - (2*muRat^2*Rc))/(2*Rc)
  return(myThreshold)
}

# rearranged for muRat^2
oneToTwoAlpha_muRatSq <- function(Rc, Erat){
  myThreshold <- (4*Rc*Erat)/(2-(4*Rc))
  return(myThreshold)
}

# if muA>muB:

# Rc is the subject
# oneToTwoAlpha_Rc_muA <- function(Erat, muRat){
#   myThresh <- 1/(2*(Erat+muRat^2))
#   return(myThresh)
# }
# 
# # rearranged for Erat (using symbolab)
# oneToTwoAlpha_Erat_muA <- function(Rc, muRat){
#   myThreshold <- (1/(2*Rc)) - muRat^2
#   return(myThreshold)
# }
# 
# # rearranged for muRat^2
# oneToTwoAlpha_muRatSq_muA <- function(Rc, Erat){
#   myThreshold <- (1/(2*Rc)) - Erat
#   return(myThreshold)
# }

### Find upper threshold ----

#this will be the highest possible C threshold (where detectabilities are equal)
# T1_test_equalmu <- function(Ea,Eb,C){
#   value <- 2*Eb +(2*Ea)
#   result <- ifelse(C>= (2*Ea +(2*Eb)), "LTS", "opt")
#   
#   myList <- list(result, value)
#   return(myList)
# }
# 
# #this will be the highest possible C threshold (where detectabilities are not equal)
# T1_test_diffmu <- function(Ea, Eb, C, muRat){
#   value <- (2*muRat*Eb)+(2*Ea)
#   result <- ifelse(C>= value, "LTS", "opt")
#   myList <- list(result, value)
#   return(myList)
# }

## Benefits ----

### benefit single sp ----
predBen <- function(Rc){
  3/2*((1+Rc)/(1+sqrt(Rc/2))^2)
}

### benefit multisp ----

b2 <- function(Rc, ER, muR){
  
  ben <- (3 *(1 + Rc + ER *Rc) *(ER + muR^2)* sqrt(Rc* (1 + Rc)* ((2 *ER) + (3 *muR^2))))/
    ((ER* Rc *sqrt(1 + Rc) + (1 + Rc) *sqrt(Rc *((2* ER) + (3* muR^2))))* (3 *sqrt(1 + Rc)* muR^2 + 
   ER *(2 *sqrt(1 + Rc) + sqrt(Rc *((2* ER) + (3 *muR^2))))))
  return(ben)
}

b3 <- function(Rc, ER, muR){

  ben <- (6 *Rc *(1 + Rc + ER *Rc)* (ER + muR^2))/(sqrt(2)* Rc *(ER + muR) + 
  (2 *sqrt(Rc* (ER + muR^2))))^2
  return(ben)
}


## Survey Planner ----

# takes a dataframe (must be specific format: colnames and data types), checks which strategy applies, calculates the optimal proportions and returns a df with the key variable values (Rc, mu ratio, E ratio) and optimal proportions (if relevant)

surveyPlan <- function(data){
  if (data$mu[1]==data$mu[2]){
    speciesA <- data$ID[data$d==min(data$d)]
    speciesB <- data$ID[data$ID != speciesA]
  } else {
    speciesA <- data$ID[data$mu == min(data$mu)]
  speciesB <- data$ID[data$ID != speciesA]
  }
  
  muRat <- data$mu[data$ID==speciesB]/data$mu[data$ID==speciesA]
  Erat <- data$E[data$ID==speciesB]/data$E[data$ID==speciesA]
  Rc <- mean(data$cm)/data$cw[data$ID==speciesA]
  keyVars <- c(Rc, muRat, Erat)
  
  #is two props <1 optimal
  if (keyVars[1]> oneToTwoAlpha_Rc(Erat = keyVars[3], muRat = keyVars[2])) {
    
    print("alpha A<1 and alpha B<1 is optimal")
    
    #what are the optimal proportions?
    alphaA <- optAB_Rc(Rc = keyVars[1], muRat = keyVars[2], Erat = keyVars[3])[[1]]
    alphaB <- optAB_Rc(Rc = keyVars[1], muRat = keyVars[2], Erat = keyVars[3])[[2]]
    
    print(paste0("alpha A = ", alphaA, "; alpha B = ", alphaB))
    
    
    keyVars <- data.frame("Rc" = keyVars[1], "muRat" = keyVars[2], "Erat" = keyVars[3],
                          "alphaA" = alphaA, "alphaB" = alphaB, "a1_A" = data$a1[data$ID == speciesA], "a1_B" = data$a1[data$ID== speciesB])
    
  } else {
    #is alpha B<1 optimal
    if (keyVars[1]>alphBToLTS_Rc(Erat = keyVars[3], muRat = keyVars[2])) {
      print("alpha A = 1, alpha B<1 is optimal")
      
      #what is alpha B*?
      alphaB <- optB_Rc(Rc = keyVars[1], muRat = keyVars[2], Erat = keyVars[3])
      print(paste0("alpha B = ", alphaB))
      
      keyVars <- data.frame("Rc" = keyVars[1], "muRat" = keyVars[2], "Erat" = keyVars[3],
                          "alphaB" = alphaB)
      
    } else{
      #LTS is optimal
      print("LTS is optimal")
      keyVars <- data.frame("Rc" = keyVars[1], "muRat" = keyVars[2], "Erat" = keyVars[3])
    }
  }
  return(keyVars)
}
