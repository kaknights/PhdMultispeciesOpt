# Check that the expressions for the optimal proportion are actually the minima re the objective function.
source("scripts/multiSpOptFuncs.R")

objFunc_opt_AB <- function(Da = Da, Db = Db, Ea = Ea, Eb = Eb, 
                           Aa = Aa, Ab = Ab, B = B, Cl = Cl, Cm = Cm){
varD_AB <- Da^2* ((Cl+(Ea*Aa*Cm)+(Eb*Ab*Cm))/ (B*Ea) + 
                    ((Cl+(Ea*Aa*Cm)+(Eb*Ab*Cm))/(2*B*Ea*Aa)))  +
  Db^2* ((Cl+(Ea*Aa*Cm)+(Eb*Ab*Cm))/(B*Eb) + 
           ((Cl+(Ea*Aa*Cm)+(Eb*Ab*Cm))/(2*B*Eb*Ab)))  
return(varD_AB)
}

# species A (less detectable)
Da <- 0.07
sigm_a <- 1.5
Ea <- 2*(sqrt(pi*(sigm_a^2)/2))*Da

# species B (more detectable)
Db <- 0.1
sigm_b <- 5
Eb <- 2*(sqrt(pi*(sigm_b^2)/2))*Db

#calcs for the opt alpha B function
muA <- sqrt(pi*(sigm_a^2)/2)
muB <- sqrt(pi*(sigm_b^2)/2)

# assuming the opt A/B equations are correct, and the thresholds are calculated in the right place:
#where alpha A = 1 to where both alphas = 1:
(upperT_C(EA = Ea, EB = Eb, muA = muA, muB = muB)) 

(lowerT_C(EA = Ea, EB = Eb, muA = muA, muB = muB)) 

# survey characteristics

B <- 1000
C <- 3.2
Cl <- 0.2
Cm <- Cl/C

# optimal proportions

Aa <- 1
#objective function for two species solution

Ab <- seq(0.1, 0.999, length.out = 100)


myDf_alphB <- data.frame("alphB" = Ab, "varD_AB" = objFunc_opt_AB(Da = Da, Db = Db, Ea = Ea, Eb = Eb, 
                           Aa = Aa, Ab = Ab, B = B, Cl = Cl, Cm = Cm))

(optB <- opt_alpha_B(Ea = Ea, Eb = Eb, muA = muA, muB = muB, C = C))

plot(x = myDf_alphB$alphB, y = myDf_alphB$varD_AB, xlab = "alpha B", ylab = "varD (spA) + varD (spB)", type = "l")
abline(v = optB, col = "red")

#It seems that the alpha B equation is not correct - possible bracket issue in the function!!!!

#do this again for alpha A and B - it seems something still isn't quite right... the function according to ratio muA/muB is not continuous (whereas it was before fixing the original problem)

#Use a value of C under the lower threshold:
C2 <- 0.5
Cm2 <- Cl/C2

#Choose a value for alpha B to plot with
(optAlphA <- new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = Eb, C = C2)[[1]])
optAlphB <- new_optAB(muA = muA, muB = muB, Ea = Ea, Eb = Eb, C = C2)[[2]]

opt_alpha_AB(muA = muA, muB = muB, Da = Da, Db = Db, C = C2)


Aa2 <- seq(0.1, 0.999, length.out = 100)

optAB_df1 <- data.frame("Aa" = Aa2, "varD_AB" = 
                       objFunc_opt_AB(Da = Da, Db = Db, Ea = Ea, Eb = Eb, Aa = Aa2, 
                                      Ab = optAlphB, B = B, Cl = Cl, Cm = Cm2))
plot(x = optAB_df1$Aa, y = optAB_df1$varD_AB, xlab = "alpha A", ylab = "varD (spA) + varD (spB)", type = "l")
abline(v = optAlphA, col = "red")

optAB_df2 <- data.frame("Ab" = Ab, 
                        "varD_AB" = objFunc_opt_AB(Da = Da, Db = Db, Ea = Ea, Eb = Eb, Aa = optAlphA, 
                                      Ab = Ab, B = B, Cl = Cl, Cm = Cm2))

plot(x = optAB_df2$Ab, y = optAB_df2$varD_AB, xlab = "alpha B", ylab = "varD (spA) + varD (spB)", type = "l")
abline(v = optAlphB, col = "red")

#Minima confirmed: the expressions for the single optimal alpha B and the two optimal alphas A and B both find the minimum point on the variance function.