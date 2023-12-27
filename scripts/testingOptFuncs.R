# testing whether all the various version of the equations for optimal proportions and thresholds give the same answer

# Optimal proportions

# Dummy data:

# costs: need Cw (cost walking per detection), Cm (cost measuring per detection), Cl (cost walking per unit length), from which the various ratios can be calculated.

# population pars: density, sigma, all others (mu and encounter rate) can be calculated from these.

dA <- 0.005 # individuals per square m
dB <- 0.1
sigmA <- 1.5
sigmB <- 3.5

muA <- sqrt(pi*(sigmA^2)/2)
muB <- sqrt(pi*(sigmB^2)/2)

EA <- 2*muA*dA
EB <- 2*muB*dB

muRat <- muB/muA
Erat <- EB/EA

Cw <- 0.5
Cm <- 1

Rc <- Cm/Cw

Cl <- Cw*EA

C <- Cl/Cm

Cr <- Cw/Cm

## Testing threshold calculations

test_thresh_upperC <- alphBToLTS_C(EA = EA, EB = EB, muA = muA, muB = muB)
test_thresh_upperRc <- alphBToLTS_Rc(Erat = Erat, muRat = muRat)
test3 <- alphBToLTS_muRat_sq(Rc = Rc, Erat = Erat)

# if C = Cl/Cm then C/Ea = Cr = 1/Rc
step1 <- EA/test_thresh_upperC

test_lowC <- oneToTwoAlpha_C(EA = EA, EB = EB, muA = muA, muB = muB)
test_lowRc <- oneToTwoAlpha_Rc(Erat = Erat, muRat = muRat)

step2 <- EA/test_lowC

#trying rearrangements for change from one to two opt proportions for when muA>muB

test1_muAgreater <- oneToTwoAlpha_Rc_muA(Erat, muRat)
test2_muAg <- oneToTwoAlpha_Erat_muA(Rc = test1_muAgreater, muRat)
test3_muAg <- sqrt(oneToTwoAlpha_muRatSq_muA(Rc = test1_muAgreater, Erat = test2_muAg))

## Testing opt calcs

# alphas A and B
(orig <- opt_alpha_AB(muA = muA, muB = muB, Da = dA, Db = dB, C = C))
(versionC <- new_optAB(muA = muA, muB = muB, Ea = EA, Eb = EB, C = C))
(v3_Cr <- v3_alphaAB(muRat = muRat, Erat = Erat, Cr = Cr))
(v4_Rc <- optAB_Rc(Rc = Rc, muRat = muRat, Erat = Erat))

# these ar all giving the same result (even with two different values of dB)

# alpha B

(orig <- opt_alpha_B(Ea = EA, Eb = EB, muA = muA, muB = muB, C = C))
(v2Cr <- altOpt_alpha_B(muRat = muRat, Erat = Erat, Cr = Cr))
(v3Rc <- optB_Rc(Rc = Rc, muRat = muRat, Erat = Erat))

#these are also all giving the same result