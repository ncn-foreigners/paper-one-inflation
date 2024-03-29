library(maxLik)
library(VGAM)
source(file = "functions.r")

# Set the data generating process:
set.seed(1)
n <- 100
li <- 1
omega <- 0.3
phi <- log(omega / (1 - omega))

# The true unobservable sample size:
Nreal <- n / (1 - exp(-li))

# Set the number of Monte Carlo repititions
nrep <- 10000

# Assign empty vectors to store the MLEs
lhatpps <- lhatoipps <- whatoipps <- rep(0,nrep)
ahatztnb <- ahatoinb <- rep(0, nrep)
Npp <- Noipp <- Nztnb <- Noiztnb <- rep(0, nrep)
AICoiztnb <- AICztnb <- AICoipp <- AICpp <- rep(0,nrep)

# Start MC loop
jj <- 1
while(jj <= nrep){
  y <- oippv(li, omega)
  
  # Estimate OIZTNB model, and record the Horvitz-Thompson estimate
  res1 <- maxLik(lloiztnb_nocov, method="nm", start=c(li, 1, phi), 
                 iterlim=10000)$estimate
  lhatoinb <- res1[1]
  ahatoinb[jj] <- res1[2]
  Noiztnb[jj] <- n / (1 - (1 + lhatoinb / ahatoinb[jj]) ^ -ahatoinb[jj])
  
  # Estimate ZTNB model, and record the Horvitz-Thompson estimate
  res2 <- maxLik(llztnb_nocov, method="nm", start=c(li, 1), iterlim=10000)$estimate
  lhatztnb <- res2[1]
  ahatztnb[jj] <- res2[2]
  Nztnb[jj] <- n / (1 - (1  + lhatztnb / ahatztnb[jj]) ^ -ahatztnb[jj])
  
  # Estimate OIPP model, and record the Horvitz-Thompson estimate
  res3 <- maxLik(lloipp_nocov, method="nm", start=c(li, omega), iterlim=10000)$estimate
  lhatoipp <- res3[1]
  Noipp[jj] <- (n / (1 - exp(-lhatoipp)))
  
  lhatoipps[jj] <- lhatoipp
  whatoipps[jj] <- res3[2]
  
  # Estimate PP model, and record the Horvitz-Thompson estimate
  res4 <- maxLik(llpp_nocov, method="nm", start=li, iterlim=10000)$estimate
  lhatpp <- res4[1]
  Npp[jj] <- (n / (1 - exp(-lhatpp)))
  
  lhatpps[jj] <- lhatpp
  
  # Akaike information criterion
  AICoiztnb[jj] <- 6 - 2 * lloiztnb_nocov(res1)
  AICztnb[jj] <- 4 - 2 * llztnb_nocov(res2)
  AICoipp[jj] <- 4 - 2 * lloipp_nocov(res3)
  AICpp[jj] <- 2 - 2 * llpp_nocov(res4)
  
    # Iterate the MC loop forward
  jj <- jj + 1
}

# The following produces the 1st row of Table 3 in the main paper
# Percent boundary solutions:
sum(ahatztnb < 0.05) / nrep
sum(ahatoinb < 0.05) / nrep

# Bias of the H-T estimators, including boundary solutions
bias(Nreal, Npp)
bias(Nreal, Noipp)
bias(Nreal, Nztnb)
bias(Nreal, Noiztnb)

# Bias of the H-T estimators, excluding boundary solutions
NztnbD <- Nztnb[ahatztnb > 0.05]
bias(Nreal, NztnbD)
NoiztnbD <- Noiztnb[ahatoinb > 0.05]
bias(Nreal, NoiztnbD)

# portion of smaller AIC than OIPP
sum(AICpp < AICoipp) / nrep
sum(AICztnb < AICoipp) / nrep
sum(AICoiztnb < AICoipp) / nrep

# portion of smaller AIC than OIPP, excluding boundary cases
sum(AICztnb[ahatztnb > 0.05] < AICoipp[ahatztnb > 0.05]) / length(NztnbD)
sum(AICoiztnb[ahatoinb > 0.05] < AICoipp[ahatoinb > 0.05]) / length(NoiztnbD)