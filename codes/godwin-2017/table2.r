# Reproduces the first row of Table 2 in the main paper. Other rows may be 
# reproduced by assigning different values for "n".

library(maxLik)
source(file = "functions.r")

# Set the data generating process:
set.seed(1)
n <- 100
omega <- 0.2
a <- .5
li <- 1
phi <- log(omega / (1 - omega))

# The true unobservable sample size is then:
Nreal <- n / (1 - (1 + li / a) ^ -a)

# Set the number of Monte Carlo repititions
nrep <- 10000

# Assign empty vectors to store the MLEs
ahatztnb <- ahatoinb <- rep(0, nrep)
Npp <- Noipp <- Nztnb <- Noiztnb <- rep(0, nrep)
lhatpps <- lhatoipps <- whatoipps <-rep(0,nrep)

# Start the Monte Carlo loop
jj <- 1
while(jj <= nrep) {
  y <- oiztnbv(li, a, omega)
  
  # Estimate OIZTNB model, and record the Horvitz-Thompson estimate
  res1 <- maxLik(lloiztnb_nocov, method="nm", start=c(li, a, phi), 
                 iterlim=10000)$estimate
  lhatoinb <- res1[1]
  ahatoinb[jj] <- res1[2]
  Noiztnb[jj] <- n / (1 - (1 + lhatoinb / ahatoinb[jj]) ^ -ahatoinb[jj])

  # Estimate ZTNB model, and record the Horvitz-Thompson estimate
  res2 <- maxLik(llztnb_nocov, method="nm", start=c(li, a), iterlim=10000)$estimate
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
  
  # Iterate the MC loop forward
  jj <- jj + 1
}

# The following produces the 1st row of Table 2 in the main paper
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