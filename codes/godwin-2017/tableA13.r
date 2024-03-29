# The main program -------------------------------------------------------------
# The following program will reproduce the top-left entry of each of the three 
# panels in Table A13. The other entries can be reproduced by changing the
# sample size "n" and the value of "omega".

library(maxLik)
source(file = "functions.r")

# Set the data generating process:
set.seed(1)
n <- 100
li <- 1
a <- 0.5
omega <- 0.05
phi <- log(omega / (1 - omega))

# Set the number of Monte Carlo repititions
nrep <- 10000

# Assign an empty vector to store the LRTs
LRTztnb = rep(0, nrep)


# Start the Monte Carlo loop
jj <- 1
while(jj <= nrep) {
  y <- oiztnbv(li, a, omega)

  # Estimate the two models, and store the LRT
  max_oiztnb <- maxLik(lloiztnb_nocov, method="nm", start=c(li,a,phi),
                      iterlim=10000)$maximum
  max_ztnb <- maxLik(llztnb_nocov, method="nm", start=c(li,a),iterlim=10000)$maximum
  LRTztnb[jj] = 2 * (max_oiztnb - max_ztnb)

  # Iterate the MC loop forward
  jj = jj + 1
}

# Display empirical power 
#1%
sum(LRTztnb > 5.411894) / nrep
#5%
sum(LRTztnb > 2.705543) / nrep
#10%
sum(LRTztnb > 1.642374) / nrep
