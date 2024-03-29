# Reproduces the top-left three entries of Table 1 in the main paper. Other
# entries may be reproduced by changing the DGP, i.e., by assigning different
# values for the parameters "n", "li", "a", and "omega".

library(maxLik)
source(file = "functions.r")

# Set the data generating process:
set.seed(1)
n <- 50
li <- 2
a <- .5
omega <- 0.1
phi <- log(omega / (1 - omega))

# Set the number of Monte Carlo repititions
nrep <- 10000

# Assign empty vectors to store the MLEs
lhatoinb <- ahatoinb <- ohatoinb <- rep(0,nrep)

# Start the Monte Carlo loop
jj <- 1
while(jj <= nrep) {
  # Generate random OIZTNB variates
  y <- oiztnbv(li, a, omega)

  # Estimate OIZTNB model, and record the MLEs
  res <- maxLik(lloiztnb_nocov, method="nm", start=c(li,a,phi),iterlim=10000)$estimate
  lhatoinb[jj] <- res[1]
  ahatoinb[jj] <- res[2]
  ohatoinb[jj] <- 1/(1+exp(res[3]))

  # Iterate the MC loop forward
  jj = jj + 1
}

# Display %bias and %MSE (the first three entries of Table 1 in the main paper)
bias(li,lhatoinb)
bias(a,ahatoinb)
bias(omega,ohatoinb)