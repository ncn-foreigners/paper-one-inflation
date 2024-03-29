# The main program -------------------------------------------------------------
# The following  program will reproduce the first row of Table A8 in the Web 
# Appendix. Subsequent rows can be reproduced by changing the data generating
# process, i.e. by assigning the appropriate values to the parameters "n", "a",
# and "li".

library(maxLik)
source(file = "functions.r")

# Set the data generating process:
set.seed(1)
n <- 50
a <- 0.5
li <- 1

# Set the number of Monte Carlo repititions
nrep = 10000

# Assign an empty vector to store the LRTs
LRTztnb <- rep(0, nrep)

# Start the Monte Carlo loop
jj <- 1
while(jj <= nrep) {
  y <- ztnbv(li, a)
  # Estimate the two models, and store the LRT
  max_oiztnb <- maxLik(lloiztnb_nocov, method="nm", start=c(li,a,0.1),
                       iterlim=10000)$maximum
  max_ztnb <- maxLik(llztnb_nocov, method="nm", start=c(li,a),
                     iterlim=10000)$maximum
  LRTztnb[jj] <- 2*(max_oiztnb - max_ztnb)

  # Iterate the MC loop forward
  jj = jj + 1
}
forcrit <- sort(LRTztnb)

# Display the first row of WebTable 3
matrix(c((sum(LRTztnb > 5.411894) / nrep),(forcrit[nrep * .99]),(sum(LRTztnb >
       2.705543) / nrep),(forcrit[nrep * .95]),(sum(LRTztnb > 1.642374) / nrep),
       (forcrit[nrep * .9])),1,6)