library(maxLik)
source(file = "functions.r")

# Input the data ---------------------------------------------------------------
y <- c(rep(1,1206),rep(2,474),rep(3,198),rep(4,95),rep(5,29),rep(6,19),rep(7,5),
      rep(8,2),rep(10,1))
# Determine sample size
n <- length(y)
# Input the covariate matrix. The code below produces a column of "ones" (no 
# covariates) 
x <- matrix(rep(1, n), n, 1)
# Determine the columns of "X" (1 column means no covariates)
p <- ncol(x)

# Estimate various models, and record the MLEs and LRTs-------------------------
# Estimate PP
resPP <- maxLik(llpp, method="nm", start=c(1, rep(0, p-1)), iterlim=10000)
liPP <- exp(x %*% resPP$estimate[1:p])

# Estimate OIPP. Use the estimates from PP for initial values
resOIPP <- maxLik(lloipp, method="nm", start=c(resPP$estimate,0.1), 
                  iterlim=10000)
liOIPP <- exp(x %*% resOIPP$estimate[1:p])
wOIPP <- resOIPP$estimate[p + 1] / (1 + resOIPP$estimate[p + 1])

# Estimate ZTNB. Use the estimates from PP for initial values
resZTNB <- maxLik(llztnb, method="nm", start=c(resPP$estimate,1), iterlim=10000)
liZTNB <- exp(x %*% resZTNB$estimate[1:p])
a <- resZTNB$estimate[p + 1]

# Estimate OIZTNB. Use the estimates from PP for initial values
resOIZTNB <- maxLik(lloiztnb, method="nm", start=c(resPP$estimate,1,0.1), 
                    iterlim=10000)
li1 <- exp(x %*% resOIZTNB$estimate[1:p])
a1 <- resOIZTNB$estimate[p + 1]
w1 <- 1 / (1 + exp(resOIZTNB$estimate[p + 2]))

# Calculate the LRT statistics
LRTpp <- 2 * (resOIZTNB$maximum - resPP$maximum)
LRToipp <- 2 * (resOIZTNB$maximum - resOIPP$maximum)
LRTztnb <- 2 * (resOIZTNB$maximum - resZTNB$maximum)

# Reproduce the results in Table 3 ---------------------------------------------
# Display population size estimates
# PP
sum(1 / (1 - exp(-liPP)))
# OIPP
sum(1 / (1 - exp(-liOIPP)))
# ZTNB
sum(1 / (1 - (1 + liZTNB / a) ^ (-a)))
# OIZTNB
sum(1 / (1 - (1 + li1 / a1) ^ (-a1)))

# Display the LRT statistics
LRTpp
LRToipp
LRTztnb

# Display the p-values for the LRT statistics
# H0:OIPP vs. HA:OIZTNB
0.5 * (1 - pchisq(LRToipp,1))
# H0:ZTNB vs. HA:OIZTNB
0.5 * (1 - pchisq(LRTztnb,1))

# Display the estimated MLEs
# The mean lambda-hat (if it varies through covariates)
# PP:
mean(liPP)
# OIPP:
mean(liOIPP)
# ZTNB:
mean(liZTNB)
# OIZTNB:
mean(li1)

# alpha-hat
# ZTNB:
a
# OIZTNB:
a1

# omega-hat 
# OIPP:
wOIPP
# OIZTNB:
w1

# Generate the predicted counts. First choose the maximum count to predict:
maxpred = 5
# Predicted counts for PP
predPP(maxpred, liPP)
# Predicted counts for OIPP
predOIPP(maxpred, liOIPP, wOIPP)
# Predicted counts for ZTNB
predZTNB(maxpred, liZTNB, a)
# Predicted counts for OIZTNB
predOIZTNB(maxpred, li1, a1, w1)
# Actual counts from the data
tabulate(y)

# Parametrically bootstrap the standard errors and 95% confidence intervals
# for the various estimators:
bootNPP(liPP)
bootNOIPP(liOIPP, wOIPP)
bootNZTNB(liZTNB, a)
bootNOIZTNB(li1, a1, w1)
