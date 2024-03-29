# Generating random variates for OIZTNB, ZTNB and OIPP -------------------------

# Generates random OIZTNB variates. "li" is the lambda parameter, "a" is the
# alpha parameter and "omega" is the omega parameter from the OIZTNB 
# distribution. The sample size "n" must be defined in the main program.
oiztnbv <- function(li, a, omega) {
  # Builds up the CMF using the PMF. There is a max count that can be generated,
  # determined by calculating probabilities for the first 99.99% of the CMF.
  k <- 1
  phi <- omega / (1 - omega)
  probs <- c((phi / (1 + phi)) + (1 / (1 + phi)) * a * ((a / (a + li)) ^ a) * 
          (li / (a + li - a * (1 + (li / a)) ^ (1 - a))))
  while(probs[k] < 0.9999) {
    k <- k + 1
    probs <- c(probs, probs[k-1] + (1 / (1 + phi)) * (gamma(a + k) / (gamma(a) * 
    gamma(k + 1))) * ((a / (a + li)) ^ a) * ((li / (a + li)) ^ k) * (1 / (1 - 
    (a / (a + li)) ^ a)))
  }
  y <- rep(0, n)
  dice <- runif(n)
  for(ii in 1:n) {
    y[ii] <- sum(dice[ii] > probs) + 1
  }
  return(y)
}

# Generates random ZTNB variates. 
ztnbv <- function(li, a) {
  # Builds up the CMF using the PMF. There is a max count that can be generated,
  # determined by calculating probabilities for the first 99.99% of the CMF.
  k <- 1
  probs <- (gamma(a + k) / (gamma(a) * gamma(k + 1))) * ((a / (a + li)) ^ a) * 
    ((li / (a + li)) ^ k) * (1 / (1 - (a / (a + li)) ^ a))
  while(probs[k] < 0.9999){
    k <- k + 1
    probs <- c(probs, probs[k - 1] + (gamma(a + k) / (gamma(a) * gamma(k+1))) *
                 ((a / (a + li)) ^ a) * ((li / (a + li)) ^ k) * (1 / (1 - (a / 
                 (a + li)) ^ a)))
  }
  y <- rep(0, n)
  dice <- runif(n)
  for(ii in 1:n){
    y[ii] <- sum(dice[ii] > probs) + 1
  }
  return(y)
}

# Generates random OIPP variates.
oippv <- function(li, omega) {
  y <- rpospois(n,li)
  y[runif(n) <= omega] <- 1
  return(y)
}

# Log-likelihood functions for PP, OIPP, ZTNB, and OIZTNB (with and without 
# covariates) ------------------------------------------------------------------

# Log likelihood for PP. "li" is the lambda parameter from PP.
llpp <- function(param) {
  li <- exp(x %*% param[1:p])
  return(sum(y * log(li) - log(exp(li) - 1) - log(factorial(y))))
}

# Log likelihood for OIPP. "li" is the lambda parameter and "thet" is the
# theta parameter from OIPP.
lloipp <- function(param) {
  li <- exp(x %*% param[1:p])
  thet <- param[p + 1]
  return(sum(-log(1 + thet) + (y == 1) * log(thet + (li / (exp(li) - 1))) + (1 -
         (y==1)) * (y * log(li) - log(exp(li) - 1) - log(factorial(y)))))
}

# Log likelihood for ZTNB. "li" is the lambda parameter and "a" is the alpha
# parameter from ZTNB.
llztnb <- function(param) {
  li <- exp(x %*% param[1:p])
  a <- param[p + 1]
  ymax <- max(y)
  terms <- weights <- rep(0, ymax)
  for(ii in 1:ymax) {
    terms[ii] <- log(a + ii - 1)
  }
  weights[1] <- sum(y>1)
  for(ii in 2:ymax) {
    weights[ii] <- sum(y > (ii - 1))
  }
  gterm <- sum(terms * weights)
  return(sum((y == 1) * log(a) - log(factorial(y)) + a * log(a) + y * log(li) - 
         (a + y) * log(a + li) - log(1 - (a / (a + li)) ^ a)) + gterm)
}

# Log likelihood function for OIZTNB. "li" is the lambda parameter, "a" is the
# alpha parameter and "omega" is the omega parameter from OIZTNB.
lloiztnb <- function(param) {
  li <- exp(x %*% param[1:p])
  a  <- param[p + 1]
  
  # Use the logistic link (Section 2.1 of the main paper)
  phi <- param[p + 2]
  w <- 1 / (1 + exp(phi))
  
  ymax <- max(y)
  terms <- weights <- rep(0, ymax)
  for(ii in 1:ymax) {
    terms[ii] <- log(a + ii - 1)
  }
  weights[1] <- sum(y > 1)
  for(ii in 2:ymax) {
    weights[ii] <- sum(y > (ii - 1))
  }
  gterm <- terms * weights
  return(sum(log(1 - w) + (y == 1) * (log(w / (1 - w) + a * ((a / (a + li)) ^ a) 
         * (li / (a + li - a * (1 + (li / a)) ^ (1 - a))))) + (1 - (y == 1)) * 
         (a * log(a) - log(factorial(y)) + y * log(li) - (a + y) * log(a + li) - 
         log(1 - (a / (a + li)) ^ a))) + sum(gterm))
}

# The following log-likelihood allows covariates to be linked to the "omega"
# parameter. A new matrix of covariates, "z", must be defined in the main
# program before calling this function, but may be identical to "x".
lloiztnbz <- function(param) {
  li <- exp(x %*% param[1:p])
  a  <- param[p + 1]
  
  # Use the logistic link with covariates (Section 2.1 of the main paper)
  w <- 1 / (1 + exp(z %*% param[(p + 2):(2 * p + 1)]))
  
  ymax <- max(y)
  terms <- weights <- rep(0, ymax)
  for(ii in 1:ymax) {
    terms[ii] <- log(a + ii - 1)
  }
  weights[1] <- sum(y > 1)
  for(ii in 2:ymax) {
    weights[ii] <- sum(y > (ii - 1))
  }
  gterm <- terms * weights
  return(sum(log(1 - w) + (y == 1) * (log(w / (1 - w) + a * ((a / (a + li)) ^ a) 
         * (li / (a + li - a * (1 + (li / a)) ^ (1 - a))))) + (1 - (y == 1)) * 
         (a * log(a) - log(factorial(y)) + y * log(li) - (a + y) * log(a + li) - 
         log(1 - (a / (a + li)) ^ a))) + sum(gterm))
}

# Log likelihood for PP - NO COVARIATES - for MC only. Same as above, except
# simplified in the case of no covariates, for faster computation.
llpp_nocov <- function(l) {
  n0 <- length(y)
  sum(y)*(log(l)) - n0*(log(exp(l)-1)) - sum(log(factorial(y)))
}

# Log likelihood for OIPP - NO COVARIATES - for MC only. Same as above, except
# simplified in the case of no covariates, for faster computation.
lloipp_nocov <- function(param){
  la = param[1]
  theta = param[2]
  n0 <- length(y)
  n1 <- sum(y==1)
  w = 1/(1+exp(-theta))
  n1*log(w + (1-w)*(la/(exp(la)-1))) + (n0-n1)*log(1-w) + (sum(y)-n1)*log(la) - 
    (n0-n1)*log(exp(la)-1) - sum(log(factorial(y)))
}

# Log likelihood for ZTNB - NO COVARIATES - for MC only. Same as above, except
# simplified in the case of no covariates, for faster computation.
llztnb_nocov <- function(param) {
  li <- param[1]
  a <- param[2]
  ymax <- max(y)
  terms <- weights <- rep(0, ymax)
  for(ii in 1:ymax) {
    terms[ii] <- log(a + ii - 1)
  }
  weights[1] <- sum(y>1)
  for(ii in 2:ymax) {
    weights[ii] <- sum(y > (ii - 1))
  }
  gterm <- sum(terms * weights)
  return(sum((y == 1) * log(a) - log(factorial(y)) + a * log(a) + y * log(li) - 
         (a + y) * log(a + li) - log(1 - (a / (a + li)) ^ a)) + gterm)
}

# Log likelihood function for OIZTNB - NO COVARIATES - for MC only Same as above, 
# except simplified in the case of no covariates, for faster computation.
lloiztnb_nocov <- function(param) {
  li <- param[1]
  a  <- param[2]
  
  # Use the logistic link (Section 2.1 of the main paper)
  phi <- param[3]
  w <- 1 / (1 + exp(phi))
  
  ymax <- max(y)
  terms <- weights <- rep(0, ymax)
  for(ii in 1:ymax) {
    terms[ii] <- log(a + ii - 1)
  }
  weights[1] <- sum(y > 1)
  for(ii in 2:ymax) {
    weights[ii] <- sum(y > (ii - 1))
  }
  gterm <- terms * weights
  return(sum(log(1 - w) + (y == 1) * (log(w / (1 - w) + a * ((a / (a + li)) ^ a) 
         * (li / (a + li - a * (1 + (li / a)) ^ (1 - a))))) + (1 - (y == 1)) * 
         (a * log(a) - log(factorial(y)) + y * log(li) - (a + y) * log(a + li) - 
         log(1 - (a / (a + li)) ^ a))) + sum(gterm))
}

# The following four functions are for parametrically bootstrapping standard
# errors and 95% confidence intervals. -----------------------------------------

# Function returns parametrically bootstrapped standard error and 95% confidence 
# interval for N_hat OIZTNB
bootNOIZTNB <- function(li,a,w) {
  
  library(VGAM)
  
  # Note that the log-likelihood must be redefined here.
  lloiztnb <- function(param) {
    li <- exp(x %*% param[1:p])
    a  <- param[p + 1]
    
    # Use the logistic link (Section 2.1 of the main paper)
    phi <- param[p + 2]
    w <- 1 / (1 + exp(phi))
    
    ymax <- max(y)
    terms <- weights <- rep(0, ymax)
    for(ii in 1:ymax) {
      terms[ii] <- log(a + ii - 1)
    }
    weights[1] <- sum(y > 1)
    for(ii in 2:ymax) {
      weights[ii] <- sum(y > (ii - 1))
    }
    gterm <- terms * weights
    return(sum(log(1 - w) + (y == 1) * (log(w / (1 - w) + a * ((a / (a + li)) 
           ^ a) * (li / (a + li - a * (1 + (li / a)) ^ (1 - a))))) + (1 - 
           (y == 1)) * (a * log(a) - log(factorial(y)) + y * log(li) - (a + y) * 
           log(a + li) - log(1 - (a / (a + li)) ^ a))) + sum(gterm))
  }
  
  # The following is for generating random OIZTNB variates.
  oiztnbv <- function(li, a, omega) {
    k <- 1
    phi <- omega / (1 - omega)
    probs <- c((phi / (1 + phi)) + (1 / (1 + phi)) * a * ((a / (a + li)) ^ a) * 
                 (li / (a + li - a * (1 + (li / a)) ^ (1 - a))))
    while(probs[k] < 0.9999) {
      k <- k + 1
      probs <- c(probs, probs[k-1] + (1 / (1 + phi)) * (gamma(a + k) / (gamma(a)
         * gamma(k + 1))) * ((a / (a + li)) ^ a) * ((li / (a + li)) ^ k) * (1 / 
         (1 - (a / (a + li)) ^ a)))
    }
    y <- sum(runif(1) > probs) + 1
    return(y)
  }
  
  # Set the number of bootstrap replications
  nboot <- 1000
  
  set.seed(1)
  
  # Empty vector for the bootstrap statistics
  N1boots <- rep(0,nboot)
  
  # The bootstrap loop
  bb <- 1
  while(bb <= nboot) {
    y <- mapply(oiztnbv,li=li,a=a, omega=w)   
    resboot <- maxLik(lloiztnb, method="nm", start=resOIZTNB$estimate, 
                        iterlim=10000)
    li1boot <- exp(x %*% resboot$estimate[1:p])
    a1boot <- resboot$estimate[p + 1]
    if(a1boot < 0.05) {next}
    N1boots[bb] <- sum(1 / (1 - (1 + li1boot / a1boot) ^ (-a1boot)) - 1) + n
    bb <- bb + 1
  }
  
  seNOIZTNB <- sd(N1boots)
  NOIZTNBsort <- sort(N1boots)
  left95CI <- NOIZTNBsort[.025 * nboot]
  right95CI <- NOIZTNBsort[.975 * nboot]
  
  list(seNOIZTNB = seNOIZTNB, left95CI = left95CI, right95CI = right95CI)
}

# Function returns parametrically bootstrapped standard error and 95% confidence
# interval for N_hat ZTNB
bootNZTNB <- function(li,a) {
  
  library(VGAM)
  
  # Note that the log-likelihood must be redefined here.
  llztnb <- function(param) {
    li <- exp(x %*% param[1:p])
    a <- param[p + 1]
    ymax <- max(y)
    terms <- weights <- rep(0, ymax)
    for(ii in 1:ymax) {
      terms[ii] <- log(a + ii - 1)
    }
    weights[1] <- sum(y>1)
    for(ii in 2:ymax) {
      weights[ii] <- sum(y > (ii - 1))
    }
    gterm <- sum(terms * weights)
    return(sum((y == 1) * log(a) - log(factorial(y)) + a * log(a) + y * log(li) 
               - (a + y) * log(a + li) - log(1 - (a / (a + li)) ^ a)) + gterm)
  }
  
  # The following is for generating random ZTNB variates.
  ztnbv <- function(li, a) {
    k <- 1
    probs <- (gamma(a + k) / (gamma(a) * gamma(k + 1))) * ((a / (a + li)) ^ a) * 
      ((li / (a + li)) ^ k) * (1 / (1 - (a / (a + li)) ^ a))
    while(probs[k] < 0.9999){
      k <- k + 1
      probs <- c(probs, probs[k - 1] + (gamma(a + k) / (gamma(a) * gamma(k+1))) 
                 * ((a / (a + li)) ^ a) * ((li / (a + li)) ^ k) * (1 / (1 - (a / 
                 (a + li)) ^ a)))
    }
    y <- sum(runif(1) > probs) + 1
    return(y)
  }
  
  # Set the number of bootstrap replications
  nboot <- 1000
  
  set.seed(1)
  
  # Empty vector for the bootstrap statistics
  NZTNBboots <- rep(0,nboot)
  
  # The bootstrap loop
  bb <- 1
  while(bb <= nboot) {
    y <- mapply(ztnbv,li=li,a=a)   
    resboot <- maxLik(llztnb, method="nm", start=resZTNB$estimate,iterlim=10000)
    liboot <- exp(x %*% resboot$estimate[1:p])
    aboot <- resboot$estimate[p+1]
    if(aboot < 0.05) {next}
    NZTNBboots[bb] <- sum(1 / (1 - (1 + liboot / aboot) ^ (-aboot)) - 1) + n
    bb <- bb + 1
  }
  
  seNZTNB <- sd(NZTNBboots)
  NZTNBsort <- sort(NZTNBboots)
  left95CI <- NZTNBsort[.025 * nboot]
  right95CI <- NZTNBsort[.975 * nboot]
  
  list(seNZTNB = seNZTNB, left95CI = left95CI, right95CI = right95CI)
}

# Function returns parametrically bootstrapped standard error and 95% confidence 
# interval for N_hat OIPP
bootNOIPP <- function(li,w) {
  
  library(VGAM)
  
  # Note that the log-likelihood must be redefined here.
  lloipp <- function(param) {
    li <- exp(x %*% param[1:p])
    thet <- param[p + 1]
    return(sum(-log(1 + thet) + (y == 1) * log(thet + (li / (exp(li) - 1))) + (1 
              - (y==1)) * (y * log(li) - log(exp(li) - 1) - log(factorial(y)))))
  }
  
  # Function for generating a random OIPP variate
  OIPPvariate <- function(li,w) {
    OIPPv <- rpospois(1,li)
    if(runif(1) <= w) {OIPPv <- 1}
    return(OIPPv)
  }
  
  # Set the number of bootstrap replications
  nboot <- 1000
  
  set.seed(1)
  
  # Empty vector for the bootstrap statistics
  NOIPPboots <- rep(0,nboot)
  
  # The bootstrap loop
  for(bb in 1:nboot) {
    y <- mapply(OIPPvariate,li=li,w=w)   
    liboots <- exp(x %*% maxLik(lloipp, method="nm", start=resOIPP$estimate, 
                   iterlim=10000)$estimate[1:p])
    NOIPPboots[bb] <- sum(1 / (1 - exp(-liboots)) - 1) + n
  }
  
  seNOIPP <- sd(NOIPPboots)
  NOIPPsort <- sort(NOIPPboots)
  left95CI <- NOIPPsort[.025 * nboot]
  right95CI <- NOIPPsort[.975 * nboot]
  
  list(seNOIPP = seNOIPP, left95CI = left95CI, right95CI = right95CI)
}

# Function returns parametrically bootstrapped standard error and 95% confidence 
# interval for N_hat PP 
bootNPP <- function(li) {
  
  library(VGAM)
  
  # Note that the log-likelihood must be redefined here, otherwise when the 
  # "llpp" function is called, the variables defined in "bootNPP" are lost.
  llpp <- function(param) {
    li <- exp(x %*% param[1:p])
    return(sum(y * log(li) - log(exp(li) - 1) - log(factorial(y))))
  }
  
  # Set the number of bootstrap replications
  nboot <- 1000
  
  set.seed(1)
  
  # Empty vector for the bootstrap statistics
  NPPboots <- rep(0,nboot)
  
  # The bootstrap loop
  for(bb in 1:nboot) {
    y <- mapply(function(l) {rpospois(1,l)},l=li)   
    liboots <- exp(x %*% maxLik(llpp, method="nm", start=resPP$estimate, 
                   iterlim=10000)$estimate[1:p])
    NPPboots[bb] <- sum(1 / (1 - exp(-liboots)) - 1) + n
  }
  
  seNPP <- sd(NPPboots)
  NPPsort <- sort(NPPboots)
  left95CI <- NPPsort[.025 * nboot]
  right95CI <- NPPsort[.975 * nboot]
  
  list(seNPP = seNPP, left95CI = left95CI, right95CI = right95CI)
}

# Functions to return the predicted counts from fitted PP, OIPP, ZTNB and OIZTNB
# models -----------------------------------------------------------------------

# Return the predicted counts in a fitted PP model. "maxpred" is the maximum 
# count to be predicted, "li" is the estimated lambda parameter from PP.
predPP <- function(maxpred, li) {
  preds <- matrix(,maxpred,n)
  for(k in 1:maxpred) {
    preds[k,] <- (li ^ k) / ((exp(li) - 1) * factorial(k))
  }
  return(rowSums(preds))
}

# Return the predicted counts in a fitted OIPP model. "maxpred" is the maximum 
# count to be predicted, "li" and "w" are the estimated lambda and omega 
# parameters from OIPP.  
predOIPP <- function(maxpred, li, w) {
  preds <- matrix(,maxpred,n)
  preds[1,] <- w + (1 - w) * (li / (exp(li) - 1))
  for(k in 2:maxpred) {
    preds[k,] <- (1 - w) * (li ^ k) / ((exp(li) - 1) * factorial(k))
  }
  return(rowSums(preds))
}

# Return the predicted counts in a fitted ZTNB model. "maxpred" is the maximum 
# count to be predicted, "li" and "a" are the estimated lambda and alpha 
# parameters from ZTNB.  
predZTNB <- function(maxpred, li, a) {
  preds <- matrix(,maxpred,n)
  preds[1,] <- (gamma(a + 1) / (gamma(a) * gamma(2))) * ((a / (a + li)) ^ a) *
                ((li / (a + li))) * (1 / (1 - (a / (a + li)) ^ a))
  for(k in 2:maxpred) {
    preds[k,] <- (gamma(a + k) / (gamma(a) * gamma(k + 1))) * ((a / (a + li)) ^
                   a) * ((li / (a + li)) ^ k) * (1 / (1 - (a / (a + li)) ^ a))
  }
  return(rowSums(preds))
}

# Return the predicted counts in a fitted OIZTNB model. "maxpred" is the maximum 
# count to be predicted, "li", "a" and "w" are the estimated lambda, alpha and 
# omega parameters from OIZTNB.  
  predOIZTNB <- function(maxpred, li, a, w) {
  preds <- matrix(,maxpred,n)
  preds[1,] <- w + (1 - w) * a * ((a / (a + li)) ^ a) * (li / (a + 
               li - a * (1 + (li / a)) ^ (1 - a)))
  for(k in 2:maxpred){
    preds[k,] <- (1 - w)*(gamma(a + k) / (gamma(a) * gamma(k + 1))) * ((a / (a + 
              li)) ^ a) * ((li / (a + li)) ^ k) * (1 / (1 - (a / (a + li)) ^ a))
  }
  return(rowSums(preds))
}

# Returns %bias, %MSE and %median squared error from the true parameter value 
# "true", and a vector of estimated MLEs "mles". ------------------------------- 

bias <- function(true, mles) {
  pbias <- (100 / abs(true)) * (mean(mles) - true)
  pmse  <- (100 / true^2) * ((mean(mles) - true) ^ 2 + (sum((mles - 
           mean(mles)) ^ 2))/ length(mles))
  pmedse <- (100 / true^2) * median((mles - true) ^ 2)
  list(pbias = pbias, pmse = pmse, pmedse = pmedse)
}