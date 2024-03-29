library(singleRcapture)
library(data.table)

## setting similar to Godwin (2017)

# Poisson -----------------------------------------------------------------
set.seed(2024)
n_sims <- 500
n <- c(50, 100, 250, 500, 1000)
li <- 2
omega <- 0.1

oipp_logit <- oiztpoisson(lambdaLink = "log", omegaLink = "logit")
oipp_cloglog <- oiztpoisson(lambdaLink = "log", omegaLink = "cloglog")
oipp_probit <- oiztpoisson(lambdaLink = "log", omegaLink = "probit")

results_all <- list() 

for (i in 1:NROW(n)) {
  
  results_n <- list() 
  
  for (k in 1:n_sims) {
    
    set.seed(2024 + k)
    
    eta_sim <- cbind(rep(li, n[i]), rep(omega, n[i]))
    
    df_ys <- data.frame(y_logit = simulate(oipp_logit, eta = eta_sim),
                        y_cloglog = simulate(oipp_cloglog, eta = eta_sim),
                        y_probit = simulate(oipp_probit, eta = eta_sim))
    
    m1_1 <- estimatePopsize(formula = y_logit ~ 1, data = subset(df_ys, y_logit >0), model = oipp_logit)
    m1_2 <- estimatePopsize(formula = y_logit ~ 1, data = subset(df_ys, y_logit >0), model = oipp_cloglog)
    m1_3 <- estimatePopsize(formula = y_logit ~ 1, data = subset(df_ys, y_logit >0), model = oipp_probit)
    
    m2_1 <- estimatePopsize(formula = y_cloglog ~ 1, data = subset(df_ys, y_cloglog >0), model = oipp_logit)
    m2_2 <- estimatePopsize(formula = y_cloglog ~ 1, data = subset(df_ys, y_cloglog >0), model = oipp_cloglog)
    m2_3 <- estimatePopsize(formula = y_cloglog ~ 1, data = subset(df_ys, y_cloglog >0), model = oipp_probit)
    
    m3_1 <- estimatePopsize(formula = y_probit ~ 1, data = subset(df_ys, y_probit >0), model = oipp_logit)
    m3_2 <- estimatePopsize(formula = y_probit ~ 1, data = subset(df_ys, y_probit >0), model = oipp_cloglog)
    m3_3 <- estimatePopsize(formula = y_probit ~ 1, data = subset(df_ys, y_probit >0), model = oipp_probit)
    
    coefs <- rbind(coef(m1_1), coef(m1_2), coef(m1_3),
                   coef(m2_1), coef(m2_2), coef(m2_3),
                   coef(m3_1), coef(m3_2), coef(m3_3))
    
    results_n[[k]] <- data.frame(n = n[i],
                                 link = rep(c("logit", "cloglog", "probit"), each = 3),
                                 link_sel = rep(c("logit", "cloglog", "probit"), times = 3),
                                 lambda = coefs[, 1],
                                 omega = coefs[, 2])
    
  }
  
  results_all[[i]] <- rbindlist(results_n, idcol = "iter")
             
}


results_all <- rbindlist(results_all)

