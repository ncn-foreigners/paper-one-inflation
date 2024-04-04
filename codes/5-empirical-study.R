library(data.table)
library(stringr)
library(xtable)
library(singleRcapture)


# data preparation --------------------------------------------------------

police <- readRDS("data/police-data-poland-raw.rds")
police[, obyw_count := uniqueN(obyw), id]
police[, date:= format(data, "%Y-%m-%d")]

police <- police[rok_wpis == 2022 & 
                   str_detect(kwalifikacja, "178") & 
                   (2022 - rok_ur) >= 18 & 
                   obyw_count == 1 & 
                   obyw != "" & 
                   !str_detect(kpw, "CBŚP")][order(id, date)]

police[, age := 2022 - rok_ur]
police[, country:= ifelse(obyw %in% c("POLSKA", "UKRAINA", "GRUZJA", "BIAŁORUŚ"), str_to_title(obyw), "Other")]
police[, country:= factor(country, 
                          c("Polska", "Ukraina", "Gruzja", "Białoruś", "Other"),
                          c("Poland", "Ukraine", "Georgia", "Belarus", "Other"))]

police_data_model <- police[, .(counts = uniqueN(date)), .(id, sex=ifelse(plec=="M", "Male", "Female"), age, country)]
police_data_model_agg <- police_data_model[, .(cases = .N) ,.(counts, sex, age, country)]


# models (intercept) ------------------------------------------------------

## standard models

m1_1 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "ztpoisson"
)

m1_2 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "ztgeom")

m1_3 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "zelterman")

m1_4 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "chao")

## one inflation (stanard)

m2_1 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "ztoipoisson",
  controlModel = controlModel(
    omegaFormula = ~ 1,
  )
)

m2_2 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "ztoigeom",
  controlModel = controlModel(
    omegaFormula = ~ 1
  ),
  controlMethod = controlMethod(
    momentumFactor = .1,
    stepsize = .9
  )
)

m2_3 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "oiztpoisson",
  controlModel = controlModel(
    omegaFormula = ~ 1
  )
)

m2_4 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "oiztgeom",
  controlModel = controlModel(
    omegaFormula = ~ 1
  ),
  controlMethod = controlMethod(
    momentumFactor = .1,
    stepsize = .9
  )
)

## proposed

m3_1 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "ztHurdlepoisson",
  controlModel = controlModel(
    piFormula = ~ 1
  )
)

m3_2 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "ztHurdlegeom",
  controlModel = controlModel(
    piFormula = ~ 1
  )
)

m3_3 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "Hurdleztpoisson",
  controlModel = controlModel(
    piFormula = ~ 1
  ),
  controlMethod = controlMethod(
    momentumFactor = .1,
    stepsize = .9
  )
)

m3_4 <- estimatePopsize(
  formula = counts ~ 1,
  data = police_data_model,
  model = "Hurdleztgeom",
  controlModel = controlModel(
    piFormula = ~ 1
  )
)

## summary

## reporting
models <- list("ztpoisson"=m1_1, "ztgeom"=m1_2, "zelterman"=m1_3, "chao"=m1_4, 
               "ztoipoisson"=m2_1, "ztoigeom"=m2_2, "oiztpoisson"=m2_3, "oiztgeom"=m2_4, 
               "ztHurdlepoisson"=m3_1, "ztHurdlegeom"=m3_2, "Hurdleztpoisson"=m3_3, "Hurdleztgeom"=m3_4)

results <- data.frame(est = names(models), 
                      N_hat = sapply(models, function(x) popSizeEst(x)$pointEstimate),
                      se_hat = sapply(models, function(x) sqrt(popSizeEst(x)$variance)),
                      ci_low = sapply(models, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                      ci_hig = sapply(models, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                      iter = sapply(models, "[[", "iter"))

AICs <- sapply(models, AIC)
BICs <- sapply(models, BIC)
deviances <- sapply(models, deviance)

n_obs <- police_data_model_agg[,.(m=sum(cases)), keyby=counts]$m

n_hats <- sapply(models, function(x) {
  singleRcapture::marginalFreq(x)$table[-1]
})

gof <- apply(n_hats, 2, function(x) {
  cbind(sum((n_obs - x)^2/x), 
        2*sum(n_obs*log(n_obs/x)))
})



# models (with covs) ------------------------------------------------------------------

## standard models

m1_1_c <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model,
  model = "ztpoisson"
)

m1_2_c <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model,
  model = "ztgeom")

m1_3_c <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model,
  model = "zelterman")

m1_4_c <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model,
  model = "chao")

## one inflation (stanard)

m2_1_c <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model,
  model = "ztoipoisson",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country,
  )
)

m2_2_c <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model,
  model = "ztoigeom",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country
  ),
  controlMethod = controlMethod(
    momentumFactor = .1,
    stepsize = .9
  )
)

m2_3_c <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model,
  model = "oiztpoisson",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country
  )
)

m2_4_c <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model,
  model = "oiztgeom",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country
  ),
  controlMethod = controlMethod(
    momentumFactor = .1,
    stepsize = .9
  )
)

## proposed

m3_1_c <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model,
  model = "ztHurdlepoisson",
  controlModel = controlModel(
    piFormula = ~ sex + age + country
  )
)

m3_2_c <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model,
  model = "ztHurdlegeom",
  controlModel = controlModel(
    piFormula = ~ sex + age + country
  )
)

## does not converge
m3_3_c <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model,
  model = "Hurdleztpoisson",
  controlModel = controlModel(
    piFormula = ~ sex + country
  ),
  controlMethod = controlMethod(
    momentumFactor = .1,
    stepsize = .9
  )
)

m3_4_c <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model,
  model = "Hurdleztgeom",
  controlModel = controlModel(
    piFormula = ~ sex + age + country
  )
)

## reporting
models_c <- list("ztpoisson"=m1_1_c, "ztgeom"=m1_2_c, "zelterman"=m1_3_c, "chao"=m1_4_c, 
               "ztoipoisson"=m2_1_c, "ztoigeom"=m2_2_c, "oiztpoisson"=m2_3_c, "oiztgeom"=m2_4_c, 
               "ztHurdlepoisson"=m3_1_c, "ztHurdlegeom"=m3_2_c, "Hurdleztpoisson"=m3_3_c, "Hurdleztgeom"=m3_4_c)


results_c <- data.frame(est = names(models_c), 
                       N_hat = sapply(models_c, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(models_c, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(models_c, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(models_c, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       iter = sapply(models_c, "[[", "iter"))

AICs_c <- sapply(models_c, AIC)
BICs_c <- sapply(models_c, BIC)
deviances_c <- sapply(models_c, deviance)

n_obs <- police_data_model_agg[,.(m=sum(cases)), keyby=counts]$m

n_hats_c <- sapply(models_c, function(x) {
  singleRcapture::marginalFreq(x)$table[-1]
})
  
gof_c <- apply(n_hats_c, 2, function(x) {
  cbind(sum((n_obs - x)^2/x), 
        2*sum(n_obs*log(n_obs/x)))
})


## all results

tab1 <- cbind(results[1:5], AICs, BICs, gof[1, ])
tab2 <- cbind(results_c[1:5], AICs_c, BICs_c, gof_c[1, ])

rbind(
  tab1, setNames(tab2, names(tab1))
) |>
  xtable(digits = 0) |>
  print.xtable(format.args = list(big.mark = ","), include.rownames = F)

