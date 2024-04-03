library(singleRcapture)
library(xtable)
options(xtable.include.rownames = FALSE)
options(xtable.table.placement = "ht!")
options(xtable.caption.placement = "top")


list_models <- c("ztpoisson", "ztnegbin", "ztgeom", 
                 "ztoipoisson", "ztoinegbin", "ztoigeom", 
                 "oiztpoisson",  "oiztnegbin", "oiztgeom",
                 "ztHurdlepoisson", "ztHurdlenegbin", "ztHurdlegeom", 
                 "Hurdleztpoisson",  "Hurdleztnegbin", "Hurdleztgeom",
                 "zelterman", "chao")

## Prostitutes in Vancouver
y <- rep(1:6, c(541,169,95,37,21,23))
df <- data.frame(y)
res1 <- lapply(list_models, function(x) try(estimatePopsize(formula = y ~ 1, data=df, model = x)))

res1_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res1, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res1, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res1, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res1, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res1, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res1, "[[", "iter"))

res1_tab |> 
  xtable(digits = 0, caption = "Prostitutes in Vancouver") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 

## H5N1 flu outbreaks
y <- rep(1:10, c(410,161,87,46,26,21,8,4,6,10))
df <- data.frame(y)
res2 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))

res2_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res2, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res2, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res2, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res2, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res2, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res2, "[[", "iter"))

res2_tab |> 
  xtable(digits = 0, caption = "H5N1 flu outbreaks") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 

## Rotterdam opiate users
y <- rep(c(1:8,10), c(1206,474,198,95,29,19,5,2,1))
df <- data.frame(y)
res3 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))
res3_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res3, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res3, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res3, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res3, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res3, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res3, "[[", "iter"))
res3_tab |> 
  xtable(digits = 0, caption = "Rotterdam opiate users") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 



## Dutch unauthorized/undocumented immigrants
y <- rep(1:6, c(1645, 183, 37, 13, 1, 1))
df <- data.frame(y)
res4 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))
res4_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res4, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res4, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res4, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res4, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res4, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res4, "[[", "iter"))

res4_tab |> 
  xtable(digits = 0, caption = "Dutch unauthorized/undocumented immigrants") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 

## Illegal firearms
y <- rep(1:3, c(2561, 72, 5))
df <- data.frame(y)
res5 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))
res5_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res5, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res5, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res5, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res5, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res5, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res5, "[[", "iter"))

res4_tab |> 
  xtable(digits = 0, caption = "Illegal firearms") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 


## Drunk-driving (errors)
y <- rep(1:5, c(8877, 481, 52, 8, 1))
df <- data.frame(y)
res6 <- lapply(list_models[c(1:4, 6:17)], function(x) 
  {print(x)
  estimatePopsize(formula = y ~ 1, data=df, model = x)
  })

res6_tab <- data.frame(est = list_models[c(1:4, 6:17)], 
                       N_hat = sapply(res6, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res6, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res6, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res6, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res6, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res6, "[[", "iter"))

res6_tab |> 
  xtable(digits = 0, caption = "Drunk-driving") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), )

## Snowshoe hares
y <- rep(1:6, c(67, 26, 18, 7, 4, 3))
df <- data.frame(y)
res7 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))

res7_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res7, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res7, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res7, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res7, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res7, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res7, "[[", "iter"))

res7_tab |> 
  xtable(digits = 0, caption = "Snowshoe hares") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 


## the French scrapie data
y <- rep(1:4, c(121,13,5,2))
df <- data.frame(y)
res8 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))

res8_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res8, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res8, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res8, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res8, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res8, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res8, "[[", "iter"))

res8_tab |> 
  xtable(digits = 0, caption = "the French scrapie data") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), ) 

## Bangkok heroin users
y <- rep(1:21, c(2176,1600,1278,976,748,570,455,368,281,254,188,138,99,67,44,34,17,3,3,2,1))
df <- data.frame(y)
res9 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))

res9_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res9, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res9, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res9, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res9, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res9, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res9, "[[", "iter"))

res9_tab |> 
  xtable(digits = 0, caption = "Bangkok heroin users") |>
  print(format.args = list(big.mark = ",", decimal.mark = "."), )



