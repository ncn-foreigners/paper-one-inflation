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
y <- c(rep(1,541), rep(2,169), rep(3,95), rep(4,37), rep(5,21), rep(6,23))
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
y <- c(rep(1,410),rep(2,161),rep(3,87),rep(4,46),rep(5,26),rep(6,21),rep(7,8),rep(8,4),rep(9,6),rep(10,10))
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
y <- c(rep(1,1206),rep(2,474),rep(3,198),rep(4,95),rep(5,29),rep(6,19),rep(7,5),
       rep(8,2),rep(10,1))
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
res6 <- lapply(list_models, function(x) estimatePopsize(formula = y ~ 1, data=df, model = x))
res6_tab <- data.frame(est = list_models, 
                       N_hat = sapply(res6, function(x) popSizeEst(x)$pointEstimate),
                       se_hat = sapply(res6, function(x) sqrt(popSizeEst(x)$variance)),
                       ci_low = sapply(res6, function(x) popSizeEst(x)$confidenceInterval[1,1]),
                       ci_hig = sapply(res6, function(x) popSizeEst(x)$confidenceInterval[1,2]),
                       X2 = sapply(res6, function(x) summary(marginalFreq(x), dropl5="no")[[1]][[1]][1]),
                       iter = sapply(res6, "[[", "iter"))

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
y <- c(rep(1,121),rep(2,13),rep(3,5),rep(4,2))
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
y <- c(rep(1,2176), rep(2,1600), rep(3,1278), rep(4,976), rep(5,748), rep(6,570), rep(7,455),
      rep(8,368), rep(9,281), rep(10,254), rep(11,188), rep(12,138), rep(13,99), rep(14,67),
      rep(15,44), rep(16,34), rep(17,17), rep(18,3), rep(19,3), rep(20,2), rep(21,1))
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



