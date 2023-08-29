#remotes::install_github("ncn-foreigners/singleRcapture")
library(singleRcapture)

df_drunk_no_location <- read.csv("data/df_drunk_no_location", row.names=1)
df_drunk_no_location <- df_drunk_no_location |>
  mutate(citizenship = ifelse(citizenship %in% c("POLSKA", "UKRAINA"), 
                              citizenship, "OTHER"))
summary(df_drunk_no_location)

## these take a long time to compute it is best to skip actually running those
## and instead load completed models later
##### Computing ####
m1 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "ztoipoisson",
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m1)

m2 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "oiztpoisson",
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m2)

m3 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "ztHurdlepoisson",
  controlModel = controlModel(
    piFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m3)

m4 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "Hurdleztpoisson",
  controlModel = controlModel(
    piFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m4)

# geometrics
m11 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "ztoigeom",
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m11)

m12 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "oiztgeom",
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m12)

m13 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "Hurdleztgeom",
  controlModel = controlModel(
    piFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(m13)

m14 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "ztHurdlegeom",
  controlModel = controlModel(
    piFormula = ~ gender + poly(age, degree = 4) + previous_offences + citizenship
  ),
  controlMethod = controlMethod(
    verbose = 5, stepsize = .4
  )
)
summary(m14)

# final model
m1_a <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "ztoipoisson",
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  ),
  popVar = "bootstrap",
  controlPopVar = controlPopVar(
    bootType = "semiparametric", 
    B = 5000, cores = 16
  )
)
summary(m1_a)

m1_b <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk_no_location,
  model = "ztoipoisson",
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3) + previous_offences
  ),
  controlMethod = controlMethod(
    verbose = 5
  ),
  popVar = "bootstrap",
  controlPopVar = controlPopVar(
    bootType = "parametric", 
    B = 5000, cores = 16
  )
)
summary(m1_b)

dfb <- dfbeta(m1_a, cores = 16)
dfp <- dfpopsize(m1, dfbeta = dfb)
save.image(file = "data/fitted.RData")

##### Using results from previous section to reproduce figures ####
# load data
load("data/fitted.RData")

png("figures/model_deletion_effect.png")
plot(y = predict(m1_a, type = "contr"),
     x = dfpopsize(m1, dfbeta = dfb),
     main = paste0("Observation deletion effect on point estimate of",
                   "\npopulation size estimate vs observation contribution"),
     xlab = "Deletion effect", ylab = "Observation contribution")
abline(a = 0, b = 1, col = "red")
# Closing the graphical device
dev.off()

png("figures/model_semi_bootstrap.png")
plot(m1_a, plotType = "bootHist", breaks = 50, ylim = c(0, 325))
dev.off()

png("figures/model_bootstrap.png")
plot(m1_b, plotType = "bootHist", breaks = 50, ylim = c(0, 325))
dev.off()

png("figures/model_rootogram.png")
plot(m1_a, plotType = "rootogram")
dev.off()

stratifyPopsize(m1_a, stratas = ~ gender * previous_offences)

summary(marginalFreq(m1_b), dropl5 = "group", df = 1)

