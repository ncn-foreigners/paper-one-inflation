library(data.table)
library(stringr)
library(xtable)
library(singleRcapture)
library(ggplot2)


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


# models ------------------------------------------------------------------

## standard models

m1_1 <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "ztpoisson")

m1_2 <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "ztgeom")

m1_3 <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "zelterman")

m1_4 <- estimatePopsize(
  formula = counts ~ sex + age + country,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "chao")

## one inflation (stanard)

m2_1 <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "ztoipoisson",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country,
    weightsAsCounts = TRUE
  )
)

m2_2 <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "ztoigeom",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country,
    weightsAsCounts = TRUE
  )
)

m2_3 <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "oiztpoisson",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country,
    weightsAsCounts = TRUE
  )
)

m2_4 <- estimatePopsize(
  formula = counts ~ sex + age, 
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "oiztgeom",
  controlModel = controlModel(
    omegaFormula = ~ sex + age + country,
    weightsAsCounts = TRUE
  )
)

## proposed

m3_1 <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "ztHurdlepoisson",
  controlModel = controlModel(
    piFormula = ~ sex + age + country,
    weightsAsCounts = TRUE
  )
)

m3_2 <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "ztHurdlegeom",
  controlModel = controlModel(
    piFormula = ~ sex + age + country,
    weightsAsCounts = TRUE
  )
)

## does not converge
# m3_3 <- estimatePopsize(
#   formula = counts ~ sex + age,
#   data = police_data_model_agg,
#   weights = police_data_model_agg$cases,
#   model = "Hurdleztpoisson",
#   controlModel = controlModel(
#     piFormula = ~ sex + country,
#     weightsAsCounts = TRUE
#   )
# )

m3_3 <- estimatePopsize(
  formula = counts ~ sex + age,
  data = police_data_model_agg,
  weights = police_data_model_agg$cases,
  model = "Hurdleztgeom",
  controlModel = controlModel(
    piFormula = ~ sex + age,#, + country,
    weightsAsCounts = TRUE
  )
)

