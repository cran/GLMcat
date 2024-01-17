## -----------------------------------------------------------------------------
# Load required packages
library(GLMcat)

# Load the data
data(TravelChoice)

# Define the full model formula
full_model_formula <- choice ~ hinc[air] + psize[air] + gc + ttme

## -----------------------------------------------------------------------------
mod0 <- discrete_cm(
  formula = choice ~ 1,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod0 <- AIC(mod0)

## -----------------------------------------------------------------------------
mod11 <- discrete_cm(
  formula = choice ~ hinc[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod11 <- AIC(mod11)

## -----------------------------------------------------------------------------
mod12 <- discrete_cm(
  formula = choice ~ psize[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod12 <- AIC(mod12)

## -----------------------------------------------------------------------------
mod13 <- discrete_cm(
  formula = choice ~ gc,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod13 <- AIC(mod13)

## -----------------------------------------------------------------------------
mod14 <- discrete_cm(
  formula = choice ~ ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod14 <- AIC(mod14)

## -----------------------------------------------------------------------------
aic_values_1 <- c(aicmod11, aicmod12, aicmod13, aicmod14)
AIC1 <- min(aic_values_1)
which_min_AIC1 <- which.min(aic_values_1)
which_min_AIC1

## -----------------------------------------------------------------------------
AIC0_is_smaller_than_AIC1 <- AIC1 < aicmod0
AIC0_is_smaller_than_AIC1

## -----------------------------------------------------------------------------
mod21 <- discrete_cm(
  formula = choice ~ hinc[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod21 <- AIC(mod21)

## -----------------------------------------------------------------------------
mod22 <- discrete_cm(
  formula = choice ~ psize[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod22 <- AIC(mod22)

## -----------------------------------------------------------------------------
mod23 <- discrete_cm(
  formula = choice ~ gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmod23 <- AIC(mod23)

## -----------------------------------------------------------------------------
aic_values_2 <- c(aicmod21, aicmod22, aicmod23)
AIC2 <- min(aic_values_2)
which_min_AIC2 <- which.min(aic_values_2)
which_min_AIC2

## -----------------------------------------------------------------------------
AIC2_is_greater_than_AIC1 <- AIC2 > AIC1
AIC2_is_greater_than_AIC1

## -----------------------------------------------------------------------------
modc0 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc0 <- AIC(modc0)

## -----------------------------------------------------------------------------
modc11 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc11 <- AIC(modc11)


modc12 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc12 <- AIC(modc12)


modc13 <- discrete_cm(
  formula = choice ~ hinc[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc13 <- AIC(modc13)

## -----------------------------------------------------------------------------
aic_values_backward <- c(aicmodc11, aicmodc12, aicmodc13)
AICC1 <- min(aic_values_backward)
which_min_AICC1 <- which.min(aic_values_backward)
which_min_AICC1

## -----------------------------------------------------------------------------
AICC1_is_smaller_than_all_predictors <- AICC1 < aicmodc0
AICC1_is_smaller_than_all_predictors

## -----------------------------------------------------------------------------
modc21 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc21 <- AIC(modc21)

modc22 <- discrete_cm(
  formula = choice ~ hinc[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc22 <- AIC(modc22)

modc23 <- discrete_cm(
  formula = choice ~ psize[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc23 <- AIC(modc23)

## -----------------------------------------------------------------------------
aic_values_backward_2 <- c(aicmodc21, aicmodc22, aicmodc23)
AICC2 <- min(aic_values_backward_2)
which_min_AICC2 <- which.min(aic_values_backward_2)
which_min_AICC2

## -----------------------------------------------------------------------------
AICC2_is_smaller_than_AICC1 <- AICC2 < AICC1
AICC2_is_smaller_than_AICC1

## -----------------------------------------------------------------------------
modc31 <- discrete_cm(
  formula = choice ~ psize[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc31 <- AIC(modc31)

modc32 <- discrete_cm(
  formula = choice ~ ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = list("student"),
  find_nu = TRUE
)
aicmodc32 <- AIC(modc32)

## -----------------------------------------------------------------------------
aic_values_backward_3 <- c(aicmodc31, aicmodc32)
AICC3 <- min(aic_values_backward_3)
which_min_AICC3 <- which.min(aic_values_backward_3)
which_min_AICC3

## -----------------------------------------------------------------------------
AICC3_is_smaller_than_AICC2 <- AICC3 < AICC2
AICC3_is_smaller_than_AICC2

## -----------------------------------------------------------------------------
mod0 <- discrete_cm(
  formula = choice ~ 1,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = "logistic"
)
mod0$cdf

## -----------------------------------------------------------------------------
aicmod0 <- AIC(mod0)

## -----------------------------------------------------------------------------
mod11 <- discrete_cm(
  formula = choice ~ hinc[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = "logistic"
)
mod11$cdf

## -----------------------------------------------------------------------------
aicmod11 <- AIC(mod11)

## -----------------------------------------------------------------------------
mod12 <- discrete_cm(
  formula = choice ~ psize[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  data = TravelChoice,
  cdf = "logistic"
)
mod12$cdf

## -----------------------------------------------------------------------------
aicmod12 <- AIC(mod12)

## -----------------------------------------------------------------------------
mod13 <- discrete_cm(
  formula = choice ~ gc,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc"),
  data = TravelChoice,
  cdf = "logistic"
)
mod13$cdf

## -----------------------------------------------------------------------------
aicmod13 <- AIC(mod13)

## -----------------------------------------------------------------------------
mod14 <- discrete_cm(
  formula = choice ~ ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = "logistic"
)
mod14$cdf

## -----------------------------------------------------------------------------
aicmod14 <- AIC(mod14)

## -----------------------------------------------------------------------------
aic_values_1 <- c(aicmod11, aicmod12, aicmod13, aicmod14)
AIC1 <- min(aic_values_1)
which_min_AIC1 <- which.min(aic_values_1)
which_min_AIC1

## -----------------------------------------------------------------------------
AIC0_is_smaller_than_AIC1 <- AIC1 < aicmod0
AIC0_is_smaller_than_AIC1

## -----------------------------------------------------------------------------
mod21 <- discrete_cm(
  formula = choice ~ hinc[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

## -----------------------------------------------------------------------------
aicmod21 <- AIC(mod21)

## -----------------------------------------------------------------------------
mod22 <- discrete_cm(
  formula = choice ~ psize[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

## -----------------------------------------------------------------------------
aicmod22 <- AIC(mod22)

## -----------------------------------------------------------------------------
mod23 <- discrete_cm(
  formula = choice ~ gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

## -----------------------------------------------------------------------------
aicmod23 <- AIC(mod23)

## -----------------------------------------------------------------------------
aic_values_2 <- c(aicmod21, aicmod22, aicmod23)
AIC2 <- min(aic_values_2)
which_min_AIC2 <- which.min(aic_values_2)
which_min_AIC2

## -----------------------------------------------------------------------------
AIC2_is_greater_than_AIC1 <- AIC2 > AIC1
AIC2_is_greater_than_AIC1

## -----------------------------------------------------------------------------
mod231 <- discrete_cm(
  formula = choice ~ gc + ttme + hinc[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)
aicmod231 <- AIC(mod231)

## -----------------------------------------------------------------------------
mod232 <- discrete_cm(
  formula = choice ~ gc + ttme + psize[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)
aicmod232 <- AIC(mod232)

## -----------------------------------------------------------------------------
AIC3 <- min(aicmod231, aicmod232)
which_min_AIC3 <- which.min(c(aicmod231, aicmod232))
which_min_AIC3

## -----------------------------------------------------------------------------
AIC3_is_smaller_than_AIC2 <- AIC3 < AIC2
AIC3_is_smaller_than_AIC2

## -----------------------------------------------------------------------------
mod2321 <- discrete_cm(
  formula = choice ~ gc + ttme + psize[air] + hinc[air],
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)
aicmod2321 <- AIC(mod2321)

## -----------------------------------------------------------------------------
aicmod2321_is_smaller_than_AIC3 <- aicmod2321 < AIC3
aicmod2321_is_smaller_than_AIC3

## -----------------------------------------------------------------------------
modc0 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

aicmodc0 <- AIC(modc0)
aicmodc0

modc11 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc"),
  data = TravelChoice,
  cdf = "logistic"
)

aicmodc11 <- AIC(modc11)

modc12 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

aicmodc12 <- AIC(modc12)

modc13 <- discrete_cm(
  formula = choice ~ hinc[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

aicmodc13 <- AIC(modc13)

modc14 <- discrete_cm(
  formula = choice ~ psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)

aicmodc14 <- AIC(modc14)

## -----------------------------------------------------------------------------
aic_values_backward <- c(aicmodc11, aicmodc12, aicmodc13, aicmodc14)
AICC1 <- min(aic_values_backward)
which_min_AICC1 <- which.min(aic_values_backward)
which_min_AICC1

## -----------------------------------------------------------------------------
AICC1_is_smaller_than_aicmodc0 <- AICC1 < aicmodc0
AICC1_is_smaller_than_aicmodc0

