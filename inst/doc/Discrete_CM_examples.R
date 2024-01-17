## -----------------------------------------------------------------------------
# devtools::load_all()
library(GLMcat)

## -----------------------------------------------------------------------------
data("TravelChoice")
head(TravelChoice)
str(TravelChoice)

## -----------------------------------------------------------------------------
exp_8.4 <- discrete_cm(
  formula = choice ~ hinc + gc + invt,
  case_id = "indv",
  alternatives = "mode",
  reference = "air",
  data = TravelChoice,
  alternative_specific = c("gc", "invt"),
  cdf = "logistic")
summary(exp_8.4)

## -----------------------------------------------------------------------------
(constant_model <- discrete_cm(
  formula = choice ~ 1 ,
  case_id = "indv",
  alternatives = "mode",
  reference = c("air", "train", "bus", "car"),
  data = TravelChoice,
  cdf = "logistic"
))

(car_0 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = c("air", "train", "bus", "car"),
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
))

## -----------------------------------------------------------------------------
mod_1 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "air",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = "logistic"
)
logLik(mod_1)

## -----------------------------------------------------------------------------
mod_2 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "bus",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = list("student",30)
)
logLik(mod_2)

## -----------------------------------------------------------------------------
mod_3 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "car",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = list("student",0.2)
)
logLik(mod_3)

## -----------------------------------------------------------------------------
mod_4 <- discrete_cm(
  formula = choice ~ hinc[air] + psize[air] + gc + ttme,
  case_id = "indv",
  alternatives = "mode",
  reference = "train",
  alternative_specific = c("gc", "ttme"),
  data = TravelChoice,
  cdf = list("student",1.35)
)
logLik(mod_4)

