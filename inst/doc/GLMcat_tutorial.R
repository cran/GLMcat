## ---- include = FALSE---------------------------------------------------------
# devtools::load_all()
library(GLMcat)

## -----------------------------------------------------------------------------
data("DisturbedDreams")
summary(DisturbedDreams)

## -----------------------------------------------------------------------------
mod_ref_log_c <- GLMcat(
  formula = Level ~ Age, ratio = "reference",
  cdf = "logistic", ref_category = "Very.severe",
  data = DisturbedDreams
)

## -----------------------------------------------------------------------------
summary(mod_ref_log_c)

## -----------------------------------------------------------------------------
nobs_glmcat(mod_ref_log_c)

## -----------------------------------------------------------------------------
coef(mod_ref_log_c)

## -----------------------------------------------------------------------------
logLik(mod_ref_log_c)

## -----------------------------------------------------------------------------
AIC(mod_ref_log_c)
BIC(mod_ref_log_c)

## -----------------------------------------------------------------------------
# Random observations
set.seed(13)
ind <- sample(x = 1:nrow(DisturbedDreams), size = 3)
# Probabilities
predict_glmcat(mod_ref_log_c, data = DisturbedDreams[ind, ], type = "prob")
# Linear predictor
predict_glmcat(mod_ref_log_c, data = DisturbedDreams[ind, ], type = "linear.predict")

## -----------------------------------------------------------------------------
# New data
Age <- c(5, 9.5, 15)
predict_glmcat(mod_ref_log_c, data = Age, type = "prob")

## -----------------------------------------------------------------------------
mod2 <- GLMcat(
  formula = Level ~ Age, cdf = "logistic",
  parallel = "Age", ref_category = "Very.severe",
  data = DisturbedDreams
)
summary(mod2)
logLik(mod2)

## -----------------------------------------------------------------------------
mod3 <- GLMcat(
  formula = Level ~ Age, ref_category = "Very.severe",
  data = DisturbedDreams, cdf = student.glmcat(df = 0.5)
)
summary(mod3)
logLik(mod3)

## -----------------------------------------------------------------------------
logLik(mod_ref_log_c) # recall (ref,logit,com)
mod_adj_log_c <- GLMcat(
  formula = Level ~ Age, ratio = "adjacent",
  data = DisturbedDreams, cdf = "logistic"
)
logLik(mod_adj_log_c)
summary(mod_adj_log_c)

## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='tex'----
#  library(xtable)
#  print(xtableMatharray(matrix(c(1, -1, 0, 0, 1, -1, 0, 0, 1), nrow = 3)), type = "latex")

## -----------------------------------------------------------------------------
mod_adj_cau_c <- GLMcat(
  formula = Level ~ Age,
  ratio = "adjacent", cdf = "cauchy",
  categories_order = c("Not.severe", "Severe.1", "Severe.2", "Very.severe"),
  data = DisturbedDreams
)
logLik(mod_adj_cau_c)
summary(mod_adj_cau_c)

## -----------------------------------------------------------------------------
mod_adj_cau_c_rev <- GLMcat(
  formula = Level ~ Age,
  ratio = "adjacent", cdf = "cauchy",
  categories_order = c("Very.severe", "Severe.2", "Severe.1", "Not.severe"),
  data = DisturbedDreams
)
logLik(mod_adj_cau_c_rev)
summary(mod_adj_cau_c_rev)

## -----------------------------------------------------------------------------
adj_gumbel_p <- GLMcat(
  formula = Level ~ Age,
  ratio = "adjacent", cdf = "gumbel",
  categories_order = c("Not.severe", "Severe.1", "Severe.2", "Very.severe"),
  parallel = c("(Intercept)", "Age"),
  data = DisturbedDreams
)
logLik(adj_gumbel_p)
summary(adj_gumbel_p)

## -----------------------------------------------------------------------------
adj_gompertz_rev <- GLMcat(
  formula = Level ~ Age,
  ratio = "adjacent", cdf = "gompertz",
  categories_order = c("Very.severe", "Severe.2", "Severe.1", "Not.severe"),
  parallel = c("(Intercept)", "Age"),
  data = DisturbedDreams
)
logLik(adj_gompertz_rev)
summary(adj_gompertz_rev)

## -----------------------------------------------------------------------------
seq_probit_c <- GLMcat(
  formula = Level ~ Age,
  ratio = "sequential", cdf = "normal",
  data = DisturbedDreams
)
logLik(seq_probit_c)
summary(seq_probit_c)

## -----------------------------------------------------------------------------
cum_log_co <- GLMcat(
  formula = Level ~ Age,
  cdf = "logistic",
  ratio = "cumulative",
  data = DisturbedDreams
)
logLik(cum_log_co)
summary(cum_log_co)

## -----------------------------------------------------------------------------
cum_log_co_e <- GLMcat(
  formula = Level ~ Age,
  cdf = "logistic",
  ratio = "cumulative",
  data = DisturbedDreams,
  parallel = "Age",
  threshold = "equidistant",
)
logLik(cum_log_co_e)
summary(cum_log_co_e)

## -----------------------------------------------------------------------------
cum_log_c <- GLMcat(
  formula = Level ~ Age,
  cdf = student.glmcat(df = 0.8),
  ratio = "cumulative",
  data = DisturbedDreams,
  control = control.glmcat(beta_init = coef(cum_log_co))
)
logLik(cum_log_c)
summary(cum_log_c)

## -----------------------------------------------------------------------------
cum_gom_p <- GLMcat(
  formula = Level ~ Age,
  cdf = "gompertz",
  ratio = "cumulative",
  data = DisturbedDreams,
  parallel = "Age"
)
logLik(cum_gom_p)
summary(cum_gom_p)

seq_gom_p <- GLMcat(
  formula = Level ~ Age,
  cdf = "gompertz",
  ratio = "sequential",
  data = DisturbedDreams,
  parallel = "Age"
)
logLik(seq_gom_p)
summary(seq_gom_p)

