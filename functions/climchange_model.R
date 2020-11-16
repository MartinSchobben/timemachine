#-------------------------------------------------------------------------------
# Tidymodel exercise
#-------------------------------------------------------------------------------

library(tidyverse)
library(rlang)

# the simplest model https://rpubs.com/mengxu/exponential-model

curve_fit <- function(df, time, proxy) {

  time <- enquo(time)
  proxy <- enquo(proxy)

  # rescale age
  df <- mutate(
    df,
    sc.age = -({{time}} - max({{time}})),
    sc.proxy = ({{proxy}} - (min({{proxy}}) - 0.1))
    )

  # carrying capacity (saturation point) upper asymptotic bound
  K <- max(pull(df, sc.proxy))
  P0 <- filter(df, sc.age == min(pull(df, sc.age))) %>% pull(sc.proxy)

  # Estimate the rest parameters using a linear model
  model.0 <- lm(log(sc.proxy)~sc.age, data = df)

  beta.0 <- coef(model.0)[2]
  alpha.0 <- exp(coef(model.0)[1])

  # Starting parameters exp
  start <- list(alpha = alpha.0, beta = beta.0)

  # Starting parameters logistic
  xmid <- -P0 / beta.0
  scal <- -1 / beta.0

  model_lm <- lm(sc.proxy~sc.age, df)
  model_exp <- nls(sc.proxy ~ alpha * exp(beta * sc.age), data = df, start = start)
  model_logistic <- try(nls(sc.proxy ~ SSlogis(sc.age, K, xmid, scal), data = df), silent = TRUE)
  model_ls <- lst(model_lm, model_exp, model_logistic)

  # extract model sigma
  sigma_vc <- map_dbl(model_ls, possibly(function(x) {pull(broom::glance(x), "sigma")}, NA_real_))
  sigma <- sigma_vc[which(sigma_vc == min(sigma_vc, na.rm = TRUE))]
  name_model <- names(sigma)
  # select model with lowest sigma
  model <-  model_ls[[name_model]]
  expr_formula <- form_generator(model, name_model)

  if(max(pull(df, {{time}})) > 0.1) int <- 10^-3 else int <- 10^-5

  Age <- seq(min(df$sc.age), max(df$sc.age), int)
  pred <- predict(model, list(sc.age = Age)) + (min(pull(df, {{proxy}})) - 0.1)

  return(lst(df = tibble(Age = max(pull(df, {{time}})) - Age, Proxy = pred), form = expr_formula, sel_mdl = names(sigma), model = model))
}



form_generator <- function(model, type){

  coefs <- coef(model)
  if (type == "model_logistic"){
    coefs[3] <- coefs[3]/1
    coefs <- sapply(coefs , sprintf, fmt = "%.1e") %>% unname()
    a <- coefs[1]
    b <- coefs[3]
  } else {
      coefs <- sapply(coefs , sprintf, fmt = "%.1e") %>% unname()
      a <- coefs[1]
      b <- coefs[2]
      }

  switch(
    type,
    model_lm = paste0("$$Temp = ",a ," + ",b ,"Age$$"),
    model_exp = paste0("$$Temp = ",a ,"\\exp^{",b ,"Age}$$"),
    model_logistic = paste0("$$Temp = \\frac{",a ,"}{1 + \\exp^{",b ,"Age}}$$")
    )

}


