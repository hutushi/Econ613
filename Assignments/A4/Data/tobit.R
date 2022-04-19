dat3 <- dat2 %>%
  filter(!is.na(work_exp)) %>%
  filter(!is.na(ysch)) %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(math)) %>%
  filter(!is.na(CV_URBAN.RURAL_2019)) %>%
  mutate(income = YINC_1700_2019)

library(AER)
tobit <- tobit(income ~ work_exp + ysch + gender + math + CV_URBAN.RURAL_2019, left = -Inf, right = 100000, data = dat3)
summary(tobit)

olstobit <- lm(income ~ work_exp + ysch + gender + math + CV_URBAN.RURAL_2019, data =  dat3)

X <- model.matrix(olstobit)

init <- c(coef(tobit), log_sigma = log(summary(olstobit)$sigma))

tobit.ll <- function(par, X, y, ul = -Inf, ll = Inf) {
  sigma = exp(par[length(par)])
  beta = par[-length(par)]
  
  if(!is.infinite(ll)){
    limit = ll
    indicator = y > ll
  } else {
    limit = ul
    indicator = y < ul
  }
  
  est = X %*% beta
  
  ll = sum(indicator * log((1/sigma) * dnorm((y-est)/sigma))) + sum((1-indicator) * log(pnorm((est-limit)/sigma, lower = is.finite(ll))))
  
  -ll
}

fit_tobit = optim(
  par = init,
  tobit.ll,
  y  = dat3$income,
  X  = X,
  ul = 100000,
  method  = 'BFGS',
  control = list(maxit = 16000, reltol = 1)
)

?optim
fit_tobit$par
olstobit$coefficients
tobit$coefficients


initmod = lm(income ~ work_exp + ysch + gender + math + CV_URBAN.RURAL_2019, data = dat3)
X = model.matrix(initmod)
init = c(coef(initmod), log_sigma = log(summary(initmod)$sigma))


tobit_ll <- function(par, X, y, ul = -Inf, ll = Inf) {

  sigma = exp(par[length(par)]) 
  beta  = par[-length(par)]
  
  if (!is.infinite(ll)) {
    limit = ll
    indicator = y > ll
  } else {
    limit = ul
    indicator = y < ul
  }
  
  # linear predictor
  lp = X %*% beta
  
  # log likelihood
  ll = sum(indicator * log((1/sigma)*dnorm((y-lp)/sigma)) ) + 
    sum((1-indicator) * log(pnorm((lp-limit)/sigma, lower = is.infinite(ll))))
  
  -ll
}


fit_tobit = optim(
  par = init,
  tobit_ll,
  y  = dat3$income,
  X  = X,
  ul = 100000,
  method  = 'BFGS',
  control = list(maxit = 2000, reltol = 1e-15)
)

fit_tobit$par
initmod$coefficients

