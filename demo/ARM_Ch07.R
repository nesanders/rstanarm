# loads packages, creates ROOT, SEED, and DATA_ENV
demo("SETUP", package = "rstanarm", verbose = FALSE, echo = FALSE, ask = FALSE)

source(paste0(ROOT, "ARM/Ch.7/congress.data.R"), local = DATA_ENV, verbose = FALSE)

# The stuff in sections 7.0 -- 7.2 is not very relevant 

post1 <- stan_lm(vote_88 ~ vote_86 + incumbency_88, data = DATA_ENV, 
                 prior = R2(0.9, what = "mean"), seed = SEED,
                 control = list(adapt_delta = 0.99, max_treedepth = 11))
post1 # badly underfitting
y_tilde <- posterior_predict(post1) # incumbency_90 is not available
summary(rowSums(y_tilde > 0.5))

source(paste0(ROOT, "ARM/Ch.6/wells.data.R"), local = DATA_ENV, verbose = FALSE)
post2 <- stan_glm(switc ~ I(dist / 100), data = DATA_ENV, seed = SEED,
                  family = binomial(link = "logit"))
table(c(posterior_predict(post2))) / (4000 * 3020)

# the compound model is not good because it assumes the two errors are independent
# but rstanarm does not currently support Heckman models

ANSWER <- tolower(readline("Do you want to remove the objects this demo created? (y/n) "))
if (ANSWER != "n") {
  rm(y_tilde, ANSWER)
  # removes stanreg and loo objects, plus what was created by STARTUP
  demo("CLEANUP", package = "rstanarm", verbose = FALSE, echo = FALSE, ask = FALSE)
}