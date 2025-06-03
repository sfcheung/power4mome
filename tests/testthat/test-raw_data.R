skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("User raw data", {

split_data <- function(x,
                       group) {
  out <- split(x, x[, group, drop = TRUE])
  out
}

mod1_pop <-
"
m ~ .3*x
y ~ .4*m + .1*x
"

dat1 <- simulateData(mod1_pop,
                     sample.nobs = 99,
                     seed = 2718)
set.seed(5678)
dat1$gp <- sample(c("gp1", "gp2"),
                    nrow(dat1),
                    replace = TRUE)
dat1x <- split_data(dat1, "gp")

mod2_pop <-
"
m ~ .3*x
y ~ .4*m + .1*x
m =~ .8*m_1 + .7*m_2 + .6*m_3 + .9*m4 + .6*m5 + .7*m6
x =~ .8*x_1 + .7*x_2 + .6*x_3 + .3*x4 + .7*x5
y =~ .8*y_1 + .7*y_2 + .6*y_3 + .9*y4
"

dat2 <- simulateData(mod2_pop,
                     sample.nobs = 199,
                     seed = 2718)

set.seed(2468)
dat2$gp <- sample(c("gp1", "gp2", "gp3"),
                    nrow(dat2),
                    replace = TRUE)
dat2x <- split_data(dat2, "gp")

mod1 <-
"
m ~ x
y ~ m + x
"

mod2 <-
"
m ~ x
y ~ m + x
m =~ m_1 + m_2 + m_3 + m4 + m5 + m6
x =~ x_1 + x_2 + x_3 + x4 + x5
y =~ y_1 + y_2 + y_3 + y4
"

# Test sim_data_raw_data_i()

data_i <- sim_data_raw_data_i(raw_data = dat1x,
                              raw_data_group = "gp",
                              seed = 1234)

data_i <- sim_data_raw_data_i(model = mod1,
                              raw_data = dat1x,
                              raw_data_group = "gp",
                              seed = 1234)
fit_i <- fit_model_i(data_i,
                     model = mod1)

data_i <- sim_data_raw_data_i(raw_data = list(dat1),
                              seed = 1234)

data_i <- sim_data_raw_data_i(model = mod1,
                              raw_data = list(dat1),
                              seed = 1234)
fit_i <- fit_model_i(data_i,
                     model = mod1)


})