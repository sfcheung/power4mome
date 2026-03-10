library(testthat)
suppressMessages(library(lavaan))

test_that("sim_data: lambda vary", {

mod <-
"
m ~ x
y ~ m + x
"

pop_es <-
"
m ~ x: s
y ~ m: s
y ~ x: s
"

data_all <- sim_data(nrep = 1,
                     model = mod,
                     pop_es = pop_es,
                     n = 10000,
                     number_of_indicators = c(m = 3),
                     reliability = c(m = .70),
                     loading_difference = c(m = .10),
                     reference = c(m = "strongest"),
                     progress = !is_testing(),
                     parallel = FALSE,
                     iseed = 1234)

chk <- c("y", "x", "m1", "m2", "m3")
expect_setequal(chk,
                colnames(data_all[[1]]$mm_lm_dat_out))

tmp <- pool_sim_data(data_all)
mod_m <- "m =~ m1 + m2 + m3"
fit <- cfa(mod_m, tmp)
std <- standardizedSolution(fit)[1:3, "est.std"]
expect_true(which.max(std) == 1)

data_all <- sim_data(nrep = 1,
                     model = mod,
                     pop_es = pop_es,
                     n = 10000,
                     number_of_indicators = c(m = 3, x = 5),
                     reliability = c(x = .8, m = .70),
                     loading_difference = c(x = .05, m = .10),
                     reference = c(m = "medium", x = "weakest"),
                     progress = !is_testing(),
                     iseed = 1234,
                     parallel = FALSE)

chk <- c("y", "x1", "x2", "x4", "x3", "x5", "m1", "m2", "m3")
expect_setequal(chk,
                colnames(data_all[[1]]$mm_lm_dat_out))

tmp <- pool_sim_data(data_all)

mod_m <- "m =~ m1 + m2 + m3"
fit <- cfa(mod_m, tmp)
std <- standardizedSolution(fit)[1:3, "est.std"]
expect_false(which.max(std) == 1)
expect_false(which.min(std) == 1)

mod_x <- "x =~ x1 + x2 + x3 + x4 + x5"
fit <- cfa(mod_x, tmp)
std <- standardizedSolution(fit)[1:5, "est.std"]
expect_true(which.min(std) == 1)

expect_equal(data_all[[1]]$lambda$x,
             lambda_from_reliability(p = 5, omega = .80, d = .05, ref = "weakest"))
expect_equal(data_all[[1]]$lambda$m,
             lambda_from_reliability(p = 3, omega = .70, d = .10, ref = "medium"))

})

test_that("sim_data: lambda vary: multigroup", {

mod <-
"
m ~ x
y ~ m + x
"

pop_es <-
"
m ~ x:
 - s
 - m
y ~ m: s
y ~ x: s
"

data_all <- sim_data(nrep = 1,
                     model = mod,
                     pop_es = pop_es,
                     n = 10000,
                     number_of_indicators = c(m = 3),
                     reliability = c(m = .70),
                     loading_difference = c(m = .10),
                     reference = c(m = "strongest"),
                     progress = !is_testing(),
                     parallel = FALSE,
                     iseed = 1234)

expect_equal(data_all[[1]]$lambda$Group1$m,
             lambda_from_reliability(p = 3, omega = .70, d = .10, ref = "strongest"))

})
