skip_on_cran()

library(testthat)

skip_if_not_installed("lmhelprs")

test_that("pwoer_curve", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = ".2")

sim_only <- power4test(nrep = 50,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 50,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234)

# By n

out1 <- power4test_by_n(sim_only,
                        nrep = 100,
                        test_fun = test_parameters,
                        test_args = list(par = "y~x"),
                        n = c(25, 50, 100, 150, 200, 250, 500),
                        by_seed = 1234)

get_rejection_rates_by_n(out1)

pout1 <- power_curve_x(out1,
                       verbose = TRUE)
pout1

print(pout1,
      data_used = TRUE)

plot(pout1)

# By es

out2 <- power4test_by_pop_es(sim_only,
                             nrep = 100,
                             test_fun = test_parameters,
                             test_args = list(par = "y~x"),
                             pop_es_name = "y ~ x",
                             pop_es_values = seq(0, .5, .05),
                             by_seed = 1234)

get_rejection_rates_by_pop_es(out2,
                              all_columns = TRUE)

pout2 <- power_curve_x(out2,
                       verbose = TRUE)
pout2

print(pout2,
      data_used = TRUE)

plot(pout2)

})
