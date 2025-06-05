library(testthat)
suppressMessages(library(lavaan))

test_that("power4test", {

simple_amp <- function(complete_data,
                       prop = .10) {
  out0 <- complete_data
  if ((prop > 0) && (prop < 1)) {
    n <- nrow(complete_data)
    p <- ncol(complete_data)
    nstart <- n * p
    n0 <- sample.int(nstart, size = ceiling(nstart * prop))
    m0 <- matrix(FALSE,
                 nrow = n,
                 ncol = p)
    m0[n0] <- TRUE
    out0[m0] <- NA
  }
  list(sth_missing = out0)
}

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 3,
       m = 3,
       x = 3)
rel <- c(y = .70,
         m = .70,
         x = .70)

power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 200,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 process_data = list(fun = simple_amp,
                                                     sim_data_name = "complete_data",
                                                     processed_data_name = "sth_missing",
                                                     args = list(prop = .10)),
                                 R = 50,
                                 do_the_test = FALSE,
                                 iseed = 1234)
chk <- power_all_sim_only$sim_all[[1]]$mm_lm_dat_out
expect_equal(sum(is.na(chk)) / (nrow(chk) * ncol(chk)),
             .1)

power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 200,
                                 process_data = list(fun = simple_amp,
                                                     sim_data_name = "complete_data",
                                                     processed_data_name = "sth_missing",
                                                     args = list(prop = .20)),
                                 R = 50,
                                 do_the_test = FALSE,
                                 iseed = 1234)
chk <- power_all_sim_only$sim_all[[1]]$mm_lm_dat_out
expect_equal(sum(is.na(chk)) / (nrow(chk) * ncol(chk)),
             .2)

})

