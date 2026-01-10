library(testthat)
suppressMessages(library(lavaan))

test_that("sim_data: one lv", {

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
                     n = 100,
                     number_of_indicators = c(m = 3),
                     reliability = c(m = .70),
                     iseed = 1234)

chk <- c("y", "x", "m1", "m2", "m3")
expect_setequal(chk,
                colnames(data_all[[1]]$mm_lm_dat_out))

})
