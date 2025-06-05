library(testthat)
suppressMessages(library(lavaan))

test_that("power4test", {

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

out1 <- power4test(nrep = 2,
                   model = model_simple_med,
                   pop_es = model_simple_med_es,
                   n = 199,
                   do_the_test = FALSE,
                   iseed = 1234,
                   progress = FALSE)

out2 <- power4test(nrep = 2,
                   model = model_simple_med,
                   pop_es = model_simple_med_es,
                   n = 199,
                   number_of_indicators = k,
                   reliability = rel,
                   do_the_test = FALSE,
                   iseed = 1234,
                   progress = FALSE)

chk1a <- pool_sim_data(out1)
chk1b <- pool_sim_data(out1$sim_all)
expect_identical(chk1a,
                 chk1b)

chk2a <- pool_sim_data(out2, as_list = TRUE)
chk2b <- pool_sim_data(out2$sim_all, as_list = TRUE)
expect_identical(chk2a,
                 chk2b)

expect_true(is.data.frame(chk1a))
expect_true(is.list(chk2a) && !is.data.frame(chk2a))

})

