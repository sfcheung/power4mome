library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test: mg", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
y ~ m: l
m ~ x:
  - nil
  - s
y ~ x: nil
"

# Generate the data

out1 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = c(100, 50),
          test_fun = test_parameters,
          test_args = list(par = c("m~x", "m~x.g2")),
          progress = !is_testing(),
          iseed = 1234
        )

out2 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = 100,
          test_fun = test_parameters,
          test_args = list(par = c("m~x", "m~x.g2")),
          progress = !is_testing(),
          iseed = 1234,
          n_ratio = c(1, .5)
        )

dat1 <- pool_sim_data(out1)
dat2 <- pool_sim_data(out2)

expect_equal(table(dat1$group),
             table(dat2$group))

out3 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = 100,
          test_fun = test_parameters,
          test_args = list(par = c("m~x", "m~x.g2")),
          progress = !is_testing(),
          iseed = 1234,
          n_ratio = 1
        )

dat3 <- pool_sim_data(out3)
expect_true(length(unique(table(dat3$group))) == 1)

expect_error(power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = 100,
          test_fun = test_parameters,
          test_args = list(par = c("m~x", "m~x.g2")),
          progress = !is_testing(),
          iseed = 1234,
          n_ratio = c(1, 2, 3)
        ),
        "n_ratio")

out4 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = 100,
          test_fun = test_parameters,
          test_args = list(par = c("m~x", "m~x.g2")),
          progress = !is_testing(),
          iseed = 1234,
          n_ratio = c(.5, 1)
        )

dat4 <- pool_sim_data(out4)

expect_true(all.equal(as.vector(table(dat4$group)),
                      c(100, 200)))

outby <- power4test_by_n(
            out2,
            n = c(150, 200),
            by_seed = 4567
          )

expect_equal(as.vector(table(pool_sim_data(outby[[1]])$group)),
             c(300, 150))
expect_equal(as.vector(table(pool_sim_data(outby[[2]])$group)),
             c(400, 200))

})

