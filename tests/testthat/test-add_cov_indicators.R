skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("add_cov", {

model <-
"
m ~ x + z + x:z
y ~ m + x + w + m:w
"

model_es <-
"
m ~ x:z: s
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

model_es_mg <-
"
m ~ x:z: [s, l]
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

out <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es,
            n = 100,
            number_of_indicators = c(y = 4,
                                     x = 3,
                                     z = 5),
            reliability = c(x = .70,
                            z = .80,
                            y = .60),
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())

out_mg <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es_mg,
            n = 100,
            number_of_indicators = c(y = 4,
                                     w = 3,
                                     x = 5),
            reliability = c(x = .70,
                            w = .80,
                            y = .60),
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())

sim_out <- out$sim_all[[1]]
names(sim_out)
model_original <- sim_out$model_original
model_final <- sim_out$model_final
expect_true(all(c("y2", "x3", "z5")
                %in%
                colnames(sim_out$mm_lm_dat_out)))
if (utils::packageVersion("lavaan") <= "0.6.19") {
  expect_true(is.data.frame(attr(model_final, "ptable")))
}

sim_out_mg <- out_mg$sim_all[[1]]
model_original_mg <- sim_out_mg$model_original
model_final_mg <- sim_out_mg$model_final
expect_true(all(c("w2", "x2", "y4")
                %in%
                colnames(sim_out_mg$mm_lm_dat_out)))
if (utils::packageVersion("lavaan") <= "0.6.19") {
  expect_true(is.data.frame(attr(model_final_mg, "ptable")))
}

})
