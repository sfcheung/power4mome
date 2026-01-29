library(testthat)

skip_if_not_installed("lmhelprs")

test_that("indirect effect by lm", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_ind <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE),
                       progress = !is_testing())

(chk <- test_summary(test_ind))

fits <- lapply(sim_only$sim_all,
               function(x) x$extra$fit)
boot_out <- lapply(sim_only$sim_all,
                   function(x) x$extra$boot_out)
chk_outs <- mapply(manymome::indirect_effect,
                   fit = fits,
                   boot_out = boot_out,
                   MoreArgs = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   boot_ci = TRUE),
                  SIMPLIFY = FALSE)
chk_cis <- lapply(chk_outs,
                  confint)
chk_cis <- do.call(rbind, chk_cis)
chk_sigs <- (chk_cis[, 1] > 0) | (chk_cis[, 2] < 0)

expect_equal(chk[[1]]["sig"],
             mean(chk_sigs),
             ignore_attr = TRUE)
expect_equal(chk[[1]][c("cilo", "cihi")],
             colMeans(chk_cis),
             ignore_attr = TRUE)

})
