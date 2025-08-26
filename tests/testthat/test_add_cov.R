skip("WIP")

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
            iseed = 1234,
            parallel = FALSE,
            progress = FALSE)

out_mg <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es_mg,
            n = 100,
            iseed = 1234,
            parallel = FALSE,
            progress = FALSE)



sim_out <- out$sim_all[[1]]
names(sim_out)
model_original <- sim_out$model_original
fit <- sim_out$fit0

pt <- ptable_pop(
        model = model,
        pop_es = model_es
      )

pt_mg <- ptable_pop(
        model = model,
        pop_es = model_es_mg
      )

pt_fixed <- pt_with_int(
              ptable = pt,
              model = model
            )
pt_fixed_mg <- pt_with_int(
              ptable = pt_mg,
              model = model
            )

sim_out <- out$sim_all[[1]]
sim_out_mg <- out_mg$sim_all[[1]]

tmpnames <- setdiff(lavNames(pt, "ov"), lavNames(pt, "ov.interaction"))
dat <- sim_out$mm_lm_dat_out[, tmpnames]

fit2 <- sem(pt_fixed,
            dat,
            fixed.x = FALSE)
fit3 <- sem(pt_fixed,
            dat + 2,
            fixed.x = FALSE)
expect_equal(fitMeasures(fit2, "chisq"),
             fitMeasures(fit3, "chisq"))

fit2b <- sem(model,
             dat,
             fixed.x = FALSE)
fit3b <- sem(model,
             dat + 2,
             fixed.x = FALSE)
expect_false(fitMeasures(fit2b, "chisq") == fitMeasures(fit3b, "chisq"))

tmpnames <- setdiff(lavNames(pt_mg, "ov"), lavNames(pt_mg, "ov.interaction"))
dat_mg <- sim_out_mg$mm_lm_dat_out[, c(tmpnames, "group")]
dat_mg_shifted <- dat_mg
dat_mg_shifted[, tmpnames] <- dat_mg_shifted[, tmpnames] + 2

fit2_mg <- sem(pt_fixed_mg,
               dat_mg,
               group = "group",
               fixed.x = FALSE)
fit3_mg <- sem(pt_fixed_mg,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE)
expect_equal(fitMeasures(fit2_mg, "chisq"),
             fitMeasures(fit3_mg, "chisq"))

fit2b_mg <- sem(model,
               dat_mg,
               group = "group",
               fixed.x = FALSE)
fit3b_mg <- sem(pt_fixed_mg,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE)
expect_false(fitMeasures(fit2b_mg, "chisq") == fitMeasures(fit3b_mg, "chisq"))

})
