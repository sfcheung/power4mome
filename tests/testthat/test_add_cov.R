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

tmp <- m_moderated(model)

expect_equal(tmp,
             "m")

out <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es,
            n = 100,
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())

out_mg <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es_mg,
            n = 100,
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())



sim_out <- out$sim_all[[1]]
names(sim_out)
model_original <- sim_out$model_original
fit <- sim_out$fit0

pt <- ptable_pop(
        model = model,
        pop_es = model_es,
        add_cov_for_moderation = FALSE
      )

pt_mg <- ptable_pop(
        model = model,
        pop_es = model_es_mg,
        add_cov_for_moderation = FALSE
      )

pt_fixed <- pt_with_int(
              ptable = pt,
              model = model
            )
pt_fixed_mg <- pt_with_int(
              ptable = pt_mg,
              model = model
            )

pt_fixed2 <- ptable_pop(
        model = model,
        pop_es = model_es
      )

pt_fixed_mg2 <- ptable_pop(
        model = model,
        pop_es = model_es_mg
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
fit4 <- sem(pt_fixed2,
            dat + 2,
            fixed.x = FALSE)
expect_equal(fitMeasures(fit2, "chisq"),
             fitMeasures(fit3, "chisq"))
expect_equal(fitMeasures(fit2, "chisq"),
             fitMeasures(fit4, "chisq"))

fit2b <- sem(model,
             dat,
             fixed.x = FALSE)
fit3b <- sem(model,
             dat + 2,
             fixed.x = FALSE)
fit4b <- sem(pt,
             dat + 2,
             fixed.x = FALSE)
expect_false(fitMeasures(fit2b, "chisq") == fitMeasures(fit3b, "chisq"))
expect_false(fitMeasures(fit2b, "chisq") == fitMeasures(fit4b, "chisq"))

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
fit4_mg <- sem(pt_fixed_mg2,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE)
expect_equal(fitMeasures(fit2_mg, "chisq"),
             fitMeasures(fit3_mg, "chisq"))
expect_equal(fitMeasures(fit2_mg, "chisq"),
             fitMeasures(fit4_mg, "chisq"))

fit2b_mg <- sem(model,
               dat_mg,
               group = "group",
               fixed.x = FALSE)
fit3b_mg <- sem(pt_fixed_mg,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE)
fit2c_mg <- sem(model,
               dat_mg,
               group = "group",
               fixed.x = FALSE,
               meanstructure = FALSE)
fit4b_mg <- sem(pt_mg,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE,
               meanstructure = FALSE)
expect_false(fitMeasures(fit2b_mg, "chisq") == fitMeasures(fit3b_mg, "chisq"))
expect_false(fitMeasures(fit2c_mg, "chisq") == fitMeasures(fit4b_mg, "chisq"))

fix_cov_order <- function(pt) {
  for (i in seq_len(nrow(pt))) {
    if (pt$op[i] == "~~") {
      tmp <- sort(c(pt[i, "lhs"], pt[i, "rhs"]))
      pt[i, "lhs"] <- tmp[1]
      pt[i, "rhs"] <- tmp[2]
    }
  }
  pt
}

fit_extra <- sim_out$extra$fit
pt_extra <- parameterTable(fit_extra)

pt_extra <- fix_cov_order(pt_extra)
pt_fixed <- fix_cov_order(pt_fixed)

pt_extra$tmplabel <- lav_partable_labels(pt_extra)
pt_fixed$tmplabel <- lav_partable_labels(pt_fixed)
expect_equal(sort(pt_extra$tmplabel),
             sort(pt_fixed$tmplabel))

fit_extra_mg <- sim_out_mg$extra$fit
pt_extra_mg <- parameterTable(fit_extra_mg)

pt_extra_mg <- fix_cov_order(pt_extra_mg)
pt_fixed_mg <- fix_cov_order(pt_fixed_mg)

pt_extra_mg$tmplabel <- lav_partable_labels(pt_extra_mg)
pt_fixed_mg$tmplabel <- lav_partable_labels(pt_fixed_mg)
expect_equal(sort(pt_extra_mg$tmplabel),
             sort(pt_fixed_mg$tmplabel))

})
