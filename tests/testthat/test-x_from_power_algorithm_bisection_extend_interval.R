library(testthat)

test_that("extend_interval: n", {

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: s
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  progress = !is_testing(),
                  iseed = 1234)
out_by <- as.power4test_by_n(out)
f0 <- gen_objective(out,
                    x = "n",
                    pop_es_name = NULL,
                    target_power = .80,
                    ci_level = .95,
                    progress = !is_testing(),
                    digits = 3,
                    nrep = 20,
                    R = NULL,
                    what = "point",
                    simulation_progress = !is_testing(),
                    save_sim_all = TRUE,
                    store_output = TRUE)
set.seed(1234)
f.lower <- f0(20)
f.upper <- f0(200)
as.numeric(f.lower)
as.numeric(f.upper)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 20,
                       upper = 200,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 1000,
                       extendInt = "no",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             1,
             ignore_attr = TRUE)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 20,
                       upper = 200,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 1000,
                       extendInt = "yes",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             0,
             ignore_attr = TRUE)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 20,
                       upper = 200,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 1000,
                       extendInt = "downX",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             3,
             ignore_attr = TRUE)

set.seed(12)
f.lower <- f0(1200)
f.upper <- f0(1250)
as.numeric(f.lower)
as.numeric(f.upper)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 1200,
                       upper = 1250,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 1000,
                       extendInt = "no",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             1,
             ignore_attr = TRUE)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 1200,
                       upper = 1250,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 5000,
                       extendInt = "yes",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             0,
             ignore_attr = TRUE)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 1200,
                       upper = 1250,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 1000,
                       extendInt = "downX",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             0,
             ignore_attr = TRUE)

chk <- extend_interval(f = f0,
                       nrep = attr(out, "args")$nrep,
                       x_type = "n",
                       lower = 1200,
                       upper = 1250,
                       f.lower = f.lower,
                       f.upper = f.upper,
                       lower_hard = 10,
                       upper_hard = 1000,
                       extendInt = "upX",
                       trace = !is_testing(),
                       by_x_1 = out_by)
expect_equal(chk$extend_status,
             2,
             ignore_attr = TRUE)
})
