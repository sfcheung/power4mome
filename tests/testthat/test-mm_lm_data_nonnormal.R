library(testthat)
suppressMessages(library(lavaan))

test_that("Generate nonnormal data", {

mod <-
"
m ~ x + w + x:w
y ~ m + x
"

set.seed(1234)
n <- 100000
pop_es <- c(".beta." = "m")

ptable <- ptable_pop(model = mod,
                     pop_es = pop_es)
mm_out <- model_matrices_pop(ptable)
mm_lm_out <- mm_lm(mm_out)

set.seed(1234)
mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                            n = 10000,
                            x_fun = list(x = list(rexp_rs),
                                         w = list(rbinary_rs,
                                                  p1 = .70)))
chk <- mm_lm_dat_out
head(chk)
expect_equal(length(table(chk[, "w"])),
             2)
expect_equal(chk[1:10, "x"] * chk[1:10, "w"],
             chk[1:10, "x:w"])


psi <- mm_lm_out$psi
all_vars <- colnames(psi)
x_raw <- gen_all_x(psi = psi,
                   n = 10000,
                   all_x = all_vars,
                   x_fun = list(x = list(rexp_rs),
                                 w = list(rbinary_rs,
                                          p1 = .70)))
chk <- x_raw
head(chk)
expect_equal(length(table(chk[, "w"])),
             2)
expect_equal(chk[1:10, "x"] * chk[1:10, "w"],
             chk[1:10, "x:w"])

x_raw <- gen_pure_x(psi = psi,
                    n = n,
                    x_fun = list(x = list(rexp_rs),
                                 w = list(rbinary_rs,
                                          p1 = .70)))
expect_equal(length(table(x_raw[, "w"])),
             2)

sim_out <- sim_data_i(ptable = ptable,
                      n = n,
                      seed = 1234,
                      x_fun = list(x = list(rexp_rs),
                                 w = list(rbinary_rs,
                                          p1 = .70)))
chk <- sim_out$mm_lm_dat_out
head(chk)
expect_equal(length(table(chk[, "w"])),
             2)
expect_equal(chk[1:10, "x"] * chk[1:10, "w"],
             chk[1:10, "x:w"])

sim_out <- sim_data(nrep = 2,
                    model = mod,
                    pop_es = pop_es,
                    n = n,
                    iseed = 1234,
                    x_fun = list(x = list(rexp_rs),
                               w = list(rbinary_rs,
                                        p1 = .70)),
                    progress = !is_testing())
chk <- sim_out[[2]]$mm_lm_dat_out
head(chk)
expect_equal(length(table(chk[, "w"])),
             2)
expect_equal(chk[1:10, "x"] * chk[1:10, "w"],
             chk[1:10, "x:w"])
})
