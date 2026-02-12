library(testthat)

test_that("Power by es: MG", {

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

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       parallel = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(op = "~"))

rejection_rates(test_out)

out1 <- power4test(
          object = test_out,
          pop_es = c("y ~ m.g2" = .30)
        )

out2 <- power4test(
          object = test_out,
          pop_es = c("y ~ m" = "s")
        )

pt1 <- out1$sim_all[[1]]$ptable
pt2 <- out2$sim_all[[1]]$ptable

expect_equal(pt1$start[(pt1$lhs == "y") & (pt1$op == "~") & (pt1$rhs == "m") & (pt1$group == 2)],
             .3)
expect_equal(pt2$start[(pt2$lhs == "y") & (pt2$op == "~") & (pt2$rhs == "m") & (pt2$group == 1)],
             .1)

out1 <- power4test_by_es(
            test_out,
            pop_es_name = "y ~ m",
            pop_es_values = c(.15, .25),
            by_seed = 1357,
            progress = !is_testing())

pt1 <- out1[[1]]$sim_all[[1]]$ptable
pt2 <- out1[[2]]$sim_all[[1]]$ptable

expect_equal(pt1$start[(pt1$lhs == "y") & (pt1$op == "~") & (pt1$rhs == "m") & (pt1$group == 1)],
             .15)
expect_equal(pt2$start[(pt2$lhs == "y") & (pt2$op == "~") & (pt2$rhs == "m") & (pt2$group == 1)],
             .25)

out2 <- power4test_by_es(
            test_out,
            pop_es_name = "y ~ m.g2",
            pop_es_values = c(.35, .45),
            by_seed = 1357,
            progress = !is_testing())

pt1 <- out2[[1]]$sim_all[[1]]$ptable
pt2 <- out2[[2]]$sim_all[[1]]$ptable

expect_equal(pt1$start[(pt1$lhs == "y") & (pt1$op == "~") & (pt1$rhs == "m") & (pt1$group == 2)],
             .35)
expect_equal(pt2$start[(pt2$lhs == "y") & (pt2$op == "~") & (pt2$rhs == "m") & (pt2$group == 2)],
             .45)

})
