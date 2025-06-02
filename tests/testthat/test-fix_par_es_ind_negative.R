# Test

test_that("fix_par_es: indirect, negative", {

model <-
"
m1 ~ x1
m2 ~ m1
m3 ~ x2
y ~ m2 + m3 + x1
y2 ~ m3 + x2
"

model_es <- c("m1 ~ x1" = "-m",
              "m2 ~ m1" = "s",
              "y ~ m3" = "l",
              ".beta." = "m",
              ".cov." = "-s",
              ".ind.(x1-> m1->m2->y)" = "mi",
              ".ind.(y~m3~ x2)" = "-li")

expect_error(fix_par_es(model_es, model))

model_es <- c("m1 ~ x1" = "-m",
              "m2 ~ m1" = "s",
              "y ~ m3" = "l",
              ".beta." = "m",
              ".cov." = "-s",
              ".ind.(x1-> m1->m2->y)" = "mi",
              ".ind.(y~m3~ x2)" = "-.30")

expect_error(fix_par_es(model_es, model))

})
