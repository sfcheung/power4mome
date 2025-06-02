# Test

test_that("fix_par_es: indirect, numeric", {

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
              ".ind.(x1-> m1->m2->y)" = ".25",
              ".ind.(y~m3~ x2)" = ".70")

tmp1 <- fix_par_es(model_es,
                   model)
tmp1_chk <- c(`m1 ~ x1` = ".25_3", `m2 ~ m1` = ".25_3", `y ~ m2` = ".25_3",
`y ~ m3` = ".70_2", `m3 ~ x2` = ".70_2", `y ~ x1` = "m", `y2 ~ m3` = "m",
`y2 ~ x2` = "m", `x1 ~~ x2` = "-s")

expect_equal(sort(tmp1),
             sort(tmp1_chk))

# Set the values

es_tmp1 <- set_pop(tmp1,
                   es1 = c("n" = .00,
                           "nil" = .00,
                           "s" = .10,
                           "m" = .30,
                           "l" = .50,
                           "si" = .141,
                           "mi" = .361,
                           "li" = .510),
                   es2 = c("n" = .00,
                           "nil" = .00,
                           "s" = .05,
                           "m" = .10,
                           "l" = .15))

es_tmp1$lavlabel <- lavaan::lav_partable_labels(es_tmp1)

expect_equal(es_tmp1[es_tmp1$lavlabel == "m1~x1", "pop"],
             .25^(1 / 3),
             ignore_attr = TRUE)
expect_equal(es_tmp1[es_tmp1$lavlabel == "m3~x2", "pop"],
             .70^(1 / 2),
             ignore_attr = TRUE)

})
