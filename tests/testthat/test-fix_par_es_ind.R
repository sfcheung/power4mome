# Test

test_that("fix_par_es: indirect", {

x1 <- ".ind.(x1-> m1->m2->y)"
x2 <- ".ind. (   x1   -> m1 -> m2-> y)"
x3 <- ".ind. (y ~ m2 ~ m1~x1  )  "

tmp1 <- expand_to_components(x1)
tmp2 <- expand_to_components(x2)
tmp3 <- expand_to_components(x3)

expect_equal(tmp1,
             tmp2)
expect_equal(sort(tmp1),
             sort(tmp3))


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
              ".ind.(y~m3~ x2)" = "li")

tmp1 <- fix_par_es(model_es,
                   model)
tmp1_chk <- c(`m1 ~ x1` = "(mi)^(1 / 3)", `m2 ~ m1` = "(mi)^(1 / 3)", `y ~ m2` = "(mi)^(1 / 3)",
`y ~ m3` = "(li)^(1 / 2)", `m3 ~ x2` = "(li)^(1 / 2)", `y ~ x1` = "m", `y2 ~ m3` = "m",
`y2 ~ x2` = "m", `x1 ~~ x2` = "-s")

expect_equal(sort(tmp1),
             sort(tmp1_chk))

# max_num_comp() is no longer needed
# # Get maximum numbers of components

# expect_equal(max_num_comp(tmp1),
#              3)
# expect_equal(max_num_comp(tmp1[6:9]),
#              2)

# Set the values

es1_tmp <- c("n" = .00,
             "nil" = .00,
             "s" = .10,
             "m" = .30,
             "l" = .50,
             "si" = .141,
             "mi" = .361,
             "li" = .510)
es_long(es1_tmp)

expand_ind_labels(es1_tmp, num_comp = 2)
expand_ind_labels(es1_tmp, num_comp = 10)

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
             es1_tmp["mi"]^(1 / 3),
             ignore_attr = TRUE)
expect_equal(es_tmp1[es_tmp1$lavlabel == "m3~x2", "pop"],
             es1_tmp["li"]^(1 / 2),
             ignore_attr = TRUE)

})
