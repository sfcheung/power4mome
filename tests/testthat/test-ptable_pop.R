
test_that("ptable_pop", {

# Simple mediation model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c(".beta." = "m",
                         "y ~ x" = "n")

ptable_simple_med <- ptable_pop(model_simple_med,
                                pop_es = model_simple_med_es)

expect_equal(ptable_simple_med$start[1:3],
             c(.30, .30, .00))

mm_lm_out <- mm_lm(model_matrices_pop(ptable_simple_med,
                                      pop_es = model_simple_med_es))

# Parallel mediation model
# Correlated errors

model_med_parallel <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
m1 ~~ m2
"

model_med_parallel_es <- c(".beta." = "s",
                           "y ~ x" = "-m",
                           "m1 ~~ m2" = "l")

ptable_med_parallel_es <- ptable_pop(model_med_parallel,
                                     pop_es = model_med_parallel_es)

expect_equal(ptable_med_parallel_es$start[6],
             .50)

mm_out <- model_matrices_pop(ptable_med_parallel_es,
                             pop_es = model_med_parallel_es)

mm_lm_out <- mm_lm(mm_out)

# Parallel mediation model
# Correlated errors not specified in the model

model_med_parallel <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"

model_med_parallel_es <- c(".beta." = "s",
                           "y ~ x" = "-m",
                           "m1 ~~ m2" = "l")

expect_warning(ptable_med_parallel_es <- ptable_pop(model_med_parallel,
                                                    pop_es = model_med_parallel_es))


# Moderated mediation model

model_mod_med <-
"
m ~ x + w + x:w
y ~ m + z + m:z + x + u + x:u
"

model_mod_med_es <- c(".beta." = "s",
                      "y ~ x" = "-m",
                      "m ~ x:w" = "l",
                      "x ~~ w + z + u" = "s",
                      "w ~~ z + u" = "l")

ptable_mod_med_es <- ptable_pop(model_mod_med,
                                pop_es = model_mod_med_es)
expect_equal(ptable_mod_med_es[(ptable_mod_med_es$lhs == "w") &
                               (ptable_mod_med_es$rhs == "u"), "start"],
             .50)

model2 <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"

model2_es <- c("m1 ~ x" = "-m",
               "m2 ~ m1" = "s",
               "y ~ m2" = "l",
               "y ~ x" = "m",
               "y ~ w" = "s",
               "y ~ x:w" = "s",
               "x ~~ w" = "s")

ptable2_es <- set_pop(model2_es)[, -5]
ptable2_es

ptable_final1 <- ptable_pop(model2,
                            pop_es = ptable2_es)
ptable_final2 <- ptable_pop(model2,
                            pop_es = model2_es)
expect_identical(ptable_final1,
                 ptable_final2)

})
