skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("update par_pop: Update keys", {

# Single-group

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c(".beta." = "m",
                         "m ~ x" = "l",
                         "y ~ x" = "n")

par_pop <- pop_es2par_pop(model_simple_med_es,
                          es1 = eval(formals(ptable_pop)$es1),
                          es2 = eval(formals(ptable_pop)$es2),
                          es_ind = eval(formals(ptable_pop)$es_ind),
                          model = model_simple_med)

par_pop

# It works with .ind.
par_pop_add_ind <- pop_es2par_pop(c(".ind.(x->m->y)" = "s"),
                              es1 = eval(formals(ptable_pop)$es1),
                              es2 = eval(formals(ptable_pop)$es2),
                              es_ind = eval(formals(ptable_pop)$es_ind),
                              model = model_simple_med)

# It is wrong with .beta. because all paths will be updated
par_pop_add_beta <- pop_es2par_pop(c(".beta." = "s"),
                              es1 = eval(formals(ptable_pop)$es1),
                              es2 = eval(formals(ptable_pop)$es2),
                              es_ind = eval(formals(ptable_pop)$es_ind),
                              model = model_simple_med)

# It is wrong with .beta_nil. because they cannot be processed
par_pop_add_beta_nil <- pop_es2par_pop(c(".beta_nil." = ".12"),
                              es1 = eval(formals(ptable_pop)$es1),
                              es2 = eval(formals(ptable_pop)$es2),
                              es_ind = eval(formals(ptable_pop)$es_ind),
                              model = model_simple_med)

# It is wrong with .fm.(cfi) because they cannot be processed
par_pop_add_fm <- pop_es2par_pop(c(".fm.(cfi)" = ".90"),
                              es1 = eval(formals(ptable_pop)$es1),
                              es2 = eval(formals(ptable_pop)$es2),
                              es_ind = eval(formals(ptable_pop)$es_ind),
                              model = model_simple_med)

# TO PROCESS

par_pop2 <- update_par_pop(add = par_pop_add,
                           par_pop = par_pop)

expect_equal(par_pop2[[1]][1, "es"],
             "s")

ptable <- ptable_pop(model = model_simple_med,
                     pop_es = model_simple_med_es)

ptable2 <- update_ptable_pop(ptable,
                             new_pop_es = c("m ~ x" = "s"))

expect_equal(ptable2[1, "start"],
             .10)

expect_equal(ptable[1, "start"],
             .50)

# Multigroup

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- list(".beta." = "m",
                            "m ~ x" = c("n", "s"),
                            "y ~ x" = "n")

par_pop <- pop_es2par_pop(model_simple_med_es,
                          es1 = eval(formals(ptable_pop)$es1),
                          es2 = eval(formals(ptable_pop)$es2),
                          es_ind = eval(formals(ptable_pop)$es_ind),
                          model = model_simple_med)

par_pop

par_pop_add <- pop_es2par_pop(list("m ~ x" = c("l", "m")),
                              es1 = eval(formals(ptable_pop)$es1),
                              es2 = eval(formals(ptable_pop)$es2),
                              es_ind = eval(formals(ptable_pop)$es_ind),
                              model = model_simple_med)

par_pop2 <- update_par_pop(add = par_pop_add,
                           par_pop = par_pop)

expect_equal(par_pop2[[1]][1, "es"],
             "l")
expect_equal(par_pop2[[2]][1, "es"],
             "m")

ptable <- ptable_pop(model = model_simple_med,
                     pop_es = model_simple_med_es)

ptable2 <- update_ptable_pop(ptable,
                             new_pop_es = list("m ~ x" = c("s", "l")))

expect_equal(ptable2[1, "start"],
             .10)
expect_equal(ptable2[7, "start"],
             .50)

expect_equal(ptable[1, "start"],
             .00)
expect_equal(ptable[7, "start"],
             .10)

})
