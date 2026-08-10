skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("update par_pop: Update keys", {

# Single-group

# model_simple_med <-
# "
# m ~ x
# y ~ m + x
# "

# model_simple_med_es <- c(".beta." = "m",
#                          "m ~ x" = "l",
#                          "y ~ x" = "n")

# par_pop <- pop_es2par_pop(model_simple_med_es,
#                           es1 = eval(formals(ptable_pop)$es1),
#                           es2 = eval(formals(ptable_pop)$es2),
#                           es_ind = eval(formals(ptable_pop)$es_ind),
#                           model = model_simple_med)

# par_pop

# It works with .ind.
# par_pop_add_ind <- pop_es2par_pop(c(".ind.(x->m->y)" = "s"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med)

# It is wrong with .beta_nil. because they cannot be processed
# par_pop_add_beta_nil <- pop_es2par_pop(c(".beta_nil." = ".12"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med)

# .beta.

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c(".beta." = "m",
                         "m ~ x" = "l",
                         "y ~ x" = "n")

ptable <- ptable_pop(model = model_simple_med,
                     pop_es = model_simple_med_es)

ptable2a <- update_ptable_pop(ptable,
                             new_pop_es = c(".beta." = .12))
expect_equal(ptable2a[2, "start"],
             .12)
expect_false(
  isTRUE(all.equal(
    ptable[2, "start"],
    ptable2a[2, "start"],
  ))
)

# .fm.

model_simple_med <-
"
m ~ x
y ~ m
"

model_simple_med_es <- c("m ~ x" = "l",
                         "y ~ m" = "s")

ptable <- ptable_pop(model = model_simple_med,
                     pop_es = model_simple_med_es)

ptable2a <- update_ptable_pop(ptable,
                             new_pop_es = c(".fm.(cfi)" = .99))

expect_equal(
  as.numeric(attr(ptable2a, "pop_es_fm_target")),
  .99
)

expect_false(
  isTRUE(all.equal(
    ptable[3, "start"],
    ptable2a[3, "start"],
  ))
)

# .beta_nil.

model_simple_med <-
"
m ~ x
y ~ m
"

model_simple_med_es <- c("m ~ x" = "l",
                         "y ~ m" = "s")

ptable <- ptable_pop(model = model_simple_med,
                     pop_es = model_simple_med_es)

ptable2a <- update_ptable_pop(ptable,
                             new_pop_es = c(".beta_nil." = .13))
expect_equal(ptable2a[3, "start"],
             .13)
expect_false(
  isTRUE(all.equal(
    ptable[3, "start"],
    ptable2a[3, "start"],
  ))
)

# Multigroup

# model_simple_med <-
# "
# m ~ x
# y ~ m + x
# "

# model_simple_med_es <- list(".beta." = "m",
#                             "m ~ x" = c("n", "s"),
#                             "y ~ x" = "n")

# par_pop <- pop_es2par_pop(model_simple_med_es,
#                           es1 = eval(formals(ptable_pop)$es1),
#                           es2 = eval(formals(ptable_pop)$es2),
#                           es_ind = eval(formals(ptable_pop)$es_ind),
#                           model = model_simple_med)

# par_pop

# # It works with .ind.
# par_pop_add_ind <- pop_es2par_pop(c(".ind.(x->m->y).g2" = "s"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med,
#                               ngroups = 2)
# # It works with .ind.
# par_pop_add_ind <- pop_es2par_pop(c(".ind.(x->m->y).g1" = "s"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med,
#                               ngroups = 2)

# # It is wrong with .beta. because all paths will be updated
# par_pop_add_beta <- pop_es2par_pop(c(".beta..g2" = "s"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med,
#                               ngroups = 2)

# # It is wrong with .beta_nil. because they cannot be processed
# par_pop_add_beta_nil <- pop_es2par_pop(c(".beta_nil..g1" = ".12"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med,
#                               ngroups = 2)

# # It is wrong with .fm.(cfi) because they cannot be processed
# par_pop_add_fm <- pop_es2par_pop(c(".fm.(cfi)" = ".90"),
#                               es1 = eval(formals(ptable_pop)$es1),
#                               es2 = eval(formals(ptable_pop)$es2),
#                               es_ind = eval(formals(ptable_pop)$es_ind),
#                               model = model_simple_med,
#                               ngroups = 2)

model_simple_med <-
"
m1 ~ x
m2 ~ m1
y ~ m2
"

model_simple_med_es <- list(".beta." = .13,
                            "m1 ~ x" = c("n", "s"),
                            "y ~ m2" = "s")

ptable <- ptable_pop(model = model_simple_med,
                     pop_es = model_simple_med_es)

# .beta.

ptable2a <- update_ptable_pop(ptable,
                             new_pop_es = c(".beta." = .03))
i <- (ptable2a$lhs == "m2") & (ptable2a$rhs == "m1")
expect_equal(
  ptable2a[i, "start"],
  c(.03, .03)
)
expect_false(
  isTRUE(all.equal(
    ptable[i, "start"],
    ptable2a[i, "start"],
  ))
)

# .beta_nil.

ptable2a <- update_ptable_pop(ptable,
                             new_pop_es = c(".beta_nil." = .03))
i <- (ptable2a$lhs == "y") & (ptable2a$rhs == "m1")
expect_equal(
  ptable2a[i, "start"],
  c(.03, .03)
)
i <- (ptable$lhs == "y") & (ptable$rhs == "m1")
expect_false(any(i))

# .fm.

ptable2a <- update_ptable_pop(ptable,
                             new_pop_es = c(".fm.(cfi)" = .95))
i <- (ptable$lhs == "y") & (ptable$rhs == "m1")
expect_false(any(i))
expect_equal(
  as.numeric(attr(ptable2a, "pop_es_fm_target")),
  .95
)

})
