library(testthat)
suppressMessages(library(lavaan))

test_that("nil_paths", {

# ---- Simple mediation model ----

model_simple_med <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + x
"

model_simple_med_es <- c(".beta." = "m",
                         "m1 ~ x" = "l",
                         "y ~ x" = "n")

out <- ptable_pop(
  model = model_simple_med,
  pop_es = model_simple_med_es
)

out2 <- nil_paths(out)

expect_setequal(
  out2,
  c("y~m1", "m2~x")
)

# ---- One y ----

model_simple_med <-
"
y ~ x1 + x2
"

model_simple_med_es <- c(".beta." = "m")

out <- ptable_pop(
  model = model_simple_med,
  pop_es = model_simple_med_es
)

out2 <- nil_paths(out)

expect_setequal(
  out2,
  character(0)
)

# ---- Two ys, no mediators ----

model_simple_med <-
"
y1 ~ x1 + x2
y2 ~ x1
y3 ~ x2
"

model_simple_med_es <- c(".beta." = "m")

out <- ptable_pop(
  model = model_simple_med,
  pop_es = model_simple_med_es
)

out2 <- nil_paths(out)

expect_setequal(
  out2,
  c("y2~x2", "y3~x1")
)

# ---- Simple mediation model: MG ----

model_simple_med <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + x
"

model_simple_med_es <-
"
y ~ x: [m, l, nil]
"

out <- ptable_pop(
  model = model_simple_med,
  pop_es = model_simple_med_es
)

out2 <- nil_paths(out)

expect_setequal(
  out2,
  c("y~m1", "m2~x")
)


})
