skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("Handle fit measure key", {

# ---- Check fit measure target key ----

es <-
c("y1 ~ x1" = .90,
  "y2 ~ x2" = "m",
  "x1 ~~ x2" = "l")

expect_null(
  target_fm_from_pop_es(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
"

expect_null(
  target_fm_from_pop_es(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
.rmsea.: .12
"

expect_equal(
  as.numeric(target_fm_from_pop_es(es)),
  .12
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
"

expect_null(
  target_fm_from_pop_es(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
.cfi.: .80
"

expect_equal(
  as.numeric(target_fm_from_pop_es(es)),
  .8
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
.cfi.: [.80, .90]
"

pop_es_yaml_check(es)

expect_error(
  target_fm_from_pop_es(es),
  "value"
)

# ---- Check stripping fit measure target key ----

es <-
c("y1 ~ x1" = .90,
  "y2 ~ x2" = "m",
  "x1 ~~ x2" = "l")

expect_identical(
  strip_keys_from_pop_es(es),
  es
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
"

expect_identical(
  strip_keys_from_pop_es(es),
  pop_es_yaml_check(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
.rmsea.: .12
"
tmp <- pop_es_yaml_check(es)
expect_identical(
  strip_keys_from_pop_es(es),
  tmp[!(names(tmp) %in% ".rmsea.")]
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
.cfi.: .80
.beta.: .30
.beta_nil.: .20
"
tmp <- pop_es_yaml_check(es)

expect_identical(
  strip_keys_from_pop_es(es),
  tmp[!(names(tmp) %in% ".cfi.")]
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: [nil, m]
.cfi.: [.80, .90]
"
tmp <- pop_es_yaml_check(es)

expect_identical(
  strip_keys_from_pop_es(es),
  tmp[!(names(tmp) %in% ".cfi.")]
)

})
