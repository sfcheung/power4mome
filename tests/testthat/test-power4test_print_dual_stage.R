test_that("w on more than one component paths", {

model <-
"
m ~ x + w + x:w
y ~ m + w + m:w + x
"
model_es <-
"
m ~ x: s
m ~ x:w: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

# Test the Model Specification

out <- power4test(
  nrep = 2,
  model = model,
  pop_es = model_es,
  n = 100,
  fit_model_args = list(fit_function = "lm"),
  iseed = 1234,
  progress = !is_testing()
)

tmp <- capture.output(print(out))

expect_all_false(grepl("Conditional on moderator(s): w, w", tmp))

expect_true(any(grepl("[w] (w)", tmp, fixed = TRUE)))

})
