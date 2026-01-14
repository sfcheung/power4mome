skip("WIP")

# Experiment: How to specify k-way moderation

library(testthat)
library(lavaan)

model1 <-
"
y ~  x + w + z + x:w + x:z + w:z + x__w__z
"

model1_es <-
"
y ~ x: s
y ~ w: nil
y ~ z: nil
y ~ x:w: s
y ~ x:z: m
y ~ w:z: nil
y ~ x__w__z: s
"

lavParseModelString(model1, as.data.frame. = TRUE)

set.seed(1234)
out <- ptable_pop(model = model1,
                  pop_es = model1_es,
                  n_std = 5e4)

out
