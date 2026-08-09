suppressMessages(library(lavaan))

# Test

test_that("beta_nil: multigroup", {

# .beta_nil. works for multigroup models

mod <-
"
m1 ~ x
m2 ~ m1
y ~ m2
"

# ---- One for all ----

mod_es1 <-
"
y ~ m2:
 - s
 - m
.beta_nil.: .05
"

pop_es <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod
          )

pop_es_nil <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod,
            return_beta_nil = TRUE
          )

out <- ptable_pop(
  model = mod,
  pop_es = mod_es1
)

out$lavlabel <- lav_partable_labels(out)

i1 <- gsub(" ", "", names(pop_es_nil))
i2 <- paste0(i1, ".g2")

expect_equal(
  out[out$lavlabel %in% union(i1, i2), "start"],
  c(.05, .05, .05, .05, .05, .05)
)

# ---- One for One ----

mod_es1 <-
"
y ~ m2:
 - s
 - m
.beta.:
 - m
.beta_nil.(y~m1): .05
.beta_nil.(y~x): .10
"

pop_es <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod
          )

pop_es_nil <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod,
            return_beta_nil = TRUE
          )

out <- ptable_pop(
  model = mod,
  pop_es = mod_es1
)

out$lavlabel <- lav_partable_labels(out)

i1 <- gsub(" ", "", names(pop_es_nil))
i2 <- paste0(i1, ".g2")

expect_equal(
  out[out$lavlabel %in% union(i1, i2), "start"],
  c(.05, .00, .10, .05, .00, .10)
)

# ---- Discard unknown keys ----

mod_es1 <-
"
y ~ m2:
 - s
 - m
.beta.:
 - m
.beta_nil.(y~m1): .05
.beta_nil.(y~x): .10
.cfa.: .80
.rmsea.: .12
"

pop_es <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod
          )

pop_es_nil <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod,
            return_beta_nil = TRUE
          )

out <- ptable_pop(
  model = mod,
  pop_es = strip_keys_from_pop_es(mod_es1)
)

out$lavlabel <- lav_partable_labels(out)

i1 <- gsub(" ", "", names(pop_es_nil))
i2 <- paste0(i1, ".g2")

expect_equal(
  out[out$lavlabel %in% union(i1, i2), "start"],
  c(.05, .00, .10, .05, .00, .10)
)

# ---- One for all: Per group ----

mod_es1 <-
"
y ~ m2:
 - s
 - m
.beta_nil.:
 - .05
 - .09
"

pop_es <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod
          )

pop_es_nil <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod,
            return_beta_nil = TRUE
          )

out <- ptable_pop(
  model = mod,
  pop_es = mod_es1
)

out$lavlabel <- lav_partable_labels(out)

i1 <- gsub(" ", "", names(pop_es_nil))
i2 <- paste0(i1, ".g2")

expect_equal(
  out[out$lavlabel %in% i1, "start"],
  c(.05, .05, .05)
)

expect_equal(
  out[out$lavlabel %in% i2, "start"],
  c(.09, .09, .09)
)

# ---- One for One ----

mod_es1 <-
"
y ~ m2:
 - s
 - m
.beta.:
 - m
.beta_nil.(y~m1): .05
.beta_nil.(y~x):
 - .07
 - -.09
"

pop_es <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod
          )

pop_es_nil <- fix_par_es(
            pop_es_yaml_check(mod_es1),
            model = mod,
            return_beta_nil = TRUE
          )

out <- ptable_pop(
  model = mod,
  pop_es = mod_es1
)

out$lavlabel <- lav_partable_labels(out)

i1 <- gsub(" ", "", names(pop_es_nil))
i2 <- paste0(i1, ".g2")

expect_equal(
  out[out$lavlabel %in% i1, "start"],
  c(.05, .00, .07)
)

expect_equal(
  out[out$lavlabel %in% i2, "start"],
  c(.05, .00, -.09)
)

})
