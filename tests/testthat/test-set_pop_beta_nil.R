suppressMessages(library(lavaan))

# Test

test_that("beta_nil", {

mod <-
"
m1 ~ x
m2 ~ m1
y ~ m2
"

mod_es1 <- c(".beta." = "m",
             "y~m2" = "s",
             ".beta_nil." = .05)

pop_es <- fix_par_es(
            mod_es1,
            model = mod
          )

pop_es_nil <- fix_par_es(
            mod_es1,
            model = mod,
            return_beta_nil = TRUE
          )

tmp <- set_pop(pop_es)

out <- ptable_pop(
  model = mod,
  pop_es = mod_es1
)

out$lavlabel <- lav_partable_labels(out)

i <- gsub(" ", "", names(pop_es_nil))

expect_equal(
  out[out$lavlabel %in% i, "start"],
  c(.05, .05, .05)
)

attr(out, "model")
attr(out, "model_nil")

})
