skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("add_cov", {

model <-
"
m ~ x
y ~ m + x + w + m:w
"

model_es <-
"
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 100,
                  iseed = 1234,
                  parallel = FALSE,
                  progress = FALSE)

sim_out <- out$sim_all[[1]]
names(sim_out)
model_original <- sim_out$model_original
fit <- sim_out$fit0

cov_to_add <- function(fit) {
  pt <- lavaan::parameterTable(fit)
  ov_x <- lavaan::lavNames(fit, "ov.x")
  ov_int <- lavaan::lavNames(fit, "ov.interaction")
  ov_nox <- lavaan::lavNames(fit, "ov.nox")
  ov_nox <- setdiff(ov_nox, ov_int)
  cov_add <- list()
  for (xx in ov_int) {
    ww <- strsplit(xx, ":", fixed = TRUE)[[1]]
    tmp <- combn(ww, m = 2, simplify = TRUE)
    cov_add <- c(cov_add,
                mapply(function(lhs, rhs) {
                  data.frame(lhs = lhs,
                             op = "~~",
                             rhs = rhs)
                },
                lhs = c(ww, tmp[1, ]),
                rhs = c(rep(xx, length(ww)),
                        tmp[2, ]),
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE))
  }
  cov_add <- do.call(rbind,
                     cov_add)
  i_w <- cov_add$lhs %in% ov_x
  if (any(i_w)) {
    cov_add2 <- list()
    for (xx in unique(cov_add$rhs[i_w])) {
      cov_add2 <- c(cov_add2,
                  mapply(function(lhs, rhs) {
                    data.frame(lhs = lhs,
                               op = "~~",
                               rhs = rhs)
                  },
                  lhs = ov_x,
                  rhs = xx,
                  SIMPLIFY = FALSE,
                  USE.NAMES = FALSE))
    }
    cov_add <- rbind(cov_add,
                     do.call(rbind, cov_add2))
  }
  cov_add <- unique(cov_add)
  pt_cov <- pt[(pt$op == "~~") &
               (pt$free > 0) &
               (pt$lhs != pt$rhs) &
               ((pt$lhs %in% ov_x) & (pt$rhs %in% ov_x)), ]
  i1 <- grepl(":", pt_cov$lhs)
  tmp1 <- pt_cov[i1, "lhs"]
  pt_cov[i1, "lhs"] <- pt_cov[i1, "rhs"]
  pt_cov[i1, "rhs"] <- tmp1
  pt_cov1 <- pt_cov[, c("lhs", "op", "rhs")]
  pt_cov_all <- rbind(cov_add,
                      pt_cov1)
  pt_cov_all <- unique(pt_cov_all)
  i <- pt_cov_all$lhs != pt_cov_all$rhs
  pt_cov_all <- pt_cov_all[i, ]
  pt_cov_all <- apply(pt_cov_all,
                      MARGIN = 1,
                      paste,
                      collapse = " ")
  pt_cov_all <- unname(pt_cov_all)
  pt_cov_all
}

out <- cov_to_add(fit)
model_fixed <- paste(c(model_original,
                       out,
                       "\n"),
                     collapse = "\n")
model_fixed <- gsub(":", "__xx__", model_fixed,
                    fixed = TRUE)
cat(model_fixed)

dat_tmp <- sim_out$mm_lm_dat_out
colnames(dat_tmp) <- gsub(":", "__xx__", colnames(dat_tmp), fixed = TRUE)
head(dat_tmp)
fit_fixed <- sem(model_fixed,
                 data = dat_tmp)
fit_fixed
parameterEstimates(fit_fixed)
})