skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("add_cov", {

model <-
"
m ~ x + z + x:z
y ~ m + x + w + m:w
"

model_es <-
"
m ~ x:z: s
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

model_es_mg <-
"
m ~ x:z: [s, l]
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

out <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es,
            n = 100,
            iseed = 1234,
            parallel = FALSE,
            progress = FALSE)

out_mg <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es_mg,
            n = 100,
            iseed = 1234,
            parallel = FALSE,
            progress = FALSE)



sim_out <- out$sim_all[[1]]
names(sim_out)
model_original <- sim_out$model_original
fit <- sim_out$fit0

pt <- ptable_pop(
        model = model,
        pop_es = model_es
      )

pt_mg <- ptable_pop(
        model = model,
        pop_es = model_es_mg
      )

cov_to_add <- function(object) {
  # Also works for a multigroup model
  if (inherits(object, "lavaan")) {
    pt <- lavaan::parameterTable(object)
  } else {
    pt <- object
  }
  ov_x <- lavaan::lavNames(pt, "ov.x")
  ov_int <- lavaan::lavNames(pt, "ov.interaction")
  ov_nox <- lavaan::lavNames(pt, "ov.nox")
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
  # Remove duplicated covariances
  for (i in seq_len(nrow(pt_cov_all))) {
    tmp_i <- unlist(pt_cov_all[i, c("lhs", "rhs")])
    tmp2 <- order(tmp_i)
    pt_cov_all[i, c("lhs", "rhs")] <- unname(tmp_i[tmp2])
  }
  pt_cov_all <- pt_cov_all[!duplicated(pt_cov_all), , drop = FALSE]
  pt_cov_all <- apply(pt_cov_all,
                      MARGIN = 1,
                      paste,
                      collapse = " ")
  pt_cov_all <- unname(pt_cov_all)
  pt_cov_all
}

fake_fit_for_int <- function(
                        ptable,
                        model_int
                      ) {
  ngroups <- max(ptable$group)
  gpnames <- paste0("gp", seq_len(ngroups))
  fit0 <- lavaan::sem(model_int,
                      do.fit = FALSE)
  vnames <- lavaan::lavNames(fit0,
                             type = "ov")
  p <- length(vnames)
  d1 <- diag(p)
  colnames(d1) <- rownames(d1) <- vnames
  m1 <- vector(mode = "numeric",
               length = p)
  names(m1) <- vnames
  dat_cov <- lapply(seq_len(ngroups),
                    function(x) d1)
  dat_means <- lapply(seq_len(ngroups),
                     function(x) m1)
  fit1 <- lavaan::sem(model_int,
                      sample.cov = dat_cov,
                      sample.mean = dat_means,
                      sample.nobs = rep(10000, ngroups),
                      do.fit = FALSE,
                      group.label = gpnames,
                      meanstructure = (ngroups > 1),
                      fixed.x = FALSE)
  list(fit = fit1,
       data = dat_cov)
}

order_cov <- function(pt) {
  for (i in seq_len(nrow(pt))) {
    if (pt[i, "op", drop = TRUE] != "~~") next
    tmp_i <- unlist(pt[i, c("lhs", "rhs")])
    tmp2 <- order(tmp_i)
    pt[i, c("lhs", "rhs")] <- unname(tmp_i[tmp2])
  }
  pt
}

merge_start <- function(pt_source,
                        pt_target) {
  if (is.null(pt_source$group)) {
    ngroups <- 1
  } else {
    ngroups <- max(pt_source$group)
  }
  pt_source <- order_cov(pt_source)
  pt_target <- order_cov(pt_target)
  pt_source$tmplabel <- lavaan::lav_partable_labels(pt_source)
  pt_target$tmplabel <- lavaan::lav_partable_labels(pt_target)
  pt_target$tmpid <- seq_len(nrow(pt_target))
  col0 <- c("lhs", "op", "rhs", "tmplabel")
  tmp <- merge(pt_target[, c(col0, "tmpid")],
               pt_source[, c(col0, "start")],
               all.x = TRUE,
               sort = FALSE)
  tmp <- tmp[order(tmp$tmpid), ]
  i <- !is.na(tmp$start)
  pt_target[i, "start"] <- tmp[i, "start"]
  pt_target$tmplabel <- NULL
  pt_target$tmpid <- NULL
  pt_target
}

pt_with_int <- function(ptable,
                        model_original) {
  to_add <- cov_to_add(ptable)
  model_fixed <- paste(
                  c(model_original,
                    to_add,
                    "\n"),
                  collapse = "\n")
  model_fixed_2 <- gsub(
                     ":",
                     "__xx__",
                     model_fixed,
                     fixed = TRUE)
  fit_fixed <- fake_fit_for_int(
                  ptable = ptable,
                  model_int = model_fixed_2)
  pt_fixed <- lavaan::parameterTable(fit_fixed$fit)
  pt_fixed$lhs <- gsub("__xx__",
                       ":",
                       pt_fixed$lhs,
                       fixed = TRUE)
  pt_fixed$rhs <- gsub("__xx__",
                       ":",
                       pt_fixed$rhs,
                       fixed = TRUE)
  pt_fixed <- merge_start(pt_source = ptable,
                          pt_target = pt_fixed)
  pt_fixed
}

pt_fixed <- pt_with_int(
              ptable = pt,
              model = model
            )
pt_fixed_mg <- pt_with_int(
              ptable = pt_mg,
              model = model
            )

sim_out <- out$sim_all[[1]]
sim_out_mg <- out_mg$sim_all[[1]]

tmpnames <- setdiff(lavNames(pt, "ov"), lavNames(pt, "ov.interaction"))
dat <- sim_out$mm_lm_dat_out[, tmpnames]

fit2 <- sem(pt_fixed,
            dat,
            fixed.x = FALSE)
fit3 <- sem(pt_fixed,
            dat + 2,
            fixed.x = FALSE)
expect_equal(fitMeasures(fit2, "chisq"),
             fitMeasures(fit3, "chisq"))

fit2b <- sem(model,
             dat,
             fixed.x = FALSE)
fit3b <- sem(model,
             dat + 2,
             fixed.x = FALSE)
expect_false(fitMeasures(fit2b, "chisq") == fitMeasures(fit3b, "chisq"))

tmpnames <- setdiff(lavNames(pt_mg, "ov"), lavNames(pt_mg, "ov.interaction"))
dat_mg <- sim_out_mg$mm_lm_dat_out[, c(tmpnames, "group")]
dat_mg_shifted <- dat_mg
dat_mg_shifted[, tmpnames] <- dat_mg_shifted[, tmpnames] + 2

fit2_mg <- sem(pt_fixed_mg,
               dat_mg,
               group = "group",
               fixed.x = FALSE)
fit3_mg <- sem(pt_fixed_mg,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE)
expect_equal(fitMeasures(fit2_mg, "chisq"),
             fitMeasures(fit3_mg, "chisq"))

fit2b_mg <- sem(model,
               dat_mg,
               group = "group",
               fixed.x = FALSE)
fit3b_mg <- sem(pt_fixed_mg,
               dat_mg_shifted,
               group = "group",
               fixed.x = FALSE)
expect_false(fitMeasures(fit2b_mg, "chisq") == fitMeasures(fit3b_mg, "chisq"))

})
