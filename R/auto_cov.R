#' @noRd
# Input:
# - Parameter table
# - Original model syntax
# Output:
# -
pt_with_int <- function(ptable,
                        model_original) {
  to_add <- cov_to_add(ptable)
  # TODO:
  # - When `x:w ~~ y:z` is available in
  #   lavaan, return the model syntax directly.
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

#' @noRd
# Input:
# - A parameter table or a lavaan object
# Output:
# - A vector of covariances to be added
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

#' @noRd
# Input:
# - A parameter table.
# - A model syntax.
# Output:
# - A list:
#   - A fitted object
#   - A list of fake covariance matrices
#   - A list of fake mean vectors
fake_fit_for_int <- function(
                        ptable,
                        model_int
                      ) {
  # Multigroup models supported
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
       cov = dat_cov,
       mean = dat_means)
}

#' @noRd
# Input:
# - A parameter table.
# Output:
# - A parameter table, with covariance terms
#   ordered.
order_cov <- function(pt) {
  for (i in seq_len(nrow(pt))) {
    if (pt[i, "op", drop = TRUE] != "~~") next
    tmp_i <- unlist(pt[i, c("lhs", "rhs")])
    tmp2 <- order(tmp_i)
    pt[i, c("lhs", "rhs")] <- unname(tmp_i[tmp2])
  }
  pt
}

#' @noRd
# Input:
# - The source parameter table with starting values to be imported
# - The target parameter table.
# Output:
# - The target parameter table with starting values updated.
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

#' @noRd
# Check whether at least one mediator
# is involved in moderation.
# Input:
# - Model syntax or parameter table
# Output:
# - A character vector of mediator(s)
#   involved in moderation.
m_moderated <- function(object) {
  if (is.character(object)) {
    fit <- lavaan::sem(
              object,
              do.fit = FALSE
            )
    pt <- lavaan::parameterTable(fit)
  }
  int_term <- union(
                lavaan::lavNames(pt, "ov.interaction"),
                lavaan::lavNames(pt, "lv.interaction")
              )
  int_term_comp <- strsplit(
                      int_term,
                      split = ":"
                    )
  int_term_comp <- unique(unlist(int_term_comp))
  if (length(int_term) == 0) {
    return(character(0))
  }
  ovlv_y <- union(
              lavaan::lavNames(pt, "ov.nox"),
              lavaan::lavNames(pt, "lv.nox")
            )
  ovlv_y <- setdiff(ovlv_y, int_term)
  m_in_int <- intersect(ovlv_y, int_term_comp)
  return(m_in_int)
}