Stratify_m <- function (data.obj, strata.group, sp, subset) {
  browser()
  if (!inherits(data.obj, "strata.data")) 
    stop("Not a legitimate strata data object")
  m <- match.call()
  attach(data.obj)
  sp <- sp
  if (!missing(subset)) {
    data.obj <- data.obj[subset, ]
    sp <- sp[subset]
  }
  detach("data.obj")
  s.group <- is.element(strata.group$strat, data.obj$strat)
  s.group.Strata <- strata.group$strat[s.group]
  s.group.NH <- strata.group$nh[s.group]
  s.obj <- is.element(data.obj$strat, s.group.Strata)
  sp <- sp[s.obj]
  data.obj <- data.obj[s.obj, ]
  yhi <- split(sp, data.obj$strat)
  nh <- as.vector(sapply(yhi, length))
  nhws <- sapply(yhi,function(x) length(x[x>0]))
  res <- list(yhi = yhi, Strata = s.group.Strata, Nh = s.group.NH, 
              Wh = s.group.NH/sum(s.group.NH), nh = nh, call = m,nhws = nhws)
  class(res) <- "strata"
  res
}