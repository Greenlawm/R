calculate.ci.strata.wgt = function (species=species, data.yrs) {
#-----------------------------------------------------------------

data.yrs = 1983:2017
species = c('10')
#atlantic cod = 10


# BCa:  Find nonparametric BCa intervals.
# License: BSD_3_clause + file LICENSE (q.v.)

# Please refer to the package documentation for details.

# NIST assumes no responsibility for use of this software by other parties
# and makes no guarantees, expressed or implied, about its quality,
# reliability, or any other characteristic.

BCa <- function(x,delta,theta,...,alpha=c(0.025,0.975),verbose=F,M=25000,
                Mlimit=1500000,qtype=1) {
  
  # --- Begin input validation ---
  
  # Input validation in R is extremely difficult since R has so many data
  # types that are substitutable in some contexts but cause errors in others,
  # and many of the is.* functions will themselves throw errors on certain
  # data types.
  
  # Findings from interactive testing with R 3.1.0:
  #   NA is logical and has length 1.
  #   NULL is interchangeable with c() and has length 0.
  #   Typed zero-length vectors (numeric(0), logical(0), etc.) have length 0 but are not null.
  #   numeric(0) is numeric, logical(0) is logical, etc.
  #   Character strings, complex numbers, NULL, and NA are not numeric.
  #   Lists and expressions are not numeric.  They crash is.infinite, is.finite, and is.nan,
  #   but not is.na.
  #   Functions are not numeric.  They crash all four of those.
  #   Functions and expressions have length 1.
  #   NaN, NA_integer_, and NA_real_ are numeric.
  #   Dates are finite but not numeric.
  #   Character strings, NAs of all kinds, and NaNs are neither finite nor infinite.
  #   NaNs are NA (is.na), but NAs are not NaN (is.nan).
  #   A matrix, array, or data frame can substitute for a vector.
  #   A vector of length 1 and a scalar are substitutable for each other.
  #   Factors are not numeric.
  
  # The following functions will return a vector of length >= 2 if given one
  # as input:
  #   is.infinite, is.finite, is.nan, is.na
  # Similarly, they will return a zero-length vector (logical(0)) if given
  # one as input.  Conditionals expecting a single T or F value must be
  # guarded against these occurrences.
  
  # x can't be NA, can't be a zero-length vector, and can't be anything
  # from which it is impossible to sample.
  if (is.function(x))        stop("BCa: x must not be a function")
  if (is.expression(x))      stop("BCa: x must not be an expression")
  if (is.factor(x))          stop("BCa: x must not be a factor")
  if (length(x)==0)          stop("BCa: x must have nonzero length")
  if (length(x)<2&&is.na(x)) stop("BCa: x must not be NA")
  
  # delta can be NA or it can be a scalar number, possibly infinite.
  # It can't be NaN, it can't be a complex number.
  if (is.function(delta))                stop("BCa: delta must not be a function")
  if (is.expression(delta))              stop("BCa: delta must not be an expression")
  if (length(delta)!=1)                  stop("BCa: delta value must be scalar")
  if (!is.na(delta)&&!is.numeric(delta)) stop("BCa: delta value must be numeric")
  if (is.nan(delta))                     stop("BCa: delta value must not be NaN")
  
  # theta must be a function.
  if (!is.function(theta)) stop("BCa: theta must be a function")
  
  # alpha can't be zero-length and the contents must be numbers between 0 and
  # 1 (exclusive).
  if (is.function(alpha))            stop("BCa: alpha must not be a function")
  if (is.expression(alpha))          stop("BCa: alpha must not be an expression")
  if (length(alpha)==0)              stop("BCa: alpha must have nonzero length")
  if (length(alpha)<2&&is.na(alpha)) stop("BCa: alpha must not be NA")
  if (any(!is.finite(alpha)))        stop("BCa: alpha values must be finite")
  if (any(!is.numeric(alpha)))       stop("BCa: alpha values must be numeric")
  if (any(alpha<=0|alpha>=1))        stop("BCa: alpha values must lie between 0 and 1 exclusive")
  
  # verbose must be either T or F.
  if (is.function(verbose))   stop("BCa: verbose must not be a function")
  if (is.expression(verbose)) stop("BCa: verbose must not be an expression")
  if (length(verbose)!=1)     stop("BCa: verbose must be scalar")
  if (is.na(verbose))         stop("BCa: verbose must not be NA")
  if (!is.logical(verbose))   stop("BCa: verbose must be T or F")
  
  # M must be a finite, scalar value greater than 1.
  if (is.function(M))   stop("BCa: M must not be a function")
  if (is.expression(M)) stop("BCa: M must not be an expression")
  if (length(M)!=1)     stop("BCa: M must be scalar")
  if (!is.finite(M))    stop("BCa: M must be finite")
  if (!is.numeric(M))   stop("BCa: M must be numeric")
  if (M<2)              stop("BCa: M must be greater than 1")
  
  # Mlimit must be a scalar numeric value, possibly infinite.
  # If delta is not NA, Mlimit must be >= 2M.
  if (is.function(Mlimit))       stop("BCa: Mlimit must not be a function")
  if (is.expression(Mlimit))     stop("BCa: Mlimit must not be an expression")
  if (length(Mlimit)!=1)         stop("BCa: Mlimit must be scalar")
  if (is.na(Mlimit))             stop("BCa: Mlimit must not be NA")
  if (!is.numeric(Mlimit))       stop("BCa: Mlimit must be numeric")
  if (!is.na(delta)&&Mlimit<2*M) stop("BCa: Mlimit cannot be less than 2M")
  
  # qtype must be a scalar between 1 and 9 inclusive.
  if (is.function(qtype))   stop("BCa: qtype must not be a function")
  if (is.expression(qtype)) stop("BCa: qtype must not be an expression")
  if (length(qtype)!=1)     stop("BCa: qtype must be scalar")
  if (!is.finite(qtype))    stop("BCa: qtype must be finite")
  if (!is.numeric(qtype))   stop("BCa: qtype must be numeric")
  if (qtype<1||qtype>9)     stop("BCa: qtype must be between 1 and 9 inclusive")
  
  # --- End input validation ---
  
  n <- length(x)
  if (verbose) {
    if (is.na(delta))
      message(sprintf("BCa call n=%d, non-adaptive, fixed M=%d", n, M))
    else
      message(sprintf("BCa call n=%d, delta=%f, M=%d, Mlimit=%d",
                      n, delta, M, Mlimit))
  }
  
  # Handle the degenerate case.  Only in this case, NAs and suchlike from
  # theta are passed back.  theta=sd ends up here when length(x)==1.
  thetahat <- theta(x,...)
  if (length(thetahat)!=1) stop("BCa: theta returned a non-scalar value")
  if (length(unique(x))==1) {
    if (verbose) warning("BCa: handling degenerate case (all values are the same).")
    return(BCa_ret(c(0,0,rep(thetahat,1+length(alpha))),alpha))
  }
  
  # From now on, it has to be valid.
  BCa_validate_theta(thetahat)
  
  # Values to calculate once.
  u <- rep(0,n)
  for (i in 1:n) u[i] <- BCa_validate_theta(theta(x[-i],...))
  uu <- mean(u)-u
  if (all(uu==0)) stop("BCa: acceleration is indeterminate for insensitive function")
  acc <- sum(uu*uu*uu)/(6*(sum(uu*uu))^1.5)
  if (verbose) message(sprintf("acceleration=%f", acc))
  zalpha <- qnorm(alpha)
  
  # Do the first batch (only batch for non-adaptive) and initialize loop variables.
  thetastar <- rep(NA,M)
  for (i in 1:M) thetastar[i] <- BCa_validate_theta(theta(sample(x,size=n,replace=T),...))
  answers <- BCa_fn(thetastar,thetahat,acc,zalpha,qtype)
  if (is.na(delta)) return(BCa_ret(c(M,NA,answers),alpha)) # Early out for non-adaptive.
  h <- 1
  u <- delta
  allthetastar <- thetastar
  
  # Iterate till we have enough bootstrap replications or reach the limit.
  while (u>=delta & h*M<Mlimit) {
    for (i in 1:M) thetastar[i] <- BCa_validate_theta(theta(sample(x,size=n,replace=T),...))
    answers <- rbind(answers,BCa_fn(thetastar,thetahat,acc,zalpha,qtype))
    allthetastar <- rbind(allthetastar,thetastar)
    h <- h+1
    u <- 2*max(apply(answers,2,sd))/sqrt(h)
    if (verbose) message(sprintf("u=%f nboot=%d delta=%f", u, h*M, delta))
  }
  
  # Return the final answer.
  BCa_ret(c(h*M,u,BCa_fn(allthetastar,thetahat,acc,zalpha,qtype)),alpha)
}

# BCa_validate_theta:  Helper function to throw exceptions when theta
# misbehaves.  Returns the input if no problem.  Infinities are rejected
# because the sd of a set containing Inf is NaN, so u>=delta is NA and it
# fails.  Note from discussion above that !is.finite is not equivalent to
# is.infinite:  it also catches character strings, NAs, and NaNs.
BCa_validate_theta <- function(x) {
  if (length(x)!=1)   stop("BCa: theta returned a non-scalar value")
  if (!is.finite(x))  stop("BCa: theta returned a non-finite value")
  if (!is.numeric(x)) stop("BCa: theta returned a non-numeric value")
  x
}

# BCa_fn:  Helper function to calculate BCa outputs for a given batch.
# Output is a vector:
#   [1]   Bootstrap estimate.
#   rest  Points corresponding to alpha values.
BCa_fn <- function(thetastar,thetahat,acc,zalpha,qtype) {
  nboot <- length(thetastar)
  z0    <- qnorm(sum(thetastar<thetahat)/nboot)        # Range -Inf..Inf
  if (any(acc*(z0+zalpha)>=1)) stop("BCa: alpha value too extreme for these data")
  tt    <- pnorm(z0+ (z0+zalpha)/(1-acc*(z0+zalpha)))  # Range 0..1
  c(mean(thetastar),quantile(x=thetastar,probs=tt,type=qtype))
}

# BCa_ret:  Helper function to apply names to the elements of the returned vector.
BCa_ret <- function(v,alpha) {
  names(v) <- c("nboot","prec","est",sapply(alpha, function(x) sprintf("%0.3f",x)))
  v
}




#methods for calculating confidence intervals for grounfish survey indices
#based on the paper by Stephen Smith (1997) 
#"Bootstrap confidence limits for groundfish trawl survey estimates of mean abundance. Can. J. Fish. Aquat. Sci.
#This script uses the mirror match bootstrap method (BWR), described in the paper, which was shown to perform better on average with respect to accuracy of the confidence
#intervals being estimated. And the Bias Corrected (BC) method of calculating confidence intervals is used.  
#Three methods of bootstrapping and calculating confidence intervals were compared in the paper. 
  
#Setup
boot.strata <- function (data, nless = 0, nresamp = 1, method = c("RESCALE",   "BWR", "NAIVE")) {
  if (!inherits(data, "strata")) 
    stop("Not a legitimate strata object, need to run Stratify() first")
  call <- match.call(expand = FALSE)
  method <- match.arg(method)
  res1 <- data
  res3 <- summary(res1)
  if (nresamp == 0) 
    stop("No resampling done.")
  out <- matrix(0, nrow = nresamp + 1, ncol = 3, dimnames = list(c("Actual", 
                                                                   1:nresamp), c("Mean", "Variance",'gini')))
  out[1, ] <- c(res3$yst, (res3$se.yst)^2,res3$gini)
  if (method == "RESCALE") {
    res1$nh <- res1$nh - nless
    for (i in 1:nresamp) {
      yhib <- sapply(res1$yhi, rescale.boot, nless)
      out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, 
                                                       mean)), na.rm = TRUE), (sum((((res1$Nh * (res1$Nh - 
                                                                                                   res1$nh))/sum(res1$Nh)^2) * (as.vector(sapply(yhib, 
                                                                                                                                                 var))))/res1$nh, na.rm = TRUE)))
    }
    
  }
  if (method == "BWR") {
    fh <- res1$nh/res1$Nh
    kh <- (res1$nh - 1)/(1 - fh)
    ph <- ((1/kh) - (1/ceiling(kh)))/((1/floor(kh)) - (1/ceiling(kh)))
    for (i in 1:nresamp) {
      yhib <- bwr.boot(res1$yhi, kh, ph, sample, replace = TRUE, 
                       simplify = FALSE)
      yhib[res1$nh == 1] <- res1$yhi[res1$nh == 1]
      nhws = sapply(yhib,FUN=function(x) sum(x>0))
      
      out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, 
                                                       mean)), na.rm = TRUE), (sum((((res1$Nh * (res1$Nh - 
                                                                                                   as.vector(sapply(yhib, length))))/sum(res1$Nh)^2) * 
                                                                                      (as.vector(sapply(yhib, var))))/as.vector(sapply(yhib, 
                                                                                                                                       length)), na.rm = TRUE)),gini(x=as.vector(sapply(yhib, 
                                                                                                                                                                                        mean)),y=res1$Nh))
    }
  }
  if (method == "NAIVE") {
    for (i in 1:nresamp) {
      yhib <- sapply(res1$yhi, sample, replace = TRUE, 
                     simplify = FALSE)
      out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, 
                                                       mean)), na.rm = TRUE), (sum((((res1$Nh * (res1$Nh - 
                                                                                                   as.vector(sapply(yhib, length))))/sum(res1$Nh)^2) * 
                                                                                      (as.vector(sapply(yhib, var))))/as.vector(sapply(yhib, 
                                                                                                                                       length)), na.rm = TRUE)))
    }
  }
  res <- list(orig.mean = out[1, 1], orig.var = out[1, 2], 
              boot.means = out[c(2:(nresamp + 1)), 1], boot.vars = out[c(2:(nresamp + 
                                                                              1)), 2],gini = out[c(2:(nresamp + 1)), 3], accel = accel.str(res1), call = call, method = method)
  oldClass(res) <- "boot"
  return(res)
}

#' @export
bwr.boot <- function (X, kh, ph, FUN, ..., simplify = TRUE) {
  if (is.character(FUN)) 
    FUN <- get(FUN, mode = "function")
  else if (mode(FUN) != "function") {
    farg <- substitute(FUN)
    if (mode(farg) == "name") 
      FUN <- get(farg, mode = "function")
    else stop(paste("\"", farg, "\" is not a function", sep = ""))
  }
  class(X) <- NULL
  answer <- vector("list", length(X))
  nms <- names(X)
  if (is.recursive(X)) 
    names(X) <- NULL
  n <- length(X)
  all.same <- integer(n)
  for (i in seq(length = n)) {
    if (is.na(ph[i])) {
      nsam <- 1
    }
    else {
      if (rbinom(1, 1, ph[i]) == 1) {
        nsam <- floor(kh[[i]])
      }
      else {
        nsam <- ceiling(kh[[i]])
      }
    }
    ans <- FUN(X[[i]], size = nsam, ...)
    answer[i] <- list(ans)
    all.same[i] <- length(ans)
  }
  names(answer) <- nms
  if (simplify && length(all.same <- unique(all.same)) == 1 && 
      all.same > 0) {
    if (all.same[1] == 1) 
      unlist(answer, recursive = FALSE)
    else array(unlist(answer, recursive = FALSE), c(all.same, 
                                                    n), list(NULL, names(answer)))
  }
  else answer
}

gini = function(x,y,plot=F){
  Xi <- data.frame(yh=x,Ah=y)
  Xi$P = Xi$yh * Xi$Ah
  Xi = Xi[order(Xi$P),]
  Xi$cP = cumsum(Xi$P) / sum(Xi$P)
  Xi$cA = cumsum(Xi$Ah) / sum(Xi$Ah)
  if(plot) {
    plot(cA,cP,type='l')
    abline(a=0,b=1,col='blue')
  }
  gI=NA
  
  if(nrow(Xi)>1)  gI = 1-(2*sum(rowMeans(embed(Xi$cP,2)) * diff(Xi$cA)))
  return(gI)
}


#Extract data from groundfish survey
#Extract Cat data from groundfish survey
k = groundfish.db(DS="cat.base") #export from grounfish survey database

j = survey.process(k, species, data.yrs)
#Gulf Strata
j = subset(j, !(strat == '558' | strat == '559'))
#Georges Bank Strata
j = subset(j, !(strat == "5Z1"| strat == "5Z2"| strat == "5Z3"| strat == "5Z4"| strat == "5Z8"| strat == "5Z7" |
                  strat == "5Z6"| strat == "5Z5"| strat == "5Z9" ))
#Spring Strata
j = subset(j, !(strat == "396"| strat == "397" | strat == "398" |strat == "399" |strat == "400" |strat == "406" | strat == "401" | 
                  strat == "402" |  strat == "403"| strat ==  "404" |strat ==  "407" | 
                  strat == "405" | strat == "408" | strat == "409" | strat ==  "410" | strat == "411"|
                  strat == "436" |  strat == "437"| strat ==  "438" |strat ==  "439" ))
#Deep Water Strata
j = subset(j, !(strat =="503" | strat == "501" | strat == "502" | strat == "505" | strat == "504" | strat == "558" | strat == "559"))
j = subset(j, !(strat =="498" |strat =="497" |strat == "496"))

j$month = month(j$sdate.x)
j = j[which(j$month > 5 & j$month < 9), ]

#Test
#j = j[which(j$yr == 2006), ]

j$Strata = j$strat
speciesn = j$name.common[1]

#choose variable name
#variable.n = "totno_sd"
variable.n = "totwgt_sd"

#Load file from groundfish.stratum where area is converted to trawlable units divide area by 0.011801 (41ft by 1.75 nm)
#Already converted to list NH = trawalable units, Strata = strata
#Extract Strata Area Information

strat1 = groundfish.db(DS="gsstratum")

#convert strata area to trawlable units 41ft by 1.75 nm, divide area by 0.011801
strat1$nh = as.numeric(strat1$area)/0.0118

strat1 = strat1[order(strat1$strat),]

#convert to list
strat1 = list(Strata = strat1$strat, NH = strat1$nh)

sc.t <- j
st = strat1

#Calculate stratified estimates from the groundfish survey
#---------------------------------------------------------------------------

ci.n = data.frame()
ci.w = data.frame()
yrs = data.yrs
alpha.t = 0.05 #confidence interval eg. 0.05 = 95%, 0.1 = 90%
alpha.b = 0.05
nresamp = 1000
prints = T
method = "BWR"
CI.method = 'BC'
m <- match.call()

for (yr in yrs){
  #subset by year
  sc = sc.t[which(sc.t$yr == yr), ]
  
  #Choose Variable
  variable = sc$totwgt_sd
  
  s.group <- is.element(st$Strata, sc$Strata)
  s.group.Strata <- st$Strata[s.group]
  s.group.NH <- st$NH[s.group]
  s.obj <- is.element(sc$Strata, s.group.Strata)
  var <- variable[s.obj]
  sc <- sc[s.obj, ]

  yhi <- split(var, sc$Strata) #split the variable by strata
  nh <- as.vector(sapply(yhi, length)) #numer of tows per strata
  nhws <- sapply(yhi, function(x) length(x [x > 0])) #calculate the number of samples > 1 in each strata
  
  Strata = s.group.Strata #list of strata
  Nh = s.group.NH #trawalable units in each strata
  Wh = s.group.NH/sum(s.group.NH) #strata percent of the total area in trawlable units
  Sh = sum(s.group.NH)
  call = m
  
  #Calculate Stratified Estimates and Confidence intervals 
  #-------------------------------------------------------------------------------------
  yh <- as.vector(sapply(yhi, mean)) #mean of variable for each strata
  yst <- sum(Wh * yh, na.rm = TRUE) #sum of the mean of the variable for each strata, multiplied by percent area of each strata

  sh <- as.vector(sapply(yhi, var)) #calculate variance of each variable, per strata
  se.yst <- sqrt(sum((((Nh * (Nh - nh))/sum(Nh)^2) * sh)/nh, na.rm = TRUE)) #calculate standard error
  ah <- (Nh * (Nh - nh))/nh #
  df.yst <- (sum(ah * sh, na.rm = TRUE)^2)/(sum(((ah * sh)^2)/(nh - 1), na.rm = TRUE)) #degrees of freedom

  #Calculate the confidence interval, based on the t distribution, rather than the normal distribution
  #qt fuction calculates the value for the 95% confidence interval by looking up the t distribution,
  #based on the degrees of freedom calculated above. This is multiplied by the standard error calculated above
  #formulas are: Lower limit = M - (tCL)(sM), Upper limit = M + (tCL)(sM)
  #------------------------------------------------------------------------------------
  ci.yst <- yst + (c(qt(alpha.t/2, df.yst), -qt(alpha.t/2, df.yst)) * se.yst) #confidence interval
  
  #Calculate Design Weighted Area Occupied
  dwao <- sum(Wh*(nhws / nh)) * sum(Nh) * 0.011801
  
  #Calculate Gini Index
  gi=NA
  
  gi <- gini(x=yh,y=Nh)
  
  #Calculate the total population
  Yst = yst * sum(Nh) 
 
  #Use mirror match method (BWR) to calculate confidence intervals in Bias Corrected (BC) method to calculate confidence interval
  #-------------------------------------------------------------------------
  call <- match.call(expand = FALSE)
  
  out <- matrix(0, nrow = nresamp + 1, ncol = 3, dimnames = list(c("Actual", 1:nresamp), c("Mean", "Variance",'gini')))
  out[1, ] <- c(yst, (se.yst)^2, gi)
  
  
  fh <- nh/Nh
  kh <- (nh - 1)/(1 - fh)
  ph <- ((1/kh) - (1/ceiling(kh)))/((1/floor(kh)) - (1/ceiling(kh)))
  
  for (i in 1:nresamp) {
      yhib <- bwr.boot(yhi, kh, ph, sample, replace = TRUE, simplify = FALSE)
      yhib[nh == 1] <- yhi[nh == 1]
      nhws = sapply(yhib, FUN = function(x) sum(x > 0))
      out[i + 1, ] <- c(sum(Wh * as.vector(sapply(yhib, mean)), na.rm = TRUE),
                      (sum((((Nh * (Nh - as.vector(sapply(yhib, length))))/sum(Nh)^2) *
                      (as.vector(sapply(yhib, var))))/as.vector(sapply(yhib, length)), na.rm = TRUE)),
                       gini(x = as.vector(sapply(yhib, mean)), y = Nh))
    }
  
  
  orig.mean = out[1, 1]
  orig.var = out[1, 2]
  boot.means = out[c(2:(nresamp + 1)), 1]
  boot.vars = out[c(2:(nresamp + 1)), 2]
  gi = out[c(2:(nresamp + 1)), 3]
  call = call
  method = method
  
  #Summary of Bootstrapped means
  #-------------------------------------------------------------------------------------------------------
  options(digits = 4)
  boot.est <- mean(boot.means) #bootstrap mean
  gini.mean <- mean(gi)
  ci.boot=list()
  
  loc.bc <- sum(boot.means < boot.est)
  lim.bc <- sort(boot.means)[c(loc.bc, loc.bc + 1)]
  z0 <- (loc.bc + ((boot.est - lim.bc[1])/(lim.bc[2] - lim.bc[1])))
  z0 <- qnorm(z0/length(boot.means))
  probs.z0 <- pnorm(qnorm(c(alpha.b/2, (1 - alpha.b/2), 0.5)) + 2 * z0)
  
  ci.boot.meanBCA <- BCa(boot.means,0.01,alpha=c(0.025,0.975),mean)
  ci.boot[[1]] <- ci.boot.mean <- c(ci.boot.meanBCA[4:5],ci.boot.meanBCA[3])  #ci.boot.mean <- quantile(boot.means, probs = probs.z0)
  ci.boot.gini <- quantile(gi, probs = c(alpha.b/2, (1 - alpha.b/2), 0.5), na.rm=T)
  
    
  #Print out the yearly estimates and write them to a data frame  
  #--------------------------------------------------------------------------------------------------------
  options(digits = max(options()$digits - 5, 5))
  
  
  if(prints) {cat("\n", "Pop Total =", format(Yst), "\n", 
                    "Original Mean =", format(orig.mean), "\n",
                    "Year =", yr, "\n",
                    "Original Variance =", format(orig.var), "\n",
                    "Number of bootstraps = ", length(boot.means), "\n",
                    "Bootstrap Mean=", format(boot.est), "\n",
                    "Variance of Bootstrap Mean=", format(var(boot.means)), "\n",
                    "CI Method=", c(CI.method), "\n",
                    "CI's for alpha=", alpha.b, "are ", format(ci.boot.mean[1:2]), "\n",
                    "Length =", format(ci.boot.mean[2] - ci.boot.mean[1]), "\n",
                    "Shape=", format(log((ci.boot.mean[2] - ci.boot.mean[3])/(ci.boot.mean[3] - ci.boot.mean[1]))), "\n",
                    "Resample Method = ", method, "\n")
    }
  
  ci.boot[[2]] = ci.boot.gini
  
  n.row <- data.frame(speciesn,
            year = as.numeric(yr),
            pop.total = Yst,
            variable = variable.n,
            orig.mean = as.numeric(format(orig.mean)),
            boot.mean = as.numeric(format(boot.est)),
            var.boot.mean = as.numeric(format(var(boot.means))),
            lower.ci = as.numeric(format(ci.boot.mean[1])),
            upper.ci = as.numeric(format(ci.boot.mean[2])),
            length = as.numeric(format(ci.boot.mean[2] - ci.boot.mean[1])),
            dwao = dwao,
            gini = gini.mean,
            lower.ci.gini = format(ci.boot.gini[1]),
            upper.ci.gini = format(ci.boot.gini[2]))
  

  ci.w = rbind(n.row, ci.w )

}

library(zoo)
#Calculate 3 yr Mean
ci.w$mean.3.yr <- zoo::rollapply(ci.w$boot.mean, 3, mean, fill=NA, align="right")

#Calculate Mean Lines
ci.w$median <- median(ci.w$boot.mean)
ci.w$median.50 <- median(ci.w$boot.mean) * 0.5
#ci.w$gm.40 <- geometric.mean (ci.w$boot.mean) * 0.4

print(ci.w)

return(ci.w)
}





