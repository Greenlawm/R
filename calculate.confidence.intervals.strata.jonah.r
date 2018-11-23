calculate.confidence.intervals.strata = function (species, data.yrs) {
#-----------------------------------------------------------------
p = setup.parameters()
loadfunctions()
#data.yrs = 2000:2017

#species = c('6600', '6611') #sea cucumber, #species = c('2511') #Jonah Crab, #c('4211', '4210', '1510') #whelk, c('25') #tilefish
#halibut = c('30') 

#species =c('30')
#Extract Cat data from groundfish survey
k = groundfish.db(DS="cat.base") #export from grounfish survey database

#k = k[k$strat %in% c('454'), ]
#k = k[k$yr %in% c('2016'), ]

j = survey.process(k, species)

j$Strata = j$strat

#species = c("sea cucumber")
species = c('jonah crab')

#Extract Strata Information
strat1 = groundfish.db(DS="gsstratum")

#convert strata area to trawlable units 41ft by 1.75 nm, divide area by 0.011801
strat1$nh = as.numeric(strat1$area)/0.011801

#convert to list
strat1 = list(Strata = strat1$strat, NH = strat1$nh)

sc <- sc.t <- j
st = strat1
#---------------------------------------------------------------------------

#load('StrataDetails.rdata') # these are called st --
#load('StrataData.rdata') #theses are called sc -- make your data files look like this
ci.n = data.frame()
ci.w = data.frame()
yrs = p$data.yrs
alpha.t = 0.05
alpha.b = 0.05
nresamp=1000
prints= T
method = "BWR"
CI.method = 'BWR'
variable.n = "totno_sd"
#variable.n = "totwgt_sd"
m <- match.call()

for (yr in yrs){
sc = sc.t[which(sc.t$yr == yr), ]
variable = sc$totno_sd
#variable = sc$totwgt_sd

s.group <- is.element(st$Strata, sc$Strata)
s.group.Strata <- st$Strata[s.group]
s.group.NH <- st$NH[s.group]
s.obj <- is.element(sc$Strata, s.group.Strata)
variable <- variable[s.obj]
sc <- sc[s.obj, ]

yhi <- split(variable, sc$Strata) #split the variable by strata
nh <- as.vector(sapply(yhi, length))
nhws <- sapply(yhi, function(x) length(x[x>0])) #Calculate the number of samples >1 in each strata

res <- list(yhi = yhi, #variable per strata
            Strata = s.group.Strata, #list of strata
            Nh = s.group.NH, #trawalable units in each strata
            Wh = s.group.NH/sum(s.group.NH), #strata percent of the total area in trawlable units
            nh = nh, #number of tows per strata
            call = m,
            nhws = nhws) #number of tows with > 1 species, in each strata

#Stratified Estimates
#ssW = summary.strata(sW)
#summary.strata <- function (res, alpha.t = 0.05, effic = FALSE, nopt = FALSE, ...)
#-------------------------------------------------------------------------------------
yh <- as.vector(sapply(res$yhi, mean)) #mean of variable for each strata
yst <- sum(res$Wh * yh, na.rm = TRUE) #sum of the mean of variable for each strata multiplied by percent area of each strata
sh <- as.vector(sapply(res$yhi, var)) #calculate variance of each variable, per strata
se.yst <- sqrt(sum((((res$Nh * (res$Nh - res$nh))/sum(res$Nh)^2) * sh)/res$nh, na.rm = TRUE)) #calculate standard error?
ah <- (res$Nh * (res$Nh - res$nh))/res$nh #
df.yst <- (sum(ah * sh, na.rm = TRUE)^2)/(sum(((ah * sh)^2)/(res$nh - 1), na.rm = TRUE)) #degrees of freedom
ci.yst <- yst + (c(qt(alpha.t/2, df.yst), -qt(alpha.t/2, df.yst)) * se.yst) #confidence interval

if(any(length(res$Wh) != length(res$nhws) | length(res$nhws)!= length(res$nh))) browser()

#Calculate Design Weighted Area Occupied
dwao <- sum(res$Wh*(res$nhws / res$nh)) * sum(res$Nh) * 0.011801

#Calculate Gini Index
gi=NA

gi <- gini(x=yh,y=res$Nh)

res2 <- list(yst = yst, #stratified mean
            se.yst = se.yst, #standard error
            Yst = yst * sum(res$Nh), #population total
            df.yst = df.yst, #degrees of freedom
            alpha = alpha.t, #interval
            ci.yst = ci.yst, #confidence interval
            dwao = dwao, #design weighted area occupied
            gini = gi) #pachiness index


options(digits = max(options()$digits - 5, 5))

#Bootstrapping with replacement
#boot.strata <- function (data, nless = 0, nresamp = 1, method = c("RESCALE",   "BWR", "NAIVE")) {
#bsW = summary.boot(boot.strata(sW,method='BWR', nresamp=1000), ci.method='BC') #bootstrapping with replacement BWR
#-------------------------------------------------------------------------
call <- match.call(expand = FALSE)
res1 <- res
res3 <- res2

out <- matrix(0, nrow = nresamp + 1, ncol = 3, dimnames = list(c("Actual", 1:nresamp), c("Mean", "Variance",'gini')))
out[1, ] <- c(res3$yst, (res3$se.yst)^2, res3$gini)


fh <- res1$nh/res1$Nh
kh <- (res1$nh - 1)/(1 - fh)
ph <- ((1/kh) - (1/ceiling(kh)))/((1/floor(kh)) - (1/ceiling(kh)))

for (i in 1:nresamp) {
    yhib <- bwr.boot(res1$yhi, kh, ph, sample, replace = TRUE, simplify = FALSE)
    yhib[res1$nh == 1] <- res1$yhi[res1$nh == 1]
    nhws = sapply(yhib,FUN=function(x) sum(x>0))
    out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, mean)), na.rm = TRUE),
                    (sum((((res1$Nh * (res1$Nh - as.vector(sapply(yhib, length))))/sum(res1$Nh)^2) *
                    (as.vector(sapply(yhib, var))))/as.vector(sapply(yhib, length)), na.rm = TRUE)),
                     gini(x=as.vector(sapply(yhib, mean)),y=res1$Nh))
  }


res4 <- list(orig.mean = out[1, 1], orig.var = out[1, 2],
              boot.means = out[c(2:(nresamp + 1)), 1],
              boot.vars = out[c(2:(nresamp + 1)), 2],
              gini = out[c(2:(nresamp + 1)), 3],
              accel = accel.str(res1),
              call = call,
              method = method)

#Summary of Bootstrapped means
# bsW = summary.boot(boot.strata(sW,method='BWR',nresamp=1000),ci.method='BC') #bootstrapping with replacement BWR
# CI.method = c("Percentile", "BC", "BCa")
#-------------------------------------------------------------------------------------------------------

#summary.boot <- function (res, CI.method = c("Percentile", "BC", "BCa"), gini=T, alpha.b = 0.05, prints=F, big.ci=F,    ...) {
  options(digits = 4)
  boot.est <- mean(res4$boot.means)
  ci.boot=list()

  loc.bc <- sum(res4$boot.means < boot.est)
  lim.bc <- sort(res4$boot.means)[c(loc.bc, loc.bc + 1)]
  z0 <- (loc.bc + ((boot.est - lim.bc[1])/(lim.bc[2] - lim.bc[1])))
  z0 <- qnorm(z0/length(res4$boot.means))
  probs.z0 <- pnorm(qnorm(c(alpha.b/2, (1 - alpha.b/2), 0.5)) + 2 * z0)
  ci.boot[[1]] <- ci.boot.mean <- quantile(res4$boot.means, probs = probs.z0)
  ci.boot.gini <- quantile(res4$gini, probs = c(alpha.b/2, (1 - alpha.b/2), 0.5), na.rm=T)

if(prints) {cat("\n", "Original Mean =", format(res4$orig.mean), "\n",
                  "Year =", yr, "\n",
                  "Original Variance =", format(res4$orig.var), "\n",
                  "Number of bootstraps = ", length(res4$boot.means), "\n",
                  "Bootstrap Mean=", format(boot.est), "\n",
                  "Variance of Bootstrap Mean=", format(var(res4$boot.mean)), "\n",
                  "Method=", c(CI.method), "\n",
                  "CI's for alpha=", alpha.b, "are ", format(ci.boot.mean[1:2]), "\n",
                  "Length =", format(ci.boot.mean[2] - ci.boot.mean[1]), "\n",
                  "Shape=", format(log((ci.boot.mean[2] - ci.boot.mean[3])/(ci.boot.mean[3] - ci.boot.mean[1]))), "\n",
                  "Method = ", res4$method, "\n")
  }

ci.boot[[2]] = ci.boot.gini

  n.row <- data.frame(species,
             year = as.numeric(yr),
             variable = variable.n,
             orig.mean = as.numeric(format(res4$orig.mean)),
             boot.mean = as.numeric(format(boot.est)),
             var.boot.mean = as.numeric(format(var(res4$boot.mean))),
             lower.ci = as.numeric(format(ci.boot.mean[1])),
             upper.ci = as.numeric(format(ci.boot.mean[2])),
             length = as.numeric(format(ci.boot.mean[2] - ci.boot.mean[1])),
             dwao = dwao,
             gini = res2$gini,
             lower.ci.gini = format(ci.boot.gini[1]),
             upper.ci.gini = format(ci.boot.gini[2]))




  ci.w = rbind(n.row, ci.w )

}
print(ci.w)

return(ci.w)

}









