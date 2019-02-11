
#### Fit models for specific countries
library(rstan)
library(coda)

### Source likelihood
source("Code/ll.1_pois.R")

### Source function to plot output
source("Code/figure_plot_stan_output.R")

#### Load example Brazil data
load("Data/dhf_brazil_2002_2015.Rdata")
load("Data/census_brazil_2002_2015.Rdata")

######## Generate matrix with indices for lambda (needed to ease likelihood calculation)
age<-1:60
years.int<-seq(2015, by=-1, length.out=73)  ##Years represented in the data
year.counts<-2002:2015 ## Years of available data
#Assuming age represents ceiling of age group
dob<-t(outer(year.counts, (age-1), FUN="-"))
dob2<-apply(dob, 2, function(x) {seq(max(x), min(x), -1)}) # necessary if there are gaps

indices.lambda<-apply(dob2, 2,  function(x)(which(is.element(years.int, x))))

### Assign constant lambda for specific periods of time - in this case we are estimating constant lambda for the last 20 years.
indices.lambda2<-matrix(as.numeric(cut(indices.lambda, c(0, 20, 75))), ncol=14)

### If instead we want to estimate yearly lambda
#indices.lambda2<-indices.lambda

### Calculate mid-point of age group (needed to calculate likelihood)
roof_age<-c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 100)
mid_age<-ceiling(roof_age[1:18] +diff(roof_age)/2)

    
#### ML optimization to obtain reasonable proposal distributions - may need this for certain datasets
# vals<-100000
# k<-1
# 
# while(k<16 | (k<30 & !exists("error.mat"))) {
#   if(k%%5==0) {print(k)}
#   init.guess<-rnorm(length(rep(-3.5, times=c(max(indices.lambda2)+length(years.int)))), -4)
#   
#   opt.prov.i<-  optim (par=init.guess, fn=ll.1_pois,  counts=cases_dhf_prov[1:13,1:14], census=census.data_prov[1:13, 1:14],  years=years.int, age=mid_age[1:13], indices.lambda=indices.lambda2[1:59, 1:14],
#                        method="BFGS",
#                        control=list(trace=FALSE,  maxit=1000000),
#                        hessian=TRUE)
#   
#   if(opt.prov.i$val<vals) {opt.prov<-opt.prov.i
#   vals<-opt.prov.i$val}
#   
#   if(k==14) {try(error.mat <- diag(sqrt(solve(opt.prov$hessian)))[1:5], silent=T)    } 							
#   if(k>=15 & !exists("error.mat")) {
#     try(error.mat <- diag(sqrt(solve(opt.prov$hessian)))[1:5], silent=T)	
#     try(error.mat2 <- diag(sqrt(solve(opt.prov.i$hessian)))[1:5], silent=T) 
#     if (round(opt.prov$par[1],4)==round(opt.prov.i$par[1],4) & round(opt.prov$val, 3) == round(opt.prov.i$val, 3)) {
#       error.mat<-error.mat2
#       opt.prov<-opt.prov.i
#       vals<-opt.prov.i$val}
#     print(opt.prov$val)
#     print(opt.prov.i$val)
#   }
#   k<-k+1
#   
# }
# 
# 
## Starting values function (from optim) to feed stan
# init.fun<-function(fit.mod) {list(logit_lambda=rnorm(max(indices.lambda2), mean=opt.prov$par[1:max(indices.lambda2)], sd=.5), logit_phi=rnorm(length(year.counts), mean=opt.prov$par[(max(indices.lambda2)+1):length(opt.prov$par)], 1))}


### Generate data list to feed into stan     
data_prov<-list(A=13, Ag=60, T=ncol(cases_dhf_prov[1:13,1:14]), Y=length(years.int), L=max(indices.lambda2), I=cases_dhf_prov[1:13,1:14], C=floor(census.data_prov[1:13, 1:14]), ind_lambda=indices.lambda2[1:60,], ind_age=mid_age[1:13])
 

### Compile Stan code  
comp2<-stan(file="Code/stan_code1.stan", data=data_prov, chains=0) 

### Run chains 

### With starting values
#system.time(post1_braz<-stan(fit = comp2, data = data_prov, chains = 4, iter=20000, thin=10, init=init.fun))

### Without starting values
set.seed(.98342)
system.time(post1_braz<-stan(fit = comp2, data = data_prov, chains = 4, iter=2000, thin=10, control = list(adapt_delta = 0.80)))

### Extract and explore chains
ext_lp<-extract(post1_braz, pars="lp__", inc_warmup=FALSE, permute=F)

### Check _lp of chains
plot(ext_lp)
#ind.keep<-which(max(apply(ext_lp, 2, mean))-apply(ext_lp, 2, mean)<10) # if need to exclude a given chain

### Extract parameters of interest
#ext_keep<-extract(post1_braz, pars=c("lambda", "phi"))
ext_lambda<-extract(post1_braz, pars="lambda", inc_warmup=FALSE, permute=F)
ext_lambda_keep<-ext_lambda
#ext_lambda_keep<-ext_lambda[, ind.keep, ]

## Get quantiles
apply(ext_lambda_keep, 3, mean)
apply(ext_lambda_keep, 3, function(x) quantile(x, c(0.025, 0.975)))

## Some diagnostic plots in Coda package
#posterior_coda <- do.call(mcmc.list, alply(ext_lambda_keep, 2, mcmc) )
#plot(posterior_coda)

### Plot fits
### Extract generated quantiles
#ext_poi<-extract(post1_braz, pars="poi_rate_g")

#pdf(file="Figures/fit_Brazil.pdf", height=10, width=7)       
#plot.stan.fun(ext_poi, count.data=cases_dhf_prov[1:13,1:14], mid_age=ceiling(mid_age[1:13]), title="Brazil", year.counts=year.counts, ind.keep=ind.keep)
#dev.off()
