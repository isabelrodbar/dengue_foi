
expit.fun<-function(x) {exp(x)/(1+exp(x))}

#### POISSON
ll.1_pois<- function(theta, counts, census, years,  age, indices.lambda) {
  
  theta<-exp(theta)
  
  #logdataterms<-rep(0, length(counts[1,]))
  age<-age
  
  lengthage<-length(age)
  
  ### Get mid points of age groups that will serve as indices
  
  
  ### Generate a matrix that will contain all of the appropriate lambdas
  mat.lambda<-matrix(NA, nrow=max(age)+1, ncol=ncol(counts))
  theta.ag<-theta[1:max(indices.lambda)]
  theta.cur.year<-theta.ag[indices.lambda[1,]]
  report.rate<-theta[(max(indices.lambda)+1):length(theta)]
  mat.lambda[,]<-theta.ag[indices.lambda]
  mat.lambda<-mat.lambda[2:nrow(mat.lambda),] ## susceptibles up to last   year
  
  ### Sum the forces of infection up to age a
  cum.lambda<-apply(mat.lambda, 2, cumsum)
  
  ##Susceptibles
  x <- exp(-4*cum.lambda)
  
  ### People who have only experienced one infection up to the previous year and are therefore susceptible to suffer a secondary infection during this year
  zi<-4*x*(exp(cum.lambda)-1)
  
  ### Multitypics
  #zm_all<-1+ 3*exp(-4*cum.lambda)- 4*exp(-3*cum.lambda)
  
  ###Monotypics in counts
  zi.dat<-zi[age,]
  
  ### Multiply these proportions by reporting rate and lambda for that particular year
  lambda.report<- 3*theta.cur.year*rev(report.rate)
  ##Assuming year dependent (age inespecific reporting rates)
  zi.report<-sweep(zi.dat, 2, lambda.report, "*")
  
  #Multiply this matrix by census
  zi.report.pop<-zi.report*census
  
  ### Compute likelihood
  likeli<-dpois(counts, as.matrix(zi.report.pop), log=T)
  
  sum.likeli<-sum(likeli)
  
  #print(sumlogl)
  
  return(-sum.likeli)
}




ll.1_pois_logit<- function(theta, counts, census, years,  age, indices.lambda) {

        theta<-expit.fun(theta)

	#logdataterms<-rep(0, length(counts[1,]))
        age<-age

	lengthage<-length(age)

        ### Get mid points of age groups that will serve as indices
  
      
        ### Generate a matrix that will contain all of the appropriate lambdas
        mat.lambda<-matrix(NA, nrow=max(age)+1, ncol=ncol(counts))
        theta.ag<-theta[1:max(indices.lambda)]
        theta.cur.year<-theta[1:ncol(counts)]
        report.rate<-theta[(max(indices.lambda)+1):length(theta)]
        mat.lambda[,]<-theta.ag[indices.lambda]
        mat.lambda<-mat.lambda[2:nrow(mat.lambda),] ## susceptibles up to last   year

        ### Sum the forces of infection up to age a
        cum.lambda<-apply(mat.lambda, 2, cumsum)

        ##Susceptibles
        x <- exp(-4*cum.lambda)

### People who have only experienced one infection up to the previous year and are therefore susceptible to suffer a secondary infection during this year
        zi<-4*x*(exp(cum.lambda)-1)

        ### Multitypics
        #zm_all<-1+ 3*exp(-4*cum.lambda)- 4*exp(-3*cum.lambda)

        ###Monotypics in counts
        zi.dat<-zi[age,]

        ### Multiply these proportions by reporting rate and lambda for that particular year
        lambda.report<- rev(3*theta.cur.year)*rev(report.rate)
        ##Assuming year dependent (age inespecific reporting rates)
        zi.report<-sweep(zi.dat, 2, lambda.report, "*")

        #Multiply this matrix by census
        zi.report.pop<-zi.report*census

        ### Compute likelihood
        likeli<-dpois(counts, as.matrix(zi.report.pop), log=T)

        sum.likeli<-sum(likeli)

	#print(sumlogl)

	return(-sum.likeli)
}

ll.1_pois2<- function(theta, counts, census, years,  age, indices.lambda) {
  
  theta<-exp(theta)
  
  #logdataterms<-rep(0, length(counts[1,]))
  age<-age
  
  lengthage<-length(age)
  
  ### Get mid points of age groups that will serve as indices
  
  
  ### Generate a matrix that will contain all of the appropriate lambdas
  mat.lambda<-matrix(NA, nrow=max(age)+1, ncol=ncol(counts))
  theta.ag<-theta[1:max(indices.lambda)]
  theta.cur.year<-theta[1:ncol(counts)]
  report.rate<-theta[(max(indices.lambda)+1):length(theta)]
  mat.lambda[,]<-theta.ag[indices.lambda]
  mat.lambda<-mat.lambda[2:nrow(mat.lambda),] ## susceptibles up to last   year
  
  ### Sum the forces of infection up to age a
  cum.lambda<-cumsum(mat.lambda)
  
  ##Susceptibles
  x <- exp(-4*cum.lambda)
  
  ### People who have only experienced one infection up to the previous year and are therefore susceptible to suffer a secondary infection during this year
  zi<-4*x*(exp(cum.lambda)-1)
  
  ### Multitypics
  #zm_all<-1+ 3*exp(-4*cum.lambda)- 4*exp(-3*cum.lambda)
  
  ###Monotypics in counts
  zi.dat<-zi[age]
  
  ### Multiply these proportions by reporting rate and lambda for that particular year
  lambda.report<- rev(3*theta.cur.year)*rev(report.rate)
  ##Assuming year dependent (age inespecific reporting rates)
  zi.report<-zi.dat*lambda.report
  
  #Multiply this matrix by census
  zi.report.pop<-zi.report*census
  
  ### Compute likelihood
  likeli<-dpois(counts, as.matrix(zi.report.pop), log=T)
  
  sum.likeli<-sum(likeli)
  
  #print(sumlogl)
  
  return(-sum.likeli)
}


	