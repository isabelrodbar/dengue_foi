data {
  int <lower=0> A; //the number of age classes
  int <lower=0> Ag; //the max age
  int <lower=0> T; //the number of observation time points
  int <lower=0> Y; // number of years represented in the data
int <lower=0> L; // number of lambdas represented in the data
  int <lower=0>	I[A,T]; //the number of cases reported at each time point for each age group
  int <lower=0> C[A,T]; // the total population at each time point for each age group
  int <lower=0> ind_lambda[Ag,T]; // indices for lambdas
  int <lower=0> ind_age[A]; // indices for ages

}


parameters {
real logit_lambda[L]; //  specific hazards
real logit_phi[T]; // logit of reporting rates

}

transformed parameters {
  real lambda_year[Ag,T];
real cum_lambda[Ag-1,T];
real susc[Ag-1, T];
real mono[Ag-1, T];
real mono_dat[A, T];
real poi_rate[A, T];
real lambda[L]; // yearly specific hazards
real phi[T]; // reporting rates 


for(i in 1:L) {
lambda[i]=inv_logit(logit_lambda[i]);
}

for(i in 1:T) {
phi[i]=inv_logit(logit_phi[i]);
}


for (i in 1:T) {
    for (j in 1:Ag) {
        //print(ind_lambda[j,i])
    	lambda_year[j,i] = lambda[ind_lambda[j,i]] ;
	}
 }

//print(ind_lambda);
for (i in 1:T) {
    cum_lambda[1,i]=lambda_year[2,i];
    for (j in 2:(Ag-1)) {
    	cum_lambda[j,i]=cum_lambda[j-1, i] + lambda_year[j+1,i];
					 }
			}

for( i in 1:T) {
     for (j in 1:Ag-1) {
     	 susc[j,i]=exp(-4*cum_lambda[j,i]);
	 mono[j,i]=4*susc[j,i]*(exp(cum_lambda[j,i])-1);
	 }
}

for (i in 1:T) {
    for (j in 1:A) {
    	mono_dat[j,i]=mono[ind_age[j], i];
	}
}

for (i in 1:T) {
    for (j in 1:A) {
    	poi_rate[j,i]=mono_dat[j,i]*3*lambda[ind_lambda[1,T-i+1]]*phi[T-i+1]*C[j,i];
	}
}
}

model {
for(i in 1:L) {
 logit_lambda[i]~normal(0,1000);
 }

for(i in 1:T) {
logit_phi[i] ~ normal(0,1000);
}

for (i in 1:T) {
    for (j in 1:A) {
    	I[j,i]~ poisson(poi_rate[j,i]);
	}
}

}













