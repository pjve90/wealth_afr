##################################################################### Helper function
library(cmdstanr)
library(rstan)
library(scales)

inv_logit = function (x){
    p = 1/(1 + exp(-x))
    p = ifelse(x == Inf, 1, p)
    p
}

###################################################################### First simulate data
N = 50
A = 18

Wealth = Wealth_diff = Wealth_sd = matrix(NA, nrow=N, ncol=A)

Alpha = 0.65
Beta = 4.2
Sigma = 1

################## Wealth data
for(n in 1:N){
	Wealth[n,1] = rnorm(1, Beta, Sigma)
 for(a in 2:A){
    Wealth[n,a] = Alpha*Wealth[n,a-1] + (1-Alpha)*rnorm(1, Beta, Sigma)
 }
}

################## Wealth change data
for(n in 1:N){
	Wealth_diff[n,1] = 0
 for(a in 2:A){
    Wealth_diff[n,a] = Wealth[n,a] - Wealth[n,a-1] 
 }
}

################## Wealth sd
for(n in 1:N){
	Wealth_sd[n,1:5] = 0 
 for(a in 6:A){
    Wealth_sd[n,a] = sd(Wealth[n,(a-5):a])
 }
}

################################################################## Now birth choice
Birth = matrix(NA, nrow=N, ncol=A)

Gamma = c(-5, 0.9, -0.5, -5)

for(n in 1:N){
 Birth[n,1] = 0

 for(a in 2:A){
 	if(Birth[n,a-1]==0){
 		Birth[n,a] = rbinom(1,size=1, prob=inv_logit(Gamma[1] + Gamma[2]*Wealth[n,a] + Gamma[3]*Wealth_diff[n,a] + Gamma[4]*Wealth_sd[n,a]))
 	}

 	if(Birth[n,a-1]==1 | Birth[n,a-1]== -99){
 	    Birth[n,a] = -99
       }
 }
}


################################################################## Now simulate Monique
WealthObserved = Wealth

 for(a in 2:A){
 	if(!a %in% c(3,5,9,12,14,16,17)){
    WealthObserved[,a] = -99
 }
}

Loc_miss = which(WealthObserved==-99,arr.ind=TRUE)
N_miss = nrow(Loc_miss)

WealthObserved2 = WealthObserved
WealthObserved2[which(WealthObserved2==-99)] = NA

#################################################################### Fit in Stan
model = cmdstan_model("Misc/wealth_imputation.stan")

model_dat = list(N=N, A=A, Wealth_raw=WealthObserved, Loc_miss=Loc_miss, N_miss=N_miss, Birth=Birth)

m1 = model$sample(
        seed=1,
        data=model_dat, 
        iter_warmup=1000,
        iter_sampling=1000,
        chains=1,
        refresh=1,
        max_treedepth=12, #default treedepth is 10
        adapt_delta=0.97
      )

stanfit = rstan::read_stan_csv(m1$output_files())
WealthEstimate = rstan::extract(stanfit,pars="Wealth")$Wealth
WealthDiffEstimate = rstan::extract(stanfit,pars="WealthDiff")$WealthDiff
WealthSDEstimate = rstan::extract(stanfit,pars="WealthSD")$WealthSD

print(stanfit, pars=c("Alpha","Beta","Gamma"))

###################### Plot
L = 2
H = 8
focal = 25

plot(Wealth[focal,], type="n", ylim=c(L,H))

for(i in 1:1000)
 lines(WealthEstimate[i,focal,], col=alpha("slateblue", 0.05))
points(Wealth[focal,], col="black",pch=20)
points(WealthObserved2[focal,], col="darkred",pch=20)


########## Plot 2
L = 0
H = 9
focal = 25

plot(Wealth_diff[focal,], type="n", ylim=c(-3,3))

for(i in 1:1000)
 lines(WealthDiffEstimate[i,focal,], col=alpha("slateblue", 0.05))
points(Wealth_diff[focal,], col="black",pch=20)


########## Plot 3
L = 0
H = 9
focal = 25

plot(Wealth_sd[focal,], type="n", ylim=c(-3,3))

for(i in 1:1000)
 lines(WealthSDEstimate[i,focal,], col=alpha("slateblue", 0.05))
points(Wealth_sd[focal,], col="black",pch=20)

