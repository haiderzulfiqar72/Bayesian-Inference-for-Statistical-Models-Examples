---
title: "Take Home Task"
author: "Haider Zulfiqar"
date: "October 26, 2022"
output:
  pdf_document: default
  word_document: default
---
  
  #Libraries
  ```{r}
library(rstan)
library(StanHeaders)
library(ggplot2)
library(bayesplot)
```  
# Question 1 (a)
## Stan Model with Uniform Prior (0,1)
```{r}
model =" 

data { 
  int<lower=0> n;
  int<lower=0, upper=n> y;
} 

parameters { 
  real<lower=0, upper=1> theta;
} 

model { 
// prior
  theta ~ uniform(0, 1);
// likelihood
  y ~ binomial(n,theta);
} 
" 
```

## Fitting the Model and Plot
``` {r}
fit_1 <- stan(model_code = model, data = list(n=20, y=12), warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1) 
posterior_1 <- as.matrix(fit_1)
plot(fit_1)

plot_title <- ggtitle("Posterior Distributions")
mcmc_dens(posterior_1,
          pars = c("theta"),alpha=0.2) + 
  plot_title  + 
  geom_density(data=data.frame(list(x=runif(1000000, 0,1))), aes(x=x))
```

# Part b
## Stan Model with Beta Prior (10,2)
```{r}
model =" 

data { 
  int<lower=0> n;
  int<lower=0, upper=n> y;
} 

parameters { 
  real<lower=0, upper=1> theta;
} 

model { 
// prior
  theta ~ beta(10, 2);
// likelihood
  y ~ binomial(n,theta);
} 
" 
```  

## Fitting the Model and Plot
```{r}
fit_2 <- stan(model_code = model, data = list(n=20, y=12), warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1) 
posterior_2 <- as.matrix(fit_2)
plot(fit_2)

plot_title <- ggtitle("Posterior Distributions")
mcmc_dens(posterior_2,
          pars = c("theta"),alpha=0.2) + 
  plot_title  + 
  geom_density(data=data.frame(list(x=rbeta(1000000, 10,2))), aes(x=x))
```  

Comments: In the first part where we have a uniform prior(0,1), our prior is not influential at all for our poster distribution, and likelihood is highly influencial in determining the posterior distribution here In the second case with beta as a the prior, now our posterior distribution has been pushed to the right as prior is influential in this case now.
 
# Question 2 
## Stan Model 
```{r}
model =" 

data { 
  real y[15];
} 

parameters { 
  real<lower=0> si;
  real mu;
} 

model { 
// hyper prior
  si ~ gamma(0.35,1.01);
// prior
  mu ~ normal(0,(1/0.003)/si);
// likelihood
  y ~ normal(mu,1/si);

} 
" 
```
## Fitting the Model and Plot
```{r}
fit_3 <- stan(model_code = model, data = list(y= c(-9, -4, -21, -3, -20, -31, -17, -26, -26, -10, -23, -33, -19, -19, -23)), warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1) 
posterior_3 <- as.matrix(fit_3)
plot(fit_3)

plot_title <- ggtitle("Posterior Distributions")
mcmc_dens(posterior_3,
          pars = c("mu","si"),alpha=0.2) + 
  plot_title  

mcmc_areas(posterior_3, pars = c("mu"),prob=0.95)
```
Comments: As 95% CI for mu is centred around -19, there would be a big effect of drug

# Question 3 
## Fitting the Stan Model
```{r}
model =" 
data {
  int<lower=0> N;
  real y[N];
  vector[N] w;  
  vector[N] scyl; 
  vector[N] ecyl;
  vector[4] x_not;
  // matrix[1,4] x_not;
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(beta[1] + beta[2] * w + beta[3] * scyl + beta[4] * ecyl, sigma);
  beta ~ normal(0,10000);
  1/sigma ~ gamma(0.01,0.01);
}
generated quantities {
  real y_tilde = normal_rng(x_not[1]*beta[1]+x_not[2]*beta[2]+x_not[3]*beta[3]+x_not[4]*beta[4],sigma);
}
"
```
## Fitting the Model and Plot 
```{r}
fit_4 <- stan(model_code = model, data=list(N=32,
                                            y= c(21.0, 21.0, 22.8, 21.4,18.7,18.1,14.3, 24.4, 22.8,19.2,17.8,16.4,17.3,15.2,10.4,10.4,14.7,
                                                 32.4, 30.4, 33.9, 21.5, 15.5,15.2,13.3,19.2, 27.3, 26.0, 30.4,15.8,19.7,15.0, 21.4), 
                                            w=c(2.620, 2.875, 2.320,3.215,3.440, 3.460, 3.570, 3.190, 3.150, 3.440, 3.440, 4.070, 3.730,
                                                3.780, 5.250, 5.424, 5.345, 2.200, 1.615, 1.835, 2.465, 3.520, 3.435, 3.840, 3.845, 1.935, 2.140,
                                                1.513, 3.170, 2.770, 3.570, 2.780), 
                                            scyl=c(1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), 
                                            ecyl=c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0),
                                            x_not=c(1,3.5,0,0)), warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1) 
posterior_4 <- as.matrix(fit_4)
plot(fit_4)

plot_title <- ggtitle("Posterior Distributions")
mcmc_dens(posterior_4,
          pars = c("beta[1]", "beta[2]", "beta[3]", "beta[4]","sigma"),alpha=0.2) + 
  plot_title  
mcmc_areas(posterior_4, pars = c("beta[1]"),prob=0.95)
mcmc_areas(posterior_4, pars = c("beta[2]"),prob=0.95)
mcmc_areas(posterior_4, pars = c("beta[3]"),prob=0.95)
mcmc_areas(posterior_4, pars = c("beta[4]"),prob=0.95)
```  
Comment: 95% CI for beta 2 is somewhere around -3.1 and beta 3 around -4, so the effect on mpg is bit different in cars with six cylinders compared to cars with 8 cylinders.
