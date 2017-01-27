##############
### FUNCTION TO CALCULATE NAIVE, INEFFICIENT AND CS ESTIMATORS
### WITH BOOTSTRAP ESTIMATOR FOR CS VARIANCE
##############
calculateEstimators = function(k1,stage1Mu,stage1SE,stage2Mu,stage2SE,pvalThresh,boot) {

# INPUTS
#
# k1 = number of effect sizes being estimated
# stage1Mu = estimates from the first stage (eg a GWAS)
# 	   (in a case/control study these should be LOG odds ratios)
# stage1SE = standard error of the estimates in first stage
# stage2Mu = estimates from the second stage (eg replication study)
# stage2SE = standard error of the estimates in second stage
# pvalThresh = p-value threshold for selecting effects from the first stage
# 	     if not known, make it equal or close to the p-value for Mu/SE
#	     in the last effect taken forward from stage 1
# boot = number of bootstrap samples to draw in estimating SE of the UMVCUE
#      	 (try a small number eg 10 to estimate the running time)

# OUTPUTS
#
# naive = k1-vector of "naive" (max likelihood) combination of stages 1 and 2
# ineff = k1-vector of "inefficient" estimates, should be equal to stage2Mu
# CS = k1-vector of modified Cohen/Sackrowitz estimators - the unbiased UMVCUE
# CSboot_var = k1-vector of boostrap variance estimats for the CS estimators

  X = NULL
  Y = NULL
  SE.X = NULL
  SE.Y = NULL
  naive = NULL
  ineff = NULL
  CS = NULL
  CSboot_var = NULL

  for(p in 1:k1){

    # rank by p-value
    group        = which(rank(abs(stage1Mu)/stage1SE) == (k1+1)-p)
    X[p]         = stage1Mu[group]
    Y[p]         = stage2Mu[group]
    SE.X[p]      = stage1SE[group]
    SE.Y[p]      = stage2SE[group]
  }

  sd1    = SE.X
  sd2    = SE.Y
  sd21   = (sd2/sd1)
  sd12   = (sd1/sd2)
  SD     = sqrt(sd1^2 + sd2^2)
  alpha1 = sd21 + sd12
  alpha2 = SD/(1 + sd12^2)
  
###########################
# limits for integration  #
###########################

  # allow for the pvalue threshold in the integration
  lastLimit=qnorm(pvalThresh/2,lower=F)
  A = c(Inf,abs(X)/SE.X,lastLimit)
  
  for(p in 1:k1){

    group=which(rank(abs(stage1Mu)/stage1SE) == (k1+1)-p)
    
    Z    = sd21[p]*X[p] + sd12[p]*Y[p]
    UB   = A[p]*SE.X[p]
    LB   = A[p+2]*SE.X[p]

###################
# naive estimator #
###################

    naive[p] = Z/alpha1[p]

#########################
# inefficient estimator #
#########################

    ineff[p] = Y[p]

#########################
# modified cohen and    #
# sackrowitz estimator  #
#########################

    Wpm1.plus    = ((Z/alpha1[p]) - UB)/((sd12^2)[p]*alpha2[p])
    Wpp1.plus    = ((Z/alpha1[p]) - LB)/((sd12^2)[p]*alpha2[p])
    Wpm1.minus    = ((Z/alpha1[p]) + UB)/((sd12^2)[p]*alpha2[p])
    Wpp1.minus    = ((Z/alpha1[p]) + LB)/((sd12^2)[p]*alpha2[p])
    #print(c(Wpm1.plus,Wpp1.plus,Wpm1.minus,Wpp1.minus))
    NUM     = (dnorm(Wpm1.plus) - dnorm(Wpp1.plus) + dnorm(Wpp1.minus) - dnorm(Wpm1.minus)) 
    DEN     = (pnorm(Wpm1.plus) - pnorm(Wpp1.plus) + pnorm(Wpp1.minus) - pnorm(Wpm1.minus))
    CS[p] = (Z/alpha1[p]) - alpha2[p]*(NUM/DEN)

###############################
# estimate bootstrap variance #
# for chosen single SNP       #
###############################

# generate boot X data subject to 
# real data contraints

  upperpower1 = pnorm(UB,mean=CS[p],sd=SE.X[p])
  upperpower2 = pnorm(LB,mean=CS[p],sd=SE.X[p])
  upperpower  = upperpower1-upperpower2
  lowerpower1 = pnorm(-LB,mean=CS[p],sd=SE.X[p]) 
  lowerpower2 = pnorm(-UB,mean=CS[p],sd=SE.X[p])
  lowerpower  = lowerpower1 - lowerpower2
  ratio       = upperpower/(upperpower+lowerpower)
  Xboot       = NULL

  for(i in 1:boot){
  	if (runif(1)<ratio){Xboot[i]=qnorm(runif(1,upperpower2,upperpower1),mean=CS[p],sd=SE.X[p])}
	else {Xboot[i]=qnorm(runif(1,lowerpower2,lowerpower1),mean=CS[p],sd=SE.X[p])}
  }

  Yboot         = rnorm(boot,mean=CS[p],sd=SE.Y[p])
  Zboot         = sd21[p]*Xboot + sd12[p]*Yboot
  Wpm1.plus     = ((Zboot/alpha1[p]) - (1)*UB)/((sd12^2)[p]*alpha2[p])
  Wpp1.plus     = ((Zboot/alpha1[p]) - (1)*LB)/((sd12^2)[p]*alpha2[p])
  Wpm1.minus    = ((Zboot/alpha1[p]) - (-1)*UB)/((sd12^2)[p]*alpha2[p])
  Wpp1.minus    = ((Zboot/alpha1[p]) - (-1)*LB)/((sd12^2)[p]*alpha2[p])
  NUM           = (dnorm(Wpm1.plus) - dnorm(Wpp1.plus) + dnorm(Wpp1.minus) - dnorm(Wpm1.minus))
  DEN           = (pnorm(Wpm1.plus) - pnorm(Wpp1.plus) + pnorm(Wpp1.minus) - pnorm(Wpm1.minus))
  CSboot        = (Zboot/alpha1[p]) - alpha2[p]*(NUM/DEN)
  CSboot_var[p] = var(CSboot)

  } # for p
  
  return(list(naive=naive,ineff=ineff,CS=CS,CSboot_var=CSboot_var))
}
