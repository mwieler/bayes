library("ggplot2")
#define the prior distribution. For now, define it as a discrete
possible_values = seq(0.05,0.95,by=0.1)
weights = c(2,4,8,8,4,2,1,1,1,1)
prior_probabilities = weights/sum(weights)
#
discrete.prior<-cbind("p" = possible_values,"p(p)" = prior_probabilities)


#plot the prior distribution
title = "Proportion of Students that get Enough Sleep (p)"
ylab = "Prior (Estimated) Probability"
xlab = "Possible True Values of p"
possible_values_range = max(possible_values)-min(possible_values)
qplot(x=possible_values,geom="bar",weight=prior_probabilities,
      main=title,xlab=xlab,ylab=ylab, binwidth=possible_values_range/18 )
      #without weight parameter, plots frequency count
      # of possible_values

#define likelihood function when
#there are two possible outcomes: success & failure
#and where p(success) is the parameter p
#data = (# of successes,# of failures)

# likelihood function for binomial distribution
BinomialLikelihood = function(a,p,data) {
  # Computes the probability of data given p
  # 
  # Args:
  #   pp: array containing p & prior vectors
  #   data: 2-element vector #successes, #failures
  #   p: discrete possible true values of the parameter p

  # Returns:
  #   The probability of observing the data given probability p
    if (length(data) != 2) {
      stop("valid data argument: (#successes,#failures)")
      } else if (!(0 < a[p] & a[p] < 1)) {
      stop("p is probability of success. Bounded by (0,1)")
    }
    s <- data[1]
    f <- data[2]
    prob.of.data.given.p <- choose(sum(s,f),s)*a[p]^s*(1-a[p])^f
    return(prob.of.data.given.p)
}

# posterior for a discrete prior & binomial likelihood function
ComputeDataProb = function(discrete.prior,data,likelihood.fn) {
  #   prior: prior probability of p being the true value of the parameter
  #
  #
  prob.of.p <- discrete.prior[match(p,discrete.prior),"p(p)"]
  prob.of.data.given.p <- get(likelihood.fn)(data,p)
  sum.is.prob.of.data <- prob.of.p*prob.of.data.given.p
  return(sum.is.prob.of.data)
}

DiscretePriorPosterior(prior.table=pdist,p="p",discrete.prior="prior",likelihood.fn="BinomialLikelihood",data=data)
DiscretePriorPosterior = function(prior.table,p,discrete.prior,likelihood.fn,data) {
  #generate numerator via an apply function
  apply(prior.table,1,get(likelihood.fn),p,prior,data)
  
  prob.of.p.according.to.prior <- a[discrete.prior]
  prob.of.data.given.p <- get(likelihood.fn)(data,p)#note that as written,
    #this assumes the arguments are the same as those for the BinomialLikelihood
    #function (a,p,data), which may or may not be the case going forward
  numerator <- prob.of.p.according.to.prior*prob.of.data.given.p

  
  
  # compute probability of p * probability of data given probability of p
  # then sum up all these to get the total probability of the data 
  
}

ComputeProduct = function(matrix,col1,col2) {
  return(matrix[col1]*matrix[col2])
}

mytable2<-cbind(p=possible_values,prior=prior_probabilities)
