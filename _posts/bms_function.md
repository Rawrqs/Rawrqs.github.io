---
layout: post
title: "My bms functions"
author: "Jakub Winter"
date: "Friday, March 01, 2015"
header-img: "img/post-bg-01.jpg"
highlighter: pygments
comments: TRUE
---

#Functions that I created in use with [BMS package](http://bms.zeugner.eu/)

##Newly defined m_priors

```r
my_fixed <- function (K, mpparam, ...) {
  if (is.na(mpparam[1]))
    mpparam <- K/2
  if ((mpparam[[1]] >= K) & (length(mpparam) == 1)) {
    warning("Submitted prior model size is >= than the nr. of   regressors\n, used K/2 instead\n\n")
    mpparam <- K/2
  }
  m = mpparam[[1]]
  return(list(mp.mode = "fixed", mp.msize = m, pmp = function(ki, mdraw,
                                                              ...) { 
          assign("tmp", mdraw, envir = .tempEnv)
          eval(expression(i <- i + 1),envir = .tempEnv)
          eval(expression(mdraw_matrix[i,] <- tmp),envir = .tempEnv)
    
    post.odds1 = ki * log(m/K) + {
      K - ki
    } * log(1 - m/K)
    return(post.odds1)
  }, mp.Kdist = dbinom(x = 0:K, size = K, prob = m/K, log = FALSE)))
}


my_random <-
  function (K, mpparam, ...)
  {
    if (is.na(mpparam[1]))
      mpparam <- K/2
    if ((mpparam[[1]] >= K) & (length(mpparam) == 1)) {
      warning("Submitted prior model size is >= than the nr. of   regressors\n, used K/2 instead\n\n")
      mpparam <- K/2
    }
    m = mpparam[[1]]
    vecofpriors = lgamma(1 + 0:K) + lgamma({
      K - m
    }/m + K - 0:K)
    beta.bin = function(a = 1, b = (K - m)/m, K = K, w = 0:K) {
      lgamma(a + b) - {
        lgamma(a) + lgamma(b) + lgamma(a + b + K)
      } + log(choose(K, w)) + lgamma(a + w) + lgamma(b + K -
                                                       w)
    }
    return(list(mp.mode = "random", mp.msize = m, pmp = function(ki, mdraw,
                                                                 ...) {
      assign("tmp", mdraw, envir = .tempEnv)
      eval(expression(i <- i + 1),envir = .tempEnv)
      eval(expression(mdraw_matrix[i,] <- tmp),envir = .tempEnv)
      
      return(vecofpriors[[ki + 1]])
    }, mp.Kdist = exp(beta.bin(a = 1, b = {
      K - m
    }/m, K = K, w = 0:K))))
  }


my_uniform <-
  function (K, ...)
  {
    return(list(mp.mode = "uniform", mp.msize = K/2, pmp = function(ki, mdraw, ...) {
      assign("tmp", mdraw, envir = .tempEnv)
      eval(expression(i <- i + 1),envir = .tempEnv)
      eval(expression(mdraw_matrix[i,] <- tmp),envir = .tempEnv)
      
      return(0)
    },
    mp.Kdist = exp(lchoose(K, 0:K) - K * log(2))))
  }
```

##Wrapper around the BMS::bms for newly defined mpriors

```r
my_bms <- function(X.data, burn = 100, iter = 1000, ...){
  
  .tempEnv <<- new.env()
  assign("mdraw_matrix", matrix(ncol = (dim(X.data)[2] - 1 ), nrow = (burn + iter + 1)), envir = .tempEnv)
  assign("i"           , 0, envir = .tempEnv)

  bms_output <- bms(X.data = X.data, burn = burn, iter = iter, ...)
  bms_output$mprior.info$mp.draws <- get("mdraw_matrix", envir = .tempEnv)

  rm(.tempEnv, envir = .GlobalEnv)
  return(bms_output)
}
```

##Function which allows to calculate joint statistic from a list of models

```r
getJointLS2 <- function(model, log.it = FALSE) {
  
  pips            <- coef.bma(model, order.by.pip = FALSE)[, 1] #tutaj sa pipy
  
  temp            <- model$topmod$bool_binary()
  rownames(temp)  <- rownames(coef.bma(model, order.by.pip = FALSE))
  
  combinations    <- combn(rownames(coef.bma(model, order.by.pip = FALSE)), 2)
  statistic       <- lapply(as.list(as.data.frame((combinations))), function(x) {
    
    boolean <- temp[as.character(x[1]), , drop = FALSE] * temp[as.character(x[2]), , drop = FALSE]
    pab     <- boolean %*% pmp.bma(model)[,2] 
    stat    <- (pab)/(pips[as.character(x[1])] + pips[as.character(x[2])] - 2*pab)
    return(stat)
  })
  statistic <- unlist(statistic)
  temp_matrix <- matrix(nrow = length(coef.bma(model, order.by.pip = FALSE)[, 1]), ncol = length(coef.bma(model, order.by.pip = FALSE)[, 1]))
  rownames(temp_matrix) <- colnames(temp_matrix) <- names(coef.bma(model, order.by.pip = FALSE)[, 1])
  
  for( i in 1:length(statistic) ) temp_matrix[as.character(combinations[1, i]), as.character(combinations[2, i])] <- statistic[i]
  for( i in 1:length(statistic) ) temp_matrix[as.character(combinations[2, i]), as.character(combinations[1, i])] <- statistic[i]
  
  if (log.it == FALSE)  return(temp_matrix) else return(ln(temp_matrix))
}
```

##Function which allows to calculate HPD's from a list of models

```r
getHPD <- function(dd, hpd = 0.95){ # tutaj trzeba dokonczyc funkcje 
  results <- matrix(nrow=length(dd), ncol=2)
  for (i in 1:length(dd)){
    df <- data.frame(y = dd[[i]]$y, x = dd[[i]]$x)
    df <- arrange(df, -y)
    df$prob.sum <- cumsum(df$y / sum(df$y))
    
    df.95 <- subset(df, prob.sum <hpd)
    results[i,1] = min(df.95$x)
    results[i,2] = max(df.95$x)
    #c( min(df.95$x), max(df.95$x) ) # the 95% HPD
    
  }
  return(results)
}
```
