## ----echo=FALSE----------------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ---- tidy=FALSE, echo = TRUE--------------
arg_explore = function(arg1, rg2, rg3)
    paste("a1, a2, a3 = ", arg1, rg2, rg3)

## ---- eval=FALSE, echo = TRUE--------------
#  arg_explore(1, 2, 3)
#  arg_explore(2, 3, arg1 = 1)
#  arg_explore(2, 3, a = 1)
#  arg_explore(1, 3, rg = 1)

## ---- tidy=FALSE---------------------------
## SOLUTION
## See http://goo.gl/NKsved for the offical document
## To summeriase, matching happens in a three stage pass:
#1. Exact matching on tags
#2. Partial matching on tags.
#3. Positional matching

## ---- fig.keep="none", echo = TRUE---------
plot(type="l", 1:10, 11:20)

## ---- results='hide', echo = TRUE----------
rnorm(mean=4, 4, n=5)

## ---- tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
#plot(type="l", 1:10, 11:20) is equivilent to
plot(x=1:10, y=11:20, type="l")
#rnorm(mean=4, 4, n=5) is equivilent to
rnorm(n=5, mean=4, sd=4)

## ---- echo = TRUE--------------------------
## Use regression as an example
stat_ana = function(x, y) {
  lm(y ~ x)
}

## ----  tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
stat_ana = function(x, y, trans=NULL) {
  lm(y ~ x)
}

## ---- eval=FALSE, echo = TRUE--------------
#  stat_ana(x, y, trans=log)

## ----  tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
stat_ana = function(x, y, trans=NULL) {
  if(is.function(trans)) {
    x = trans(x)
    y = trans(y)
  }
  lm(y ~ x)
}

## ----  tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
stat_ana = function(x, y, trans=NULL) {
  if(is.function(trans)) {
    x = trans(x)
    y = trans(y)
  } else if (trans == "normalise") {
    x = scale(x)
    y = scale(y)
  }
  lm(y ~ x)
}

## ---- results='hide', echo = TRUE----------
f = function(x) return(x + 1)
f(10)

## ----  tidy=FALSE--------------------------
##Nothing strange here. We just get
f(10)

## ---- results='hide', echo = TRUE----------
f = function(x) {
  f = function(x) {
    x + 1
  }
  x = x + 1
  return(f(x))
}
f(10)

## ---- results='hide', echo = TRUE----------
f = function(x) {
  f = function(x) {
    f = function(x) {
      x + 1
    }
    x = x + 1
    return(f(x))
  }
  x = x + 1
  return(f(x))
}
f(10)

## ------------------------------------------
## Solution: The easiest way to understand is to use print statements
f = function(x) {
  f = function(x) {
    f = function(x) {
      message("f1: = ", x)
      x + 1
    }
    message("f2: = ", x)
    x = x + 1
    return(f(x))
  }
  message("f3: = ", x)
  x = x + 1
  return(f(x))
}
f(10)

## ---- results='hide'-----------------------
f = function(x) {
  f = function(x) {
    x = 100
    f = function(x) {
      x + 1
    }
    x = x + 1
    return(f(x))
  }
  x = x + 1
  return(f(x))
}
f(10)

## ----  results='hide'----------------------
##Solution: The easiest way to understand is to use print statements as above

## ------------------------------------------
poisson = function(lambda) {
     r = function(n=1) rpois(n, lambda)
     d = function(x, log=FALSE) dpois(x, lambda, log=log)
     return(list(r=r, d=d))
}

## ------------------------------------------
geometric = function(prob) {
     r = function(n=1) rgeom(n, prob)
     d = function(x, log=FALSE) dgeom(x, prob, log=log)
     return(list(r=r, d=d))
}

## ----echo = TRUE, error = TRUE, eval = FALSE----
#  dd = data.frame(w = rnorm(10),
#                  x = letters[1:10],
#                  y = rnorm(10),
#                  z = rnorm(10)
#  )
#  
#  max_cols = rep(NA, ncol(dd))
#  for (i in seq_along(dd)) {
#    max_cols[i] = max(dd[, i])
#  }
#  max_cols

## ---- eval = FALSE, error = FALSE----------
#  dd = data.frame(w = rnorm(10),
#                  x = letters[1:10],
#                  y = rnorm(10),
#                  z = rnorm(10)
#  )
#  
#  max_cols = rep(NA, ncol(dd))
#  for (i in seq_along(dd)) {
#    try(max_cols[i] <- max(dd[, i]))
#  }
#  max_cols

## ---- eval=FALSE, echo = TRUE--------------
#  library("jrAdvPackage")
#  vignette("solutions2", package="jrAdvPackage")

