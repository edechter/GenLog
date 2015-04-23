
## Gradient based optimization routines for use with the SDCL package.
## Author: Eyal Dechter
## ----------------------------------------------------------------------

## library(rgl)
## r3dDefaults$windowRect <- c(0,50, 700, 700)

## Load optimx library
library("optimx")

## ----------------------------------------------------------------------
## Params

## Symmetric Dirichlet hyperparam 

## ----------------------------------------------------------------------


## Edges
## an edge consists of a rule and the context in which it fired
edge1 <- list(functor=1, rule=1, context=c(1, 2))
edge2 <- list(functor=1, rule=2, context=c(2, 3))

## Derivations
derivation <- list(edges=list(edge1, edge2),
                   condp=0.3)


## rbind(derivationedges=c(edge1, edge2))
## condp=0.3,
                         

## fixme: this is just a default value
alphas <- function (functor, rule) 0.1

logistic <- function (x) 1/(1+exp(-x))

logit <- function (x) log(x) - log(1-x)



## Weights
weights <- list(
    c(0.1, 0.3, 0.4),
    c(0.1, 0.5)
    )

us <- sapply(weights, logit)


## a function from the list of parameters us
## and a rule i to a weight
## this is a convenience function
## for i=1..length(us)-1
## w_i = logit(u_i)
## w_n = 1 - sum(w_i)
##
## f is the functor index
weight <- function (f, us, i) {
    f.us <- us[[f]]
    nMinus <- length(f.us)
    if (i <= nMinus) {
        return(logistic(f.us[[i]]))
    } else if (i == nMinus + 1) {
        return(1-sum(logistic(f.us)))
    }
}

## log.p(alpha, us)
##
## us is a list of list (functors X rule) where each list corresponds
## to (n-1) of the functor's n rules. u[functor][rule] <-- logit of
## w_r of functor number f
##
## alpha is the dirichlet hyparameter
## FIXME: allow different hyperparameter for each rule or by functor
##
## note: this is only the log prior part of the objective function and
## does not include any constant term, so this does not have
## to be less than zero.
logprior <- function (alpha, us) {
    ## for each functor f compute sum_i^{n_f-1} (alpha_fi - 1) log(w_fi)
    nfunctors <- length(us)
    
    v <- 0
    for (f in 1:nfunctors) {
         f.weights <- logistic(us[[f]])
         nweights <- length(f.weights)
         for (r in 1:nweights) {
             v <- v + (alphas(f, r) - 1) * log(f.weights[[r]])
             v <- v - log(f.weights[[r]])
             v <- v - log(1 - f.weights[[r]])
         }
         # last rule doesn't have a weight but does
         # have an alpha value, so there is an extra
         # alpha at position n = length(us) + 1
         n <- nweights + 1
         alpha.n <- alphas(f, n)
         z <- sum(f.weights)
         v <- v + (alpha.n - 1) * log(1-z)
    }

    return(v)
}



loglike <- function (derivations, us) {
    # derivations: a list in which each element has $edges and $condp

    nderivs <- length(derivations)
    v <- 0
    for (i in 1:nderivs) {
        deriv <- derivations[[i]]
        gamma <- deriv$condp
        edges <- deriv$edges
        nedges <- length(edges)
        for (j in 1:nedges) {
            edge <- edges[[j]]
            f <- edge$functor
            s <- edge$rule
            C <- edge$context
            w.s <- weight(f, us, s)
            w.C <- sapply(C, function (c) weight(f, us, c))
            v <- v + gamma * (log(w.s) - log(sum(w.C)))
        }
    }
    return(v)
}











                   
                   

## ----------------------------------------------------------------------
