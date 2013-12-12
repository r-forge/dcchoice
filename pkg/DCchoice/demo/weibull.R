if(dist == "logistic" | dist == "log-logistic"){  # logistic or log-logistic error distribution
    glm.out <- glm(y1~. -1, family = binomial(link = "logit"), data = tmp.data)  # unrestricted model
    glm.null <- update(glm.out, .~ 1)  # restricted (only the intercept) model
    
    npar <- length(glm.out$coefficients) # the number of parameters
    if(substr(dist, 1, 4) == "log-") names(glm.out$coefficients)[npar] <- "log(bid)" # changing the name label if the model is log-logistic
    names(glm.out$coefficients)[1] <- "(Intercept)"
    estimates <- glm.out$coefficients  # taking out the estimates
    
} else if(dist == "normal" | dist == "log-normal") {  # normal or log-normal error distribution
    glm.out <- glm(y1~. -1, family = binomial(link = "probit"), data = tmp.data)
    glm.null <- update(glm.out, .~ 1)
    
    npar <- length(glm.out$coefficients)
    if(substr(dist, 1, 4) == "log-") names(glm.out$coefficients)[npar] <- "log(bid)"
    names(glm.out$coefficients)[1] <- "(Intercept)"
    estimates <- glm.out$coefficients
} else if(dist == "weibull"){
    const <- rep(1, nobs)
    
    # likelihood function
    sbLL <- function(param, dvar, ivar, bid){
        npar <- length(npar)
        b <- param[npar]
        param[-npar]
        y1 <- dvar
        X <- ivar
        ll <- 
            sum(pweibull(bid[y1==1]*b, shape = exp(X[y1==1, ]%*%param), lower.tail=FALSE, log.p=TRUE)) + 
            sum(pweibull(bid[y1==0]*b, shape = exp(X[y1==0, ]%*%param), lower.tail=TRUE, log.p=TRUE))
        ifelse(is.finite(ll), return(-ll), NaN) 
    }
    sbLL.null <- function(param, dvar, ivar, bid){
        npar <- length(npar)
        b <- param[npar]
        param[-npar]
        y1 <- dvar
        X <- ivar   # should be only constant
        ll <- 
            sum(pweibull(bid[y1==1]*b, shape = exp(X[y1==1]*param), lower.tail=FALSE, log.p=TRUE)) + 
            sum(pweibull(bid[y1==0]*b, shape = exp(X[y1==0]*param), lower.tail=TRUE, log.p=TRUE))
        ifelse(is.finite(ll), return(-ll), NaN) 
    }
    # initial parameter values
    ini.par <- glm(y1~. -1, family = binomial(link = "probit"), data = tmp.data)$coefficients
    ini.par.null <- glm(y1 ~ 1, family = binomial(link = "probit"), data = tmp.data)$coefficients
    
    suppressWarnings(# the names "glm.out" and "glm.null" are nothing to do with glm. They are used solely because of compatibility
        glm.out <- optim(ini.par, fn = sbLL, method="BFGS", hessian = TRUE, dvar = y1, ivar = X, bid = BID, control=list(abstol=10^(-30)))
        glm.null <- optim(ini.par.null, fn = sbLL.null, method="BFGS", hessian = TRUE, dvar = y1, ivar = const, bid = BID, control=list(abstol=10^(-30)))
    )
    npar <- length(glm.out$par)     # the number of parameters
    # if(substr(dist, 1, 4) == "log-") names(glm.out$coefficients)[npar] <- "log(bid)"
    names(glm.out$par)[1] <- "(Intercept)"
    estimates <- glm.out$par
} else {
    stop("dist must be logistic, normal or weibull")
}

# summary関数などの修正必要


