# Kaplan-Meier-Turnbull nonparametric approach to analyze 
# double-bounded dichotomous choice contingent valuation data.
# Functions for summarizing the object and plotting the survivor 
# function are also defined.

turnbull.db <- function(formula, data){
  if(missing(data)) data <- environment(formula)

  # stop if the LHS does not contain two variables
  if(length(formula[[2]]) != 3) stop("LHS variable in the formula must be like y1 + y2 ")
  
  # removing observations with missing values
  na.num <- max(sum(as.numeric(is.na(data))))
  if(na.num != 0){ 
    d1 <- nrow(data)
    data <- na.omit(data)
    d2 <- nrow(data)
    warning(paste("Missing values detected.", d1 - d2, "rows are removed.", sep = " "))
  }

  # defining the dependent variable
  lhs1 <- formula[[2]][[2]]       # extracting from the formula the name of the variable for the yes/no to the first bid
  lhs2 <- formula[[2]][[3]]       # extracting from the formula the name of the variable for the yes/no to the second bid
  y1 <- eval(lhs1, data)          # yes/no to the first bid
  y2 <- eval(lhs2, data)          # yes/no to the second bid

  nobs <- length(y2)

  P1 <- formula[[3]][[2]]   # extracting from the formula the name of the variable for the first bid
  P2 <- formula[[3]][[3]]   # extracting from the formula the name of the variable for the second bid
  first <- eval(P1, data)       # the first bids
  second <- eval(P2, data)      # the second bids
  
  # making dummy variables for the first and second bids
  if(is.factor(y1)){   # when the yes/no variables are defined as factor
    yy <- ifelse(y1 == "yes" & y2 == "yes", 1, 0)
    yn <- ifelse(y1 == "yes" & y2 == "no", 1, 0)
    ny <- ifelse(y1 == "no" & y2 == "yes", 1, 0)
    nn <- ifelse(y1 == "no" & y2 == "no", 1, 0)
  } else {
     yy <- ifelse(y1 == 1 & y2 == 1, 1, 0)
     yn <- ifelse(y1 == 1 & y2 == 0, 1, 0)
     ny <- ifelse(y1 == 0 & y2 == 1, 1, 0)
     nn <- ifelse(y1 == 0 & y2 == 0, 1, 0)
   }

left <- ifelse(yy == 1 | ny == 1, second, ifelse(yn == 1, first, 0))    # lower bound of WTP
right <- ifelse(yn ==1 | nn == 1, second, ifelse(ny == 1, first, Inf))  # upper bound of WTP
unq.bid <- sort(unique(c(left, right)))   # unique bids including Inf

turnbull <- icfit(L = left, R = right)    # estimating nonparametric survival function. icfit function is defined in interval package

  # arranging outcomes into a single list variable
output <- list(
  left = left,
  right = right,
  turnbull = turnbull,
  unq.bid = unq.bid
)

  class(output) <- "turnbull"
  return(output)

}

summary.turnbull <- function(object, ...){
  p <- object$turnbull$pf
  x.str <- object$turnbull$strata   # the number of intervals
  unq.bid <- object$unq.bid  # a vector of unique bids
  blabel <- unq.bid          # to be used as a name label for intervals
  nbid <- length(unq.bid)    # the number of unique bids
  unq.bid <- unq.bid[-nbid]  # excluding Inf
  suv <- 1 - round(cumsum(p), 12)
  
  # adjusting probabilities for missing intervals from icfit()
  ip <- object$turnbull$intmap[1,]
  if(nbid != length(ip)){
  for(i in 2:(nbid-1)){
    if(unq.bid[i] != ip[i]){
       ip <- append(ip, unq.bid[i], after= (i-1))
       suv <- append(suv, suv[i-1], after= (i-1))
    }
  }
  }
  suv <- c(1, suv)      # adding probability for bid = 0

  names(suv) <- blabel  # labels for survival probabilities
  x.ax <- blabel[-length(blabel)]   # points on the x-ax in the plot
  x.interval <- diff(x.ax)          # a vector of intervals

  # Kaplan-Meier WTP
  object$meanWTP <- sum(x.interval*suv[-c(1, length(suv))])        # lower bound 
  # Spearman-Karber WTP
  med.suv <- 0.5*(suv[-length(suv)] + suv[-1])    # median 
  med.suv <- med.suv[-length(med.suv)]
  object$med.meanWTP <- sum(med.suv*x.interval)

  # Median
  object$x.ax <- c(x.ax, 1.1*max(x.ax))
  object$medianWTP <- c(x.ax[max(which(suv > 0.5))], x.ax[min(which(suv < 0.5))])

  # making an output table
  estimates <- cbind(blabel, suv)
  colnames(estimates) <- c("Upper", "Prob.")
  rownames(estimates) <- seq(1, nrow(estimates))
  
  object$estimates <- estimates

  class(object) <- "summary.turnbull"
  return(object)

}

print.turnbull <- function(x, digits = max(3, getOption("digits") - 1), ...){
  
  if(!x$turnbull$converge)  cat("The optimization did not converge\n")
  cat("\nProbability:", formatC(x$turnbull$pf, digits = digits), "\n", sep = " ")
  
  invisible(x)
}

print.summary.turnbull <- function(x, digits = max(3, getOption("digits") - 1), ...){
  
  cat("Survival probability:", "\n", sep = " ")
  print.default(x$estimates, digits = 4, right = TRUE, print.gap = 2)
  
  cat("\nWTP estimates:", sep = " ")
  cat("\n Mean:", formatC(x$meanWTP, digits = digits), "", sep = " ")
  cat(" (Kaplan-Meier)", sep = "")
  cat("\n Mean:", formatC(x$med.meanWTP, digits = digits), "", sep = " ")
  cat(" (Spearman-Karber)", sep = "")
  cat("\n Median in:", "[", formatC(x$medianWTP[1], digits = digits), ",", formatC(x$medianWTP[2], digits = digits), "]", "\n", sep = " ")
  
}

# plotting the estimated survivor function
plot.turnbull <- function(x, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, lwd = NULL, lty = NULL, ...){
  
  if(is.null(main)) main <- ""                       # main title
  if(is.null(sub)) sub <- ""                         # subtitle
  if(is.null(xlab)) xlab <- "Bid"                    # label of x-axis
  if(is.null(ylab)) ylab <- "Survival Probability"   # label of y-axis
  if(is.null(lwd)) lwd <- 3                          # line width
  if(is.null(lty)) lty <- 1                          # line type
  
  plot.x <- summary.turnbull(x)                      # summarizing the object for plot
  n.ax <- length(plot.x$x.ax)                        # the number of points on the x-axis
  
  plot.default(plot.x$x.ax, plot.x$estimates[, 2], axes = F, xlab = xlab, ylab = ylab, main = main, sub = sub, lwd = lwd, lty = lty, type = "S", ...)
  axis(1, pos = 0, at = plot.x$x.ax[-n.ax], adj = 0)            # adding the x-axis
  axis(2, pos = 0, at = seq(0, 1, by = 0.2), las = 2, adj = 1)  # adding the y-axis

}