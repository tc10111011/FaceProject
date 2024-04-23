data$Resp <- as.factor(data$Resp)#p is a vector containing initial estimates of the free to vary parameters
lmlcm <- function(p, d){
  n <- max(d[, 2:3]) - 1# n is the number of free to vary parameters along a dimension
  pcur <- c(0, p[1:n], 0, p[-(1:n)])#this is the vector of parameter estimates
  rv <- pcur[d[, 2]] -  pcur[d[, 3]] + #these next lines calculate the negative log-likelihoods for data given estimates. 
    pcur[d[, 4] + 5] -  pcur[d[, 5] + 5]#rv is the sum of the differences between the coefficients for each of the physical scales between the 2 stimuli. the plus 5 indexes into the parameter estimates for the next physical scale
  -sum(ifelse(d[, 1] == "1", pnorm(rv, log.p = TRUE), #calculates Loglik of difference X>x or opposite according to choice in trial 
              pnorm(rv, lower.tail = FALSE, log.p = TRUE)))
}
p <- c(seq(0.1, 1, len = 4), seq(1, 10, len = 4))#this is the vector of initial parameter estimates (excluding the 2 0s which are not free to)
bg.opt <- optim(p, lmlcm, d = data, 
                method = "BFGS", hessian = TRUE)#optimisation of the parameter estimates.
make.wide <- function(d){
  nr <- nrow(d)
  wts <- rep(c(1, -1), each = nr)
  ix.mat <- matrix(0, ncol = max(d), nrow = nr)
  ix.mat[matrix(c(rep(1:nr, 2), as.vector(unlist(d))), 
                ncol = 2)] <- wts
  ix.mat <- t(apply(ix.mat, 1, function(x) 
    if (sum(x) == 0) x else
      rep(0, max(d))))
  ix.mat[, -1]
}
bg.lst <- lapply(seq(2, length(data) - 1, 2), 
                 function(x, d){ make.wide(d[, x:(x+1)]) }, 
                 d = data)
X <- do.call(cbind, bg.lst)
colnames(X) <- c(paste("L", 2:5, sep = ""), 
                 paste("S", 2:5, sep = ""))

bg.df <- data.frame(resp = data$Resp, X)
bg.glm <- glm(resp ~ . -1, binomial(probit), bg.df)
bg.Ind <- update(bg.glm, . ~ . - L2 - L3 - L4 - L5)
anova(bg.Ind, bg.glm, test = "Chisq")
make.wide.full <- function(d){
  cn <- function(y, mx) (y[, 2] - 1) * mxd[2] + y[, 1]
  nr <- nrow(d)
  mxd <- c(max(d[, 1:2]), max(d[, 3:4]))
  nc <- prod(mxd)	
  nms <- sapply(seq(1, ncol(d), 2), function(x) 
    substring(names(d)[x], 1, nchar((names(d)[x])) - 1))
  fnm <- mapply(paste, nms, 
                list(seq_len(mxd[1]), seq_len(mxd[2])),
                sep = "", SIMPLIFY = FALSE)
  nms.f <- interaction(do.call(expand.grid, fnm), 
                       sep = ":")
  ix.mat <- matrix(0, ncol = nc, nrow = nr)
  ix.mat[cbind(seq_len(nr), cn(d[, c(1, 3)], mxd))] <- 1
  ix.mat[cbind(seq_len(nr), cn(d[, c(2, 4)], mxd))] <- -1
  ix.mat <- t(apply(ix.mat, 1, function(x) if (sum(x) == 0) 
    x  else rep(0, nc)))
  colnames(ix.mat) <- levels(nms.f)
  ix.mat[, -1]
}
Xf <- make.wide.full(data[, -1])
bg.dff <- data.frame(resp = data[, 1], Xf)
#saturated vs additive
bg.saturated <- update(bg.glm, data = bg.dff)
anova(bg.glm, bg.saturated, test = "Chisq")

