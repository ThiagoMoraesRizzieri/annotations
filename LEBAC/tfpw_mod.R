
tfpw_mod <- function(x) {
  # Initialize the test Parameters
  # Time-Series Vector
  x = x
  # Modified Z-Statistic after Pre-Whitening
  z = NULL
  # Modified P-value after Pre-Whitening
  pval = NULL
  # Initialize Mann-Kendall 'S'- Statistic
  S = 0
  # Initialize Mann-Kendall var.S
  var.S = NULL
  # Initialize Mann-Kendall Tau
  Tau = NULL
  # To test whether the data is in vector format
  if (is.vector(x) == FALSE) {
    stop("Input data must be a vector")
  }
  # To test whether the data values are finite numbers and attempting to eliminate nonfinite numbers
  if (any(is.finite(x) == FALSE)) {
    x[-c(which(is.finite(x) == FALSE))] -> x
    warning("The input vector contains non-finite numbers. An attempt was made to
remove them")
  }
  
  
  # Calculating Sen's slope
  n <- length(x)
  rep(NA, n * (n - 1)/2) -> V
  k = 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      k = k+1
      V[k] = (x[j]-x[i])/(j-i)
    }
  }
  median(V,na.rm=TRUE)->slp
  
  # Calculating Trend-Free Series (xt)
  t=1:length(x)
  xt<-(x[1:n])-((slp)*(t))
  
  # Calculating lag-1 auto-correlation coefficient of Trend-Free Series (ro)
  acf(xt, lag.max=1, plot=FALSE)$acf[-1] -> ro
  
  # Calculating Trend-Free Pre-Whitened Series (xp)
  a=1:(length(xt)-1)
  b=2:(length(xt))
  xp<-(xt[b]-(xt[a]*ro))
  
  # Calculating blended series to which MK test is to be applied in presence of significant serial correlation
  l<-length(xp)
  q=1:l
  y<-(xp[1:l]+((slp)*(q)))
  n1<-length(y)
  
  # Calculating Mann-Kendall 'S'- Statistic
  for (i in 1:(n1-1)) {
    for (j in (i+1):n1) {
      S = S + sign(y[j]-y[i])
    }
  }
  # Calculating Mann-Kendall Variance (Var(s))
  var.S = n1*(n1-1)*(2*n1+5)*(1/18)
  if(length(unique(y)) < n1) {
    unique(y) -> aux
    for (i in 1:length(aux)) {
      length(which(y == aux[i])) -> tie
      if (tie > 1) {
        var.S = var.S - tie*(tie-1)*(2*tie+5)*(1/18)
      }
    }
  }
  # Calculating Z-Statistic values before and after Variance coorection
  if (S == 0) {
    z = 0
  }
  if (S > 0) {
    z = (S-1)/sqrt(var.S)
  } else {
    z = (S+1)/sqrt(var.S)
  }
  # Calculating P-Value before and after Variance coorection
  pval = 2*pnorm(-abs(z))
  # Calculating kendall's Tau
  Tau = S/(.5*n1*(n1-1))
  # Calculating Sen's slope for TFPW series
  rep(NA, n1 * (n1 - 1)/2) -> W
  m = 0
  for (i in 1:(n1-1)) {
    for (j in (i+1):n1) {
      m = m+1
      W[m] = (y[j]-y[i])/(j-i)
    }
  }
  median(W,na.rm=TRUE)->slp1
  return(c("trend free pw"=y))
}