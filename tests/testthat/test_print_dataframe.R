data.frame(1)

data = data.frame(k = rep(1:31, 4))

####
generateData = function(n, p) { # data generating process
  # features X:
  mu = rep(0, p)
  sigma = matrix(.85, ncol = p, nrow = p) + diag(.15, p)
  X = mvtnorm::rmvnorm(n, mean = mu, sigma = sigma)
  # coefficients b:
  b = numeric(p)
  non.zero = sort(sample(p, 10)) # indices of non-zero coefficients
  b[non.zero] = rnorm(10, 0, .4)
  # noise eps:
  eps = rnorm(n, 0, 6.25)
  # target y:
  y = X %*% b + eps
  # data.frame
  df = data.frame(y, X)
  return(list(data = df, y = y, X = X, b = b, eps = eps, non.zero = non.zero))
}
data = generateData(300, 31)$data
# print out depends on width of console window
# so for long cells some columns might be cut from output
# which is fine behaviour so far

