######
# Asymptotic density
dens_asymp <- list(
  Frechet = function(x, a = 1) exp(-x^(-a)) * a * x^(-1 - a),
  Weibull = function(x, a = 1) exp(-(-x)^a) * a * (-x)^(a - 1),
  Gumbel = function(x, ...) exp(-exp(-x) - x))

cdf_asymp <- list(
  Frechet = function(x, a = 1) exp(-x^(-a)),
  Weibull = function(x, a = 1) exp(-(-x)^a),
  Gumbel = function(x, ...) exp(-exp(-x)))

#####
# Choices of distribution 
choices <- list(
  Cauchy = list(
    rdf = as.symbol("rcauchy"),
    norms = function(n) list(cn = n / pi, dn = 0),
    a = 1,
    Asym = "Frechet"
  ),
  
  Pareto = list(
    rdf = function(n) runif(n)^(-1), # alpha = 1, K = alpha
    norms = function(n) list(cn = n, dn = 0),
    a = 1,
    Asym = "Frechet"
  ),
  
  Uniform = list(
    rdf = as.symbol("runif"),
    norms = function(n) list(cn = n^-1, dn = 1),
    a = 1,
    Asym = "Weibull"),
  
  Normal = list(
    rdf = as.symbol("rnorm"),
    norms = function(n)
      list(
        cn = (2 * log(n))^(-1/2),
        dn = sqrt(2 * log(n)) - 
          (log(4 * pi) + log(log(n))) / 
            (2 * sqrt(2 * log(n)))),
    Asym = "Gumbel"),
  
  Exponential = list(
    rdf = function(n) rexp(n, 1),
    norms = function(n) list(cn = 1, dn = log(n)),
    Asym = "Gumbel"),
  
  Lognormal = list(
    rdf = function(n) rlnorm(n, 0, sdlog = 1),
    norms = function(n){
      dn <- exp(0 + (sqrt(2 * log(n)) - 
                       (log(4 * pi) + log(log(n))) / 
                       (2 * sqrt(2 * log(n)))))
      list(
        cn = (2 * log(n))^(-1/2) * dn, dn = dn) 
    },
    Asym = "Gumbel")
  )

#####
# Define plot function
make_plot <- function(choice, n, m, seed){
  with(choice, {
    #####
    # Simulate and standardize maximas
    set.seed(seed)
    cur_norm <- norms(n)
    out <- replicate(
      m, {
        x <- if(is.function(rdf))
          rdf(n) else eval(rdf)(n)
        cur_norm$cn^(-1) * (max(x) - cur_norm$dn)
      })
    out <- sort(out)
    
    #####
    # Plot header
    par(mar=c(2.5,2.5,1,1))
    layout(mat = matrix(c(1, 2, 1, 3), ncol = 2), 
           heights = c(1, 4))
    plot.new()
    text(
      0.5,0.5, paste0("Plot w/ ", m, " draws of maximums w/ block size ", n),
      cex = 2,font = 2)
    
    #####
    # Plot density and assymptotic density
    par(mar=c(4,5,1,1), cex = 1.5)
    
    dasym <- function(x)
      dens_asymp[[Asym]](x, a = a)
    
    cdfasym <- function(x)
      cdf_asymp[[Asym]](x, a = a)
    
    if(Asym == "Frechet" && !any(out <= 0)){
      xlab = "x (log scale)"
      logs = "x"
      
    } else {
      xlab = "x"
      logs = ""
    }
    
    ecdf_obj <- ecdf(out) 
    plot(out, ecdf_obj(out), 
         main = "", type = "l",
         xlim = range(out),
         ylab = "Cdf",
         xlab = xlab, log = logs)
    lines(out, cdfasym(out), col = "DarkGray")
    
    #####
    # Plot difference
    plot(out, ecdf_obj(out) - cdfasym(out), type = "l",
         xlab = xlab, log = logs,
         ylim = c(-.1, .1),
         ylab = substitute(
            hat(F)[n](x) - F[infinity] (x),
           list(n = n)))
    abline(h = 0, lty = 2)
  })
}

######
# Settings
m <- 10000

# #####
# # Comment back to test
# make_plot(choices[["Cauchy"]], n = 8, m = 1000, seed = 2)
