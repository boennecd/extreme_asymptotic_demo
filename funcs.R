######
# Asymptotic density
dens_assymp <- list(
  Frechet = function(x, a = 1) exp(-x^(-a)) * a * x^(-1 - a),
  Weibull = function(x, a = 1) exp(-(-x)^a) * a * (-x)^(a - 1),
  Gumbel = function(x, ...) exp(-exp(-x) - x))

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
    # See page 133
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
    a = NULL,
    Asym = "Gumbel"),
  
  Exponential = list(
    rdf = function(n) rexp(n, 1),
    norms = function(n)
      list(cn = 1, dn = log(n)),
    a = NULL,
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
    a = NULL,
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
      dens_assymp[[Asym]](x, a = a)
    
    if(Asym == "Frechet"){
      xlab = "x (log scale)"
      d <- density(log(out))
      x <- exp(d$x)
      x_range <- range(x)
      asymp <- dasym(x)
      logs = "x"
      
    } else if(Asym == "Weibull"){
      xlab = "x"
      d <- density(out)
      x <- d$x
      x_range <- range(x)
      asymp <- dasym(d$x)
      logs = ""
      
    } else{
      xlab = "x"
      d <- density(out)
      x <- d$x
      x_range <- range(x)
      asymp <- dasym(d$x)
      logs = ""
      
    }
    
    plot(x, asymp, xlim = x_range, 
         ylim = range(asymp, d$y), type = "l",
         lty = 2, ylab = "Density",
         xlab = xlab, log = logs)
    lines(x, d$y)
    
    #####
    # Plot KL dinstance
    KL <- asymp * log(asymp / d$y)
    plot(x, KL, type = "l", 
         xlab = xlab, log = logs,
         ylim = c(-.1, .1),
         ylab = substitute(
           p[infinity] (x) * log (p[infinity](x) / hat(p)[n](x)),
           list(n = n)))
    abline(h = 0, lty = 2)
  })
}

######
# Settings
m <- 10000
ns <- c(25, 250, 2500)


# #####
# # Comment back to test
# make_plot(choices[["Cauchy"]], n = 100, m = 1000, seed = 2)
