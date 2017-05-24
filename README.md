# This Repo
This repository gives a demonstration of the convergence in distribution of maximums. See Embrechts, Klüppelberg and Mikosch (1997) for details on asymptotic distributions, norming sequences etc.

``` r
#####
# Get package from 
zip_file_name <- "repo.zip"
download.file("https://github.com/boennecd/extreme_asymptotic_demo/archive/master.zip", 
              destfile = zip_file_name)
unzip(zip_file_name)
file.remove(zip_file_name)

######
# Run app
shiny::runApp("extreme_asymptotic_demo-master")
```

# References
Embrechts, P., & Klüppelberg, C. und Mikosch, T. (1997) *Modelling Extremal Events for Insurance and Finance*. Applications of mathematics––stochastic modelling and applied probability, 33.