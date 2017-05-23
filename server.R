
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny); library(htmltools)

source("funcs.R")

to_html_w_lb <- function(x){
  out <- gsub("\\n", "<br/>", htmlEscape(x))
  out <- gsub("\\ \\ ", "&nbsp;&nbsp;", out)
  HTML(out)
}

shinyServer(function(input, output) {

  output$sim_plot_small <- renderPlot({
    make_plot(
      choice = choices[[input$choice]],
      m = m, n = ns[1], seed = input$seed)
  })
  
  output$sim_plot_medium <- renderPlot({
    make_plot(
      choice = choices[[input$choice]],
      m = m, n = ns[2], seed = input$seed)
  })
  
  output$sim_plot_large <- renderPlot({
    make_plot(
      choice = choices[[input$choice]],
      m = m, n = ns[3], seed = input$seed)
  })
  
  output$explanation <- renderUI({
    out <- paste0(
      "The continous lines on the plots in the first column are the density estimates of the normalized maxima.",
      " The dashed lines are the asymptotic distribution which in this case is the ", choices[[input$choice]]$Asym, ".",
      "\n\nThe plot to the right shows the discrete terms of the KL distance between the asymptotic density and the estimated density.",
      "\n\nThe most important parts of the code is printed below in case I have made an error.")
      
    to_html_w_lb(out)
  })
  
  output$sim_text <- renderUI({
    if(is.name(choices[[input$choice]]$rdf)){
      out <- as.character(choices[[input$choice]]$rdf)
    } else
      out <- paste(head(
        capture.output(choices[[input$choice]]$rdf), 
        -1) # We dont want the byte code
        , collapse = "\n")
    to_html_w_lb(paste0("The simulation function is:\n", out))
  })
  
  output$norm_text <- renderUI({
    out <- paste(head(
      capture.output(choices[[input$choice]]$norms), 
      -1) # We dont want the byte code
      , collapse = "\n")
    
    to_html_w_lb(paste0("The norming constants are computed by:\n", out))
  })
  
  output$asymp_text <- renderUI({
    asymp <- choices[[input$choice]]$Asym
    
    out <- paste(head(
      capture.output(dens_assymp[[asymp]]), 
      -1) # We dont want the byte code
      , collapse = "\n")
    
    xtra <- if(is.null(choices[[input$choice]]$a))
      "" else
        paste0(
          "\n\nwhere a = ", choices[[input$choice]]$a, ".")
    
    to_html_w_lb(paste0(
      "The density of ", asymp, " is computed with:\n", out, xtra))
  })
})
