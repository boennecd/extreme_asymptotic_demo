
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("funcs.R")

shinyUI(fluidPage(
  
  fluidPage(
    titlePanel("Convergence of extremes"),
    
    fluidRow(
      column(
        3,
        wellPanel(
          h4("Settings"),
          
          selectInput(
            "choice", "Select distribution to simulate from",
            choices = names(choices),
            selected = "Exponential"),
          
          numericInput(
            "seed", "Select seed",
            value = 5480408, min = 1, max = 1e7, step = 1)
        ),
        
        wellPanel(
          htmlOutput("explanation")),
        
        wellPanel(
          htmlOutput("sim_text")),
        
        wellPanel(
          htmlOutput("norm_text")),
        
        wellPanel(
          htmlOutput("asymp_text"))),
      
      
      
      column(
        9,
        plotOutput("sim_plot_small"),
        plotOutput("sim_plot_medium"),
        plotOutput("sim_plot_large"))
      ))))
