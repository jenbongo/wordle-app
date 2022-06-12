
# R Shiny page to explore statistics of different strategies for solving wordle
# Jenny Neuburger - April 2022

# Read in libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(r2d3)
library(dplyr)
library(stringi)
library(stringr)

# Set working directory for app files if running code locally
# app_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(app_wd)

# Read in source files
source("data/inputs.R") # data lists and inputs saved in separate R file
source("data/functions.R") # data lists and inputs saved in separate R file
source("data/review_ideas.R") # lengthy text for final panel saved in separate file

#-----------------------------------------------------
# Set up user interface 
shinyUI <- fluidPage(
  #includeCSS(paste0(app_wd,"/www/app_style.css")), # styling defined in separate css file
  includeCSS("www/app_style.css"), # styling defined in separate css file
  
  titlePanel("Learn Statistics! Compare strategies for playing Wordle"),
  
  tabsetPanel(id = "inTabset",
              
    # Introductory page
    tabPanel(title = tags$h3("Intro"), value = "page_1",
             
             withTags(
               h4(p(br(),"People have suggested different ways to play", a("Wordle.", href = "https://www.powerlanguage.co.uk/wordle/"), "The aim is to guess a hidden five-letter word using up to six guesses.",
                   "You get a hint back with each guess, showing whether the letters are:"), 
                   li("in the right place (green 🟩);"),
                   li("in the word (yellow 🟨); or"), 
                   li("not in the word (grey ⬜)."),  br(),
                  p(a("With the best strategy", href = "http://sonorouschocolate.com/notes/index.php?title=The_best_strategies_for_Wordle"), 
                    "(by a computer), it can be solved with 3.4201 guesses on average, and 5 guesses at worst.",
                    "But there are also other nearly-as-good strategies we can try..."), 
                  p("These pages are designed to be fun, to help you explore strategies for playing wordle, and to learn statistics through playing!"),
                  br(),
                  actionButton("page_12","Next Page")
               )
             ), class = "stylepanel"
    ),
    
    # Page 2: play one game of wordle
    tabPanel(title = tags$h3("Play Wordle"), value = "page_2",
             fluidRow(
               column(6,
                      br(),
                      selectInput(inputId = "strategy", label = "Choose a strategy", choices = strategy_choices),
                      uiOutput("description"),
                      tags$br(),
                      useShinyjs(),
                      actionButton("enter","Make guess", class = "button"),
                      actionButton("clear_guesses", "Clear guesses", class = "button"),
                      actionButton("new_hidden_word", "New game", class = "button"),
                      br(), br(),
                      actionButton("page_23","Next Page")
               ),
               column(6, 
                      br(),
                      p(uiOutput("result", placeholder = TRUE)),
                      
               )
             ), class = "stylepanel"
    ),
    
    # Page 3: Take a sample
    tabPanel(tags$h3("Play lots of games!"), value = "page_3",
             br(),
             withTags(
               h4(p("Using a statistical approach, we can assess a strategy by playing many games - each with different hidden word."),
                  p("This is equivalent to drawing a sample.")
               )
             ),
             fluidRow(
               column(4,
                      selectInput(inputId = "sample_strategy", label = "Choose a strategy", choices = strategy_choices[2:5]),
                      numericInput(inputId = "sample_size", label = "Pick number of games (sample size)", value = 30),
                      actionButton("take_sample","Take a sample", class = "button"),
                      br(), br(),
                      actionButton("page_34","Next Page")
               ),
               column(8, 
                      fluidRow(
                        column(5,              
                               hidden(uiOutput("sample_scores", placeholder = FALSE))),
                        column(3, 
                               d3Output("guess_distribution", height = "90px", width = "180px"))
                      ),
                      br(), 
                      uiOutput("sample", placeholder = FALSE)
               )), class = "stylepanel"
    ),
    
    # Page 4: Compare strategies
    tabPanel(tags$h3("Compare strategies"), value = "page_4",
             br(),
             withTags(
               h4(p("We can compare the performance of different strategies using summary statistics e.g. the mean number of guesses."),
                  p("95% confidence intervals can be used to show the uncertainty in the estimate from a sample."),
                  p("You can reduce this uncertainty by taking a bigger sample."),
                  p("Try this out for yourself!")
               )
             ),   
             fluidRow(
               column(4,
                      selectInput(inputId = "comparison_statistic", label = "Summary statistic", choices = comparison_statistics),
                      numericInput(inputId = "comparison_sample_size", label = "Sample size (per strategy)", value = 30),
                      actionButton("compare_strategies","Compare strategies", class = "button"),
                      actionButton("clear_comparisons","Clear", class = "button"),
                      br(), br(),
                      actionButton("page_45","Next Page")
                      
               ),
               column(8, 
                      d3Output("comparison_plot", height = "200px", width = "500px")
               )
             ), class = "stylepanel"
    ),
    
    # Page 5: Review ideas (text saved in separate file "review_ideas.R")
    tabPanel(tags$h3("Review ideas"), value = "page_5",
             br(),
             review_ideas_text, class = "stylepanel"
    )
    
    
  )
)
