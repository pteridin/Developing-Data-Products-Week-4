#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui.getExplanaition <- function(x) {
    fluidPage(
        h1("Explaination"),
        h2("Choosing is hard"),
        p("Let's assume you want to shop for some new graphics card. One is pretty new 
            and has six 5-star reviews on Amazon. Another has 4,8 stars according to amazon, but over nine thousand 
            reviews."),
        h2("Which one should you choose?"),
        p("There comes this particular tool. You can add up to three different products and see the probabilities that
           this product really is a five star product - or not. The principle is simple:"),
        p("It uses Bayesian Ranking to estimate the probabilities that the products are really awesome - or not."),
        h2("Manual"),
        p("Tab to 'Add Data...', fill in the amount of reviews on your products of choice. Click on Plots or Table and
          see the results. Buy the best product - given your knowledge and the knowledge of the crowd!")
    )
}

ui.addData <- function(x) {
    sidebarLayout(
        sidebarPanel(
            selectInput("Product",
                        "Choose Product",
                        choices = c("Product 1",
                                    "Product 2",
                                    "Product 3"),
                        selected = "Product 1")
        ),
        
        mainPanel(
            h1("Data Input"),
            p("What is the name of this product?"),
            textInput("Product.Name",
                      "Product Name",
                      value = "Product 1"),
            h2("Votes"),
            p("Please put in all the votes for this particular product:"),
            numericInput("Votes.Five",
                         "Five Stars",
                         min = 0,
                         max = 99999,
                         value = 0),
            numericInput("Votes.Four",
                         "Four Stars",
                         min = 0,
                         max = 99999,
                         value = 0),
            numericInput("Votes.Three",
                         "Three Stars",
                         min = 0,
                         max = 99999,
                         value = 0),
            numericInput("Votes.Two",
                         "Two Stars",
                         min = 0,
                         max = 99999,
                         value = 0),
            numericInput("Votes.One",
                         "One Star",
                         min = 0,
                         max = 99999,
                         value = 0),
            
            h2("Prior"),
            p("Bayesian Statistics also takes your prior knowledge about each product
              into consideration. It could be a great idea to change these values
              accordingly!"),
            
            h3("Do you think this product is good?"),
            sliderInput("Prior.Goodness",
                        "How good is the product?",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .1),
            p("Do you think this product is good (1), bad (0) or neither (0.5)?"),
            
            h3("How confident are you in this assumption?"),
            sliderInput("Prior.Confidence",
                        "How confident are you?",
                        min = 0,
                        max = 1,
                        value = 0,
                        step = .1),
            p("I am very confident (1) or not confident at all (0). It is ok to be a
               bit less confident than you would be normally.")
        )
    )
}

ui.getPlot <- function(x) {
    fluidPage(
        h2("Posterior"),
        p("Here you can see the probability that the product has the amount of stars given your prior
           knowledge and the votes:"),
        plotOutput("Posterior.Plot"),
        p("You see the probabilities of our model for each product above, given the prior-distribution
           about how you think how good the product is and how confident you are with this assumption."),
        p("With this error-rate slider you can change the vertical lines. These represents the least amount stars 
           given the product, the model and the error rate. Or to put it in another way: Which risk are you willing 
           to take?"),
        sliderInput("Posterior.ErrorRate",
                    "Error Rate",
                    min = .01,
                    max = .99,
                    step = .01,
                    value = .05),

    
        h2("Prior"),
        plotOutput("Prior.Plot"),
        p("Bayesian Ranking takes into account what you think about each product and how confident you are.
           These probability distributions are the baseline on which the votes will be tested against.
           It is harder to make a product better that you assume is worse and easier to make it more worse
           and vice versa.")
    )
}

# Define UI for application that draws a histogram
shinyUI(

    # Application title
    navbarPage("Bayesian Ranking!",
               tabPanel("Explanaition", ui.getExplanaition()),
               tabPanel("Add Data...", ui.addData()),
               tabPanel("Plots", ui.getPlot()))
    
)
