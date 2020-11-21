#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)

theme_set(
    theme_dark() +
        theme(legend.position = "bottom",
              text = element_text(size=14))
)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    values <- reactiveValues()
    
    values$Choosen <- 1
    values$Products <- tribble(
                                ~Name, ~Votes, ~Prior,
                                "Product 1", list(0,0,0,0,0), list(.5,0),
                                "Product 2", list(0,0,0,0,0), list(.5,0),
                                "Product 3", list(0,0,0,0,0), list(.5,0)
                            )
    
    values$Priors <- tribble(~Name, ~Prior.Left, ~Prior.Right)
    
    # New Product choosen
    Product.Choosen <- observeEvent(input$Product, {
        values$Choosen <- as.integer(substr(input$Product,9,9))
        
        updateTextInput(session,
                        "Product.Name",
                        value = values$Products[values$Choosen,]$Name)
        
        updateNumericInput(session,
                           "Votes.Five",
                           value = values$Products[values$Choosen,]$Votes[[1]][[5]])
        
        updateNumericInput(session,
                           "Votes.Four",
                           value = values$Products[values$Choosen,]$Votes[[1]][[4]])
        
        updateNumericInput(session,
                           "Votes.Three",
                           value = values$Products[values$Choosen,]$Votes[[1]][[3]])
        
        updateNumericInput(session,
                           "Votes.Two",
                           value = values$Products[values$Choosen,]$Votes[[1]][[2]])
        
        updateNumericInput(session,
                           "Votes.One",
                           value = values$Products[values$Choosen,]$Votes[[1]][[1]])
        
        updateSliderInput(session,
                          "Prior.Goodness",
                          value = values$Products[values$Choosen,]$Prior[[1]][[1]])
        
        updateSliderInput(session,
                          "Prior.Confidence",
                          value = values$Products[values$Choosen,]$Prior[[1]][[2]])
        
                        
    })
    
    # Values Changed - Event Triggers
    Product.Changed.Name <- observeEvent(input$Product.Name, {
        values$Products[values$Choosen,]$Name <- input$Product.Name
    })
    
    Product.Changed.Votes.Five <- observeEvent(input$Votes.Five, {
        values$Products[values$Choosen,]$Votes[[1]][[5]] <- input$Votes.Five
    })
    Product.Changed.Votes.Four <- observeEvent(input$Votes.Four, {
        values$Products[values$Choosen,]$Votes[[1]][[4]] <- input$Votes.Four
    })
    Product.Changed.Votes.Three <- observeEvent(input$Votes.Three, {
        values$Products[values$Choosen,]$Votes[[1]][[3]] <- input$Votes.Three
    })
    Product.Changed.Votes.Two <- observeEvent(input$Votes.Two, {
        values$Products[values$Choosen,]$Votes[[1]][[2]] <- input$Votes.Two
    })
    Product.Changed.Votes.One <- observeEvent(input$Votes.One, {
        values$Products[values$Choosen,]$Votes[[1]][[1]] <- input$Votes.One
    })
    
    Product.Changed.Prior.Goodness <- observeEvent(input$Prior.Goodness, {
        values$Products[values$Choosen,]$Prior[[1]][[1]] <- input$Prior.Goodness
    })
    Product.Changed.Prior.Confidence <- observeEvent(input$Prior.Confidence, {
        values$Products[values$Choosen,]$Prior[[1]][[2]] <- input$Prior.Confidence
    })
    
    # Make plots
    output$Prior.Plot <- renderPlot({
        Priors <-   values$Products %>% 
                            rowwise() %>% 
                            filter(sum(unlist(Votes)) > 0)
        
        if(nrow(Priors)>0) {
            values$Priors <- Priors %>% 
                                mutate(Prior.ToDistribute = sum(unlist(Votes))*Prior[[2]],
                                       Prior.Left = Prior.ToDistribute*Prior[[1]] + 1,
                                       Prior.Right = Prior.ToDistribute*(1-Prior[[1]]) + 1) %>% 
                                ungroup() %>%
                                select(Name, Prior.Left, Prior.Right) %>% 
                                mutate(Name = factor(Name, levels=values$Products$Name))
            
            Prior.Data <- values$Priors %>% 
                            rowwise() %>% 
                            mutate(Stars = list(seq(0,1,by=.01))) %>% 
                            ungroup() %>% 
                            unnest_legacy() %>% 
                            mutate(`P(Stars)` = dbeta(Stars, Prior.Left, Prior.Right),
                                   `P(Stars)` = `P(Stars)`/sum(`P(Stars)`),
                                   Stars = Stars*4 + 1)
            
            ggplot(Prior.Data,
                   aes(x = Stars,
                       y = `P(Stars)`,
                       color=Name)) +
                geom_line() +
                scale_color_brewer(type="qual", palette = "Set2") +
                scale_y_continuous(labels = label_percent()) +
                coord_cartesian(xlim = c(1,5), ylim = c(0, max(Prior.Data$`P(Stars)`)))
        } else { ggplot() }
    })
    
    output$Posterior.Plot <- renderPlot({
        Priors <-   values$Products %>% 
                        rowwise() %>% 
                        filter(sum(unlist(Votes)) > 0)
        
        if(nrow(Priors)>0) {
            values$Posteriors <- values$Priors %>%
                                    left_join(values$Products,
                                              by="Name") %>% 
                                    rowwise() %>% 
                                    mutate(Posterior.Left = Prior.Left + Votes[[5]] + Votes[[4]]*3/4 + Votes[[3]]*2/4 + Votes[[2]]*1/4,
                                           Posterior.Right = Prior.Right + Votes[[1]] + Votes[[2]]*3/4 + Votes[[3]]*2/4 + Votes[[4]]*1/4) %>% 
                                    ungroup() %>% 
                                    select(Name, Posterior.Left, Posterior.Right)
            
            Posterior.Data <- values$Posteriors %>% 
                                rowwise() %>% 
                                mutate(Stars = list(seq(0,1,by=.01))) %>% 
                                ungroup() %>% 
                                unnest_legacy() %>% 
                                mutate(`P(Stars | Votes)` = dbeta(Stars, Posterior.Left, Posterior.Right),
                                       `P(Stars | Votes)` = `P(Stars | Votes)`/sum(`P(Stars | Votes)`),
                                       Stars = Stars*4 + 1)
            
            Posterior.Confidence <- values$Posteriors %>% 
                                        mutate(Limit = qbeta(input$Posterior.ErrorRate, Posterior.Left, Posterior.Right) * 4 + 1)
            
            ggplot(Posterior.Data,
                   aes(x = Stars,
                       y = `P(Stars | Votes)`,
                       color=Name)) +
                geom_vline(data=Posterior.Confidence,
                           aes(xintercept=Limit, color=Name),
                           linetype="dashed") +
                geom_line() +
                scale_color_brewer(type="qual", palette = "Set2") +
                scale_y_continuous(labels = label_percent()) +
                coord_cartesian(xlim = c(1,5), ylim = c(0, max(Posterior.Data$`P(Stars | Votes)`)))
        } else { ggplot() }
    })
    

})
