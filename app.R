#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markovchain)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Predictions in Ohio using Markov Chain"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(radioButtons("display_var",
                                  "Government Interventions and Vaccination in Ohio",
                                  choices = c("March 15: Restaurants and bars close for dine-ins" = "period1",
                                              "May 12: Restaurants and business allowed to open" = "period2",
                                              "July 23: Statewide mask mandates" = "period3",
                                              "December 14: Vaccination begins in Ohio" = "period4"),
                                  selected = "period1"
        ),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plotGraph")),
                        tabPanel("Transition Diagram", plotOutput("drawDiagram")),
                        tabPanel("Predictions", htmlOutput("text"))
    )
        )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    data_mc <- read.csv("/Users/pratisthabhandari/Documents/The Ohio State University/Autumn 2020/ISE 7300/Project revision/R/data.csv", header = TRUE)
    data_mc$Date <- as.Date(data_mc$Date, "%m/%d/%y")
    
    #Period1 - March 15 to May 11---------------------------------------------------
    period1_start_date <- as.Date("2020-03-15", format="%Y-%m-%d")
    period1_end_date <- as.Date("2020-05-11", format="%Y-%m-%d")
    period1_susceptible <- 11689100
    
    data_period1 <- filter(data_mc, Date >= period1_start_date & Date <= period1_end_date)
    # number of people susceptible at the start of the day
    period1_susceptible_by_day <- numeric()
    period1_susceptible_by_day[1] <- period1_susceptible
    for (i in 2:nrow(data_period1)) {
        period1_susceptible_by_day[i] <- period1_susceptible_by_day[i-1] - data_period1$Infected[i-1]
    }
    
    # number of active cases at the start of the day
    period1_active_cases <- numeric()
    period1_active_cases[1] <- 0
    for (i in 2:nrow(data_period1)) {
        period1_active_cases[i] <- period1_active_cases[i-1] + data_period1$Infected[i-1] - data_period1$Recovered[i-1] - data_period1$Dead[i-1]
    }
    
    period1_P01 <- mean(data_period1$Infected / period1_susceptible_by_day)
    period1_P12 <- mean(tail(data_period1$Recovered, 2) / tail(period1_active_cases, 2))
    
    period1_infected <- sum(data_period1$Infected)
    period1_removed <- sum(data_period1$Recovered) + sum(data_period1$Dead)
    
    period1_remaining <- period1_susceptible - period1_infected
    
    #Period2 - May 12 to July 22------------------------------------------------------
    period2_start_date <- as.Date("2020-05-12", format="%Y-%m-%d")
    period2_end_date <- as.Date("2020-07-22", format="%Y-%m-%d")
    period2_susceptible <- period1_remaining
    
    data_period2 <- filter(data_mc, Date >= period2_start_date & Date <= period2_end_date)
    # number of people susceptible at the start of the day
    period2_susceptible_by_day <- numeric()
    period2_susceptible_by_day[1] <- period2_susceptible
    for (i in 2:nrow(data_period2)) {
        period2_susceptible_by_day[i] <- period2_susceptible_by_day[i-1] - data_period2$Infected[i-1]
    }
    
    # number of active cases at the start of the day
    period2_active_cases <- numeric()
    period2_active_cases[1] <- 0
    for (i in 2:nrow(data_period2)) {
        period2_active_cases[i] <- period2_active_cases[i-1] + data_period2$Infected[i-1] - data_period2$Recovered[i-1] - data_period2$Dead[i-1]
    }
    
    period2_P01 <- mean(data_period2$Infected / period2_susceptible_by_day)
    period2_P12 <- mean(tail(data_period2$Recovered, 2) / tail(period2_active_cases, 2))
    
    period2_infected <- sum(data_period2$Infected) + period1_infected - period1_removed
    period2_removed <- sum(data_period2$Recovered) + sum(data_period2$Dead)
    
    period2_remaining <- period2_susceptible - period2_infected
    
    #Period3 - July 23 to Dec 13------------------------------------------------------
    period3_start_date <- as.Date("2020-07-23", format="%Y-%m-%d")
    period3_end_date <- as.Date("2020-12-13", format="%Y-%m-%d")
    period3_susceptible <- period2_remaining
    
    data_period3 <- filter(data_mc, Date >= period3_start_date & Date <= period3_end_date)
    # number of people susceptible at the start of the day
    period3_susceptible_by_day <- numeric()
    period3_susceptible_by_day[1] <- period3_susceptible
    for (i in 2:nrow(data_period3)) {
        period3_susceptible_by_day[i] <- period3_susceptible_by_day[i-1] - data_period3$Infected[i-1]
    }
    
    # number of active cases at the start of the day
    period3_active_cases <- numeric()
    period3_active_cases[1] <- 0
    for (i in 2:nrow(data_period3)) {
        period3_active_cases[i] <- period3_active_cases[i-1] + data_period3$Infected[i-1] - data_period3$Recovered[i-1] - data_period3$Dead[i-1]
    }
    period3_P01 <- mean(data_period3$Infected / period3_susceptible_by_day)
    period3_P12 <- mean(tail(data_period3$Recovered, 2) / tail(period3_active_cases, 2))
    
    period3_infected <- sum(data_period3$Infected) + period2_infected - period2_removed
    period3_removed <- sum(data_period3$Recovered) + sum(data_period3$Dead)
    
    period3_remaining <- period3_susceptible - period3_infected
    
    #Period4 - Dec 14 to Feb 20-------------------------------------------------------------------
    period4_start_date <- as.Date("2020-12-14", format="%Y-%m-%d")
    period4_end_date <- as.Date("2021-02-20", format="%Y-%m-%d")
    period4_susceptible <- period3_remaining
    
    data_period4 <- filter(data_mc, Date >= period4_start_date & Date <= period4_end_date)
    # number of people susceptible at the start of the day
    period4_susceptible_by_day <- numeric()
    period4_susceptible_by_day[1] <- period4_susceptible
    for (i in 2:nrow(data_period4)) {
        period4_susceptible_by_day[i] <- period4_susceptible_by_day[i-1] - data_period4$Infected[i-1]
    }
    
    # number of active cases at the start of the day
    period4_active_cases <- numeric()
    #period4_active_cases[1] <- 0
    period4_active_cases[1] <- tail(period3_active_cases, n=1)
    for (i in 2:nrow(data_period4)) {
        period4_active_cases[i] <- period4_active_cases[i-1] + data_period4$Infected[i-1] - data_period4$Recovered[i-1] - data_period4$Dead[i-1]
    }
    
    period4_P01 <- mean(data_period4$Infected / period4_susceptible_by_day)
    period4_P12 <- mean(tail(data_period4$Recovered, 2) / tail(period4_active_cases, 2))
    
    period4_infected <- sum(data_period4$Infected) + period3_infected - period3_removed
    period4_removed <- sum(data_period4$Recovered) + sum(data_period4$Dead)
    
    period4_remaining <- period4_susceptible - period4_infected

    #------------------------------------------------------------------------------------
    current_pop <-11689442
    current_cases <- 414432
    
    period1_alpha <-0.00004
    period1_beta <- 0.04751932
    infection_fn1 <- function(i){(period1_alpha*period1_beta*((1-period1_beta)^i) - (period1_alpha*period1_beta)*(1-period1_alpha)^i)/(period1_alpha*period1_beta-(period1_beta^2))}
    
    total_cases_p1 <- function(t) { infection_fn1(t) * period1_susceptible}
    real_total_cases_p1 <- function(t) {sum(data_period1$Infected[0:t])}
    
    new_susceptible <- current_pop - current_cases
    total_cases_currentP1 <- function(t) { infection_fn1(t) * new_susceptible + current_cases}

    #------------------------------------------------------------------------------------
    period2_alpha <- 0.00007
    period2_beta <- 0.09126608
    infection_fn2 <- function(i){(period2_alpha*period2_beta*((1-period2_beta)^i) - (period2_alpha*period2_beta)*(1-period2_alpha)^i)/(period2_alpha*period2_beta-(period2_beta^2))}
    
    total_cases_p2 <- function(t) { infection_fn2(t) * period2_susceptible}
    real_total_cases_p2 <- function(t) {sum(data_period2$Infected[0:t])}
    
    total_cases_currentP2 <- function(t) { infection_fn2(t) * new_susceptible + current_cases}
    
    #------------------------------------------------------------------------------------
    period3_alpha <- 0.0003106803
    period3_beta <- 0.0402368
    infection_fn3 <- function(i){(period3_alpha*period3_beta*((1-period3_beta)^i) - (period3_alpha*period3_beta)*(1-period3_alpha)^i)/(period3_alpha*period3_beta-(period3_beta^2))}
    
    total_cases_p3 <- function(t) { 0.35*infection_fn3(t) * period3_susceptible}
    real_total_cases_p3 <- function(t) {sum(data_period3$Infected[0:t])}
    
    total_cases_currentP3 <- function(t) { infection_fn3(t) * new_susceptible + current_cases}
        
    #------------------------------------------------------------------------------------
    period4_alpha <- 0.0004811531
    period4_beta <- 0.2775073
    infection_fn4 <- function(i){(period4_alpha*period4_beta*((1-period4_beta)^i) - (period4_alpha*period4_beta)*(1-period4_alpha)^i)/(period4_alpha*period4_beta-(period4_beta^2))}    
    
    total_cases_p4 <- function(t) { 5*infection_fn4(t) * period4_susceptible}
    real_total_cases_p4 <- function(t) {sum(data_period4$Infected[0:t])}
    
    total_cases_current4 <- function(t) { infection_fn4(t) * new_susceptible4 + current_cases}
        
    #------------------------------------------------------------------------------------
    t_matrix1 <- matrix(data=c(0.99996,0.00004,0,0,0.9525,0.0475,0,0,1),byrow=TRUE, nrow=3)
    my_mc1 <- new("markovchain", states=c("Susceptible","Infected","Removed"),
                      transitionMatrix=t_matrix1, name="Transition Diagram - March 15 to May 11")
    stateNames <- c("Susceptible","Infected","Removed")
    rownames(t_matrix1) <- stateNames; colnames(t_matrix1) <- stateNames
    
    t_matrix2 <- matrix(data=c(0.99993,0.00007,0,0,0.9087,0.0913,0,0,1),byrow=TRUE, nrow=3)
    my_mc2 <- new("markovchain", states=c("Susceptible","Infected","Removed"),
                  transitionMatrix=t_matrix2, name="Markov Chain - May 12 to July 22")
    stateNames <- c("Susceptible","Infected","Removed")
    rownames(t_matrix2) <- stateNames; colnames(t_matrix2) <- stateNames
    
    t_matrix3 <- matrix(data=c(0.9996,0.0004,0,0,0.9597,0.0403,0,0,1),byrow=TRUE, nrow=3)
    my_mc3 <- new("markovchain", states=c("Susceptible","Infected","Removed"),
                  transitionMatrix=t_matrix3, name="Transition Diagram - July 23 to Dec 13")
    stateNames <- c("Susceptible","Infected","Removed")
    rownames(t_matrix3) <- stateNames; colnames(t_matrix3) <- stateNames
    
    t_matrix4 <- matrix(data=c(0.9995,0.0005,0,0,0.7224,0.2776,0,0,1),byrow=TRUE, nrow=3)
    my_mc4 <- new("markovchain", states=c("Susceptible","Infected","Removed"),
                  transitionMatrix=t_matrix4, name="Transition Diagram - Dec 14 to Feb 20")
    stateNames <- c("Susceptible","Infected","Removed")
    rownames(t_matrix4) <- stateNames; colnames(t_matrix4) <- stateNames
    
    #--------------------------------------------------------------------------------------
    
    output$plotGraph <- renderPlot({
        if(input$display_var == "period1")
        {
            plot(sapply(0:30, real_total_cases_p1), ylim=c(0, 12000), cex = 1, pch = 19, col =alpha("red", 0.50), xlab="Number of Days", ylab="Total cases", main="Total Cases of COVID-19 in Ohio: Actual vs Predicted")
            points(sapply(0:30, total_cases_p1), cex = 1, pch = 19, col = alpha("blue",0.50))
            legend("bottomright",legend=c("Total Cases - Actual", "Total Cases - Predicted"),
                   col=c(alpha("red",0.50), alpha("blue",0.50)), pch=19)

        }
        else if (input$display_var == "period2")
        {
            plot(sapply(0:20, real_total_cases_p2), ylim=c(0, 15000), cex = 1, pch = 19, col =alpha("red", 0.50), xlab="Number of Days", ylab="Total cases", main="Total Cases of COVID-19 in Ohio: Actual vs Predicted")
            points(sapply(0:20, total_cases_p2), cex = 1, pch = 19, col = alpha("blue",0.50))
            legend("bottomright",legend=c("Total Cases - Actual", "Total Cases - Predicted"),
                   col=c(alpha("red",0.50), alpha("blue",0.50)), pch=19)
        }
        else if (input$display_var == "period3")
        {
            plot(sapply(0:20, real_total_cases_p3), ylim=c(0, 30000), cex = 1, pch = 19, col =alpha("red", 0.50), xlab="Number of Days", ylab="Total cases", main="Total Cases of COVID-19 in Ohio: Actual vs Predicted")
            points(sapply(0:20, total_cases_p3), cex = 1, pch = 19, col = alpha("blue",0.50))
            legend("bottomright",legend=c("Total Cases - Actual", "Total Cases - Predicted"),
                   col=c(alpha("red",0.50), alpha("blue",0.50)), pch=19)
            
        }
        else if (input$display_var == "period4")
        {
            plot(sapply(0:20, real_total_cases_p4), ylim=c(0, 150000), cex = 1, pch = 19, col =alpha("red", 0.50), xlab="Number of Days", ylab="Total cases", main="Total Cases of COVID-19: Actual vs Predicted")
            points(sapply(0:20, total_cases_p4), cex = 1, pch = 19, col = alpha("blue",0.50))
            legend("bottomright",legend=c("Total Cases - Actual", "Total Cases - Predicted"),
                   col=c(alpha("red",0.50), alpha("blue",0.50)), pch=19)
        }
        })
    
    output$drawDiagram <- renderPlot({
        if(input$display_var == "period1")
        {
            plot(my_mc1, pos=c(1,2),box.size = 0.15,box.prop = 0.5,arr.length=.1,
                 arr.width=.1,cex.txt = 0.8, self.cex = .4,package="diagram", main="Transition Diagram - March 15 to May 11")
            
            
        }
        else if (input$display_var == "period2")
        {
            plot(my_mc2, pos=c(1,2),box.size = 0.15,box.prop = 0.5,arr.length=.1,
                 arr.width=.1,cex.txt = 0.8, self.cex = .4,package="diagram", main="Transition Diagram - May 12 to July 22")
        }
        else if (input$display_var == "period3")
        {
            plot(my_mc3, pos=c(1,2),box.size = 0.15,box.prop = 0.5,arr.length=.1,
                 arr.width=.1,cex.txt = 0.8, self.cex = .4,package="diagram", main="Transition Diagram - July 23 to Dec 13")
            
        }
        
        else if (input$display_var == "period4")
        {
            plot(my_mc4, pos=c(1,2),box.size = 0.15,box.prop = 0.5,arr.length=.1,
                 arr.width=.1,cex.txt = 0.8, self.cex = .4,package="diagram", main="Transition Diagram - Dec 14 to Feb 20")
            
        }
        
    })
    
    
    output$text <- renderUI({
        if(input$display_var == "period1")
        {
            HTML(paste("March 13 (Actual): 986,740",
                        "May 1 (Predicted): 994,732", sep="<br/>"))
        
        }
        else if (input$display_var == "period2")
        {
            HTML(paste("March 13 (Actual): 986,740",
                       "May 1 (Predicted): 995,609", sep="<br/>"))
        }
        else if (input$display_var == "period3")
        {
            HTML(paste("March 13 (Actual): 986,740",
                       "May 1 (Predicted): 1,059,920", sep="<br/>"))
        }
        else if (input$display_var == "period4")
        {
            HTML(paste("March 13 (Actual): 986,740",
                       "May 1 (Predicted): 1,001,010", sep="<br/>"))
        }
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
