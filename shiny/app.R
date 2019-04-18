library(shiny)
library(ggplot2)
library(dplyr)
source("code/annuity.R")
source("code/futurevalue.R")
source("code/growingannuity.R")

ui <- fluidPage(
  
  titlePanel("Future value estimate calculator"),
  
  fluidRow(
    column(
      4,
      sliderInput("amount", "Initial Amount", min=0, max=100000, value=1000, step=500),
      sliderInput("contrib", "Annual Contribution", min=0, max=50000, value=2000, step=500)
    ),
    column(
      4,
      sliderInput("rate", "Return Rate (in %)", min=0., max=0.2, value=0.05, step=0.01),
      sliderInput("growth", "Growth Rate (in %)", min=0., max=0.2, value=0.02, step=0.01)
    ),
    column(
      4,
      sliderInput("years", "Years", min=0, max=50, value=20, step=1),
      selectInput("facet", "Facet?", c("No"="no", "Yes"="yes"))
    )
  ),
  
  hr(),
  
  mainPanel(
    h3("Timelines"),
    plotOutput("timelines"),
    br(),
    h3("Balances"),
    tableOutput("balances")
    )
)

server <- function(input, output) {
  
  output$timelines <- renderPlot({
    n = input$years
    a = input$amount
    r = input$rate
    c = input$contrib
    g = input$growth
      
    modalities <- matrix(rep(0, (4*(n+1))), ncol = 4, dimnames = list(0:n, c("year", "no_contrib", "fixed_contrib", "growing_contrib")))
    for(i in 1:(n+1)) {
      fvi = future_value(amount=a, rate=r, years=(i-1))
      modalities[i,1] = i-1
      modalities[i,2] = fvi
      modalities[i,3] = fvi + annuity(contrib=c, rate=r, years=(i-1))
      modalities[i,4] = fvi + growing_annuity(contrib=c, rate=r, growth=g, years=(i-1))
    }
    modalities <- as.data.frame(modalities)
    
    if(input$facet=="no") {
      ggplot(modalities, aes(x=year)) +
        geom_line(aes(y=no_contrib, color = "no_contrib")) +
        geom_line(aes(y=fixed_contrib, colour = "fixed_contrib")) +
        geom_line(aes(y=growing_contrib, colour = "growing_contrib")) +
        geom_point(aes(y=no_contrib, colour="no_contrib")) +
        geom_point(aes(y=fixed_contrib, color="fixed_contrib")) +
        geom_point(aes(y=growing_contrib, color="growing_contrib")) +
        ylab("value") +
        ggtitle("Three modes of investing") +
        scale_colour_manual(values=c("red", "green", "blue"), name="variable", limits=c("no_contrib", "fixed_contrib", "growing_contrib"))
    } else {
      no_contrib <- modalities %>% select(year, no_contrib) %>% mutate(variable="no_contrib") %>% rename(value=no_contrib)
      fixed <- modalities %>% select(year, fixed_contrib) %>% mutate(variable="fixed_contrib") %>% rename(value=fixed_contrib)
      growing <- modalities %>% select(year, growing_contrib) %>% mutate(variable="growing_contrib") %>% rename(value=growing_contrib)
      facet_graph <- rbind(no_contrib, fixed, growing)
      facet_graph$variable <- factor(facet_graph$variable, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
      
      ggplot(facet_graph, aes(x=year)) +
        geom_line(aes(y=value, color=variable)) +
        geom_point(aes(y=value, color=variable)) +
        geom_area(aes(y=value, fill=variable), alpha=0.5) +
        ylab("value") +
        ggtitle("Three modes of investing") +
        scale_colour_manual(values=c("red", "green", "blue"), name="variable") +
        facet_grid(.~variable) +
        theme_bw()
    }
  })
  
  output$balances <- renderTable({
    n = input$years
    a = input$amount
    r = input$rate
    c = input$contrib
    g = input$growth
    
    modalities <- matrix(rep(0, (4*(n+1))), ncol = 4, dimnames = list(0:n, c("year", "no_contrib", "fixed_contrib", "growing_contrib")))
    for(i in 1:(n+1)) {
      fvi = future_value(amount=a, rate=r, years=(i-1))
      modalities[i,1] = i-1
      modalities[i,2] = fvi
      modalities[i,3] = fvi + annuity(contrib=c, rate=r, years=(i-1))
      modalities[i,4] = fvi + growing_annuity(contrib=c, rate=r, growth=g, years=(i-1))
    }
    modalities <- as.data.frame(modalities)
    modalities$year <- as.integer(modalities$year)
    modalities
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

