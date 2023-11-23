library(shiny)
library(ggplot2)
library(tidyverse)

pval_plot <- function(distribution, statistic, df = NULL){
  if(distribution == 'z'){
    p_value = pnorm(statistic)
    dat <- seq(-4,4,0.1) %>% data.frame(x = ., y = dnorm(.)) 
    plot_statistic(dat, statistic = statistic,dist = 'z')
  }else if(distribution == 't'){
    p_value = pt(statistic, df = df)
    dat <- seq(-4,4,0.1) %>% data.frame(x = ., y = dt(.,df = df)) 
    plot_statistic(dat, statistic = statistic,dist = 't',df = df)
  }
}

alpha_to_statistic <- function(alpha, dof = NULL){
  if(is.null(dof)){
    x <- qnorm(alpha + ((1-alpha)/2))
  } else{
    x <- qt(alpha + ((1-alpha)/2),df = dof)
  }
  
  cat('For a value to be significant at the alpha level = ', alpha,'\n', 'You\'d need a test statistic more extreme than - ', x)
}


plot_statistic <- function(dat, statistic, dist, df = NULL){
  if(dist == 'z'){
    dat %>% ggplot(aes(x,y)) +
      geom_density(stat = 'identity', fill = 'red') +
      geom_density(data = dat %>% filter(x >= abs(statistic)), stat = 'identity', fill = 'blue') +
      geom_density(data = dat %>% filter(x <= -abs(statistic)), stat = 'identity', fill = 'blue') +
      annotate(geom = 'segment', x = -abs(statistic), y = dnorm(-abs(statistic)), xend = -abs(statistic), yend = dnorm(-abs(statistic)) + 0.05, arrow = arrow(type = 'closed', length = unit(0.02, "npc"))) +
      annotate(geom = 'segment', x = abs(statistic), y = dnorm(abs(statistic)), xend = abs(statistic), yend = dnorm(abs(statistic)) + 0.05, arrow = arrow(type = 'closed', length = unit(0.02, "npc"))) +
      annotate(geom = 'text', x = c(-abs(statistic), abs(statistic)), y = dnorm(-abs(statistic)) + 0.07, label = 'Rejection Region') +
      annotate(geom = 'text', x = 2.5, y = 0.3, label = paste0('P Value = ', 1 - (pnorm(statistic) %>% round(3))), hjust = 0.75, col = 'red', size = unit(5, 'pt')) +
      papaja::theme_apa() +
      scale_x_continuous(breaks = seq(-4, 4, 1))
  } else if (dist == 't'){
    dat %>% ggplot(aes(x,y)) +
      geom_density(stat = 'identity', fill = 'red') +
      geom_density(data = dat %>% filter(x >= abs(statistic)), stat = 'identity', fill = 'blue') +
      geom_density(data = dat %>% filter(x <= -abs(statistic)), stat = 'identity', fill = 'blue') +
      annotate(geom = 'segment', x = -abs(statistic), y = dt(-abs(statistic), df = df), xend = -abs(statistic), yend = dt(-abs(statistic), df = df) + 0.05, arrow = arrow(type = 'closed', length = unit(0.02, "npc"))) +
      annotate(geom = 'segment', x = abs(statistic), y = dt(abs(statistic), df = df), xend = abs(statistic), yend = dt(abs(statistic), df = df) + 0.05, arrow = arrow(type = 'closed', length = unit(0.02, "npc"))) +
      annotate(geom = 'text', x = c(-abs(statistic), abs(statistic)), y = dt(-abs(statistic), df = df) + 0.07, label = 'Rejection Region') +
      annotate(geom = 'text', x = 2.5, y = 0.3, label = paste0('P Value (One Sided) = ', 1 - (pt(statistic, df) %>% round(3))), hjust = 0.75, col = 'red', size = unit(5, 'pt')) +
      papaja::theme_apa() +
      scale_x_continuous(breaks = seq(-4, 4, 1)) + 
      scale_y_continuous(limits = c(0,0.4))
  }
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(includeCSS("www/quiz.css")),
  tabsetPanel(
    tabPanel(
      'Understanding Distributions',
      sidebarLayout(
        sidebarPanel(
          selectInput('dist',label = 'Choose Distribution',choices = c('Z-Distribution', 'T-Distribution'),selected = 'Z-Distribution'),
          sliderInput('statistic',label = 'Choose the Test Statistic Value', min = -4, max = 4, value = 1.96, step = 0.01,animate = T),
          sliderInput('df',label = 'Choose Degrees of Freedom',min = 1,max = 200,value = 1,step = 1),
        ),
        mainPanel(verbatimTextOutput('out1'),
                  plotOutput('plot1'))
      )
    ),
    tabPanel('Generate test statistics using alpha',
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = 'alpha', label = 'Choose your Alpha Value',min = 0.01,max = 0.99,value = 0.95,step = 0.01),
                 numericInput(inputId = 'dofalpha',label = 'Choose degrees of freedom',min = 1,max = Inf,value = 10000,step = 1),
                 tags$p('To obtain the alpha value for a Z-statistic, put in a very large number for the degrees of freedom')
               ),
               mainPanel(
                 verbatimTextOutput('alphaval')
               )
             )
             )
  )
)

server <- function(input, output) {
  
  # Observe changes in the input and update the plot accordingly
  observe({
    # Adjust inputs based on the selected distribution
    if(input$dist == 'Z-Distribution') {
      distribution_type <- 'z'
      df <- NULL
    } else if (input$dist == 'T-Distribution') {
      distribution_type <- 't'
      df <- input$df
    }
    
    # Generate the data based on the selected distribution and statistic
    dat <- if(distribution_type == 'z') {
      seq(-4, 4, 0.1) %>% data.frame(x = ., y = dnorm(.))
    } else {
      seq(-4, 4, 0.1) %>% data.frame(x = ., y = dt(., df = df))
    }
    
    # Render the plot
    output$plot1 <- renderPlot({
      plot_statistic(dat, statistic = input$statistic, dist = distribution_type, df = df)
    })
    
    # Display the calculated p-values
    output$out1 <- renderText({
      one_sided_p_value <- if(distribution_type == 'z') {
        pnorm(input$statistic)
      } else {
        pt(input$statistic, df = df)
      }
      two_sided_p_value <- (1-((one_sided_p_value) %>% round(3))) * 2
      
      
      paste("One-Sided P-Value:", 1 - (round(one_sided_p_value, 3)),
            "\nTwo-Sided P-Value:", two_sided_p_value)
    })
  })
  output$alphaval <- renderPrint({
    alpha_to_statistic(input$alpha,dof = input$dofalpha)
  })
  
}

shinyApp(ui = ui,server = server)



