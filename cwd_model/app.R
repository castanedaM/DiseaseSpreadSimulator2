# App to run CWD Model

library(shiny)
library(shinydashboard)

# Packages
if(!require(ggplot2)) install.packages(ggplot2)
if(!require(tidyverse)) install.packages(tidyverse)
if(!require(deSolve)) install.packages(deSolve)


# Improvements to do:
# - be able to compare models with different parameters
# - explain what each parameter does
# - show what parameters are being used
# - make an reactive value function so plot render is not bulky
# - make the other parameters vary, and set proper ranges
# - make better plot



# Model Specification (user defined function) ------------------------------

SIEmod <- function(times, State, Pars) {
  
  with(as.list(c(State, Pars)), {
    
    dS <-   alpha - S * (beta * I + gamma * E + m)
    dI <-   S * (beta * I + gamma * E) - I * (m + mu)
    dE <-   epsilon * I - tau * E
    
    return(list(c(dS, dI, dE)))
  })
}


model_output <- function(time, beta, mu, alpha, m, gamma, epsilon, tau){
  
  #alpha <- 4.48307
  #m <- 0.103202
  #gamma <- 0.206146
  #epsilon <- 0.150344
  #tau <- 0.135785
  S0 <- 1000
  
  parsSIE <- c(alpha = alpha,
               m = m, 
               gamma = gamma,
               epsilon = epsilon,
               tau = tau,
               S0 = S0,
               beta = beta,
               mu =  mu)
  
  times_vec <- seq(0, time, 1)
  
  out <- as.data.frame(ode(func = SIEmod, y = c(S = S0, I = 0 ,  E =0.0000003),  
                           parms = parsSIE, times = times_vec))
  
  names(out)[2:4] <- c("Susceptible", "Infectious", "Virus")
  
  # Converting to long format for plots
  outL <- out %>% 
    gather(key = Compartment, value = Number, -time) %>% 
    data.frame()
  
  # print(outL)
  
  outL$Compartment <- factor(outL$Compartment, levels = c("Susceptible", "Infectious", "Virus"))
  
  outL <- bind_cols(outL, data.frame(t(matrix(parsSIE))))
  head(outL)
  
  names(outL)[4:ncol(outL)] <- c("alpha", "m", "gamma", "epsilon",
                                 "tau", "S0", "beta", "mu")
  
  model_output <-outL  %>%  filter(Compartment== "Infectious") %>%
    mutate(beta = as.character(round(beta, 6)),
           mu = as.character(round(mu, 3))) 
  
  
  return(model_output)
}

get_changed <- function(rv){
  current <- c( isolate(rv$cur_beta), 
                isolate(rv$cur_mu), isolate(rv$cur_alpha),
                isolate(rv$cur_m), isolate(rv$cur_gamma),
                isolate(rv$cur_epsilon), isolate(rv$cur_tau))
  start <- c(0.002446, 2.62, 4,
             0.103, 0.21, 0.15, 
             0.136)
  Parameters <- c("Transmission Rate (beta)", "CWD Morality Rate (mu)", 
                  "Birth Rate (alpha)", "Natural Morality Rate (m)", 
                  "Indirect Transmission (gamma)", 
                  "Rate of Infectious Material Excretion (epsilon)", 
                  "Enviromental Infectious Material Loss Rate (tau)")
  changed = current - start
  for (i in 1:length(changed)){
    if( changed[i] != 0){
      changed[i] = current[i]
    }
  }
  df <- data.frame(Parameters, changed, start)
  changed_df <- subset(df, changed != 0)
  colnames(changed_df) <- c('Parameter_Name', 'New_Value', 'Original_Value')
  return(changed_df)
}

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "CWD SEI Model"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(column(12, wellPanel(plotOutput("plot1")))),
    fluidRow(
      column(6,
             wellPanel(title = "Inputs",
                 sliderInput(inputId = "beta", 
                             label = "Transmission Rate", 
                             min = 0, 
                             value = 0.002446, 
                             max = 1, 
                             step = 0.001),
                 numericInput("beta2", "", 
                              min = 0, 
                              value = 0.002446, 
                              max = 1),
                 
                 sliderInput(inputId = "mu", 
                             label = "CWD Morality Rate", 
                             min = 0, 
                             value = 2.617254, 
                             max = 3,
                             step = 0.05),
                 numericInput("mu2", "", 
                              min = 0, 
                              value = 2.617254, 
                              max = 3),
                 
                 sliderInput(inputId = "time", 
                             label = "Time", 
                             min = 0, 
                             value = 20, 
                             max = 50,
                             step = 5),
                 numericInput("time2", "",  
                              min = 0, 
                              value = 20, 
                              max = 50),
                 
                 sliderInput(inputId = "alpha", 
                             label = "Birth Rate", 
                             min = 0, 
                             value = 4.48307, 
                             max = 10, 
                             step = 1),
                 numericInput("alpha2", "", 
                              min = 0, 
                              value = 4.48307, 
                              max = 10),
                 
                 sliderInput(inputId = "m", 
                             label = "Natural Morality Rate", 
                             min = 0, 
                             value = 0.103202, 
                             max = 1,
                             step = 0.001),
                 numericInput("m2", "", 
                              min = 0, 
                              value = 0.103202, 
                              max = 1),
                 
                 sliderInput(inputId = "gamma", 
                             label = "Indirect Transmission", 
                             min = 0, 
                             value = 0.206146, 
                             max = 5,
                             step = .01),
                 numericInput("gamma2", "", 
                              min = 0, 
                              value = 0.206146, 
                              max = 5),
                 
                 sliderInput(inputId = "epsilon", 
                             label = "Rate of Infectious Material Excretion", 
                             min = 0, 
                             value = 0.150344, 
                             max = 5, 
                             step = .001), 
                 numericInput("epsilon2", "", 
                              min = 0, 
                              value = 0.150344, 
                              max = 5),
                 
                 sliderInput(inputId = "tau", 
                             label = "Enviromental Infectious Material Loss Rate" , 
                             min = 0, 
                             value = 0.135785, 
                             max = 5,
                             step = 0.001),
                 numericInput("tau2", "", 
                              value = 0.135785, 
                              min = 0, 
                              max = 5))),
      column(6,wellPanel(DT::dataTableOutput("data")))
    )
  )
)

update_all <- function(input, session){
  observe({
    updateSliderInput(session = session,inputId = "beta",value = input$beta2)
  })
  observe({
    updateSliderInput(session = session, inputId = "beta2",value = input$beta)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "mu",value = input$mu2)
  })
  observe({
    updateSliderInput(session = session, inputId = "mu2",value = input$mu)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "time",value = input$time2)
  })
  observe({
    updateSliderInput(session = session, inputId = "time2",value = input$time)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "alpha",value = input$alpha2)
  })
  observe({
    updateSliderInput(session = session, inputId = "alpha2",value = input$alpha)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "m",value = input$m2)
  })
  observe({
    updateSliderInput(session = session, inputId = "m2",value = input$m)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "epsilon",value = input$epsilon2)
  })
  observe({
    updateSliderInput(session = session, inputId = "epsilon2",value = input$epsilon)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "gamma",value = input$gamma2)
  })
  observe({
    updateSliderInput(session = session, inputId = "gamma2",value = input$gamma)
  })
  
  observe({
    updateSliderInput(session = session,inputId = "tau",value = input$tau2)
  })
  observe({
    updateSliderInput(session = session, inputId = "tau2",value = input$tau)
  })
}



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  update_all(input, session)
  rv <- reactiveValues()
  
  rv$cur_beta <- 0.002446
  rv$cur_mu <- 2.617254 
  rv$cur_alpha <- 4.48307
  rv$cur_m <- 0.103202
  rv$cur_gamma <- 0.206146
  rv$cur_epsilon <- 0.150344
  rv$cur_tau <- 0.135785
  
  rv$pre_beta <- 0.002446
  rv$pre_mu <- 2.617254
  rv$pre_alpha <- 4.48307
  rv$pre_m <- 0.103202
  rv$pre_gamma <- 0.206146
  rv$pre_epsilon <- 0.150344
  rv$pre_tau <- 0.135785
  
  
  
  update <- function(rv, input){
    rv$pre_beta <- isolate(rv$cur_beta)
    rv$pre_mu <- isolate(rv$cur_mu)
    rv$pre_alpha <- isolate(rv$cur_alpha)
    rv$pre_m <- isolate(rv$cur_m)
    rv$pre_gamma <- isolate(rv$cur_gamma)
    rv$pre_epsilon <- isolate(rv$cur_epsilon)
    rv$pre_tau <- isolate(rv$cur_tau)
    
    rv$cur_beta <- input$beta
    rv$cur_mu <- input$mu
    rv$cur_alpha <- input$alpha
    rv$cur_m <- input$m
    rv$cur_gamma <- input$gamma
    rv$cur_epsilon <- input$epsilon
    rv$cur_tau <- input$tau
    
    return(rv)
  }
  
  
  
  output$plot1 <- renderPlot({
    rv <- update(rv, input)
    get_changed(rv)
    previous <- model_output(isolate(input$time), beta = isolate(rv$pre_beta), mu = isolate(rv$pre_mu), alpha = isolate(rv$pre_alpha),
                             m = isolate(rv$pre_m), gamma = isolate(rv$pre_gamma), epsilon = isolate(rv$pre_epsilon),
                             tau = isolate(rv$pre_tau))
    current <- model_output(isolate(input$time), beta = isolate(rv$cur_beta), mu = isolate(rv$cur_mu), alpha = isolate(rv$cur_alpha),
                            m = isolate(rv$cur_m), gamma = isolate(rv$cur_gamma), epsilon = isolate(rv$cur_epsilon), tau = isolate(rv$cur_tau))
    
    
    SIE_plot <-  ggplot(data = current, aes(x = time, y = Number, group = mu, color = mu)) +
      ylab("Number of infectious hosts") + 
      xlab("Time") + geom_line(size = 1, color = "red") +
      geom_line(data = previous, aes(x = time, y = Number, group = mu, color = mu), size = 1, color = "blue") +
      ylim(min(previous$Number, current$Number), max(previous$Number, current$Number)) +
      theme_bw() +
      theme(text = element_text(size = 20), 
            strip.background = element_blank())
    
    #this will adjust the length of the x axis however it only does it after another variable is changed
    SIE_plot <- SIE_plot + xlim(min(previous$time, current$time), max(previous$time, current$time))
    SIE_plot
    
  })
  
  output$data <- DT::renderDataTable({
    rv <- update(rv, input)
    table <- get_changed(rv)
    DT::datatable(table)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
