# App to run CWD Model

library(shiny)

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


model_output <- function(time, beta, mu){
  
  alpha <- 4.48307
  m <- 0.103202
  gamma <- 0.206146
  epsilon <- 0.150344
  tau <- 0.135785
  S0 <- 180
  
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
  
  # print(model_output)
  
  return(model_output)
}



# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("CWD SEI Model"), 
    
    sidebarLayout(
        sidebarPanel(
            position = "right",
            
            sliderInput(inputId = "beta", 
                        label = "beta", 
                        min = 0, 
                        value = 0.002446, 
                        max = 0.005, 
                        step = 0.0001), 
            
            sliderInput(inputId = "mu", 
                        label = "mu", 
                        min = 0, 
                        value = 2.617254, 
                        max = 3,
                        step = 0.05),
            
            sliderInput(inputId = "time", 
                        label = "time", 
                        min = 0, 
                        value = 20, 
                        max = 50,
                        step = 5)
        ),
        
        mainPanel(
            plotOutput("plot1")
        )
    )
)



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()

    rv$cur_beta <- 0.002446
    rv$cur_mu <- 2.617254 
    rv$pre_beta <- 0.002446
    rv$pre_mu <- 2.617254
    
    # observeEvent(input$beta,{
    #   rv$pre_beta <- isolate(rv$cur_beta)
    #   rv$cur_beta <- input$beta
    # })
    # 
    # observeEvent(input$mu,{
    #   rv$pre_mu <- isolate(rv$cur_mu)
    #   rv$cur_mu <- input$mu
    # })

    
    output$plot1 <- renderPlot({
      rv$pre_beta <- isolate(rv$cur_beta)
      rv$cur_beta <- input$beta
      
      rv$pre_mu <- isolate(rv$cur_mu)
      rv$cur_mu <- input$mu
      
      previous <- model_output(isolate(input$time), beta = isolate(rv$pre_beta), mu = isolate(rv$pre_mu))
      current <- model_output(isolate(input$time), beta = isolate(rv$cur_beta), mu = isolate(rv$cur_mu))
      
      SIE_plot <-  ggplot(data = current, aes(x = time, y = Number, group = mu, color = mu)) +
        ylab("Number of infectious hosts") + 
        xlab("Time") + geom_line(size = 1, color = "red") +
        geom_line(data = previous, aes(x = time, y = Number, group = mu, color = mu), size = 1, color = "blue") +
        xlim(min(previous$time, current$time), max(previous$time, current$time)) +
        ylim(min(previous$Number, current$Number), max(previous$Number, current$Number)) +
        theme_bw() +
        theme(text = element_text(size = 20), 
              strip.background = element_blank())
      SIE_plot
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
