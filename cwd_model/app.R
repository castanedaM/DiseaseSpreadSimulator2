# App to run CWD Model

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)

# Packages
if (!require(ggplot2))
  install.packages(ggplot2)
if (!require(tidyverse))
  install.packages(tidyverse)
if (!require(deSolve))
  install.packages(deSolve)

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


model_output <-
  function(time,
           beta,
           mu,
           alpha,
           m,
           gamma,
           epsilon,
           tau,
           S0,
           I0,
           E0) {
    #alpha <- 4.48307
    #m <- 0.103202
    #gamma <- 0.206146
    #epsilon <- 0.150344
    #tau <- 0.135785
    #S0 <- 1000
    
    parsSIE <- c(
      alpha = alpha,
      m = m,
      gamma = gamma,
      epsilon = epsilon,
      tau = tau,
      S0 = S0,
      beta = beta,
      mu =  mu
    )
    
    times_vec <- seq(0, time, 1)
    
    # out <- as.data.frame(ode(func = SIEmod, y = c(S = S0, I = 0 ,  E =0.0000003),
    #                           parms = parsSIE, times = times_vec))
    
    out <-
      as.data.frame(ode(
        func = SIEmod,
        y = c(S = S0, I = I0 ,  E = E0),
        parms = parsSIE,
        times = times_vec
      ))
    
    names(out)[2:4] <- c("Susceptible", "Infectious", "Virus")
    
    # Converting to long format for plots
    outL <- out %>%
      gather(key = Compartment, value = Number,-time) %>%
      data.frame()
    
    # print(outL)
    
    outL$Compartment <-
      factor(outL$Compartment,
             levels = c("Susceptible", "Infectious", "Virus"))
    
    outL <- bind_cols(outL, data.frame(t(matrix(parsSIE))))
    head(outL)
    
    names(outL)[4:ncol(outL)] <- c("alpha", "m", "gamma", "epsilon",
                                   "tau", "S0", "beta", "mu")
    
    model_output <-
      outL  %>%  filter(Compartment == "Infectious") %>%
      mutate(beta = as.character(round(beta, 6)),
             mu = as.character(round(mu, 3)))
    
    
    return(model_output)
  }

get_changed <- function(rv_dt) {
  current <- c(
    isolate(rv_dt$cur_beta),
    isolate(rv_dt$cur_mu),
    isolate(rv_dt$cur_alpha),
    isolate(rv_dt$cur_m),
    isolate(rv_dt$cur_gamma),
    isolate(rv_dt$cur_epsilon),
    isolate(rv_dt$cur_tau)
  )
  previous <- c(
    isolate(rv_dt$pre_beta),
    isolate(rv_dt$pre_mu),
    isolate(rv_dt$pre_alpha),
    isolate(rv_dt$pre_m),
    isolate(rv_dt$pre_gamma),
    isolate(rv_dt$pre_epsilon),
    isolate(rv_dt$pre_tau)
  )
  start <- c(0.002, 2.6, 4.5,
             0.1, 0.21, 0.15,
             0.14)
  Parameters <-
    c(
      "Transmission Rate (beta)",
      "CWD Morality Rate (mu)",
      "Birth Rate (alpha)",
      "Natural Morality Rate (m)",
      "Indirect Transmission (gamma)",
      "Rate of Infectious Material Excretion (epsilon)",
      "Enviromental Infectious Material Loss Rate (tau)"
    )
  changed = current - start
  for (i in 1:length(changed)) {
    if (changed[i] != 0) {
      changed[i] = current[i]
    }
  }
  df <- data.frame(Parameters, changed, start, current, previous)
  changed_df <-
    subset(df, (changed != 0 | (changed == 0 & current == 0)))
  changed_df <- subset(changed_df, select = -c(current))
  colnames(changed_df) <-
    c('Parameter Name',
      'New Value',
      'Original Value',
      'Previous Value')
  return(changed_df)
}


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage(
  "CWD Model",
  theme = shinytheme("flatly"),
  tabPanel("Plot and Table",
           sidebarLayout(
             sidebarPanel(
               h2("Parameters"),
               wellPanel(
                 title = "Parameters",
                 sliderInput(
                   inputId = "beta",
                   label = "Transmission Rate",
                   min = 0.001,
                   value = 0.002,
                   max = 0.003,
                   step = 0.0005
                 ),
                 numericInput(
                   "beta2",
                   "",
                   min = 0.001,
                   value = 0.002,
                   max = 0.003
                 ),
                 
                 sliderInput(
                   inputId = "mu",
                   label = "CWD Morality Rate",
                   min = 1.3,
                   value = 2.6,
                   max = 4,
                   step = 0.2
                 ),
                 numericInput(
                   "mu2",
                   "",
                   min = 1.3,
                   value = 2.6,
                   max = 4
                 ),
                 sliderInput(
                   inputId = "alpha",
                   label = "Birth Rate",
                   min = 2.25,
                   value = 4.5,
                   max = 6.75,
                   step = .25
                 ),
                 numericInput(
                   "alpha2",
                   "",
                   min = 2.25,
                   value = 4.5,
                   max = 6.75
                 ),
                 
                 sliderInput(
                   inputId = "m",
                   label = "Natural Morality Rate",
                   min = 0.05,
                   value = 0.1,
                   max = 0.15,
                   step = 0.01
                 ),
                 numericInput(
                   "m2",
                   "",
                   min = 0.05,
                   value = 0.1,
                   max = .15
                 ),
                 
                 sliderInput(
                   inputId = "gamma",
                   label = "Indirect Transmission",
                   min = .1,
                   value = 0.21,
                   max = .31,
                   step = .01
                 ),
                 numericInput(
                   "gamma2",
                   "",
                   min = .1,
                   value = 0.21,
                   max = .31
                 ),
                 
                 sliderInput(
                   inputId = "epsilon",
                   label = "Rate of Infectious Material Excretion",
                   min = .07,
                   value = 0.15,
                   max = .23,
                   step = .01
                 ),
                 numericInput(
                   "epsilon2",
                   "",
                   min = .07,
                   value = 0.15,
                   max = .23
                 ),
                 
                 sliderInput(
                   inputId = "tau",
                   label = "Enviromental Infectious Material Loss Rate" ,
                   min = .07,
                   value = 0.14,
                   max = .21,
                   step = 0.01
                 ),
                 numericInput(
                   "tau2",
                   "",
                   value = 0.14,
                   min = .07,
                   max = .21
                 )
               ),
               width = 3
             ),
             mainPanel(
               tabName = "data",
               box(
                 h2("Plot of Infected Deer Over Time"),
                 br(),
                 div(p(
                   HTML("&emsp;&emsp;"),
                   span(style = "color:red;", "Current Parameters"),
                   HTML("&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;"),
                   span(style = "color:blue;", "Previous Parameters")
                 )),
                 wellPanel(plotOutput("plot1")),
                 
                 wellPanel(DT::dataTableOutput("data")),
                 width = 8
               ),
               
               column(1,
                      offset = 3,
                      actionButton("button", "Reset All"), ),
               
               column(
                 4,
                 h2("Scaling Inputs"),
                 wellPanel(
                   title = "Scaling Values",
                   sliderInput(
                     inputId = "time",
                     label = "Time",
                     min = 1,
                     value = 20,
                     max = 50,
                     step = 5
                   ),
                   numericInput(
                     "time2",
                     "",
                     min = 1,
                     value = 20,
                     max = 50
                   ),
                   sliderInput(
                     inputId = "S",
                     label = "Susceptible Deer",
                     min = 0,
                     value = 1000,
                     max = 2000,
                     step = 50
                   ),
                   numericInput(
                     "S2",
                     "",
                     min = 0,
                     value = 1000,
                     max = 2000
                   ),
                   
                   sliderInput(
                     inputId = "I",
                     label = "Infected Deer",
                     min = 0,
                     value = 0,
                     max = 2000,
                     
                     step = 50
                   ),
                   numericInput(
                     "I2",
                     "",
                     min = 0,
                     value =
                       0,
                     max = 2000
                   ),
                   
                   sliderInput(
                     inputId = "E",
                     label = "Mass of Infectious Material",
                     min = 0,
                     value = .005,
                     max = 1,
                     step = .001
                   ),
                   numericInput(
                     "E2",
                     "",
                     min = 0,
                     value = .005,
                     max = 1
                   )
                   
                 )
               )
             )
           ))
  ,
  tabPanel(
    "Info",
    h1("About"),
    br(),
    div(
      h2("Background Information"),
      p(
        "This web application was developed in order to better understand the spread of the disease known as Chonic Wasting Disease (CWD). CWD is a fatal disease affecting members of the deer family. The disease is
          extremely contagious because the prions spread through any direct or indirect contact. The prions also have the ability to remain in the surrounding area for long periods of time. Once CWD has entered a specific region,
          it becomes quick to spread making it difficult to prevent spread once it has reached an area. Deer are greatly affected by CWD all over the United States and tracking the spread of this disease could give more insight
          into how to prevent further outbreaks."
      )
    ),
    br(),
    div(
      h2("How to Use this Web App"),
      span(
        "When first viewing the app, a user may notice a few things. First there are two columns containing mutable values. One (located on the far left) is labled Paramters and contain all the variables which affect the
           final graph. The second (located on the far right) is labeled as Scalling Inputs and contains values which affect the iniitial scale of the graph. A user can change any of these values the graph will update in real
           time accordingly. In the middle of the page, there is the graph and table. As indicated above the graph, the red line represents the current graph while the blue line represents the graph before the
           parameter was changed. Below the graph is the aforementioned table which list the all parameters that are different from the original, preset values. These new values, along with the orinal values and
           most recent values, are veiwable in the table. Lastly, there is a reset button located at the top left corner of the page. This button clears the table and sets
           all the Parameters/Scaling Values to their original preset values."
      )
    ),
    br(),
    div(
      h2("Additonal Resources"),
      p(
        "For more information about this project please visit the following",
        a(href = "https://vtechworks.lib.vt.edu/handle/10919/103298", "link")
      )
    )
  )
  
)

update_all <- function(input, session) {
  observe({
    updateSliderInput(session = session,
                      inputId = "beta",
                      value = input$beta2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "beta2",
                       value = input$beta)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "mu",
                      value = input$mu2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "mu2",
                       value = input$mu)
  })
  
  
  observe({
    updateSliderInput(session = session,
                      inputId = "alpha",
                      value = input$alpha2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "alpha2",
                       value = input$alpha)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "m",
                      value = input$m2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "m2",
                       value = input$m)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "epsilon",
                      value = input$epsilon2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "epsilon2",
                       value = input$epsilon)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "gamma",
                      value = input$gamma2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "gamma2",
                       value = input$gamma)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "tau",
                      value = input$tau2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "tau2",
                       value = input$tau)
  })
  
  
  
}

update_SIE <- function(input, session) {
  observe({
    updateSliderInput(session = session,
                      inputId = "time",
                      value = input$time2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "time2",
                       value = input$time)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "S",
                      value = input$S2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "S2",
                       value = input$S)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "I",
                      value = input$I2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "I2",
                       value = input$I)
  })
  
  observe({
    updateSliderInput(session = session,
                      inputId = "E",
                      value = input$E2)
  })
  observe({
    updateNumericInput(session = session,
                       inputId = "E2",
                       value = input$E)
  })
}



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  update_all(input, session)
  update_SIE(input, session)
  
  rv <- reactiveValues()
  rv_dt <- reactiveValues()
  
  rv$cur_beta <- 0.002
  rv$cur_mu <- 2.6
  rv$cur_alpha <- 4.5
  rv$cur_m <- 0.1
  rv$cur_gamma <- 0.21
  rv$cur_epsilon <- 0.15
  rv$cur_tau <- 0.14
  rv$cur_time <- 20
  rv$reset <- TRUE
  rv$save <- FALSE
  
  rv$pre_beta <- 0.002
  rv$pre_mu <- 2.6
  rv$pre_alpha <- 4.5
  rv$pre_m <- 0.1
  rv$pre_gamma <- 0.21
  rv$pre_epsilon <- 0.15
  rv$pre_tau <- 0.14
  rv$pre_time <- 20
  
  rv$cur_S <- 1000
  rv$cur_I <- 0
  rv$cur_E <-  .005
  
  rv$pre_S <- 1000
  rv$pre_I <- 0
  rv$pre_E <-  .005
  
  update <- function(rv, input) {
    rv$pre_beta <- isolate(rv$cur_beta)
    rv$pre_mu <- isolate(rv$cur_mu)
    rv$pre_alpha <- isolate(rv$cur_alpha)
    rv$pre_m <- isolate(rv$cur_m)
    rv$pre_gamma <- isolate(rv$cur_gamma)
    rv$pre_epsilon <- isolate(rv$cur_epsilon)
    rv$pre_tau <- isolate(rv$cur_tau)
    
    rv$pre_time <- isolate(rv$cur_time)
    rv$cur_time <- input$time
    rv$pre_S <- isolate(rv$cur_S)
    rv$cur_S <- input$S
    rv$pre_I <- isolate(rv$cur_I)
    rv$cur_I <- input$I
    rv$pre_E <- isolate(rv$cur_E)
    rv$cur_E <- input$E
    
    rv$cur_beta <- input$beta
    rv$cur_mu <- input$mu
    rv$cur_alpha <- input$alpha
    rv$cur_m <- input$m
    rv$cur_gamma <- input$gamma
    rv$cur_epsilon <- input$epsilon
    rv$cur_tau <- input$tau
    
    return(rv)
  }
  
  rv_dt$cur_beta <- 0.002
  rv_dt$cur_mu <- 2.6
  rv_dt$cur_alpha <- 4.5
  rv_dt$cur_m <- 0.1
  rv_dt$cur_gamma <- 0.21
  rv_dt$cur_epsilon <- 0.15
  rv_dt$cur_tau <- 0.14
  
  rv_dt$pre_beta <- 0.002
  rv_dt$pre_mu <- 2.6
  rv_dt$pre_alpha <- 4.5
  rv_dt$pre_m <- 0.1
  rv_dt$pre_gamma <- 0.21
  rv_dt$pre_epsilon <- 0.15
  rv_dt$pre_tau <- 0.14
  
  update_dt <- function(rv_dt, input) {
    rv_dt$pre_beta <- isolate(rv_dt$cur_beta)
    rv_dt$pre_mu <- isolate(rv_dt$cur_mu)
    rv_dt$pre_alpha <- isolate(rv_dt$cur_alpha)
    rv_dt$pre_m <- isolate(rv_dt$cur_m)
    rv_dt$pre_gamma <- isolate(rv_dt$cur_gamma)
    rv_dt$pre_epsilon <- isolate(rv_dt$cur_epsilon)
    rv_dt$pre_tau <- isolate(rv_dt$cur_tau)
    
    rv_dt$cur_beta <- input$beta
    rv_dt$cur_mu <- input$mu
    rv_dt$cur_alpha <- input$alpha
    rv_dt$cur_m <- input$m
    rv_dt$cur_gamma <- input$gamma
    rv_dt$cur_epsilon <- input$epsilon
    rv_dt$cur_tau <- input$tau
    
    #rv$reset <- FALSE
    
    return(rv_dt)
  }
  
  observeEvent(input$button, {
    disable(id = "button")
    rv$reset <- TRUE
    updateNumericInput(session, "beta", value = 0.002)
    updateNumericInput(session, "mu", value = 2.6)
    updateNumericInput(session, "time", value = 20)
    updateNumericInput(session, "alpha", value = 4.5)
    updateNumericInput(session, "m", value = 0.1)
    updateNumericInput(session, "gamma", value = 0.21)
    updateNumericInput(session, "epsilon", value =  0.15)
    updateNumericInput(session, "tau", value = 0.14)
    
    updateNumericInput(session, "S", value = 1000)
    updateNumericInput(session, "I", value =  0)
    updateNumericInput(session, "E", value = 0.005)
    
    enable(id = "button")
    
  })
  
  observeEvent(input$button3, {
    rv$save <- TRUE
  })
  
  output$plot1 <- renderPlot({
    rv <- update(rv, input)
    
    previous <-
      model_output(
        time = isolate(rv$cur_time),
        beta = isolate(rv$pre_beta),
        mu = isolate(rv$pre_mu),
        alpha = isolate(rv$pre_alpha),
        m = isolate(rv$pre_m),
        gamma = isolate(rv$pre_gamma),
        epsilon = isolate(rv$pre_epsilon),
        tau = isolate(rv$pre_tau),
        S0 = isolate(rv$pre_S) ,
        I0 = isolate(rv$pre_I),
        E0 = isolate(rv$pre_E)
      )
    
    current <-
      model_output(
        time = isolate(rv$cur_time),
        beta = isolate(rv$cur_beta),
        mu = isolate(rv$cur_mu),
        alpha = isolate(rv$cur_alpha),
        m = isolate(rv$cur_m),
        gamma = isolate(rv$cur_gamma),
        epsilon = isolate(rv$cur_epsilon),
        tau = isolate(rv$cur_tau),
        S0 = isolate(rv$cur_S) ,
        I0 = isolate(rv$cur_I),
        E0 = isolate(rv$cur_E)
      )
    
    if (isolate(rv$reset) == FALSE)  {
      SIE_plot <-
        ggplot(data = current, aes(
          x = time,
          y = Number,
          group = mu,
          color = mu
        )) +
        ylab("Number of infectious hosts") +
        xlab("Time") + geom_line(size = 1, color = "red") +
        geom_line(
          data = previous,
          aes(
            x = time,
            y = Number,
            group = mu,
            color = mu
          ),
          size = 1,
          color = "blue"
        ) +
        ylim(min(previous$Number, current$Number),
             max(previous$Number, current$Number)) +
        theme_bw() +
        theme(text = element_text(size = 20),
              strip.background = element_blank())
    } else {
      SIE_plot <-
        ggplot(data = current, aes(
          x = time,
          y = Number,
          group = mu,
          color = mu
        )) +
        ylab("Number of infectious hosts") +
        xlab("Time") + geom_line(size = 1, color = "red") +
        ylim(min(previous$Number, current$Number),
             max(previous$Number, current$Number)) +
        theme_bw() +
        theme(text = element_text(size = 20),
              strip.background = element_blank())
      rv$reset <- FALSE
    }
    
    return(SIE_plot)
  })
  
  output$data <- DT::renderDataTable({
    rv_dt <- update_dt(rv_dt, input)
    table <- get_changed(rv_dt)
    DT::datatable(table)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
