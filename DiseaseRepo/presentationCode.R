rm(list=ls())


require(ggplot2)
require(tigris)
require(sf)
require(dplyr)
require(tibble)
library(ggplot2)
library(dplyr)


#setwd()

deer_data <-read.csv('./Master_CWD_Data_EVI.csv')


table(deer_data$status)

deer_matrix = data.matrix(deer_data)

# matrices for numerical summary
table(deer_data$county)


c_matrix = unique(matrix(deer_data[, "County"]))




county <- c(unique(as.character(deer_data[,"County"])))

cases <- vector("numeric", length = length(county))

total <- vector("numeric", length = length(county))

meanEVI <- numeric(6)

countEVI <- numeric(6)


# currently set row names to county, alternatively could have county as 
# another column, but i found this easier
df <- data.frame(cases, total, meanEVI, countEVI)
row.names(df) <- county



years <- length(unique((deer_data[,"Year"])))
yearly <- array(0, dim = c(length(county), 3, years))


# Functionality here depends on column names.
for (i in 1:nrow(deer_data)) {
  df[as.character(deer_data[i,"County"]),2] = df[as.character(deer_data[i,"County"]),2] + 1
  if (!(is.na(deer_data[i,"Mean_EVI"]))) {
    
    df[as.character(deer_data[i,"County"]),3] = df[as.character(deer_data[i,"County"]),3] + deer_data[i,"Mean_EVI"]
    df[as.character(deer_data[i,"County"]),4] = df[as.character(deer_data[i,"County"]),4] + 1
  }
  if (deer_data[i,"status"] == 1) {
    df[as.character(deer_data[i,"County"]),1] = df[as.character(deer_data[i,"County"]),1] + 1
  }
}


#for 3d, compare to county var to decide placement

# division for mean evi
for (i in 1:nrow(df)) {
  if (df[i, 4] > 1) {
    df[i, 3] = (df[i, 3]) / df[i, 4]
  }
}

# StateChoropleth

deer_frame <- data.frame(county, cases)


virginia <- tigris::counties(state = 'VA', cb = T, class = 'sf')


#ideally do this in a loop or something, so each row is entered if more are added

# Number of infections
infData <- tribble(
  ~County, ~State, ~Infections,
  county[1], 'Virginia', df[1,1],
  county[2], 'Virginia', df[2,1],
  county[3], 'Virginia', df[3,1],
  county[4],  'Virginia', df[4,1],
  county[5],  'Virginia', df[5,1],
  county[6], 'Virginia', df[6,1],
)


chart1 <- virginia %>% 
  left_join(infData, by = c("NAME" = "County"))


ggplot(chart1) +
  geom_sf(aes(fill = Infections))


# Ratio of infected deer

ratios <- c(df[1,1]/df[1,2], df[2,1]/df[2,2], df[3,1]/df[3,2], df[4,1]/df[4,2], df[5,1]/df[5,2], df[6,1]/df[6,2])

ratioData <- tribble(
  ~County, ~State, ~Infected,
  county[1], 'Virginia', ratios[1],
  county[2], 'Virginia', ratios[2],
  county[3], 'Virginia', ratios[3],
  county[4],  'Virginia', ratios[4],
  county[5],  'Virginia', ratios[5],
  county[6], 'Virginia', ratios[6]
)



chart2 <- virginia %>% 
  left_join(ratioData, by = c("NAME" = "County"))


# Will likely change this graph so that 0% and 100% infection rates are different colors to make others more distinguished
ggplot(chart2) +
  geom_sf(aes(fill = Infected))



refinedRatioData <- tribble(
  ~County, ~State, ~Infected,
  county[1], 'Virginia', ratios[1],
  county[2], 'Virginia', ratios[2],
  county[3], 'Virginia', ratios[3],
  county[4],  'Virginia', ratios[4]
)

chart2A <- virginia %>% 
  left_join(refinedRatioData, by = c("NAME" = "County"))


# Will likely change this graph so that 0% and 100% infection rates are different colors to make others more distinguished
ggplot(chart2A) +
  geom_sf(aes(fill = Infected))




ratioData <- tribble(
  ~County, ~State, ~Infected, ~CountyFP,
  county[1], 'Virginia', ratios[1], "000",
  county[2], 'Virginia', ratios[2], "000",
  county[3], 'Virginia', ratios[3], "000",
  county[4],  'Virginia', ratios[4], "000",
  county[5],  'Virginia', ratios[5], "000",
  county[6], 'Virginia', ratios[6], "000",
)



## added this due to issues with Frederick County and Frederick City
if ("Frederick" %in% county) {
  county[match("Frederick", county)] = "Frederick County"
}
row.names(df) <- county
##


for (i in 1:nrow(ratioData)) {
  tempString = lookup_code("VA", ratioData[i, 1])
  ratioData[i, 4] = str_sub(tempString, str_length(tempString)-4, str_length(tempString)-2)
}


vir <- county_subdivisions('Virginia', c('Frederick County', 'Shenandoah','Clarke', 'Warren', 'Culpeper', 'Fauquier'))
##plot(vir$geometry)




#chart3 <- vir %>% 
#  left_join(ratioData, by = c("COUNTYFP" = "CountyFP"))

# temporarily removing bc 100%'s (and 0%'s)?

#ggplot(chart3) +
#  geom_sf(aes(fill = Infected))



fipsAndRatios <- data.frame(ratioData[1:4, 4], ratioData[1:4, 3])

chart4 <- vir %>% 
  left_join(fipsAndRatios, by = c("COUNTYFP" = "CountyFP"))

# maybe take out the 0
oneDeerCtys <- data.frame(ratioData[5:6, 4], ratioData[5:6, 3])

ggplot(chart4) +
  geom_sf(aes(fill = Infected))








## testing crap

# fire example
fire1RatioData <- tribble(
  ~County, ~State, ~Infected, ~CountyFP,
  county[1], 'Virginia', (ratios[1] + ratios[1]*0.05), "069",
  county[2], 'Virginia', (ratios[2] + ratios[2]*0.03), "171",
  county[3], 'Virginia', (ratios[3] + ratios[3]*0.0499), "043",
  county[4],  'Virginia', (0.7/df[4,2] + 0.7/df[4,2]*0.0299), "187",
  county[5],  'Virginia', (0.9/df[4,2] + 0.9/df[4,2]*0.04), "047",
  county[6], 'Virginia', (0.5/df[4,2] + 0.5/df[4,2]*0.015), "061",
  
  
  "RAPPAHANNOCK", 'Virginia', (0.6/df[4,2] + 0.6/df[4,2]*0.03), "157",
  "PAGE", 'Virginia', (ratios[2]*0.6), "139"
)

virg <- county_subdivisions('Virginia', c('Frederick County', 'Shenandoah','Clarke', 'Warren', 'Culpeper', 'Fauquier', 'Rappahannock', 'Page'))
chartFireEx1 <- virg %>% 
  left_join(fire1RatioData, by = c("COUNTYFP" = "CountyFP"))



ggplot(chartFireEx1) +
  geom_sf(aes(fill = Infected))






#culling example


cull1RatioData <- tribble(
  ~County, ~State, ~Infected, ~CountyFP,
  county[1], 'Virginia', (ratios[1] - ratios[1]*0.05), "069",
  county[2], 'Virginia', (ratios[2] - ratios[2]*0.03), "171",
  county[3], 'Virginia', (ratios[3] - ratios[3]*0.0499), "043",
  county[4],  'Virginia', (0.7/df[4,2] - 0.7/df[4,2]*0.0299), "187",
  county[5],  'Virginia', (0.9/df[4,2] - 0.9/df[4,2]*0.04), "047",
  county[6], 'Virginia', (0.5/df[4,2] - 0.5/df[4,2]*0.015), "061",
)

chartCullEx1 <- vir %>% 
  left_join(cull1RatioData, by = c("COUNTYFP" = "CountyFP"))



ggplot(chartCullEx1) +
  geom_sf(aes(fill = Infected))


##





#============== UI ==============#

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Disease Spread Simulator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      #radioButtons("dist", "Distribution type:",
      #c("Normal" = "norm",
      #"Uniform" = "unif",
      #"Log-normal" = "lnorm",
      #"Exponential" = "exp")),
      selectInput("Map view", "Select counties:", choices = c("All", "Only infected counties")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("pd",
                  "Population Density:",
                  value = 0.5,
                  min = 0,
                  max = 1),
      sliderInput("a",
                  "Average Age:",
                  value = 0.5,
                  min = 0,
                  max = 1),
      sliderInput("v",
                  "Vegetation:",
                  value = 0.5,
                  min = 0,
                  max = 1),
      actionButton("start", "Start"),
      actionButton("stop", "Stop")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Predictions - Heatmap", plotOutput("map")),
                  #tabPanel("Current Data", verbatimTextOutput("graph")),
                  tabPanel("Current Data", plotOutput("graph")),
                  tabPanel("Chart", tableOutput("chart"))
      )
      
    )
  )
)

#============== Server ==============#

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    #county <- switch(input$county,
    #               norm = rnorm,
    #               unif = runif,
    #               lnorm = rlnorm,
    #               exp = rexp,
    #               rnorm)
    
    #county(input$n)
  })
  
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  #output$map <- renderPlot({
  #  ggplot(chartFireEx1) +
  #    geom_sf(aes(fill = Infected))
    #  county <- input$county
    #  n <- input$n
    
    #hist(d(),
    #     main = paste("r", dist, "(", n, ")", sep = ""),
    #     col = "#75AADB", border = "white")
    #})
    
    output$graph <- renderPlot({
    ggplot(chart2) +
      geom_sf(aes(fill = Infected))
    #This is the graph used for current data in presentation
    # if we want to keep this part this way: change sliders to options for
    # how to display the data? (heatmap, plotted dots, or total)
    
    
    # Generate a summary of the data ----
    #output$summary <- renderPrint({
    #  summary(d())
  })
  
  ## Generate an HTML table view of the data ----
  #output$table <- renderTable({
  #  d()
  #})
  
}

shinyApp(ui, server)