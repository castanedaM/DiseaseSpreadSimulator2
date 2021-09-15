# CWD Mathematical Model
# Data set-up for Shiny App
# Date created: 09/06/2021
# Data last updated: 09/14/2021

# Created by: David Paez


# Packages
if(!require(ggplot2)) install.packages(ggplot2)
if(!require(tidyverse)) install.packages(tidyverse)
if(!require(deSolve)) install.packages(deSolve)


# Model Specification (user defined formula) ------------------------------

SIEmod <- function(times, State, Pars) {
   
   with(as.list(c(State, Pars)), {
      
      dS <-   alpha - S * (beta * I + gamma * E + m)
      dI <-   S * (beta * I + gamma * E) - I * (m + mu)
      dE <-   epsilon * I - tau * E
      
      return(list(c(dS, dI, dE)))
   })
}



# Defining Parameter Values -----------------------------------------------

alpha <- 4.48307

# Generating 25 beta values from a lognormal with mean equal to estimated value
beta_vec <- rlnorm(25, log(0.002446), 2) 

# Generating 25 mu values from a lognormal with mean equal to estimated value
mu_vec <-   rlnorm(25, log(2.617254), 0.5) 

m <- 0.103202
gamma <- 0.206146
epsilon <- 0.150344
tau <- 0.135785
S0 <- 180

# Associate every value in beta_vec with every value in mu_vec (combination)
beta_mu_vals <- expand.grid(beta_vec, mu_vec)
names(beta_mu_vals) <- c("beta", "mu")


# Output Storage ----------------------------------------------------------

# Initiate an empty data frame with 11 columns (one column for time,
# compartment, number and one for each of the parameters) and 0 rows
Output <- data.frame(matrix(ncol = 11, nrow = 0))

# Can also create an empty one
Output <- data.frame()
   

# Loop to supply Ouptut varibale with information
for(p in 1:nrow(beta_mu_vals)){
   
   # Defining parameter values (create a vector of parameters)
   parsSIE <- c(alpha = alpha,
                m = m, 
                gamma = gamma,
                epsilon = epsilon,
                tau = tau,
                S0 = S0,
                beta = beta_mu_vals[p, 1],
                mu =   beta_mu_vals[p, 2])
 
   # time scale
   times <- seq(0, 20, 1)

   # Solution of diff eqn model
   out <- as.data.frame(ode(func = SIEmod, y = c(S = S0, I = 0 ,  E =0.0000003),  
                            parms = parsSIE, times = times))

   names(out)[2:4] <- c("Susceptible", "Infectious", "Virus")
   
   # Converting to long format for plots
   outL <- out %>% 
      gather(key = Compartment, value = Number, -time) %>% 
      data.frame()
   
   outL$Compartment <- factor(outL$Compartment, levels = c("Susceptible", "Infectious", "Virus"))
   
   outL <- bind_cols(outL, data.frame(t(matrix(parsSIE))))
   head(outL)
   names(outL)[4:ncol(outL)] <- c("alpha", "m", "gamma", "epsilon",
                                  "tau", "S0", "beta", "mu")
   
   Output <- rbind(Output, outL)
 }




# Plot (focusing only on infections groups) -------------------------------

SIE_plot <- Output  %>%  filter(Compartment== "Infectious") %>%
   mutate(beta = as.character(round(beta, 6)),
          mu = as.character(round(mu, 3))) %>%
   ggplot(aes(x = time, y = Number, group = mu, color = mu)) +
   ylab("Number of infectious hosts") + 
   xlab("Time") + geom_line(size = 1) + facet_wrap(~ beta) +
   theme_bw() + theme(text = element_text(size = 20), 
                      strip.background = element_blank())
SIE_plot




