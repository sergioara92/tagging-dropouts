#=============================================================================#
#   Titutlo: Aleatorizacion de la estrategia de permanencia - SED
#   Contenido:   Este archivo toma los datos entregados por la SED, y realiza 
#                una aleatorizacion a nivel de individuos, estratificada por
#                bloques de riesgo construidos a partir de los percentiles 
#                del IRD
#=============================================================================#


rm(list = ls())
setwd("C:/Users/sergi/Dropbox (Personal)/Phding/Research papers/Dropouts")
data <- read.csv("Población IRD alto 2025_Colegios Focalizados.csv")

install.packages("randomizr")
library(randomizr)
library(dplyr)

data <- data %>%
  mutate(IRD_acp_decile = ntile(IRD_acp, 10))

# Random seed for reproducibility
set.seed(1234)

# Function for flexible blocked random assignment
assign_treatment <- function(data, block_var, treat_prob = 0.8, 
                             treat_label = "Treatment", control_label = "Control") {
  
  # Treatment assignment vector
  treatment_vector <- block_ra(
    blocks = data[[block_var]],
    prob = treat_prob,
    conditions = c(control_label, treat_label)
  )
  
  # Attach to the original data
  data$treatment <- treatment_vector
  return(data)
}

# Apply the function (80% to Treatment)
data <- assign_treatment(data, block_var = "IRD_acp_decile", treat_prob = 0.8)

# Checking proportions within each block 
prop.table(table(data$IRD_acp_decile, data$treatment), 1)

# Saving file
write.csv(data, "randomized_data_SED.csv", row.names = FALSE)
