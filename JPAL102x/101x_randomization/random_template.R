##############################################
#   Template code for randomization using 
#   J-PAL's 101x imaginary data and 
#   creating a plot and summary table. 
#
#
##############################################

# Clean up workspace
rm(list = ls()) 

# Load needed libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("randomizr")
library(randomizr)

# Set directory to the folder where the data is stored
setwd("[SET DIRECTORY HERE")

# Read in data
IData <-read.csv('ImaginaryData.csv')

###################################################################
#         Baseline Test Score Scatter Plot Functions              #
###################################################################
# This section includes functions for creating a scatterplot for 
# designs with 2, 3, or 4 total arms (including the comparison group).

plot_2_arms <- function(data) {
  ggplot(data, aes(x = Baseline.Literacy, y = Baseline.Math, color = as.factor(Treat))) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("0" = "#2FAA9F75", "1" = "#E3592575"),
      labels = c("0" = "Comparison Group", "1" = "Treatment Group")
    ) +
    labs(
      x = "1st Grade Literacy Score", 
      y = "1st Grade Math Score", 
      subtitle = paste0(input$Random_type, " randomization"),
      title = "Math vs Literacy Scores", 
      color = "Group"
    )
}  




plot_3_arms <- function(data) {
  # Ensure Treat is a factor with the correct levels (Control + 3 Treatment Groups)
  data$Treat <- factor(data$Treat, levels = c(0, 1, 2, 3))
  
  ggplot(data, aes(x = Baseline.Literacy, y = Baseline.Math, color = as.factor(Treat))) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("0" = "#2FAA9F75", "1" = "#E3592575", "2" = "#FBC02D75", "3" = "#D32F2F75"),
      labels = c("0" = "Control Group", "1" = "Treatment Group 1", "2" = "Treatment Group 2", "3" = "Treatment Group 3")
    ) +
    labs(
      x = "1st Grade Literacy Score", 
      y = "1st Grade Math Score", 
      subtitle = paste0(input$Random_type, " randomization"),
      title = "Math vs Literacy Scores", 
      color = "Group"
    ) 
}

plot_4_arms <- function(data) {
  
  ggplot(data, aes(x = Baseline.Literacy, y = Baseline.Math, color = as.factor(Treat))) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("0" = "#2FAA9F75", "1" = "#E3592575", "2" = "#FBC02D75", "3" = "#D32F2F75", "4" = "#9575CD75"),
      labels = c("0" = "Control Group", "1" = "Treatment Group 1", 
                 "2" = "Treatment Group 2", "3" = "Treatment Group 3", "4" = "Treatment Group 4")
    ) +
    labs(
      x = "1st Grade Literacy Score", 
      y = "1st Grade Math Score", 
      subtitle = paste0(input$Random_type, " randomization"),
      title = "Math vs Literacy Scores", 
      color = "Group"
    )
}


###################################################################
#                     Summary Table Function                      #
###################################################################
# This section include a function for creating a summary table for 
# designs with 2, 3, or 4 total arms (including the comparison group).
create_table <- function(data) {
 
   # Make sure Treat is a factor with all levels, including possible extra treatment arms
  data$Treat <- as.factor(data$Treat)
  
  # Define the number of arms dynamically based on the levels in the 'Treat' column
  num_arms <- length(levels(data$Treat)) - 1  # Subtract 1 for the control group
  
  # Create summary table grouped by treatment
  summary_table <- data %>%
    group_by(Treat) %>%
    summarise(
      N = n(),  # Count the number of observations in each group
      Female = paste0(round(100 * mean(Gender == 0, na.rm = TRUE), 0), "%"),
      Avg_Literacy = round(mean(Baseline.Literacy, na.rm = TRUE), 2),
      Avg_Math = round(mean(Baseline.Math, na.rm = TRUE), 2)
    ) %>%
    ungroup() %>%
    
    # Recode treatment groups based on dynamic number of arms
    mutate(
      Group = case_when(
        Treat == "0" ~ "Control Group",
        Treat == "1" ~ "Treatment Group 1",
        Treat == "2" ~ "Treatment Group 2",
        Treat == "3" ~ ifelse(num_arms >= 3, "Treatment Group 3", NA_character_),
        Treat == "4" ~ ifelse(num_arms >= 4, "Treatment Group 4", NA_character_)
      )
    ) %>%
    
    # Filter out any rows with NA group labels (which won't exist if there are fewer arms)
    filter(!is.na(Group)) %>%
    select(Group, N, Male, Female, Avg_Literacy, Avg_Math)  # Select relevant columns including N
  
  return(summary_table)
}

###########################################################################################
#                         RANDOMIZATION CODE HERE                                         #
###########################################################################################
# This section includes sample code for simple and complete randomization, with and 
# without clustering, and for more than 2 treatment arms. 

# --------------- Simple Randomization --------------- #
#count the number of rows
N <- nrow(IData)
IData$Treat <- rep(0, N)

# Set seed for reproducibility 
set.seed(123) 

# Simple randomization for 2 arms
IData$Treat <- simple_ra(N = N)

# Create plot for 2 arms
plot_2_arms(IData)

# Run table 
create_table(IData)


# --------------- Complete Randomization --------------- #
#count the number of rows
N <- nrow(IData)
IData$Treat <- rep(0, N)

# Set seed for reproducibility 
set.seed(123) 

# Simple randomization for 2 arms
IData$Treat <- complete_ra(N = N)

# Create plot for 2 arms
plot_2_arms(IData)

# Run table 
create_table(IData)

# --------------- Simple, Clustered Randomization --------------- #

  
  N <- nrow(IData)
  IData$Treat <- rep(0, N)

  set.seed(123) 

  # Clustered by school -   2 arms
  IData$Treat <- cluster_ra(clusters = data$School.ID, simple = TRUE)

  # Create plot for 2 arms
  plot_2_arms(IData)

  create_table(IData)

# --------------- Complete, Clustered Randomization --------------- #
  
  
  N <- nrow(IData)
  IData$Treat <- rep(0, N)
  
  set.seed(123) 
  
  # Clustered by school -   2 arms
  IData$Treat <- cluster_ra(clusters = data$School.ID) # Assign cluster variable
  
  # Create plot for 2 arms
  plot_2_arms(IData)
  
  create_table(IData)
  
  
 # ----------- More than 2 arms (total, including the comparison group) ----- #
 # Note: to add additional arms to your design, simple add the "num_arms" argument 
 # to the randomization function and specify the total number of arms you want 
 # your study to have (including the comparison group). 
  
  N <- nrow(IData)
  IData$Treat <- rep(0, N)
  
  set.seed(123) 
  
  # Clustered by school - 3 arms total (2 treatment, 1 comparison)
  IData$Treat <- cluster_ra(clusters = data$School.ID, num_arms = 3, simple = TRUE)
  
  # Create plot for 3 arms (change the plot function to match the number of arms - this is already done below)
  plot_3_arms(IData)
  
  create_table(IData)
