# Define the function
calculate_vat_portions <- function(df, vat_rate_0, vat_rate_1, vat_rate_2) {
  # Initialize new columns
  df$proportion_vat_rate_0 <- 0
  df$proportion_vat_rate_1 <- 0
  df$proportion_vat_rate_2 <- 0
  
  # Calculate the portions for each row
  for (i in 1:nrow(df)) {
    consumption <- df$Consumption[i]
    vat_paid <- df$VAT[i]
    
    # Calculate the Effective Tax Rate (ETR)
    etr <- vat_paid / consumption
    
    # Check if VAT is zero
    if (vat_paid == 0) {
      df$proportion_vat_rate_0[i] <- 100
      df$proportion_vat_rate_1[i] <- 0
      df$proportion_vat_rate_2[i] <- 0
    } else if (etr < vat_rate_1) {
      # If ETR is below vat_rate_1, it means there is a portion taxed at 0%
      # Define the system of equations
      # x + y + z = consumption
      # vat_rate_0 * x + vat_rate_1 * y + vat_rate_2 * z = vat_paid
      
      # Coefficients matrix
      A <- matrix(c(1, 1, 1, vat_rate_0, vat_rate_1, vat_rate_2), nrow = 2, byrow = TRUE)
      
      # Constants vector
      B <- c(consumption, vat_paid)
      
      # Solve the system of equations using least squares
      solution <- lm(B ~ A - 1)$coefficients
      
      # Extract the portions
      portion_x <- solution[1]
      portion_y <- solution[2]
      portion_z <- solution[3]
      
      # Calculate the proportions as percentages
      df$proportion_vat_rate_0[i] <- (portion_x / consumption) * 100
      df$proportion_vat_rate_1[i] <- (portion_y / consumption) * 100
      df$proportion_vat_rate_2[i] <- (portion_z / consumption) * 100
    } else {
      # Define the system of equations
      # x + y = consumption
      # vat_rate_1 * x + vat_rate_2 * y = vat_paid
      
      # Coefficients matrix
      A <- matrix(c(1, 1, vat_rate_1, vat_rate_2), nrow = 2, byrow = TRUE)
      
      # Constants vector
      B <- c(consumption, vat_paid)
      
      # Solve the system of equations
      solution <- solve(A, B)
      
      # Extract the portions
      portion_x <- solution[1]
      portion_y <- solution[2]
      
      # Calculate the proportions as percentages
      df$proportion_vat_rate_0[i] <- 0
      df$proportion_vat_rate_1[i] <- (portion_x / consumption) * 100
      df$proportion_vat_rate_2[i] <- (portion_y / consumption) * 100
    }
  }
  
  return(df)
}

# Example usage
# Create a sample data frame
df <- data.frame(
  Description = c("Item1", "Item2", "Item3", "Item4"),
  Consumption = c(15000, 20000, 5000, 10000),
  VAT = c(750, 3600, 250, 0)
)

# Define VAT rates
vat_rate_0 <- 0.00
vat_rate_1 <- 0.05
vat_rate_2 <- 0.18

# Calculate the portions and update the data frame
df <- calculate_vat_portions(df, vat_rate_0, vat_rate_1, vat_rate_2)

# Print the updated data frame
print(df)




# Calculate the portions and update the data frame

#
df_test <- read_excel("Data/VAT/Calculation_R_proportions/df_test.xlsx", 
                      sheet = "df_vat_test")



df_test <- calculate_vat_portions(df_test, vat_rate_0, vat_rate_1, vat_rate_2)



View(df_test)
