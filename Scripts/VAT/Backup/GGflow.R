library(ggflowchart)

data <- tibble::tibble(from = c("A", "A", "A", "B", "C", "F"),
                       to = c("B", "C", "D", "E", "F", "G"))


ggflowchart(data)




library(ggflowchart)
library(tibble)

# Create the tibble with multi-line text, ensuring both vectors have the same length
flow_charts <- tibble(
  from = c("CPA_PRODUCTS_0", "CPA_PRODUCTS", "CPA_PRODUCTS", "CPA_PRODUCTS_1", "CPA_PRODUCTS_2"),
  to = c("Calibration factor\nSIMULATION_0", "Benchmark revenues\n", "Main estimation\nSIMULATION_0", "TE\nSIMULATION_1", "Effective VAT RATE\nSIMULATION_2")
)

# Generate the flowchart
ggflowchart(flow_charts)


# Vo delot za TE ima dve tabeli SIMULATION_0 I SIMULATION_1