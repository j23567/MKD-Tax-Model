
" Dashboard-VAT metrics"

# I.Function for Dashboard ------------------------------------------------------------------
VAT_aggregation_revenues_fun <- function(forecast_combined_agg,forecast_combined_cpa_selected,
                                     SimulationYear) {
  
# Chart 1. VAT Revenue Forecast (LCU MIL) -----------------------------------------------------------------
           # Define custom colors
          custom_colors <- c('#1f77b4', '#ff7f0e')
          
          
          VAT_revenues_tbl<-forecast_combined_agg%>%
            filter(Descriptions=='Calibrated VAT')
          
          VAT_revenues_tbl$year<-as.factor(VAT_revenues_tbl$year)
          
          # Create the plot with a dotted line for "Simulation" and fewer markers
          vat_revenue_plt <- plot_ly() %>%  # Start with an empty plot
                                add_trace(
                                  data = subset(VAT_revenues_tbl, scenario == "Baseline"),
                                  x = ~year,
                                  y = ~round(value * 1e06, 1),
                                  name = "Baseline",
                                  type = 'scatter',
                                  mode = 'lines', # Keep markers but reduce visibility
                                  hoverinfo = 'text+y',   # Keep hover info
                                  line = list(color = custom_colors[1], width = 4, dash = "solid")
                                ) %>%
                                add_trace(
                                  data = subset(VAT_revenues_tbl, scenario == "Simulation"),
                                  x = ~year,
                                  y = ~round(value * 1e06, 1),
                                  name = "Simulation",
                                  type = 'scatter',
                                  mode = 'lines', # Keep markers for hover points
                                  hoverinfo = 'text+y',   # Keep hover info
                                  line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
                                ) %>%
                                layout(
                                  title = paste("VAT Revenue Forecast (LCU MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
                                  xaxis = list(title = '', tickformat = 'd'),
                                  yaxis = list(title = ' ', rangemode = 'tozero'),
                                  annotations = list(
                                    x = -0.02,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left'
                                  )
                                )
          
          
          # Render the plot
         # vat_nace_hh_plt
  
  
  # Chart 2. Aggregate VAT by CPA sectors  -----------------------------------------------------------------
  
            vat_nace_total_plt <- plot_ly(forecast_combined_cpa_selected) %>%
                              add_trace(
                                x = ~PRODUCT_INDUSTRY_CODE, 
                                y = ~Total_VAT, 
                                type = 'bar', 
                                name = ~scenario, # Use the Scenario column to label traces
                                text = "", # Do not display text inside bars
                                hoverinfo = 'text', # Show hover text
                                hovertext = ~paste(PRODUCT_INDUSTRY_NAME, 
                                                   "<br>VAT Value:", round(Total_VAT, 1) # Round the values to 1 decimal place
                                ), 
                                showlegend = TRUE
                              ) %>%
                              layout(
                                title = list(
                                  text = paste("Breakdown of VAT by Sector,", SimulationYear), 
                                  x = 0.5, # Center the title
                                  y = 0.95, # Adjust vertical position (optional)
                                  font = list(size = 14) # Adjust font size
                                ),
                                xaxis = list(
                                  title = '', 
                                  tickangle = -90, # Tilt x-axis labels for better readability
                                  tickfont = list(size = 11) # Set smaller font size for x-axis labels
                                ), 
                                yaxis = list(title = ''),
                                legend = list(x = 0.9, y = 0.99),
                                barmode = 'group', # Group the bars by Scenario
                                annotations = list(
                                  list(
                                    x = -0.05,
                                    y = -0.13,
                                    text = "Source: WB staff estimation", # Add your source text
                                    showarrow = FALSE, # Ensure no arrow is shown
                                    xref = "paper",    # Position relative to the chart
                                    yref = "paper",    
                                    xanchor = "left",  # Align the text to the left
                                    yanchor = "top",   
                                    font = list(size = 8) # Adjust font size for annotation
                                  )
                                )
                              )
            

 
  
  # Chart 3.Breakdown of VAT Revenues by Sector, Generated by Households-------------------------------------------------------------------------
  
          # Define custom colors
         # custom_colors <- c('#1f77b4', '#ff7f0e')
          
          VAT_TE_tbl<-forecast_combined_agg%>%
            filter(Descriptions=='Policy Gap')
          
          VAT_TE_tbl$year<-as.factor(VAT_TE_tbl$year)
          
          # Create the plot with a dotted line for "Simulation" and fewer markers
          vat_TE_plt <- plot_ly() %>%  # Start with an empty plot
                        add_trace(
                          data = subset(VAT_TE_tbl, scenario == "Baseline"),
                          x = ~year,
                          y = ~round(value * 1e06, 1),
                          name = "Baseline",
                          type = 'scatter',
                          mode = 'lines', # Keep markers but reduce visibility
                          hoverinfo = 'text+y',   # Keep hover info
                          line = list(color = custom_colors[1], width = 4, dash = "solid")
                        ) %>%
                        add_trace(
                          data = subset(VAT_TE_tbl, scenario == "Simulation"),
                          x = ~year,
                          y = ~round(value * 1e06, 1),
                          name = "Simulation",
                          type = 'scatter',
                          mode = 'lines', # Keep markers for hover points
                          hoverinfo = 'text+y',   # Keep hover info
                          line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
                        ) %>%
                        layout(
                          title = paste("VAT Tax Expenditures (LCU MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
                          xaxis = list(title = '', tickformat = 'd'),
                          yaxis = list(title = ' ', rangemode = 'tozero'),
                          annotations = list(
                            x = -0.02,
                            y = -0.1,
                            text = "Source: WB staff estimation",
                            showarrow = FALSE,
                            xref = 'paper',
                            yref = 'paper',
                            align = 'left'
                          )
                        )
                      
 
  # Chart 4. Breakdown of VAT by Sector ---------------------------------------------------------------
  
  
          
          VAT_Com_Gap_tbl<-forecast_combined_agg%>%
            filter(Descriptions=='Compliance Gap')
          
          VAT_Com_Gap_tbl$year<-as.factor(VAT_Com_Gap_tbl$year)
          
          # Create the plot with a dotted line for "Simulation" and fewer markers
          vat_Com_Gap_plt <- plot_ly() %>%  # Start with an empty plot
            add_trace(
              data = subset(VAT_TE_tbl, scenario == "Baseline"),
              x = ~year,
              y = ~round(value * 1e06, 1),
              name = "Baseline",
              type = 'scatter',
              mode = 'lines', # Keep markers but reduce visibility
              hoverinfo = 'text+y',   # Keep hover info
              line = list(color = custom_colors[1], width = 4, dash = "solid")
            ) %>%
            add_trace(
              data = subset(VAT_TE_tbl, scenario == "Simulation"),
              x = ~year,
              y = ~round(value * 1e06, 1),
              name = "Simulation",
              type = 'scatter',
              mode = 'lines', # Keep markers for hover points
              hoverinfo = 'text+y',   # Keep hover info
              line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
            ) %>%
            layout(
              title = paste("VAT Compliance Gap (LCU MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
              xaxis = list(title = '', tickformat = 'd'),
              yaxis = list(title = ' ', rangemode = 'tozero'),
              annotations = list(
                x = -0.02,
                y = -0.1,
                text = "Source: WB staff estimation",
                showarrow = FALSE,
                xref = 'paper',
                yref = 'paper',
                align = 'left'
              )
            )
  
  
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    vat_revenue_plt=vat_revenue_plt,
    vat_nace_total_plt=vat_nace_total_plt,
    vat_TE_plt=vat_TE_plt,
    vat_Com_Gap_plt=vat_Com_Gap_plt
    
  )
}


