
" Dashboard1-VAT metrics"

# I.Function for Dashboard ------------------------------------------------------------------
Effective_VAT_Rates_fun <- function(vat_effective_combined,
                                     COICOP_DecileGroups_combined,
                                     COICOP_HBS_BU,
                                     COICOP_HBS_SIM,simulation_start_year_VAT) {
  
  # Chart 1. Effective VAT Rate by Percentile Groups -----------------------------------------------------------------
 
  EffectiveVAT_Percentile_HH_plt <- plot_ly(
    vat_effective_combined,
    x = ~centiles,
    y = ~effective_vat_before_reform * 100,
    name = "Before reform",
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "solid", width = 5, color = "royalblue") # Set color to royalblue
  )

  EffectiveVAT_Percentile_HH_plt <- EffectiveVAT_Percentile_HH_plt %>%
                    add_trace(
                      y = ~effective_vat_after_reform * 100,
                      name = "After reform",
                      line = list(dash = "dash", width = 5, color = "red") # Set color to red
                    ) %>%
                    layout(
                      title = list(
                        text = "Effective VAT Rate by Percentile Groups", # Add chart title
                        font = list(size = 14) # Set font size for the title
                      ),
                      xaxis = list(
                        title = 'Percentiles',
                        titlefont = list(size = 10), # Increased font size for x-axis title
                        tickfont = list(size = 10) # Increased font size for x-axis ticks
                      ),
                      yaxis = list(
                        title = 'In Percent',
                        titlefont = list(size = 10), # Increased font size for y-axis title
                        tickfont = list(size = 10) # Increased font size for y-axis ticks
                      ),
                      legend = list(
                        x = 0.01,
                        y = 0.99,
                        font = list(size = 10) # Increased font size for legend
                      ),
                      annotations = list(
                        list(
                          x = -0.05,
                          y = -0.08, # Moved source annotation higher for better readability
                          text = "Source: WB staff estimation", # Source annotation text
                          showarrow = FALSE, # Ensure no arrow is shown
                          xref = "paper",    # Position relative to the chart
                          yref = "paper",
                          xanchor = "left",  # Align the text to the left
                          yanchor = "top",
                          font = list(size = 8) # Increased font size for the annotation
                        )
                      )
                    )

  
  

  
  # Chart 2. Comparison of VAT Revenues by Decile Groups  -----------------------------------------------------------------
 
    COICOP_DecileGroups_plt <- plot_ly(
                                  COICOP_DecileGroups_combined, 
                                  x = ~list(deciles, group), 
                                  y = ~value, 
                                  color = ~variable, 
                                  colors = ~as.character(color), 
                                  type = "bar"
                                ) %>% 
                                  layout(
                                    title = list(
                                      text = paste("Comparison of VAT Revenues by Decile Groups,", simulation_start_year_VAT), 
                                      x = 0.2, # Center the title
                                      y = 0.97, # Adjust vertical position (optional)
                                      font = list(size = 14) # Adjust font size
                                    ),
                                    barmode = "stack",
                                    xaxis = list(
                                      title = '',
                                      tickfont = list(size = 9) # Small font for x-axis
                                    ),
                                    yaxis = list(
                                      title = '',
                                      tickfont = list(size = 9) # Small font for y-axis
                                    ),
                                    legend = list(
                                      x = 1.02, # Move legend to the right outside the chart
                                      y = 1,    # Align legend to the top
                                      font = list(size = 8), # Small font for legend
                                      orientation = "v" # Vertical orientation for legend items
                                    ),
                                    annotations = list(
                                      list(
                                        x = -0.05,
                                        y = -0.33, # Position source text lower
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
  
                  
  # Chart 3.Structure of VAT Revenues from Households by COICOP (Before Reform)-------------------------------------------------------------------------

  COICOP_BeforeReform_plt <- plot_ly(
                                      data = COICOP_HBS_BU, 
                                      labels = ~variable, 
                                      values = ~value, 
                                      type = "pie",
                                      marker = list(colors = ~colr), 
                                      hole = 0.6
                                    ) %>% 
                                      layout(
                                        title = list(
                                          text = "Structure of VAT Revenues from Households by COICOP (Before Reform)", # Add the desired title
                                          font = list(size = 14) # Set font size for the title
                                        ),
                                        showlegend = TRUE,
                                        legend = list(
                                          font = list(size = 8) # Set font size for the legend
                                        ),
                                        xaxis = list(
                                          showgrid = FALSE, 
                                          zeroline = FALSE, 
                                          showticklabels = FALSE
                                        ),
                                        yaxis = list(
                                          showgrid = FALSE, 
                                          zeroline = FALSE, 
                                          showticklabels = FALSE
                                        ),
                                        annotations = list(
                                          list(
                                            x = 0, 
                                            y = -0.1,
                                            text = "Source: WB staff estimation", # Add your source text
                                            showarrow = FALSE, 
                                            xref = 'paper',
                                            yref = 'paper',
                                            font = list(size = 8) # Set font size for the annotation
                                          )
                                        )
                                      )
                                    
                          
  # Chart 4. Structure of VAT Revenues from Households by COICOP (After Reform) ---------------------------------------------------------------
 
  COICOP_AfterReform_plt <- plot_ly(
                                    data = COICOP_HBS_SIM, 
                                    labels = ~variable, 
                                    values = ~value, 
                                    type = "pie",
                                    marker = list(colors = ~colr), 
                                    hole = 0.6) %>% 
                              layout(
                                title = list(
                                  text = "Structure of VAT Revenues from Households by COICOP (After Reform)", # Add the desired title
                                  font = list(size = 14) # Set font size for the title
                                ),
                                showlegend = TRUE,
                                legend = list(
                                  font = list(size = 8) # Set font size for the legend
                                ),
                                xaxis = list(
                                  showgrid = FALSE, 
                                  zeroline = FALSE, 
                                  showticklabels = FALSE
                                ),
                                yaxis = list(
                                  showgrid = FALSE, 
                                  zeroline = FALSE, 
                                  showticklabels = FALSE
                                ),
                                annotations = list(
                                  list(
                                    x = 0, 
                                    y = -0.1,
                                    text = "Source: WB staff estimation", # Add your source text
                                    showarrow = FALSE, 
                                    xref = 'paper',
                                    yref = 'paper',
                                    font = list(size = 8) # Set font size for the annotation
                                  )
                                )
                              )
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    EffectiveVAT_Percentile_HH_plt=EffectiveVAT_Percentile_HH_plt,
    COICOP_DecileGroups_plt=COICOP_DecileGroups_plt,
    COICOP_BeforeReform_plt=COICOP_BeforeReform_plt,
    COICOP_AfterReform_plt=COICOP_AfterReform_plt
    
  )
}
