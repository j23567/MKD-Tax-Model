
" Dashboard1-VAT metrics"

# I.Function for Dashboard ------------------------------------------------------------------
VAT_metrics_fun <- function(vat_gap_metrics_tbl,vat_gap_metrics_tbl_treemap,vat_sectors_pie_tbl,vat_sectors_normalized,SimulationYear) {

  # Chart 1. VAT Revenue Comparison -----------------------------------------------------------------
  

  vat_rev_tbl_plt <- plot_ly(
                                vat_gap_metrics_tbl,
                                y = ~ reorder(Descriptions, value),
                                x = ~ value,
                                type = 'bar',
                                text = ' ',
                                hoverinfo = 'text',  # Use 'text' to display custom hover text
                                hovertext = ~ paste(Descriptions, "<br>Value:", value),
                                marker = list(color = "#1f77b4")  # Set bar color
                              ) %>%
                                layout(
                                  title = paste("VAT Revenue Comparison (in LCU Millions),", SimulationYear),
                                  xaxis = list(title = ""),
                                  yaxis = list(title = ""),
                                  margin = list(b = 100),
                                  bargap = 0.6,  # Adjust this value to control spacing between bars
                                  annotations = list(
                                    x = -0.05,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    xanchor = 'center',
                                    yanchor = 'top',
                                    font = list(size = 10)
                                  )
                                )
  
                          
  # Chart 2. VAT GAP Comparison (in LCU Millions)  -----------------------------------------------------------------
          
              vat_gap_tbl_plt <- plot_ly(data = vat_gap_metrics_tbl_treemap, 
                                         type = "treemap", 
                                         values = ~round(value,0),
                                         labels = ~ Descriptions,
                                         parents = ~label,  
                                         name = " ",
                                         text = ~Descriptions,
                                         textinfo = "label+value")%>%
                                    layout(
                                      title = list(
                                        text = paste("Decomposition of Total VAT Gap (in LCU Millions),", SimulationYear)
                                      ),
                                      annotations = list(
                                        x = 0.05,
                                        y = -0.01,
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        xanchor = 'center',
                                        yanchor = 'top',
                                        font = list(size = 10)
                                      ))
                
  # Chart 3. Structure of VAT by sectors -------------------------------------------------------------------------
              
              vat_structure_plt<- plot_ly(
                                            vat_sectors_pie_tbl,
                                            labels = ~variable,
                                            values = ~round(value,0),
                                            type = 'pie',
                                            hole = 0.6,
                                            textinfo = 'label+percent',
                                            insidetextorientation = 'radial'
                                          ) %>%
                                            layout(
                                              title = paste("Structure of VAT by Institutional Sectors,", SimulationYear),
                                              showlegend = TRUE,  # Enable the legend
                                              legend = list(
                                                orientation = 'v',  # Vertical orientation (default)
                                                x = 1.05,  # Move the legend slightly to the right of the chart
                                                y = 0.5,  # Center the legend vertically
                                                xanchor = 'left',
                                                yanchor = 'middle'
                                              ),
                                              startangle = 90,  # Rotate the pie chart by 90 degrees clockwise
                                              margin = list(l = 20, r = 20, t = 50, b = 50),  # Leave space for the annotation
                                              annotations = list(
                                                list(
                                                  x = 0.19,
                                                  y = 0,
                                                  text = "Source: WB staff estimation",
                                                  showarrow = FALSE,
                                                  xref = 'paper',
                                                  yref = 'paper',
                                                  xanchor = 'center',
                                                  yanchor = 'top',
                                                  font = list(size = 10)
                                                )
                                              )
                                            )
  
  # Chart 4. Breakdown of VAT by Sector ---------------------------------------------------------------
  
              # vat_str_plt <- plot_ly(vat_sectors_normalized) %>%
              #                   add_trace(
              #                     x = ~PRODUCT_INDUSTRY_CODE, 
              #                     y = ~Businesses_pct, 
              #                     type = 'bar', 
              #                     name = 'Businesses',
              #                     hoverinfo = 'text',
              #                     hovertext = ~paste(
              #                       "Sector:", PRODUCT_INDUSTRY_NAME,
              #                       "<br>Businesses:", round(Businesses_pct, 1), "%"
              #                     )
              #                   ) %>%
              #                   add_trace(
              #                     x = ~PRODUCT_INDUSTRY_CODE, 
              #                     y = ~Households_pct, 
              #                     type = 'bar', 
              #                     name = 'Households',
              #                     hoverinfo = 'text',
              #                     hovertext = ~paste(
              #                       "Sector:", PRODUCT_INDUSTRY_NAME,
              #                       "<br>Households:", round(Households_pct, 1), "%"
              #                     )
              #                   ) %>%
              #                   add_trace(
              #                     x = ~PRODUCT_INDUSTRY_CODE, 
              #                     y = ~NPISHs_pct, 
              #                     type = 'bar', 
              #                     name = 'NPISHs',
              #                     hoverinfo = 'text',
              #                     hovertext = ~paste(
              #                       "Sector:", PRODUCT_INDUSTRY_NAME,
              #                       "<br>NPISHs:", round(NPISHs_pct, 1), "%"
              #                     )
              #                   ) %>%
              #                   add_trace(
              #                     x = ~PRODUCT_INDUSTRY_CODE, 
              #                     y = ~Government_pct, 
              #                     type = 'bar', 
              #                     name = 'Government',
              #                     hoverinfo = 'text',
              #                     hovertext = ~paste(
              #                       "Sector:", PRODUCT_INDUSTRY_NAME,
              #                       "<br>Government:", round(Government_pct, 1), "%"
              #                     )
              #                   ) %>%
              #                   layout(
              #                     title = list(
              #                       #text = "Breakdown of VAT Revenues by Institutional Sector", 
              #                       title = paste("Breakdown of VAT Revenues by Institutional Sector,", SimulationYear),
              #                       x = 0.5, 
              #                       y = 1,        # Position the title above the chart
              #                       font = list(size = 12)
              #                     ),
              #                     xaxis = list(
              #                       title = '',
              #                       tickangle = -45,
              #                       tickfont = list(size = 8)
              #                     ),
              #                     yaxis = list(
              #                       title = 'Percentage',
              #                       tickformat = "",      # Disable auto-scaling to percentage format
              #                       range = c(0, 100),    # Use 0-100 range for y-axis
              #                       ticksuffix = "%"      # Append "%" to each tick
              #                     ),
              #                     legend = list(
              #                       orientation = "h",    # Horizontal legend
              #                       x = 0.5,              # Center the legend horizontally
              #                       y = -0.1,             # Position the legend below the chart
              #                       xanchor = "center",   # Align the legend at the center
              #                       yanchor = "top"       # Align the legend at the top of its position
              #                     ),
              #                     barmode = 'stack',      # Stack the bars to maintain structure
              #                     annotations = list(
              #                       list(
              #                         x = -0.05,
              #                         y = -0.3,
              #                         text = "Source: WB staff estimation", 
              #                         showarrow = FALSE, 
              #                         xref = "paper", 
              #                         yref = "paper", 
              #                         xanchor = "left", 
              #                         yanchor = "top", 
              #                         font = list(size = 8)
              #                       )
              #                     )
              #                   )
              # 
              
              vat_str_plt <- plot_ly(vat_sectors_normalized) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~Businesses_pct, 
                  type = 'bar', 
                  name = 'Businesses',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>Businesses:", round(Businesses_pct, 1), "%"
                  )
                ) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~Households_pct, 
                  type = 'bar', 
                  name = 'Households',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>Households:", round(Households_pct, 1), "%"
                  )
                ) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~NPISHs_pct, 
                  type = 'bar', 
                  name = 'NPISHs',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>NPISHs:", round(NPISHs_pct, 1), "%"
                  )
                ) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~Government_pct, 
                  type = 'bar', 
                  name = 'Government',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>Government:", round(Government_pct, 1), "%"
                  )
                ) %>%
                layout(
                  title = list(
                    text = paste("Breakdown of VAT Revenues by Institutional Sector,", SimulationYear),
                    x = 0.5,                # Center the title horizontally
                    y = 1,               # Lower the title for better readability
                    font = list(size = 12)  # Set title font size
                  ),
                  xaxis = list(
                    title = '',
                    tickangle = -45,
                    tickfont = list(size = 6)
                  ),
                  yaxis = list(
                    title = 'Percentage',
                    tickformat = "",      # Disable auto-scaling to percentage format
                    range = c(0, 100),    # Use 0-100 range for y-axis
                    ticksuffix = "%"      # Append "%" to each tick
                  ),
                  legend = list(
                    orientation = "h",      # Horizontal legend
                    x = 0.5,                # Center the legend horizontally
                    y = -0.05,              # Move the legend closer to the chart
                    xanchor = "center",     # Align the legend at the center
                    yanchor = "top",        # Align the legend at the top of its position
                    font = list(size = 6)   # Use a smaller font for the legend
                  ),
                  barmode = 'stack',        # Stack the bars to maintain structure
                  annotations = list(
                    list(
                      # x = -0.05,
                      # y = -0.25,            # Adjust annotation position to avoid overlap
                      x = 0.0,                # Center the legend horizontally
                      y = -0.05, 
                      
                      text = "Source: WB staff estimation", 
                      showarrow = FALSE, 
                      xref = "paper", 
                      yref = "paper", 
                      xanchor = "left", 
                      yanchor = "top", 
                      font = list(size = 6)
                    )
                  )
                )
              
        
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    vat_rev_tbl_plt=vat_rev_tbl_plt,
    vat_gap_tbl_plt=vat_gap_tbl_plt,
    vat_structure_plt=vat_structure_plt,
    vat_str_plt=vat_str_plt
    
  )
}
