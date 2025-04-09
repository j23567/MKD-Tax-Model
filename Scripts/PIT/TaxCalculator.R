
#  OD OVDE

setDTthreads(threads = 8)

get_param_fun <- function(params_dt, param_name) {
  params_dt[Parameters == param_name, Value]
}



HighestBaseSSC_Employment<-16

PersonalAllowance<-101256



average_wage <- 41141
max_base_wage <- average_wage * HighestBaseSSC_Employment * 12 

SSC_rate <- 0.28

ssc_temp_rate <- 0




            base_year <- unique(dt$Year)[1]
            end_year <- base_year + 4
            #SimulationYear <- SimulationYear  # Year from slider
            simulation_year <- SimulationYear  # Year from slider
            forecast_horizon <- seq(base_year, end_year)
            
            # Define the scenarios
            scenarios <- c("t0", "t1", "t2", "t3", "t4")
            
            # Simulation parameters must be in data.table
            pit_simulation_parameters_raw <- pit_simulation_parameters_raw %>% data.table()
            pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>% data.table()


# 1. Tax Calculation Function -------------------------------------------------------
start.time <- proc.time()
tax_calc_fun <- function(dt_scn, params_dt) {
                      rate1 <- get_param_fun(params_dt, "rate1")
                      rate2 <- get_param_fun(params_dt, "rate2")
                      rate3 <- get_param_fun(params_dt, "rate3")
                      rate4 <- get_param_fun(params_dt, "rate4")
                      tbrk1 <- get_param_fun(params_dt, "tbrk1")
                      tbrk2 <- get_param_fun(params_dt, "tbrk2")
                      tbrk3 <- get_param_fun(params_dt, "tbrk3")
                      tbrk4 <- get_param_fun(params_dt, "tbrk4")
                      rate_WagesDiplomaticConsular_l <- get_param_fun(params_dt, "rate_WagesDiplomaticConsular_l")
                      rate_ded_income_agr_l <- get_param_fun(params_dt, "rate_ded_income_agr_l")
                      rate_deductions_CopyrightIncomePaintingsSculptural_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomePaintingsSculptural_l")
                      rate_deductions_CopyrightIncomeArtisticPhotography_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeArtisticPhotography_l")
                      rate_deductions_CopyrightIncomeMusicBallet_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeMusicBallet_l")
                      rate_deductions_CopyrightIncomeTranslationsLectures_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeTranslationsLectures_l")
                      rate_deductions_IndustrialPropertyRights_c <- get_param_fun(params_dt, "rate_deductions_IndustrialPropertyRights_c")
                      rate_deductions_CopyrightIncomeSuccessor_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeSuccessor_l")
                      rate_deductions_Lease_c <- get_param_fun(params_dt, "rate_deductions_Lease_c")
                      rate_deductions_LeaseBusiness_c <- get_param_fun(params_dt, "rate_deductions_LeaseBusiness_c")
                      rate_deductions_CapitalGainsSaleShareCapital_c <- get_param_fun(params_dt, "rate_deductions_CapitalGainsSaleShareCapital_c")
                      rate_deductions_CapitalGainsRealEstateThreeYear_c <- get_param_fun(params_dt, "rate_deductions_CapitalGainsRealEstateThreeYear_c")
                      rate_deductions_CapitalGainssaleOtherMovableAssets_c <- get_param_fun(params_dt, "rate_deductions_CapitalGainssaleOtherMovableAssets_c")
                      rate_deductions_SolidWaste_c <- get_param_fun(params_dt, "rate_deductions_SolidWaste_c")
                      rate_deductions_WorkIncome_l <- get_param_fun(params_dt, "rate_deductions_WorkIncome_l")
                      weight_deductions_GamesofChanceSpecific_c <- get_param_fun(params_dt, "weight_deductions_GamesofChanceSpecific_c")
                      weight_deductions_GamesofChanceBettingHouse_c <- get_param_fun(params_dt, "weight_deductions_GamesofChanceBettingHouse_c")
                      weight_deductions_Insurance_c <- get_param_fun(params_dt, "weight_deductions_Insurance_c")
                      weight_deductions_CapitalGainsSellsRealEstateFiveYear_c <- get_param_fun(params_dt, "weight_deductions_CapitalGainsSellsRealEstateFiveYear_c")
                      tax_credit_donation <- get_param_fun(params_dt, "tax_credit_donation")
                      weight_personal_allowance_w <- get_param_fun(params_dt, "weight_personal_allowance_w")
                      capital_income_rate_a<- get_param_fun(params_dt, "capital_income_rate_a")
                      capital_income_rate_g<- get_param_fun(params_dt, "capital_income_rate_g")
                      capital_income_rate_c<- get_param_fun(params_dt, "capital_income_rate_c")
                      
                      
  
# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR --------------
  # 1.Calculation of tax base wages ----------------------------------------------------
        
       
        # SSC
        max_ssc <- max_base_wage * SSC_rate #
        
                      
                      
      dt_scn[, calc_ssc := fifelse(total_ssc == 0 & g_Wages_l > 0, 
                                     0, 
                                     fifelse(g_Wages_l < max_base_wage, 
                                             SSC_rate * g_Wages_l, 
                                             max_ssc))]
  
        
  
       # Tax base
  
      
      
      # Define the second function
      dt_scn[, tax_base_w := {
        personal_allowance_new <- g_total_personal_allowance_l * weight_personal_allowance_w
        tax_base_wages1 <- pmax(fifelse(g_d_total_tax_reduction_l > 0,
                                   g_Wages_l - calc_ssc - personal_allowance_new - tax_credit_donation / 0.1,
                                   0),0)
        #tax_base_wages1 <- pmax(tax_base_wages1, 0)
        tax_base_wages2 <- pmax(fifelse(g_d_total_tax_reduction_l == 0,
                                   g_Wages_l - calc_ssc - personal_allowance_new,
                                   0),0)
        #tax_base_wages2 <- pmax(tax_base_wages2, 0)
        tax_base_wages_diplomatic_consular_l <- g_WagesDiplomaticConsular_l - g_d_WagesDiplomaticConsular_l
        tax_base_wages1 + tax_base_wages2 + tax_base_wages_diplomatic_consular_l
      }]
      
      
        
        # Donations ---------------------------------------------------------------
        
        dt_scn[, pit_tax_donation := fifelse(g_d_total_tax_reduction_l > 0, 
                                             g_Wages_l - calc_ssc - g_total_personal_allowance_l - (g_d_total_tax_reduction_l/0.1) ,
                                             0)*rate1] 
        
        
        

  # 2.Calculation of tax base for income of the basis of sale of own agricultural products ------------------------------------------------------------------------
    dt_scn[, tax_base_agr := g_AgriculturalProductsOwn_l - (g_AgriculturalProductsOwn_l * rate_ded_income_agr_l)]
    dt_scn[, pit_tax_agr := tax_base_agr*rate1] 
        
        
  # 3.Copyright Income Artistic Photography  ------------------------------------------
   dt_scn[, tax_base_CopyrightIncomeArtisticPhotography_l := 
                                                           g_CopyrightIncomeArtisticPhotography_l - 
                                                           (g_CopyrightIncomeArtisticPhotography_l * rate_deductions_CopyrightIncomeArtisticPhotography_l)
                                                  ]
    
    dt_scn[, pit_tax_CopyrightIncomeArtisticPhotography_l := tax_base_CopyrightIncomeArtisticPhotography_l*rate1]
    
  # 4.Deductions Copyright Income Music Ballet  -------------------------------------
  dt_scn[, tax_base_CopyrightIncomeMusicBallet_l := 
           g_CopyrightIncomeMusicBallet_l - 
           (g_CopyrightIncomeMusicBallet_l * rate_deductions_CopyrightIncomeMusicBallet_l)
  ]
  dt_scn[, pit_tax_CopyrightIncomeMusicBallet_l := tax_base_CopyrightIncomeMusicBallet_l*rate1] 

    
  # 5.Copyright (Deductions Copyright Income Paintings)---------------
  dt_scn[, tax_base_CopyrightIncomePaintingsSculptural_l := 
           g_CopyrightIncomePaintingsSculptural_l - 
           (g_CopyrightIncomePaintingsSculptural_l * rate_deductions_CopyrightIncomePaintingsSculptural_l)
  ]
  
  dt_scn[, pit_tax_CopyrightIncomePaintingsSculptural_l := tax_base_CopyrightIncomePaintingsSculptural_l*rate1]
  
  
  
  # 6.Copyright Income Translations Lectures ----------------------------------------------------------------------
  dt_scn[, tax_base_CopyrightIncomeTranslationsLectures_l := 
           g_CopyrightIncomeTranslationsLectures_l - 
           (g_CopyrightIncomeTranslationsLectures_l * rate_deductions_CopyrightIncomeTranslationsLectures_l)
  ]

  dt_scn[, pit_tax_CopyrightIncomeTranslationsLectures_l := tax_base_CopyrightIncomeTranslationsLectures_l*rate1] #NEW
  
  
  # 7.Successor or holder of the copyrights and related rights ----------------------------------------------------------------------
  dt_scn[, tax_base_CopyrightIncomeSuccessor_l := 
           g_CopyrightIncomeSuccessor_l - 
           (g_CopyrightIncomeSuccessor_l * rate_deductions_CopyrightIncomeSuccessor_l)
  ]  
  
  dt_scn[, pit_tax_CopyrightIncomeSuccessor_l := tax_base_CopyrightIncomeSuccessor_l*rate1] 
  
  
  # 8. Work Income ----------------------------------------------------------------------
  dt_scn[, tax_base_WorkIncome_l := 
           g_WorkIncome_l - 
           (g_WorkIncome_l * rate_deductions_WorkIncome_l)
  ]
  
  dt_scn[, pit_tax_WorkIncome_l := tax_base_WorkIncome_l*rate1] #NEW

  # 9.Total tax base OTHER INCOME from labor (not include wages )----------------------------------------------------------------------

        dt_scn[, tax_base_other := pmax(
                                        tax_base_agr + 
                                          tax_base_CopyrightIncomeArtisticPhotography_l + 
                                          tax_base_CopyrightIncomeMusicBallet_l + 
                                          tax_base_CopyrightIncomePaintingsSculptural_l + 
                                          tax_base_CopyrightIncomeTranslationsLectures_l + 
                                          tax_base_WorkIncome_l + 
                                          g_TemporaryContracts_l + 
                                          g_AgriculturalProducts_l + 
                                          g_IndependentActivity_l + 
                                          tax_base_CopyrightIncomeSuccessor_l, 
                                        0)]
        

  # 10. Calculation for PIT for wages -------------------------------------------

          dt_scn[, tti_w_I := tax_base_w + tax_base_other]
          
          dt_scn[, pit_w := 
                   (rate1 * pmin(tti_w_I, tbrk1) +
                      rate2 * pmin(tbrk2 - tbrk1, pmax(0, tti_w_I - tbrk1)) +
                      rate3 * pmin(tbrk3 - tbrk2, pmax(0, tti_w_I - tbrk2)) +
                      rate4 * pmax(0, tti_w_I - tbrk3))
          ]
          

# II. ESTIMATION TAX LIABILITY FOR INCOME FROM CAPITAL ---------------

   # 1. Estimation of tax base for capital incomes without deductions (prescribed cost) --------
        
          dt_scn[, tax_base_IndustrialPropertyRights_c := 
                   g_IndustrialPropertyRights_c- 
                   (g_IndustrialPropertyRights_c* rate_deductions_IndustrialPropertyRights_c)
          ]
          
          dt_scn[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*capital_income_rate_a] #NEW

   # 2.Income on the basis of lease -----------------------------------------------
          dt_scn[, tax_base_Lease_c := 
                   g_Lease_c- 
                   (g_Lease_c* rate_deductions_Lease_c)
          ]
  
          dt_scn[, pit_tax_Lease_c := tax_base_Lease_c*capital_income_rate_a] #NEW
          
          
   # 3. Income on the basis of lease of equipped residential and business premises-------------------------------
         
          dt_scn[, tax_base_LeaseBusiness_c := 
                   g_LeaseBusiness_c- 
                   (g_LeaseBusiness_c* rate_deductions_LeaseBusiness_c)
          ]
  
          dt_scn[, pit_LeaseBusiness_c := tax_base_LeaseBusiness_c*capital_income_rate_a] #NEW
          
          
   # 4. Solid waste -------------------------------------------------------------
          dt_scn[, tax_base_SolidWaste_c := 
                   g_SolidWaste_c- 
                   (g_SolidWaste_c* rate_deductions_SolidWaste_c)
          ]
          
          dt_scn[, pit_SolidWaste_c := tax_base_SolidWaste_c*capital_income_rate_a] #NEW
          
          
          
   # 5. Games of Chance Specific----------------------------------- --------
          dt_scn[, tax_base_GamesofChanceSpecific_c := 
                   g_GamesofChanceSpecific_c- 
                   (g_GamesofChanceSpecific_c* weight_deductions_GamesofChanceSpecific_c)
          ]
          
          dt_scn[, pit_GamesofChanceSpecific_c := tax_base_GamesofChanceSpecific_c*capital_income_rate_g] #NEW

      # 5.1 Deductions and tax base from income from Games of Chance Specific betting house -------------------------------------------------------------------------
          dt_scn[, tax_base_GamesofChanceBettingHouse_c := 
                   g_GamesofChanceBettingHouse_c-(g_d_GamesofChanceBettingHouse_c * weight_deductions_GamesofChanceBettingHouse_c)
          ]
          
         
          
          dt_scn[, pit_GamesofChanceBettingHouse_c := tax_base_GamesofChanceBettingHouse_c*capital_income_rate_g] #NEW
          
  # 6. Capital gains ---------------------------------------
              dt_scn[, tax_base_CapitalGainsSaleShareCapital_c := 
                     g_CapitalGainsSaleShareCapital_c- 
                     (g_CapitalGainsSaleShareCapital_c* rate_deductions_CapitalGainsSaleShareCapital_c)
            ]
  
          
          dt_scn[, pit_CapitalGainsSaleShareCapital_c := tax_base_CapitalGainsSaleShareCapital_c*capital_income_rate_c] #NEW
          
  # 6.1 Capital gains on the basis of sale of real estate after three years-----------------------
            dt_scn[, tax_base_CapitalGainsRealEstateThreeYear_c := 
                     g_CapitalGainsRealEstateThreeYear_c- 
                     (g_CapitalGainsRealEstateThreeYear_c* rate_deductions_CapitalGainsRealEstateThreeYear_c)
            ]
          
          dt_scn[, pit_CapitalGainsRealEstateThreeYear_c := tax_base_CapitalGainsRealEstateThreeYear_c*capital_income_rate_c] #NEW
          
          
  # 6.2 Tax base capital gains from the sale of other movable assets------
           dt_scn[, tax_base_CapitalGainssaleOtherMovableAssets_c := 
                     g_CapitalGainssaleOtherMovableAssets_c- 
                     (g_CapitalGainssaleOtherMovableAssets_c* rate_deductions_CapitalGainssaleOtherMovableAssets_c)
            ]
          
          dt_scn[, pit_CapitalGainssaleOtherMovableAssets_c := tax_base_CapitalGainssaleOtherMovableAssets_c*capital_income_rate_c] #NEW
          
  # 6.3 Deductions Capital gains on the basis of sale of real estate after five years
            dt_scn[, tax_base_CapitalGainsSellsRealEstateFiveYear_c := 
                     g_CapitalGainsSellsRealEstateFiveYear_c- 
                     (g_CapitalGainsSellsRealEstateFiveYear_c* weight_deductions_CapitalGainsSellsRealEstateFiveYear_c)
            ]
  
            dt_scn[, pit_CapitalGainsSellsRealEstateFiveYear_c := tax_base_CapitalGainsSellsRealEstateFiveYear_c*capital_income_rate_c] #NEW
            
  # 6.4  Deductions and tax base Income from insurance---------------
            dt_scn[, tax_base_Insurance_c := 
                     g_Insurance_c- 
                     (g_Insurance_c* weight_deductions_Insurance_c)
            ]
            
            dt_scn[, pit_Insurance_c := tax_base_Insurance_c*capital_income_rate_c] #NEW
            
  # 7.Total tax base based on capital income --------------------------------
        # 7.1 Calculation for total tax base from capital income OTHER THAN games of chance----------------------
          dt_scn[, tti_c_a := pmax(
                                    g_IndustrialPropertyRightsSuccessor_c + g_Insurance_c + g_Interests_c + g_OtherIncome_c + g_Sublease_c + 
                                      tax_base_IndustrialPropertyRights_c + tax_base_Lease_c + tax_base_LeaseBusiness_c + tax_base_SolidWaste_c + 
                                      tax_base_Insurance_c + g_CapitalIncome_c + tax_base_CapitalGainssaleOtherMovableAssets_c + 
                                      tax_base_CapitalGainsSellsRealEstateFiveYear_c + tax_base_CapitalGainsRealEstateThreeYear_c + 
                                      tax_base_CapitalGainsSaleShareCapital_c, 0)
                                  ]
            

  # 7.2 Calculation for total tax base from capital income ONLY from games of chance (15%)----------------
            dt_scn[, tti_c_g := pmax(
                                            tax_base_GamesofChanceSpecific_c + tax_base_GamesofChanceBettingHouse_c + g_GamesofChanceGeneral_c, 0)
                                          ]
  
  # 7.2 Total PIT on the base of income from capital----------------------
            dt_scn[, pit_c := (tti_c_a * capital_income_rate_a) + (tti_c_g * capital_income_rate_g)]
            
            
  # III. ESTIMATION TOTAL GROSS AND NET INCOME AND PIT --------------------------------       
  # Total PIT ------------------------------------------------------
          
            dt_scn[, pitax := pit_w + pit_c]
            
}    
# 2. Helper to Retrieve Growth Factors for Each Variable -------------------------------
                vars_to_grow <- c(
                                  "g_Wages_l", "g_WagesDiplomaticConsular_l", "g_TemporaryContracts_l", 
                                  "g_AgriculturalProductsOwn_l", "g_AgriculturalProducts_l", 
                                  "g_total_personal_allowance_l",
                                  "g_IndependentActivity_l", "g_CopyrightIncomeArtisticPhotography_l", 
                                  "g_CopyrightIncomeMusicBallet_l", "g_CopyrightIncomePaintingsSculptural_l", 
                                  "g_CopyrightIncomeSuccessor_l", "g_CopyrightIncomeTranslationsLectures_l", 
                                  "g_WorkIncome_l", "g_CapitalGains_c", "g_CapitalIncome_c", 
                                  "g_IndustrialPropertyRights_c", "g_IndustrialPropertyRightsSuccessor_c", 
                                  "g_Insurance_c", "g_Interests_c", "g_Lease_c", "g_LeaseBusiness_c", 
                                  "g_Sublease_c", "g_SolidWaste_c", "g_GamesofChanceSpecific_c", 
                                  "g_GamesofChanceBettingHouse_c", "g_GamesofChanceGeneral_c", 
                                  "g_OtherIncome_c", "g_CapitalGainsSellsRealEstateFiveYear_c", 
                                  "g_CapitalGainsSaleShareCapital_c", "g_CapitalGainsRealEstateThreeYear_c", 
                                  "g_CapitalGainssaleOtherMovableAssets_c"
                              )
                
                get_growth_factor_row <- function(scenario) {
                  gf_row <- growth_factors[scenarios == scenario]
                  out <- numeric(length(vars_to_grow))
                  names(out) <- vars_to_grow
                  
                  for (v in vars_to_grow) {
                    gf_col <- sub("_adjusted", "", v)  # e.g. "g_Wages_l"
                    out[v] <- gf_row[[gf_col]]
                  }
                  return(out)
                }

# 3. Business as usual  ------------------------------------------------------
          
          PIT_BU_list <- list()
          
          # Start from baseline
          dt_scn_BU <- copy(dt)
          
          for (s in scenarios) {
            
            # 1) Retrieve scenario growth factors
            gf_values <- get_growth_factor_row(s)
            
            # 2) Multiply each variable by gf_values[v] * weights[[s]]
            for (v in vars_to_grow) {
              dt_scn_BU[, (v) := get(v) * gf_values[v] * weights[[s]]]
            }
            
            # 3) Row-wise tax logic
            tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
            
            # 4) ADD a 'weight' column that references weights[[s]]
            dt_scn_BU[, weight := weights[[s]]]
            
            # 5) Store in PIT_BU_list
            PIT_BU_list[[s]] <- copy(dt_scn_BU)
          }

# 4. Simulation --------------------------------------------------------------
          start_index <- match(SimulationYear, scenario_years) 
          
          PIT_SIM_list <- list()

          if (start_index > 1) {
            for (i in seq_len(start_index - 1)) {
              s_early <- scenarios[i]
              PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
            }
          }
          
          # 2) Determine the starting data for re-simulation
          if (start_index == 1) {
            # SimulationYear=2021 => start from original dt
            dt_scn_SIM <- copy(dt)
          } else {
            # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
            prev_scenario <- scenarios[start_index - 1]
            dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
          }
          
          # 3) Chain from scenario index = start_index .. 5
          for (i in seq(from = start_index, to = length(scenarios))) {
            s <- scenarios[i]
            
            gf_values <- get_growth_factor_row(s)
            
            # Multiply each variable by growth factor * row-weight for scenario s
            for (v in vars_to_grow) {
              dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights[[s]]]
            }
            
            # Run row-wise calculations with updated parameters
            tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated)
            
            # **Add a 'weight' column** with the row-specific weights for scenario s
            dt_scn_SIM[, weight := weights[[s]]]
            
            # Store final data in PIT_SIM_list
            PIT_SIM_list[[s]] <- copy(dt_scn_SIM)
          }
          
          message("Block 2 (PIT_SIM_list) done, including early years from PIT_BU_list, plus 'weight' column.\n")
          message("All done!\n")
          
         # rm(dt_scn_BU, dt_scn_SIM)

      # 5. Aggregation of simulated data -----------------------------------------------------
          
          summarize_PIT_fun_dt <- function(PIT_list, suffix) {
            # 1) Loop (via lapply) over each named data.table in the list
            # 2) Sum columns matching regex ^(calc|pit)
            # 3) Collect results into one data.table
            summary_list <- lapply(names(PIT_list), function(scenario_name) {
              dt <- PIT_list[[scenario_name]]
              
              # Select columns starting with "calc" or "pit", and sum them
              # .SDcols = patterns("^(calc|pit)") picks columns with those prefixes
              sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|pit)")]
              
              # Add scenario name as a column
              sums_dt[, scenarios := scenario_name]
              
              # Make 'scenarios' the first column
              setcolorder(sums_dt, c("scenarios", setdiff(names(sums_dt), "scenarios")))
              sums_dt
            })
            
            # Combine all scenario summaries into one data.table
            result_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
            
            # Append 'suffix' to every column except 'scenarios'
            old_names <- setdiff(names(result_dt), "scenarios")
            new_names <- paste0(old_names, suffix)
            setnames(result_dt, old_names, new_names)
            
            # Convert to data.frame if you want the same final type as your original code
            result_df <- as.data.frame(result_dt)
            
            return(result_df)
          }
          
          
      
      # Function to sum the specified columns in the list and store the results in a data frame
      # summary_SIM <- summarize_PIT_fun(PIT_SIM_list, "_sim")
      # summary_BU <- summarize_PIT_fun(PIT_BU_list, "_bu")
          summary_SIM <- summarize_PIT_fun_dt(PIT_SIM_list, "_sim")
          summary_BU  <- summarize_PIT_fun_dt(PIT_BU_list, "_bu")
          
      
      
      merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM$year <- as.character(forecast_horizon)
      merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
      merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06



# 6. Decile ------------------------------------------------------------------

               
           # OLD APPROACH
            # #Apply the functions to each data frame in the PIT_BU_list
            # for (name in names(PIT_BU_list)) {
            #   df <- PIT_BU_list[[name]]
            #   df$decile_group <- cal_weighted_deciles_fun(df$g_total_gross, df$weight)
            #   df$centile_group <- cal_weighted_centiles_fun(df$g_total_gross, df$weight)
            #   PIT_BU_list[[name]] <- df
            # }
            # 
            # # Apply the functions to each data frame in the PIT_SIM_list
            # for (name in names(PIT_SIM_list)) {
            #   df <- PIT_SIM_list[[name]]
            #   df$decile_group <- cal_weighted_deciles_fun(df$g_total_gross, df$weight)
            #   df$centile_group <- cal_weighted_centiles_fun(df$g_total_gross, df$weight)
            #   PIT_SIM_list[[name]] <- df
            # }
      
      
      calc_weighted_groups_in_one_pass <- function(DT, inc_col = "g_total_gross", w_col = "weight") {
        # 1. Keep track of original row order so we can restore it after sorting
        DT[, row_id__tmp := .I]
        
        # 2. Sort by income (use setorderv for a character column name)
        setorderv(DT, inc_col)
        
        # 3. Compute the cumulative sum of weight
        #    (handle NA weights as 0, adjust if you prefer a different approach)
        DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
        
        # 4. Get the total weight
        total_w <- DT[.N, w_cumsum__tmp]
        
        # 5. Define breakpoints for deciles (10 groups) and centiles (100 groups)
        decile_breaks  <- seq(0, total_w, length.out = 11)   # 11 points => 10 intervals
        centile_breaks <- seq(0, total_w, length.out = 101)  # 101 points => 100 intervals
        
        # 6. Assign decile_group and centile_group
        DT[, decile_group  := findInterval(w_cumsum__tmp, decile_breaks,  rightmost.closed = TRUE)]
        DT[, centile_group := findInterval(w_cumsum__tmp, centile_breaks, rightmost.closed = TRUE)]
        
        # 7. Ensure the top boundary doesn't exceed the number of groups
        DT[, decile_group  := pmin(decile_group,  10)]
        DT[, centile_group := pmin(centile_group, 100)]
        
        # 8. Restore original row order
        setorder(DT, row_id__tmp)
        
        # 9. Clean up temporary columns
        DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
        
        # Modifies DT in place, so no return() needed
        invisible(DT)
      }
      
      # -------------------------------------------------------------------
      # Example usage: loop over your list of data.tables
      # -------------------------------------------------------------------
      for (i in seq_along(PIT_BU_list)) {
        calc_weighted_groups_in_one_pass(
          DT      = PIT_BU_list[[i]],
          inc_col = "g_total_gross",
          w_col   = "weight"
        )
      }
      
      
      for (i in seq_along(PIT_BU_list)) {
        calc_weighted_groups_in_one_pass(
          DT      = PIT_SIM_list[[i]],
          inc_col = "g_total_gross",
          w_col   = "weight"
        )
      }
      
      
      
      




                      # Convert data for presentation in GUI
                      pit_summary_df <- merged_PIT_BU_SIM %>%
                        pivot_longer(cols = -year, 
                                     names_to = c("variable", ".value"), 
                                     names_pattern = "(.*)_(bu|sim)")
                      
                      # Calculate the difference between _sim and _bu columns
                      pit_summary_df <- pit_summary_df %>%
                        mutate(difference = sim - bu)
                      
                      
                      pit_summary_df <- pit_summary_df %>%
                        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
                        filter(variable=='pitax')
                      
                      # Arrange the columns
                      pit_summary_df <- pit_summary_df %>%
                                  select(year, bu, sim, difference)%>%
                                  dplyr::rename(
                                    "Current law (LCU Mil)"="bu",
                                    "Simulation (LCU Mil)"="sim",
                                    "Fiscal impact (LCU Mil)"="difference",
                                  )
                      
                      
                      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
                      
                      pit_summary_df<-left_join(pit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
                        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
                        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
                        dplyr::select(-c(Nominal_GDP))
                      
                      
                      pit_summary_df <- as.data.table(pit_summary_df)
                      
                      
                      
                      print(merged_PIT_BU_SIM)
                      
                      end.time <- proc.time()
                      save.time <- end.time - start.time
                      cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")



                    