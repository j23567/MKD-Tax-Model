'PIT FUNCTIONS'

# objects_to_remove <- c("pit_summary_df", "te_summary_selected", "re_effects_final", 
#                        "pit_decile_distribution_bu_sim", "pit_result_bins_sim_sub", 
#                        "PIT_BU_list", "PIT_SIM_list")
# 
# rm(list = objects_to_remove[objects_to_remove %in% ls()])

# NEW FUNCTIONS 8.3.2025

##









# Input Parameters  ---------------------------------------------------------

             'Artifical data'
              HighestBaseSSC_Employment<-16
              
              PersonalAllowance<-101256
   


              average_wage <- 41141
              max_base_wage <- average_wage * HighestBaseSSC_Employment * 12 
              
              SSC_rate <- 0.28
              # # Updating table with value from GUI
              # pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>%
              #   mutate(Value = ifelse(Parameters == "SSC_rate", SSC_rate, Value))
         

              ssc_temp_rate <- 0
              
              base_year <- unique(pit_data$sample$Year)[1]  
              end_year <- base_year + 4
              simulation_year <- SimulationYear  # Year from slider
              forecast_horizon <- seq(base_year, end_year)
              
              # Define the scenarios
              scenarios <- c("t0", "t1", "t2", "t3", "t4")
              
              PIT_BU_list <- list()
              PIT_SIM_list <- list()
              
              # Convert data to data.table
              pit_data$sample <- as.data.table(pit_data$sample)
              pit_data$weights <- lapply(pit_data$weights, as.data.table)
              pit_data$growth_factors <- as.data.table(pit_data$growth_factors)

              
              
# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR ----------------------
            filter_columns <- function(data) {
              col_names <- colnames(data)
              exclude_pattern <- "^(g_|d_).+(_l|_c)$|^(tax_base(?!_other))$|^deductions_|^Year$|^g_total_net_l$"
              cols_to_keep <- col_names[!grepl(exclude_pattern, col_names, perl = TRUE)]
              cols_to_keep <- c(cols_to_keep, "g_total_net_l")
              filtered_data <- data[, ..cols_to_keep, with = FALSE]
              return(filtered_data)
            }
            
            
# Define functions
            # 1.1 SSC -----------------------------------------------------------------
            max_ssc <- max_base_wage * SSC_rate # '# <--------OVA TREBA DA SE SETIRA '
           
            
              vec_cal_ssc_w_fun <- Vectorize(function(total_ssc, g_Wages_l_weighted, SSC_rate, max_base_wage, max_ssc) {
                  if (total_ssc == 0 && g_Wages_l_weighted > 0) {
                    calc_ssc <- 0
                  } else if (g_Wages_l_weighted < max_base_wage) {
                    calc_ssc <- SSC_rate * g_Wages_l_weighted
                  } else if (g_Wages_l_weighted > max_base_wage) {
                    calc_ssc <- max_ssc
                  }
                  return(calc_ssc)
                })
            
         
            
            # 1.2 Weighting Function --------------------------------------------------
              cal_weighting_fun <- function(sample_data, weights, growth_factors, columns_to_process) {
                for (col in columns_to_process) {
                  sample_data[, (paste0(col, "_adjusted")) := get(col) * weights * growth_factors]
                }

                return(sample_data)
              }


            # 1.3 Function to determine which dataset to use based on simulation_year
                        
            get_sim_dataset <- function(current_year, simulation_year, base_year) {
              if (current_year < simulation_year) {
                return(pit_simulation_parameters_raw)
              } else {
                return(pit_simulation_parameters_updated)
              }
            }

            # 1.3 Personal allowance -----------------------------------------------------------------------
           
            
            'OVA FUNCKIJA TREBA DA SE PROSIRI'
            # 1.4 Calculation for tax base for wages --------------------------

# OD OVDE SE VNESUVA VO NOVIOT KOD 08.03.2025 -----------------------------


            vec_cal_tax_base_w_fun <- Vectorize(function(g_Wages_l_adjusted, calc_ssc, total_personal_allowance, weight_personal_allowance_w, g_WagesDiplomaticConsular_l_adjusted, g_d_WagesDiplomaticConsular_l_adjusted, g_d_total_tax_reduction_l, tax_credit_donation) {
                                          personal_allowance_new <- total_personal_allowance * weight_personal_allowance_w # Calculate the adjusted personal allowance
                                          tax_base_wages1 <- ifelse(g_d_total_tax_reduction_l > 0, # Calculate tax base wages1 (applies when g_d_total_tax_reduction_l > 0)
                                                                    g_Wages_l_adjusted - calc_ssc - personal_allowance_new - tax_credit_donation/0.1 ,
                                                                    0)
                                          tax_base_wages1 <- max(tax_base_wages1, 0)  # Ensure tax_base_wages1 is non-negative
                                          tax_base_wages2 <- ifelse(g_d_total_tax_reduction_l == 0, # Calculate tax base wages2 (applies when g_d_total_tax_reduction_l == 0)
                                                                    g_Wages_l_adjusted - calc_ssc - personal_allowance_new,
                                                                    0)
                                          tax_base_wages2 <- max(tax_base_wages2, 0)  # Ensure tax_base_wages2 is non-negative
                                          tax_base_wages_diplomatic_consular_l <- g_WagesDiplomaticConsular_l_adjusted - g_d_WagesDiplomaticConsular_l_adjusted # Calculate the diplomatic/consular adjustment
                                          tax_base_w <- tax_base_wages1 + tax_base_wages2 + tax_base_wages_diplomatic_consular_l  # Sum tax_base_wages1 and tax_base_wages2 for the final tax base
                                          return(tax_base_w)
                                        })
            
            
            #  Donations to a legal entity .----------------------------
            # in accordance with the provisions of the Law on Donations and Sponsorship in Public Activities is creditable up to 20% of the personal income tax liability, up to a maximum annual credit of MKD 24,000
            
            vec_cal_tax_base_agr_fun <- Vectorize(function(g_AgriculturalProductsOwn_l_adjusted,rate_ded_income_agr_l) {
              tax_base_agr <- g_AgriculturalProductsOwn_l_adjusted-(g_AgriculturalProductsOwn_l_adjusted *rate_ded_income_agr_l)
              return(tax_base_agr)
            })

            # # 1.5 Calculation for PIT for wages ------------------------------
          

        #   # 1.5a Wages Diplomatic Consular-Deductions -------------------------------------------------------------------

        # # 1.5b Wages Diplomatic Consular-Tax Base --------------------------------------------

        #   # 1.6 PIT Diplomatic Consular -------------------------------------------------------------------------

            # 1.6 Estimation of deductions (prescribed costs) and tax base for Agricultural/Copyright income  ------------------------------
                # 1.6.1 Income of the basis of sale of own agricultural products------------------------------

                # 1.6.2 Calculation of tax base for income of the basis of sale of own agricultural products------------------------------
              # vec_cal_tax_base_agr_fun <- Vectorize(function(g_AgriculturalProductsOwn_l, deductions_income_agr_own_l) {

            
            vec_cal_tax_base_agr_fun <- Vectorize(function(g_AgriculturalProductsOwn_l_adjusted,rate_ded_income_agr_l) {
                                    tax_base_agr <- g_AgriculturalProductsOwn_l_adjusted-(g_AgriculturalProductsOwn_l_adjusted *rate_ded_income_agr_l)
              return(tax_base_agr)
            })


            # 1.7 Copyright Income Artistic Photography --------------------------------------
            vec_cal_tax_base_CopIncArtPhoto_l_fun <- Vectorize(function(g_CopyrightIncomeArtisticPhotography_l_adjusted, rate_deductions_CopyrightIncomeArtisticPhotography_l) {
              tax_base_CopyrightIncomeArtisticPhotography_l <- g_CopyrightIncomeArtisticPhotography_l_adjusted - (g_CopyrightIncomeArtisticPhotography_l_adjusted*rate_deductions_CopyrightIncomeArtisticPhotography_l)
              return(tax_base_CopyrightIncomeArtisticPhotography_l)
            })
            
            
                
            # 1.8 Deductions Copyright Income Music Ballet ----------------------------
            vec_cal_tax_base_CopIncMusicBallet_l_fun <- Vectorize(function(g_CopyrightIncomeMusicBallet_l_adjusted, rate_deductions_CopyrightIncomeMusicBallet_l) {
              tax_base_CopyrightIncomeMusicBallet_l <- g_CopyrightIncomeMusicBallet_l_adjusted - (g_CopyrightIncomeMusicBallet_l_adjusted*rate_deductions_CopyrightIncomeMusicBallet_l)
              return(tax_base_CopyrightIncomeMusicBallet_l)
            })
            
            
            
            # 1.9 Deductions Copyright Income Paintings ------------------------------------------
                # 1.9.1 Copyright (Deductions Copyright Income Paintings)------------------------------
            
            vec_cal_tax_base_CopIncPaintScu_l_fun <- Vectorize(function(g_CopyrightIncomePaintingsSculptural_l_adjusted, rate_deductions_CopyrightIncomePaintingsSculptural_l) {
              tax_base_CopyrightIncomePaintingsSculptural_l <- g_CopyrightIncomePaintingsSculptural_l_adjusted - (g_CopyrightIncomePaintingsSculptural_l_adjusted*rate_deductions_CopyrightIncomePaintingsSculptural_l)
              return(tax_base_CopyrightIncomePaintingsSculptural_l)
            })
                
            # 2.0  Copyright Income Translations Lectures ----------------------------------
            
            vec_cal_tax_base_CopIncTranslationsLectures_l_fun <- Vectorize(function(g_CopyrightIncomeTranslationsLectures_l_adjusted, rate_deductions_CopyrightIncomeTranslationsLectures_l) {
              # Tax base = Difference between the generated total income and the prescribed costs of 20%
              tax_base_CopyrightIncomeTranslationsLectures_l <- g_CopyrightIncomeTranslationsLectures_l_adjusted - (g_CopyrightIncomeTranslationsLectures_l_adjusted*rate_deductions_CopyrightIncomeTranslationsLectures_l)
              return(tax_base_CopyrightIncomeTranslationsLectures_l)
            })
            
            # 2.1 Successor or holder of the copyrights and related rights------------------------------
            vec_cal_tax_base_CopIncSuccessor_l_fun <- Vectorize(function(g_CopyrightIncomeSuccessor_l_adjusted, rate_deductions_CopyrightIncomeSuccessor_l) {
              tax_base_CopyrightIncomeSuccessor_l <- g_CopyrightIncomeSuccessor_l_adjusted - (g_CopyrightIncomeSuccessor_l_adjusted*rate_deductions_CopyrightIncomeSuccessor_l)
              return(tax_base_CopyrightIncomeSuccessor_l)
            })
            
            
            
            
            # 2.2 Work Income ---------------------------------------
            vec_cal_tax_base_WorkIncome_l_fun <- Vectorize(function(g_WorkIncome_l_adjusted, rate_deductions_WorkIncome_l) {
              tax_base_WorkIncome_l <- g_WorkIncome_l_adjusted - (g_WorkIncome_l_adjusted*rate_deductions_WorkIncome_l)
              return(tax_base_WorkIncome_l)
            })        


            # 3. Total tax base OTHER INCOME from labor (not include wages ) ------------------------------
            vec_cal_tax_base_other_fun <- Vectorize(function(tax_base_agr, tax_base_CopyrightIncomeArtisticPhotography_l, tax_base_CopyrightIncomeMusicBallet_l, tax_base_CopyrightIncomePaintingsSculptural_l, tax_base_CopyrightIncomeTranslationsLectures_l, tax_base_WorkIncome_l,
                                           #tax_base_inc_temp
                                           g_TemporaryContracts_l, g_AgriculturalProducts_l, g_IndependentActivity_l, tax_base_CopyrightIncomeSuccessor_l) {
                                            tax_base_other <- (tax_base_agr + tax_base_CopyrightIncomeArtisticPhotography_l + tax_base_CopyrightIncomeMusicBallet_l + tax_base_CopyrightIncomePaintingsSculptural_l + tax_base_CopyrightIncomeTranslationsLectures_l + tax_base_WorkIncome_l +
                                                                 g_TemporaryContracts_l + g_AgriculturalProducts_l + g_IndependentActivity_l + tax_base_CopyrightIncomeSuccessor_l)
                                            tax_base_other <- max(tax_base_other, 0)
                                            return(tax_base_other)
                                               })

            
            # 1.5 Calculation for PIT for wages ------------------------------
            #vec_cal_pit_w_fun <- Vectorize(function(tti_w_I, rate1, rate2, rate3, rate4, tbrk1, tbrk2, tbrk3) {
            vec_cal_pit_w_fun <- Vectorize(function(tax_base_w,tax_base_other, rate1, rate2, rate3, rate4, tbrk1, tbrk2, tbrk3) {
              tti_w_I <- tax_base_w + tax_base_other
               pit_w <- (rate1 * min(tti_w_I, tbrk1) +
                          rate2 * min(tbrk2 - tbrk1, max(0, tti_w_I - tbrk1)) +
                          rate3 * min(tbrk3 - tbrk2, max(0, tti_w_I - tbrk2)) +
                          rate4 * max(0, tti_w_I - tbrk3))
              return(pit_w)
            })
            
            
 

# II. ESTIMATION TAX LIABILITY FOR INCOME FROM CAPITAL ----------------------
            # 1. Estimation of tax base for capital incomes without deductions (prescribed cost)------------------------------         
            vec_cal_tax_base_IndPropRights_c_fun <- Vectorize(function(g_IndustrialPropertyRights_c_adjusted, rate_deductions_IndustrialPropertyRights_c) {
              tax_base_IndustrialPropertyRights_c <- g_IndustrialPropertyRights_c_adjusted - (g_IndustrialPropertyRights_c_adjusted*rate_deductions_IndustrialPropertyRights_c)
              return(tax_base_IndustrialPropertyRights_c)
            })  
            
            # 2.Income on the basis of lease----------------------------------
            'OVA DA SE PROVERI DAVA 592 A BESE 533'
                        vec_cal_tax_base_Lease_c_fun <- Vectorize(function(g_Lease_c_adjusted, rate_deductions_Lease_c) {
                          tax_base_Lease_c <- g_Lease_c_adjusted - (g_Lease_c_adjusted*rate_deductions_Lease_c)
                          return(tax_base_Lease_c)
                        })
            
            
            
            # 3. Income on the basis of lease of equipped residential and business premises-------------------------------
            vec_cal_tax_base_LeaseBusiness_c_fun <- Vectorize(function(g_LeaseBusiness_c_adjusted, rate_deductions_LeaseBusiness_c) {
              tax_base_LeaseBusiness_c <- g_LeaseBusiness_c_adjusted - (g_LeaseBusiness_c_adjusted*rate_deductions_LeaseBusiness_c)
              return(tax_base_LeaseBusiness_c)
            })
            
            

            # 4. Solid waste -------------------------------------------------------------
                  vec_cal_tax_base_SolidWaste_c_fun <- Vectorize(function(g_SolidWaste_c_adjusted, rate_deductions_SolidWaste_c) {
                    tax_base_SolidWaste_c <- g_SolidWaste_c_adjusted - (g_SolidWaste_c_adjusted*rate_deductions_SolidWaste_c)
                    return(tax_base_SolidWaste_c)
                  })
            
                
            
            # 5. Games of Chance Specific------------------------------------------------
            'ovde da se dodade deductions_GamesofChanceSpecific_c sto ke bide ponderirano so _adjusment'
            
                vec_cal_tax_base_GamesofChanceSpec_c_fun <-  Vectorize(function(g_GamesofChanceSpecific_c_adjusted, weight_deductions_GamesofChanceSpecific_c) {
                  tax_base_GamesofChanceSpecific_c <- g_GamesofChanceSpecific_c_adjusted - (g_GamesofChanceSpecific_c_adjusted*weight_deductions_GamesofChanceSpecific_c)
                  return(tax_base_GamesofChanceSpecific_c)
                })
            
            
                  # 5.3 Deductions and tax base from income from Games of Chance Specific betting house------------------------------
            'ovde da se proveri dava nula iako ima golem prihod'
                 vec_cal_tax_base_GamesofChanceBettingHouse_c_fun <- Vectorize(function(g_GamesofChanceBettingHouse_c_adjusted,d_GamesofChanceBettingHouse_c, weight_deductions_GamesofChanceBettingHouse_c) {
                  tax_base_GamesofChanceBettingHouse_c <- g_GamesofChanceBettingHouse_c_adjusted - (d_GamesofChanceBettingHouse_c*weight_deductions_GamesofChanceBettingHouse_c)
                  return(tax_base_GamesofChanceBettingHouse_c)
                })    
                

            # 6. Capital gains ------------------------------------------------------
           
            vec_cal_tax_base_CapGainsSaleShareCapital_c_fun <- Vectorize(function(g_CapitalGainsSaleShareCapital_c_adjusted, rate_deductions_CapitalGainsSaleShareCapital_c) {
              tax_base_CapitalGainsSaleShareCapital_c <- g_CapitalGainsSaleShareCapital_c_adjusted - (g_CapitalGainsSaleShareCapital_c_adjusted*rate_deductions_CapitalGainsSaleShareCapital_c)
              return(tax_base_CapitalGainsSaleShareCapital_c)
            })
            
                  
              # 6.3 Capital gains on the basis of sale of real estate after three years ------------------------------
               
                    vec_cal_tax_base_CapGainsRealEstateThreeYear_c_fun <- Vectorize(function(g_CapitalGainsRealEstateThreeYear_c_adjusted, rate_deductions_CapitalGainsRealEstateThreeYear_c) {
                      tax_base_CapitalGainsRealEstateThreeYear_c <- g_CapitalGainsRealEstateThreeYear_c_adjusted - (g_CapitalGainsRealEstateThreeYear_c_adjusted*rate_deductions_CapitalGainsRealEstateThreeYear_c)
                      return(tax_base_CapitalGainsRealEstateThreeYear_c)
                    })
            
            
                  # 6.5 Deductions Capital gains from the sale of other movable assets ------------------------------
                  
                  # # 6.6 Tax base capital gains from the sale of other movable assets ------------------------------

                  vec_cal_tax_base_CapGainssaleOtherMovAssets_c_fun <- Vectorize(function(g_CapitalGainssaleOtherMovableAssets_c_adjusted, rate_deductions_CapitalGainssaleOtherMovableAssets_c) {
                                tax_base_CapitalGainssaleOtherMovableAssets_c <- g_CapitalGainssaleOtherMovableAssets_c_adjusted - (g_CapitalGainssaleOtherMovableAssets_c_adjusted*rate_deductions_CapitalGainssaleOtherMovableAssets_c)
                                return(tax_base_CapitalGainssaleOtherMovableAssets_c)
                              })
                        
            
                  # 6.7 Deductions Capital gains on the basis of sale of real estate after five years------------------------------
                    vec_cal_tax_base_CapGainsSellsRealEstateFiveYear_c_fun <- Vectorize(function(g_CapitalGainsSellsRealEstateFiveYear_c_adjusted, weight_deductions_CapitalGainsSellsRealEstateFiveYear_c) {
                      tax_base_CapitalGainsSellsRealEstateFiveYear_c <- g_CapitalGainsSellsRealEstateFiveYear_c_adjusted - (g_CapitalGainsSellsRealEstateFiveYear_c_adjusted*weight_deductions_CapitalGainsSellsRealEstateFiveYear_c)
                      return(tax_base_CapitalGainsSellsRealEstateFiveYear_c)
                    })
                  
                  # 6.9  Deductions and tax base Income from insurance------------------------------
            vec_cal_tax_bases_Insurance_c_fun <- Vectorize(function(g_Insurance_c_adjusted, weight_deductions_Insurance_c) {
              tax_base_Insurance_c <- g_Insurance_c_adjusted - (g_Insurance_c_adjusted*weight_deductions_Insurance_c)
              return(tax_base_Insurance_c)
            })
            
            

            # 7. Total tax base based on capital income-----------------------------------------
            # 7.1 Calculation for total tax base from capital income OTHER THAN games of chance------------------------------
            vec_cal_tti_c_a_fun <- Vectorize(function(g_IndustrialPropertyRightsSuccessor_c, g_Insurance_c, g_Interests_c, g_OtherIncome_c, g_Sublease_c, tax_base_IndustrialPropertyRights_c, tax_base_Lease_c, tax_base_LeaseBusiness_c, tax_base_SolidWaste_c, tax_base_Insurance_c,
                                                      g_CapitalIncome_c , tax_base_CapitalGainssaleOtherMovableAssets_c , tax_base_CapitalGainsSellsRealEstateFiveYear_c , tax_base_CapitalGainsRealEstateThreeYear_c , tax_base_CapitalGainsSaleShareCapital_c
                                                      ) {
                                      tti_c_a <- g_IndustrialPropertyRightsSuccessor_c + g_Insurance_c + g_Interests_c + g_OtherIncome_c + g_Sublease_c + tax_base_IndustrialPropertyRights_c + tax_base_Lease_c + tax_base_LeaseBusiness_c + tax_base_SolidWaste_c + tax_base_Insurance_c+
                                        g_CapitalIncome_c + tax_base_CapitalGainssaleOtherMovableAssets_c + tax_base_CapitalGainsSellsRealEstateFiveYear_c + tax_base_CapitalGainsRealEstateThreeYear_c + tax_base_CapitalGainsSaleShareCapital_c
                                      tti_c_a <- max(tti_c_a, 0)
                                      return(tti_c_a)
                                    })

            # 7.2 Calculation for total tax base from capital income ONLY from games of chance (15%)------------------------------
            vec_cal_tti_c_g_fun <-  Vectorize(function(tax_base_GamesofChanceSpecific_c, tax_base_GamesofChanceBettingHouse_c, g_GamesofChanceGeneral_c) {
                                      tti_c_g <- tax_base_GamesofChanceSpecific_c + tax_base_GamesofChanceBettingHouse_c + g_GamesofChanceGeneral_c
                                      tti_c_g <- max(tti_c_g, 0)
                                      return(tti_c_g)
                                    })



            # 7.5 Total PIT on the base of income from capital  ---------------------------------------------------------------------
            vec_cal_pit_c_fun <- Vectorize(function(capital_income_rate_a, capital_income_rate_g, tti_c_a, tti_c_g ) {
                                    pit_c <- (tti_c_a * capital_income_rate_a) + (tti_c_g * capital_income_rate_g) #+ (tti_c_c * capital_income_rate_c)
                                    return(pit_c)
                                  })

# III. ESTIMATION TOTAL GROSS AND NET INCOME AND PIT --------------------------------
            
            # 5.Total PIT ------------------------------
            vec_cal_total_pit_fun <- Vectorize(function(pit_w,pit_c) {
                                                                         # pitax <- pit_w + pit_c
                                                                          pitax <- pit_w+ pit_c
                                                                          return(pitax)
                                                                        })
            
      