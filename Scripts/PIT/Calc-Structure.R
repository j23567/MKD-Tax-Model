" Data prep Distribution Dashboard "

# I. Labor-capital -----------------------------------------------------

                  columns_gross_income <- c( #"id_n",
                                              "g_Wages_l",
                                              "g_WagesDiplomaticConsular_l",
                                              "g_TemporaryContracts_l",
                                              "g_AgriculturalProductsOwn_l",
                                              "g_AgriculturalProducts_l",
                                              "g_IndependentActivity_l",
                                              "g_CopyrightIncomeArtisticPhotography_l",
                                              "g_CopyrightIncomeMusicBallet_l",
                                              "g_CopyrightIncomePaintingsSculptural_l",
                                              "g_CopyrightIncomePaintingsSculptural_l",
                                              "g_CopyrightIncomeSuccessor_l",
                                              "g_CopyrightIncomeTranslationsLectures_l",
                                              "g_WorkIncome_l",
                                              "g_CapitalGains_c",
                                              "g_CapitalGainsSellsRealEstateFiveYear_c",
                                              "g_CapitalGainsSaleShareCapital_c",
                                              "g_CapitalGainsRealEstateThreeYear_c",
                                              "g_CapitalGainssaleOtherMovableAssets_c",
                                              "g_CapitalIncome_c",
                                              "g_GamesofChanceSpecific_c",
                                              "g_GamesofChanceBettingHouse_c",
                                              "g_GamesofChanceGeneral_c",
                                              "g_Lease_c",
                                              "g_LeaseBusiness_c",
                                              "g_Sublease_c",
                                              "g_IndustrialPropertyRights_c",
                                              "g_IndustrialPropertyRightsSuccessor_c",
                                              "g_Insurance_c",
                                              "g_Interests_c",
                                              "g_SolidWaste_c",
                                              "g_OtherIncome_c",
                                              "decile_group",
                                              "total_gross_l",
                                              "total_gross_c",
                                              "g_total_gross"

                                              )

                  # Select columns 
                  selected_gross_desc_tbl <- select(PIT_BU_list$t0, all_of(columns_gross_income))

                  
                  # Summarize the data
                  labor_capital <- selected_gross_desc_tbl %>%
                                  #select(decile_group,total_gross_l, total_gross_c) %>%
                                  dplyr::group_by(decile_group) %>%
                                  # dplyr::summarise(labor = sum(total_gross_l, na.rm = TRUE),
                                  #                  capital = sum(total_gross_c, na.rm = TRUE))
                    dplyr::summarise(# Calculate labor income
                      labor = sum(g_Wages_l + 
                        g_WagesDiplomaticConsular_l + 
                        g_TemporaryContracts_l + 
                        g_AgriculturalProductsOwn_l + 
                        g_AgriculturalProducts_l + 
                        g_IndependentActivity_l + 
                        g_CopyrightIncomeArtisticPhotography_l + 
                        g_CopyrightIncomeMusicBallet_l + 
                        g_CopyrightIncomePaintingsSculptural_l + 
                        g_CopyrightIncomeSuccessor_l + 
                        g_CopyrightIncomeTranslationsLectures_l + 
                        g_WorkIncome_l),
                      
                      # Calculate capital income
                      capital = sum(g_CapitalGains_c + 
                        g_CapitalGainsSellsRealEstateFiveYear_c + 
                        g_CapitalGainsSaleShareCapital_c + 
                        g_CapitalGainsRealEstateThreeYear_c + 
                        g_CapitalGainssaleOtherMovableAssets_c + 
                        g_CapitalIncome_c + 
                        g_Lease_c + 
                        g_LeaseBusiness_c + 
                        g_Sublease_c + 
                        g_IndustrialPropertyRights_c + 
                        g_IndustrialPropertyRightsSuccessor_c + 
                        g_Insurance_c + 
                        g_Interests_c + 
                        g_SolidWaste_c + 
                        g_OtherIncome_c)
                    )
                    
              
                  # Reshape the data into long format
                  long_labor_capital <- labor_capital %>%
                    gather(key = "gross_income", value = "value", labor, capital)
                  
                  # Reverse the order of the factors for 'gross_income'
                  long_labor_capital$gross_income <- factor(long_labor_capital$gross_income, levels = c("labor", "capital"))
                  
                  

# II. Type of Income ---------------------------------------------------------------------

types_labor_capital_tbl<-selected_gross_desc_tbl%>%
  dplyr::mutate(
                        labor_wages=g_Wages_l + g_WagesDiplomaticConsular_l,
                        labor_temporary_contract=g_TemporaryContracts_l,
                        labor_agricultural=g_AgriculturalProductsOwn_l + g_AgriculturalProducts_l,

                        labor_other= g_IndependentActivity_l
                                                                        + g_CopyrightIncomeArtisticPhotography_l
                                                                        + g_CopyrightIncomeMusicBallet_l
                                                                        + g_CopyrightIncomePaintingsSculptural_l
                                                                        + g_CopyrightIncomePaintingsSculptural_l
                                                                        + g_CopyrightIncomeSuccessor_l
                                                                        + g_CopyrightIncomeTranslationsLectures_l
                                                                        + g_WorkIncome_l,

                          capital_dividends_profits	=  g_CapitalGains_c
                                                        + g_CapitalGainsSellsRealEstateFiveYear_c
                                                        + g_CapitalGainsSaleShareCapital_c
                                                        + g_CapitalGainsRealEstateThreeYear_c
                                                        + g_CapitalGainssaleOtherMovableAssets_c
                                                        + g_CapitalIncome_c,

                          capital_games_of_chance	= g_Lease_c
                                                    + g_LeaseBusiness_c
                                                    + g_Sublease_c,

                          capital_property_income	=  g_Lease_c
                                                    + g_LeaseBusiness_c
                                                    + g_Sublease_c,

                          capital_other= g_IndustrialPropertyRights_c
                                          + g_IndustrialPropertyRightsSuccessor_c
                                          + g_Insurance_c
                                          + g_Interests_c
                                          + g_SolidWaste_c
                                          + g_OtherIncome_c

                        )%>%
                        select(decile_group,
                               labor_wages,labor_temporary_contract,labor_agricultural,labor_other,capital_dividends_profits,capital_games_of_chance,capital_property_income,capital_other)
                      
                      
                      
                            labor_capital_type <- types_labor_capital_tbl %>%
                              dplyr::group_by(decile_group) %>%
                              dplyr::summarise(labor_wages = sum(labor_wages, na.rm = TRUE),
                                               labor_temporary_contract = sum(labor_temporary_contract, na.rm = TRUE),
                                               labor_agricultural = sum(labor_agricultural, na.rm = TRUE),
                                               labor_other = sum(labor_other, na.rm = TRUE),
                                               capital_dividends_profits = sum(capital_dividends_profits, na.rm = TRUE),
                                               capital_games_of_chance = sum(capital_games_of_chance, na.rm = TRUE),
                                               capital_property_income = sum(capital_property_income, na.rm = TRUE),
                                               capital_other = sum(capital_other, na.rm = TRUE)
                                               
                              )

      # Reshape the data into long format
      labor_capital_type <- labor_capital_type %>%
        gather(key = "gross_income", value = "value", labor_wages,labor_temporary_contract,labor_agricultural,labor_other,capital_dividends_profits,capital_games_of_chance,capital_property_income,capital_other)
      
      # Reverse the order of the factors for 'gross_income'
      labor_capital_type$gross_income <- factor(labor_capital_type$gross_income, levels = c("labor_wages","labor_temporary_contract","labor_agricultural","labor_other","capital_dividends_profits","capital_games_of_chance","capital_property_income","capital_other"))


# III. Treemap GROSS INCOME --------------------------------------------------------------------


long_labor_capital_type <- types_labor_capital_tbl %>%
  dplyr::select(-c(decile_group)) %>%
  dplyr::summarise(labor_wages = sum(labor_wages, na.rm = TRUE),
                   labor_temporary_contract = sum(labor_temporary_contract, na.rm = TRUE),
                   labor_agricultural = sum(labor_agricultural, na.rm = TRUE),
                   labor_other = sum(labor_other, na.rm = TRUE),
                   capital_dividends_profits = sum(capital_dividends_profits, na.rm = TRUE),
                   capital_games_of_chance = sum(capital_games_of_chance, na.rm = TRUE),
                   capital_property_income = sum(capital_property_income, na.rm = TRUE),
                   capital_other = sum(capital_other, na.rm = TRUE)
                   
  )



# Convert the data to long format
long_labor_capital_type <- long_labor_capital_type %>%
  gather(key = "income_type", value = "value")


long_labor_capital_type$income_type<-factor( long_labor_capital_type$income_type)



long_labor_capital_type$TypeOfIncome <- "In billion LCU"



# IV. Structure of gross income by NACE sections ---------------------------------------------------------------------

columns_gross_nace_income <- c(
  "nace_section",
  #"g_total_gross"          
  "g_total_gross"
)


# Create a select statement with all the patterns

selected_gross_nace_tbl <- select(PIT_BU_list$t0, all_of(columns_gross_nace_income))


gross_nace_tbl <- selected_gross_nace_tbl %>%
  dplyr::group_by(nace_section) %>%
  #dplyr::summarise(g_total_gross = sum(g_total_gross, na.rm = TRUE)
  dplyr::summarise(g_total_gross = sum(g_total_gross, na.rm = TRUE)
  )

# # Convert the data to long format
gross_nace_tbl <- na.omit(gross_nace_tbl)
gross_nace_tbl<-left_join(gross_nace_tbl,df_nace_names,by=c("nace_section"="section"))
gross_nace_tbl$nace_section<-factor(gross_nace_tbl$nace_section)
gross_nace_tbl$TypeOfIncome <- "In billion LCU"

