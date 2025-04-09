'MAIN CALCULATIONS'

# Update the 'Value' column based on the condition
pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>%
  mutate(Value = ifelse(Parameters == 'rate_WagesDiplomaticConsular_l', 1, Value))

start.time <- proc.time()
# Setup parallel backend to use multiple processors
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
})
# Define functions in cluster
clusterExport(cl, c("pit_data", "scenarios", "base_year", "simulation_year", "get_sim_dataset", 
                    "pit_simulation_parameters_raw", "pit_simulation_parameters_updated",
                    "cal_weighting_fun",
                    "SSC_rate", "max_base_wage", "max_ssc", "PersonalAllowance", 
                    "vec_cal_ssc_w_fun", 
            # Labor
                    "vec_cal_tax_base_w_fun",
                    "vec_cal_pit_w_fun",
                    "vec_cal_tax_base_agr_fun",
                    "vec_cal_tax_base_CopIncArtPhoto_l_fun",
                    "vec_cal_tax_base_CopIncMusicBallet_l_fun",
                    "vec_cal_tax_base_CopIncPaintScu_l_fun",
                    "vec_cal_tax_base_CopIncTranslationsLectures_l_fun",
                    "vec_cal_tax_base_CopIncSuccessor_l_fun",
                    "vec_cal_tax_base_WorkIncome_l_fun",
                    "vec_cal_tax_base_other_fun",
          # Capital
                    "vec_cal_tax_base_IndPropRights_c_fun",
                    "vec_cal_tax_base_Lease_c_fun",
                    "vec_cal_tax_base_LeaseBusiness_c_fun",
                    "vec_cal_tax_base_SolidWaste_c_fun",
                    "vec_cal_tax_base_GamesofChanceSpec_c_fun",
                    "vec_cal_tax_base_GamesofChanceBettingHouse_c_fun",
                    "vec_cal_tax_base_CapGainsSaleShareCapital_c_fun",
                    "vec_cal_tax_base_CapGainsRealEstateThreeYear_c_fun",
                    "vec_cal_tax_base_CapGainssaleOtherMovAssets_c_fun",
                    "vec_cal_tax_base_CapGainsSellsRealEstateFiveYear_c_fun",
                    "vec_cal_tax_bases_Insurance_c_fun",
                    "vec_cal_tti_c_a_fun",
                    "vec_cal_tti_c_g_fun",
                    "vec_cal_pit_c_fun",
                    "vec_cal_total_pit_fun",
                    "filter_columns"
))

# Parallel computation for each scenario
parLapply(cl, seq_along(scenarios), function(scenario_index) {
                      scenario <- scenarios[scenario_index]
                      current_year <- base_year + (scenario_index - 1)
                      weights <- pit_data$weights[[scenario]]
                      growth_factors <- pit_data$growth_factors[Year == current_year & scenarios == scenario, g_Wages_l]
                      simulation_parameters_raw <- pit_simulation_parameters_raw
                      parameters_raw <- setNames(simulation_parameters_raw$Value, simulation_parameters_raw$Parameters)
                      # Determine the parameters to use for PIT_SIM based on the current year and simulation year
                      selected_parameters <- get_sim_dataset(current_year, simulation_year, base_year)
                      parameters_updated <- setNames(selected_parameters$Value, selected_parameters$Parameters)
                      columns_to_process <- names(pit_data$sample)[grepl("^g_", names(pit_data$sample))]
 # Process PIT_BU ------------
                      PIT_weighted <- cal_weighting_fun(pit_data$sample, weights, growth_factors, columns_to_process)
                      
                      PIT_BU<-PIT_weighted
                      
                      PIT_BU[, calc_ssc := vec_cal_ssc_w_fun(total_ssc, g_Wages_l_adjusted, parameters_raw["SSC_rate"], max_base_wage, max_ssc)]
            # Labor
                    #PIT_BU[, personal_allowance_new := vec_per_all_fun(g_total_personal_allowance_l_adjusted, parameters_raw["weight_personal_allowance_w"])]
                    #PIT_BU[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted, calc_ssc, personal_allowance_new,g_WagesDiplomaticConsular_l, d_WagesDiplomaticConsular_l)]
                    #PIT_BU[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted, calc_ssc, g_total_personal_allowance_l_adjusted, parameters_raw["weight_personal_allowance_w"],g_WagesDiplomaticConsular_l, g_WagesDiplomaticConsular_l_adjusted)] # old
                    #PIT_BU[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted, calc_ssc, g_total_personal_allowance_l_adjusted, parameters_raw["weight_personal_allowance_w"],g_WagesDiplomaticConsular_l, g_d_WagesDiplomaticConsular_l_adjusted)]
                    
                    PIT_BU[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted,calc_ssc, g_total_personal_allowance_l_adjusted, parameters_raw["weight_personal_allowance_w"], g_WagesDiplomaticConsular_l_adjusted, g_d_WagesDiplomaticConsular_l_adjusted, g_d_total_tax_reduction_l, parameters_raw["tax_credit_donation"])] 
                    
                    # Donation test
                    PIT_BU[, pit_tax_donation := ifelse(g_d_total_tax_reduction_l > 0, # Calculate tax base wages1 (applies when g_d_total_tax_reduction_l > 0)
                                                       g_Wages_l_adjusted - calc_ssc - g_total_personal_allowance_l_adjusted - parameters_raw["tax_credit_donation"]/0.1 ,
                                                       0)*parameters_raw["rate1"]] #NEW
                             
                    
                    
                    # Income on the basis of sale of own agricultural products 
                    PIT_BU[, tax_base_agr := vec_cal_tax_base_agr_fun(g_AgriculturalProductsOwn_l_adjusted, parameters_raw["rate_ded_income_agr_l"])]
                    PIT_BU[, pit_tax_agr := tax_base_agr*parameters_raw["rate1"]] #NEW
                    
                    # Copyright Income
                    PIT_BU[, tax_base_CopyrightIncomeArtisticPhotography_l := vec_cal_tax_base_CopIncArtPhoto_l_fun(g_CopyrightIncomeArtisticPhotography_l_adjusted, parameters_raw["rate_deductions_CopyrightIncomeArtisticPhotography_l"])]
                    PIT_BU[, pit_tax_CopyrightIncomeArtisticPhotography_l := tax_base_CopyrightIncomeArtisticPhotography_l*parameters_raw["rate1"]] #NEW
                    
                    # CopyrightIncomeMusicBallet
                    PIT_BU[, tax_base_CopyrightIncomeMusicBallet_l := vec_cal_tax_base_CopIncMusicBallet_l_fun(g_CopyrightIncomeMusicBallet_l_adjusted, parameters_raw["rate_deductions_CopyrightIncomeMusicBallet_l"])]
                    PIT_BU[, pit_tax_CopyrightIncomeMusicBallet_l := tax_base_CopyrightIncomeMusicBallet_l*parameters_raw["rate1"]] #NEW
                    
                    # CopyrightIncomePaintingsSculptural_l
                    PIT_BU[, tax_base_CopyrightIncomePaintingsSculptural_l := vec_cal_tax_base_CopIncPaintScu_l_fun(g_CopyrightIncomePaintingsSculptural_l_adjusted,  parameters_raw["rate_deductions_CopyrightIncomePaintingsSculptural_l"])]
                    PIT_BU[, pit_tax_CopyrightIncomePaintingsSculptural_l := tax_base_CopyrightIncomePaintingsSculptural_l*parameters_raw["rate1"]] #NEW
                    
                    # CopyrightIncomeTranslationsLectures_l
                    PIT_BU[, tax_base_CopyrightIncomeTranslationsLectures_l := vec_cal_tax_base_CopIncTranslationsLectures_l_fun(g_CopyrightIncomeTranslationsLectures_l_adjusted, parameters_raw["rate_deductions_CopyrightIncomeTranslationsLectures_l"])]
                    PIT_BU[, pit_tax_CopyrightIncomeTranslationsLectures_l := tax_base_CopyrightIncomeTranslationsLectures_l*parameters_raw["rate1"]] #NEW
                    
                    # CopyrightIncomeSuccessor_l
                    PIT_BU[, tax_base_CopyrightIncomeSuccessor_l := vec_cal_tax_base_CopIncSuccessor_l_fun(g_CopyrightIncomeSuccessor_l_adjusted, parameters_raw["rate_deductions_CopyrightIncomeSuccessor_l"])]
                    PIT_BU[, pit_tax_CopyrightIncomeSuccessor_l := tax_base_CopyrightIncomeSuccessor_l*parameters_raw["rate1"]] #NEW
                    
                    # WorkIncome_l
                    PIT_BU[, tax_base_WorkIncome_l := vec_cal_tax_base_WorkIncome_l_fun(g_WorkIncome_l_adjusted, parameters_updated["rate_deductions_WorkIncome_l"])]
                    PIT_BU[, pit_tax_WorkIncome_l := tax_base_WorkIncome_l*parameters_raw["rate1"]] #NEW
                    
                    # Sum of tax bases
                    PIT_BU[, tax_base_other := vec_cal_tax_base_other_fun(tax_base_agr, tax_base_CopyrightIncomeArtisticPhotography_l, tax_base_CopyrightIncomeMusicBallet_l, tax_base_CopyrightIncomePaintingsSculptural_l, tax_base_CopyrightIncomeTranslationsLectures_l, tax_base_WorkIncome_l, g_TemporaryContracts_l_adjusted, g_AgriculturalProducts_l_adjusted, g_IndependentActivity_l_adjusted, tax_base_CopyrightIncomeSuccessor_l)]
                    #PIT_BU[, tti_w_I := vec_cal_tti_w_I_fun(tax_base_w, tax_base_other)] # Tax base labor
                    #PIT_BU[, pit_w := vec_cal_pit_w_fun(tti_w_I, parameters_raw["rate1"], parameters_raw["rate2"], parameters_raw["rate3"], parameters_raw["rate4"], parameters_raw["tbrk1"], parameters_raw["tbrk2"], parameters_raw["tbrk3"])]
                    PIT_BU[, pit_w := vec_cal_pit_w_fun(tax_base_w,tax_base_other, parameters_raw["rate1"], parameters_raw["rate2"], parameters_raw["rate3"], parameters_raw["rate4"], parameters_raw["tbrk1"], parameters_raw["tbrk2"], parameters_raw["tbrk3"])]
                    
          # Capital
                    PIT_BU[, tax_base_IndustrialPropertyRights_c := vec_cal_tax_base_IndPropRights_c_fun(g_IndustrialPropertyRights_c_adjusted, parameters_raw["rate_deductions_IndustrialPropertyRights_c"])]
                    PIT_BU[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*parameters_raw["capital_income_rate_a"]] #NEW
                    
                    PIT_BU[, tax_base_Lease_c := vec_cal_tax_base_Lease_c_fun(g_Lease_c_adjusted, parameters_updated["rate_deductions_Lease_c"])]
                    PIT_BU[, pit_tax_Lease_c := tax_base_Lease_c*parameters_raw["capital_income_rate_a"]] #NEW
                    
                    PIT_BU[, tax_base_LeaseBusiness_c := vec_cal_tax_base_LeaseBusiness_c_fun(g_LeaseBusiness_c_adjusted, parameters_raw["rate_deductions_LeaseBusiness_c"])]
                    PIT_BU[, pit_LeaseBusiness_c := tax_base_LeaseBusiness_c*parameters_raw["capital_income_rate_a"]] #NEW
                    
                    PIT_BU[, tax_base_SolidWaste_c := vec_cal_tax_base_SolidWaste_c_fun(g_SolidWaste_c_adjusted,  parameters_raw["rate_deductions_SolidWaste_c"])]
                    PIT_BU[, pit_SolidWaste_c := tax_base_SolidWaste_c*parameters_raw["capital_income_rate_a"]] #NEW
                    
                    PIT_BU[, tax_base_GamesofChanceSpecific_c := vec_cal_tax_base_GamesofChanceSpec_c_fun(g_GamesofChanceSpecific_c_adjusted, parameters_raw["weight_deductions_GamesofChanceSpecific_c"])]
                    PIT_BU[, pit_GamesofChanceSpecific_c := tax_base_GamesofChanceSpecific_c*parameters_raw["capital_income_rate_g"]] #NEW
                    
                    #PIT_BU[, tax_base_GamesofChanceBettingHouse_c := vec_cal_tax_base_GamesofChanceBettingHouse_c_fun(g_GamesofChanceBettingHouse_c_adjusted, parameters_raw["weight_deductions_GamesofChanceBettingHouse_c"])]
                    PIT_BU[, tax_base_GamesofChanceBettingHouse_c := vec_cal_tax_base_GamesofChanceBettingHouse_c_fun(g_GamesofChanceBettingHouse_c_adjusted,g_d_GamesofChanceBettingHouse_c_adjusted, parameters_raw["weight_deductions_GamesofChanceBettingHouse_c"])]
                    
                    PIT_BU[, pit_GamesofChanceBettingHouse_c := tax_base_GamesofChanceBettingHouse_c*parameters_raw["capital_income_rate_g"]] #NEW
                    
                    PIT_BU[, tax_base_CapitalGainsSaleShareCapital_c := vec_cal_tax_base_CapGainsSaleShareCapital_c_fun(g_CapitalGainsSaleShareCapital_c_adjusted, parameters_raw["rate_deductions_CapitalGainsSaleShareCapital_c"])]
                    PIT_BU[, pit_CapitalGainsSaleShareCapital_c := tax_base_CapitalGainsSaleShareCapital_c*parameters_raw["capital_income_rate_c"]] #NEW
                    
                    PIT_BU[, tax_base_CapitalGainsRealEstateThreeYear_c := vec_cal_tax_base_CapGainsRealEstateThreeYear_c_fun(g_CapitalGainsRealEstateThreeYear_c_adjusted, parameters_raw["rate_deductions_CapitalGainsRealEstateThreeYear_c"])]
                    PIT_BU[, pit_CapitalGainsRealEstateThreeYear_c := tax_base_CapitalGainsRealEstateThreeYear_c*parameters_raw["capital_income_rate_c"]] #NEW
                    
                    PIT_BU[, tax_base_CapitalGainssaleOtherMovableAssets_c := vec_cal_tax_base_CapGainssaleOtherMovAssets_c_fun(g_CapitalGainssaleOtherMovableAssets_c_adjusted, parameters_raw["rate_deductions_CapitalGainssaleOtherMovableAssets_c"])]
                    PIT_BU[, pit_CapitalGainssaleOtherMovableAssets_c := tax_base_CapitalGainssaleOtherMovableAssets_c*parameters_raw["capital_income_rate_c"]] #NEW
                    
                    PIT_BU[, tax_base_CapitalGainsSellsRealEstateFiveYear_c := vec_cal_tax_base_CapGainsSellsRealEstateFiveYear_c_fun(g_CapitalGainsSellsRealEstateFiveYear_c_adjusted, parameters_raw["weight_deductions_CapitalGainsSellsRealEstateFiveYear_c"])]
                    PIT_BU[, pit_CapitalGainsSellsRealEstateFiveYear_c := tax_base_CapitalGainsSellsRealEstateFiveYear_c*parameters_raw["capital_income_rate_c"]] #NEW
                    
                    PIT_BU[, tax_base_Insurance_c := vec_cal_tax_bases_Insurance_c_fun(g_Insurance_c_adjusted,  parameters_raw["weight_deductions_Insurance_c"])]
                    PIT_BU[, pit_Insurance_c := tax_base_Insurance_c*parameters_raw["capital_income_rate_c"]] #NEW
                    
                    PIT_BU[, tti_c_a := vec_cal_tti_c_a_fun(g_IndustrialPropertyRightsSuccessor_c_adjusted, g_Insurance_c_adjusted, g_Interests_c_adjusted, g_OtherIncome_c_adjusted, g_Sublease_c_adjusted, tax_base_IndustrialPropertyRights_c, tax_base_Lease_c, tax_base_LeaseBusiness_c, tax_base_SolidWaste_c, tax_base_Insurance_c,g_CapitalIncome_c_adjusted, tax_base_CapitalGainssaleOtherMovableAssets_c, tax_base_CapitalGainsSellsRealEstateFiveYear_c, tax_base_CapitalGainsRealEstateThreeYear_c, tax_base_CapitalGainsSaleShareCapital_c)]
                    
                    PIT_BU[, tti_c_g := vec_cal_tti_c_g_fun(tax_base_GamesofChanceSpecific_c, tax_base_GamesofChanceBettingHouse_c, g_GamesofChanceGeneral_c_adjusted)]
                    
                    'OVDE OD DVE DA SE NAPRAVI EDNA FUNKCIJA, ODNOSNO EDNA SAMO ZA OSTATOKOT OD KAPITAL STO E SO 5 A VTORATA IGRI NA SREKA'
                    
                    #PIT_BU[, tti_c_c := vec_cal_tti_c_c_fun(g_CapitalIncome_c_adjusted, tax_base_CapitalGainssaleOtherMovableAssets_c, tax_base_CapitalGainsSellsRealEstateFiveYear_c, tax_base_CapitalGainsRealEstateThreeYear_c, tax_base_CapitalGainsSaleShareCapital_c)]
                    #PIT_BU[, pit_c := vec_cal_pit_c_fun(parameters_raw["capital_income_rate_a"], parameters_raw["capital_income_rate_g"], tti_c_a, tti_c_g, tti_c_c, parameters_raw["capital_income_rate_c"])]
                    PIT_BU[, pit_c := vec_cal_pit_c_fun(parameters_raw["capital_income_rate_a"], parameters_raw["capital_income_rate_g"], tti_c_a, tti_c_g)]
                    PIT_BU[, calc_pitax := vec_cal_total_pit_fun(pit_w,pit_c)]
                    PIT_BU <- filter_columns(PIT_BU)
                    
 # Process PIT_SIM ------------------------------------------
                    #PIT_SIM <- cal_weighting_fun(pit_data$sample, weights, growth_factors, columns_to_process)
                    PIT_SIM<-PIT_weighted
                    PIT_SIM[, calc_ssc := vec_cal_ssc_w_fun(total_ssc, g_Wages_l_adjusted, parameters_updated["SSC_rate"], max_base_wage, max_ssc)]
            # Labor
                    #PIT_SIM[, personal_allowance_new := vec_per_all_fun(g_total_personal_allowance_l_adjusted, parameters_updated["weight_personal_allowance_w"])]
                   # PIT_SIM[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted, calc_ssc, g_total_personal_allowance_l_adjusted, parameters_updated["weight_personal_allowance_w"],g_WagesDiplomaticConsular_l, g_WagesDiplomaticConsular_l_adjusted)] #old
                    #PIT_SIM[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted, calc_ssc, g_total_personal_allowance_l_adjusted, parameters_updated["weight_personal_allowance_w"],g_WagesDiplomaticConsular_l, g_d_WagesDiplomaticConsular_l_adjusted)]
                    PIT_SIM[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted,calc_ssc, g_total_personal_allowance_l_adjusted, parameters_updated["weight_personal_allowance_w"], g_WagesDiplomaticConsular_l_adjusted, g_d_WagesDiplomaticConsular_l_adjusted, g_d_total_tax_reduction_l, parameters_updated["tax_credit_donation"])] 

                    # Donation test
                    PIT_SIM[, pit_tax_donation := ifelse(g_d_total_tax_reduction_l > 0, # Calculate tax base wages1 (applies when g_d_total_tax_reduction_l > 0)
                                                        g_Wages_l_adjusted - calc_ssc - g_total_personal_allowance_l_adjusted - parameters_updated["tax_credit_donation"]/0.1 ,
                                                        0)*parameters_updated["rate1"]] #NEW
                    
                    # Income on the basis of sale of own agricultural products 
                    PIT_SIM[, tax_base_agr := vec_cal_tax_base_agr_fun(g_AgriculturalProductsOwn_l_adjusted, parameters_updated["rate_ded_income_agr_l"])]
                    PIT_SIM[, pit_tax_agr := tax_base_agr*parameters_updated["rate1"]] #NEW
                    
                    # Copyright Income
                    PIT_SIM[, tax_base_CopyrightIncomeArtisticPhotography_l := vec_cal_tax_base_CopIncArtPhoto_l_fun(g_CopyrightIncomeArtisticPhotography_l_adjusted, parameters_updated["rate_deductions_CopyrightIncomeArtisticPhotography_l"])]
                    PIT_SIM[, pit_tax_CopyrightIncomeArtisticPhotography_l := tax_base_CopyrightIncomeArtisticPhotography_l*parameters_updated["rate1"]] #NEW
                    
                    # CopyrightIncomeMusicBallet
                    PIT_SIM[, tax_base_CopyrightIncomeMusicBallet_l := vec_cal_tax_base_CopIncMusicBallet_l_fun(g_CopyrightIncomeMusicBallet_l_adjusted, parameters_updated["rate_deductions_CopyrightIncomeMusicBallet_l"])]
                    PIT_SIM[, pit_tax_CopyrightIncomeMusicBallet_l := tax_base_CopyrightIncomeMusicBallet_l*parameters_updated["rate1"]] #NEW
                    
                    # CopyrightIncomePaintingsSculptural_l
                    PIT_SIM[, tax_base_CopyrightIncomePaintingsSculptural_l := vec_cal_tax_base_CopIncPaintScu_l_fun(g_CopyrightIncomePaintingsSculptural_l_adjusted,  parameters_updated["rate_deductions_CopyrightIncomePaintingsSculptural_l"])]
                    PIT_SIM[, pit_tax_CopyrightIncomePaintingsSculptural_l := tax_base_CopyrightIncomePaintingsSculptural_l*parameters_updated["rate1"]] #NEW
                    
                    # CopyrightIncomeTranslationsLectures_l
                    PIT_SIM[, tax_base_CopyrightIncomeTranslationsLectures_l := vec_cal_tax_base_CopIncTranslationsLectures_l_fun(g_CopyrightIncomeTranslationsLectures_l_adjusted, parameters_updated["rate_deductions_CopyrightIncomeTranslationsLectures_l"])]
                    PIT_SIM[, pit_tax_CopyrightIncomeTranslationsLectures_l := tax_base_CopyrightIncomeTranslationsLectures_l*parameters_updated["rate1"]] #NEW
                    
                    #CopyrightIncomeSuccessor_l
                    PIT_SIM[, tax_base_CopyrightIncomeSuccessor_l := vec_cal_tax_base_CopIncSuccessor_l_fun(g_CopyrightIncomeSuccessor_l_adjusted, parameters_updated["rate_deductions_CopyrightIncomeSuccessor_l"])]
                    PIT_SIM[, pit_tax_CopyrightIncomeSuccessor_l := tax_base_CopyrightIncomeSuccessor_l*parameters_updated["rate1"]] #NEW
                    
                    # WorkIncome_l
                    PIT_SIM[, tax_base_WorkIncome_l := vec_cal_tax_base_WorkIncome_l_fun(g_WorkIncome_l_adjusted, parameters_updated["rate_deductions_WorkIncome_l"])]
                    PIT_SIM[, pit_tax_WorkIncome_l := tax_base_WorkIncome_l*parameters_updated["rate1"]] #NEW
                    
                    # Sum of tax bases
                    PIT_SIM[, tax_base_other := vec_cal_tax_base_other_fun(tax_base_agr, tax_base_CopyrightIncomeArtisticPhotography_l, tax_base_CopyrightIncomeMusicBallet_l, tax_base_CopyrightIncomePaintingsSculptural_l, tax_base_CopyrightIncomeTranslationsLectures_l, tax_base_WorkIncome_l, g_TemporaryContracts_l_adjusted, g_AgriculturalProducts_l_adjusted, g_IndependentActivity_l_adjusted, tax_base_CopyrightIncomeSuccessor_l)]
                    #PIT_SIM[, tti_w_I := vec_cal_tti_w_I_fun(tax_base_w, tax_base_other)]
    
                    #PIT_SIM[, pit_w := vec_cal_pit_w_fun(tti_w_I, parameters_updated["rate1"], parameters_updated["rate2"], parameters_updated["rate3"], parameters_updated["rate4"], parameters_updated["tbrk1"], parameters_updated["tbrk2"], parameters_updated["tbrk3"])]
                    PIT_SIM[, pit_w := vec_cal_pit_w_fun(tax_base_w,tax_base_other,parameters_updated["rate1"], parameters_updated["rate2"], parameters_updated["rate3"], parameters_updated["rate4"], parameters_updated["tbrk1"], parameters_updated["tbrk2"], parameters_updated["tbrk3"])]
                    
        # Capital
                    PIT_SIM[, tax_base_IndustrialPropertyRights_c := vec_cal_tax_base_IndPropRights_c_fun(g_IndustrialPropertyRights_c_adjusted, parameters_updated["rate_deductions_IndustrialPropertyRights_c"])]
                    PIT_SIM[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*parameters_updated["capital_income_rate_a"]] #NEW
                    
                    PIT_SIM[, tax_base_Lease_c := vec_cal_tax_base_Lease_c_fun(g_Lease_c_adjusted, parameters_updated["rate_deductions_Lease_c"])]
                    PIT_SIM[, pit_tax_Lease_c := tax_base_Lease_c*parameters_updated["capital_income_rate_a"]] 
                    
                    PIT_SIM[, tax_base_LeaseBusiness_c := vec_cal_tax_base_LeaseBusiness_c_fun(g_LeaseBusiness_c_adjusted, parameters_updated["rate_deductions_LeaseBusiness_c"])]
                    PIT_SIM[, pit_LeaseBusiness_c := tax_base_LeaseBusiness_c*parameters_updated["capital_income_rate_a"]] #NEW
                    
                    PIT_SIM[, tax_base_SolidWaste_c := vec_cal_tax_base_SolidWaste_c_fun(g_SolidWaste_c_adjusted,  parameters_updated["rate_deductions_SolidWaste_c"])]
                    PIT_SIM[, pit_SolidWaste_c := tax_base_SolidWaste_c*parameters_updated["capital_income_rate_a"]] #NEW
                    
                    PIT_SIM[, tax_base_GamesofChanceSpecific_c := vec_cal_tax_base_GamesofChanceSpec_c_fun(g_GamesofChanceSpecific_c_adjusted, parameters_updated["weight_deductions_GamesofChanceSpecific_c"])]
                    PIT_SIM[, pit_GamesofChanceSpecific_c := tax_base_GamesofChanceSpecific_c*parameters_updated["capital_income_rate_g"]] #NEW
                    
                    PIT_SIM[, tax_base_GamesofChanceBettingHouse_c := vec_cal_tax_base_GamesofChanceBettingHouse_c_fun(g_GamesofChanceBettingHouse_c_adjusted,g_d_GamesofChanceBettingHouse_c_adjusted, parameters_updated["weight_deductions_GamesofChanceBettingHouse_c"])]
                    #PIT_SIM[, tax_base_GamesofChanceBettingHouse_c := vec_cal_tax_base_GamesofChanceBettingHouse_c_fun(g_GamesofChanceBettingHouse_c_adjusted, parameters_updated["weight_deductions_GamesofChanceBettingHouse_c"])]
                    PIT_SIM[, pit_GamesofChanceBettingHouse_c := tax_base_GamesofChanceBettingHouse_c*parameters_updated["capital_income_rate_g"]] #NEW
                    
                    PIT_SIM[, tax_base_CapitalGainsSaleShareCapital_c := vec_cal_tax_base_CapGainsSaleShareCapital_c_fun(g_CapitalGainsSaleShareCapital_c_adjusted, parameters_updated["rate_deductions_CapitalGainsSaleShareCapital_c"])]
                    PIT_SIM[, pit_CapitalGainsSaleShareCapital_c := tax_base_CapitalGainsSaleShareCapital_c*parameters_updated["capital_income_rate_c"]] #NEW
                    
                    PIT_SIM[, tax_base_CapitalGainsRealEstateThreeYear_c := vec_cal_tax_base_CapGainsRealEstateThreeYear_c_fun(g_CapitalGainsRealEstateThreeYear_c_adjusted, parameters_updated["rate_deductions_CapitalGainsRealEstateThreeYear_c"])]
                    PIT_SIM[, pit_CapitalGainsRealEstateThreeYear_c := tax_base_CapitalGainsRealEstateThreeYear_c*parameters_updated["capital_income_rate_c"]] #NEW
                    
                    PIT_SIM[, tax_base_CapitalGainssaleOtherMovableAssets_c := vec_cal_tax_base_CapGainssaleOtherMovAssets_c_fun(g_CapitalGainssaleOtherMovableAssets_c_adjusted, parameters_updated["rate_deductions_CapitalGainssaleOtherMovableAssets_c"])]
                    PIT_SIM[, pit_CapitalGainssaleOtherMovableAssets_c := tax_base_CapitalGainssaleOtherMovableAssets_c*parameters_updated["capital_income_rate_c"]] #NEW
                    
                    PIT_SIM[, tax_base_CapitalGainsSellsRealEstateFiveYear_c := vec_cal_tax_base_CapGainsSellsRealEstateFiveYear_c_fun(g_CapitalGainsSellsRealEstateFiveYear_c_adjusted, parameters_updated["weight_deductions_CapitalGainsSellsRealEstateFiveYear_c"])]
                    PIT_SIM[, pit_CapitalGainsSellsRealEstateFiveYear_c := tax_base_CapitalGainsSellsRealEstateFiveYear_c*parameters_updated["capital_income_rate_c"]] #NEW
                    
                    PIT_SIM[, tax_base_Insurance_c := vec_cal_tax_bases_Insurance_c_fun(g_Insurance_c_adjusted,  parameters_updated["weight_deductions_Insurance_c"])]
                    PIT_SIM[, pit_Insurance_c := tax_base_Insurance_c*parameters_updated["capital_income_rate_c"]] #NEW
                    
                    #PIT_SIM[, tti_c_a := vec_cal_tti_c_a_fun(g_IndustrialPropertyRightsSuccessor_c_adjusted, g_Insurance_c_adjusted, g_Interests_c_adjusted, g_OtherIncome_c_adjusted, g_Sublease_c_adjusted, tax_base_IndustrialPropertyRights_c, tax_base_Lease_c, tax_base_LeaseBusiness_c, tax_base_SolidWaste_c, tax_base_Insurance_c)]
                    PIT_SIM[, tti_c_a := vec_cal_tti_c_a_fun(g_IndustrialPropertyRightsSuccessor_c_adjusted, g_Insurance_c_adjusted, g_Interests_c_adjusted, g_OtherIncome_c_adjusted, g_Sublease_c_adjusted, tax_base_IndustrialPropertyRights_c, tax_base_Lease_c, tax_base_LeaseBusiness_c, tax_base_SolidWaste_c, tax_base_Insurance_c,g_CapitalIncome_c_adjusted, tax_base_CapitalGainssaleOtherMovableAssets_c, tax_base_CapitalGainsSellsRealEstateFiveYear_c, tax_base_CapitalGainsRealEstateThreeYear_c, tax_base_CapitalGainsSaleShareCapital_c)]
                     PIT_SIM[, tti_c_g := vec_cal_tti_c_g_fun(tax_base_GamesofChanceSpecific_c, tax_base_GamesofChanceBettingHouse_c, g_GamesofChanceGeneral_c_adjusted)]
                    #PIT_SIM[, tti_c_c := vec_cal_tti_c_c_fun(g_CapitalIncome_c_adjusted, tax_base_CapitalGainssaleOtherMovableAssets_c, tax_base_CapitalGainsSellsRealEstateFiveYear_c, tax_base_CapitalGainsRealEstateThreeYear_c, tax_base_CapitalGainsSaleShareCapital_c)]
                    #PIT_SIM[, pit_c := vec_cal_pit_c_fun(parameters_updated["capital_income_rate_a"], parameters_updated["capital_income_rate_g"], tti_c_a, tti_c_g, tti_c_c, parameters_updated["capital_income_rate_c"])]
                     PIT_SIM[, pit_c := vec_cal_pit_c_fun(parameters_updated["capital_income_rate_a"], parameters_updated["capital_income_rate_g"], tti_c_a, tti_c_g )]
                    
                    PIT_SIM[, calc_pitax := vec_cal_total_pit_fun(pit_w,pit_c)]
                    PIT_SIM <- filter_columns(PIT_SIM)

  list(PIT_BU = PIT_BU, PIT_SIM = PIT_SIM, scenario = scenario)
}) -> results

# Stop the cluster
stopCluster(cl)

              # Aggregation of results in tables
              for (res in results) {
                PIT_BU_list[[res$scenario]] <- res$PIT_BU
                PIT_SIM_list[[res$scenario]] <- res$PIT_SIM
              }
              
              # Function to sum the specified columns in the list and store the results in a data frame
              summarize_PIT <- function(PIT_list, suffix) {
                                          result <- data.frame(
                                            scenarios = character(),
                                            stringsAsFactors = FALSE
                                          )
                
                for (scenario in names(PIT_list)) {
                  df <- PIT_list[[scenario]]
                  calc_columns <- names(df)[grepl("^(calc|pit)", names(df))]
                  calc_sums <- sapply(df[, ..calc_columns], function(col) sum(col, na.rm = TRUE))
                  scenario_result <- data.frame(
                    scenarios = scenario,
                    as.list(calc_sums),
                    stringsAsFactors = FALSE
                  )
                  result <- rbind(result, scenario_result)
                }
                names(result)[-1] <- paste0(names(result)[-1], suffix)
                
                return(result)
              }
              
              summary_SIM <- summarize_PIT(PIT_SIM_list, "_sim")
              summary_BU <- summarize_PIT(PIT_BU_list, "_bu")
              
              merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
              merged_PIT_BU_SIM$year <- as.character(forecast_horizon)
              merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]
              
              numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
              merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06
              
              print(merged_PIT_BU_SIM)
              # rm(results,res)


#   Weighted percentiles & deciles -------------------------------------------

              
              # Function to make breaks unique from OLD CODE !!!! 
              # make_breaks_unique <- function(breaks) {
              #   if (length(unique(breaks)) != length(breaks)) {
              #     breaks <- breaks + cumsum(c(0, diff(breaks) == 0)) * .Machine$double.eps
              #   }
              #   return(breaks)
              # }
              # 
              # Test new
              
              make_breaks_unique <- function(breaks) {
                if (length(unique(breaks)) != length(breaks)) {
                  breaks <- breaks + cumsum(c(0, diff(breaks) == 0)) * .Machine$double.eps * 1000
                }
                return(breaks)
              }
              
              
              
              # Define the function for weighted deciles
              cal_weighted_deciles <- function(g_total_gross, weight) {
                deciles <- wtd.quantile(g_total_gross, weights = weight, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
                deciles <- make_breaks_unique(deciles)  # Ensure breaks are unique
                decile_group <- cut(g_total_gross, breaks = deciles, include.lowest = TRUE, labels = FALSE)
                return(decile_group)
              }
              
              # Define the function for weighted centiles
              cal_weighted_centiles <- function(g_total_gross, weight, centiles = seq(0, 1, by = 0.01)) {
                boundaries <- wtd.quantile(g_total_gross, weights = weight, probs = centiles, na.rm = TRUE)
                boundaries <- make_breaks_unique(boundaries)  # Ensure breaks are unique
                centile_group <- cut(g_total_gross, breaks = boundaries, include.lowest = TRUE, labels = FALSE)
                return(centile_group)
              }
              
              # Apply the functions to each data frame in the PIT_BU_list
              for (name in names(PIT_BU_list)) {
                df <- PIT_BU_list[[name]]
                df$decile_group <- cal_weighted_deciles(df$g_total_gross, df$weight)
                df$centile_group <- cal_weighted_centiles(df$g_total_gross, df$weight)
                PIT_BU_list[[name]] <- df
              }
              
              # Apply the functions to each data frame in the PIT_SIM_list
              for (name in names(PIT_SIM_list)) {
                df <- PIT_SIM_list[[name]]
                df$decile_group <- cal_weighted_deciles(df$g_total_gross, df$weight)
                df$centile_group <- cal_weighted_centiles(df$g_total_gross, df$weight)
                PIT_SIM_list[[name]] <- df
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
                          filter(variable=='calc_pitax')
                        
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

              # Extract data in Excel Format
              excel_table <- createWorkbook()
              addWorksheet(excel_table, sheetName = "simulation")
              writeData(excel_table, sheet = "simulation", x = merged_PIT_BU_SIM)
              saveWorkbook(excel_table, file = paste("pit-output_summary-", Sys.Date(), ".xlsx", sep = ""), overwrite = TRUE)
              
               
              
              end.time <- proc.time()
              save.time <- end.time - start.time
              cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")
