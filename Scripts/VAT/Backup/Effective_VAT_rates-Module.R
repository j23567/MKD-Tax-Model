'
    ESTIMATION OF EFFECTIVE VAT RATE BY SECTORS AND HBS ANALISIS
'
StandardVATRate<-0.18
PreferentialVATRate_1<-0.05




suppressMessages({

# I.Business as usual -----------------------------------------------------
            # 1. ESTIMATION OF EFFECTIVE VAT RATE BY SECTORS -------------------------------------
                  # 1.1 Effective rate for : Industries,HH,NPISH and GOVERMENT First approach ----------------------------
            
            TAX_BASE_TOTAL<-CPA_PRODUCTS_BU$Est_Rev%>%
                          dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Total,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government)%>%
                          dplyr::mutate(
                                        tax_base_INDUSTRIES=Total_Revenues_from_Intermediate_Inputs/StandardVATRate,
                                        tax_base_HH=Final_Demand_HH/StandardVATRate,
                                        tax_base_NPISH=Final_Demand_NPISH/StandardVATRate,
                                        tax_base_GOVERMENT=Final_Demand_Government/StandardVATRate,
                                        tax_base_TOTAL=Final_Demand_Total/StandardVATRate)%>%
                          dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_INDUSTRIES,tax_base_HH,tax_base_NPISH,tax_base_GOVERMENT,tax_base_TOTAL)
                         TAX_BASE_TOTAL[2:6]<-abs(TAX_BASE_TOTAL[2:6])
            
            
            # Estimation of effective VAT rates based on SUT tables
           # EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIMULATION,by = c("PRODUCT_INDUSTRY_CODE"))%>%
             # NEW 7.12.2024
            # EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIMULATION_0,by = c("PRODUCT_INDUSTRY_CODE"))%>%
            
             # TEST NEW !!! 23/12/2024
             
              EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIM_SCENARIO_EST_CAL_FACTOR_BU,by = c("PRODUCT_INDUSTRY_CODE"))%>%
              dplyr::select(PRODUCT_INDUSTRY_CODE,StandardVATRate,PreferentialVATRate_1,tax_base_INDUSTRIES,
                            tax_base_HH,tax_base_NPISH,tax_base_GOVERMENT,tax_base_TOTAL,Current_Policy_Reduced_Rate,Current_Policy_Fully_Taxable,
                            Simulated_Policy_Reduced_Rate,Simulated_Policy_Fully_Taxable)%>%
              dplyr::mutate(
                          # VAT FROM PREFERENTIAL VAT RATES
                          VAT_PREFERENTIAL_R_INDUSTRIES=tax_base_INDUSTRIES*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                          VAT_PREFERENTIAL_R_HH=tax_base_HH*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                          VAT_PREFERENTIAL_R_NPISH=tax_base_NPISH*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                          VAT_PREFERENTIAL_R_GOVERMENT=tax_base_GOVERMENT*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                          VAT_PREFERENTIAL_R_TOTAL=tax_base_TOTAL*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                          # VAT FROM STANDARD RATES
                          VAT_STANDARD_R_INDUSTRIES=tax_base_INDUSTRIES*Simulated_Policy_Fully_Taxable*StandardVATRate,
                          VAT_STANDARD_R_HH=tax_base_HH*Simulated_Policy_Fully_Taxable*StandardVATRate,
                          VAT_STANDARD_R_NPISH=tax_base_NPISH*Simulated_Policy_Fully_Taxable*StandardVATRate,
                          VAT_STANDARD_R_GOVERMENT=tax_base_GOVERMENT*Simulated_Policy_Fully_Taxable*StandardVATRate,
                          VAT_STANDARD_R_TOTAL=tax_base_TOTAL*Simulated_Policy_Fully_Taxable*StandardVATRate,
                          # SUM VAT REVENUES FROM PREFERENTIAL PLUS VAT FROM STANDARD RATE
                          VAT_R_INDUSTRIES= VAT_PREFERENTIAL_R_INDUSTRIES+VAT_STANDARD_R_INDUSTRIES,
                          VAT_R_HH=VAT_PREFERENTIAL_R_HH+VAT_STANDARD_R_HH,
                          VAT_R_NPISH=VAT_PREFERENTIAL_R_NPISH+VAT_STANDARD_R_NPISH,
                          VAT_R_GOVERMENT=VAT_PREFERENTIAL_R_GOVERMENT+VAT_STANDARD_R_GOVERMENT,
                          VAT_TOTAL_R_TOTAL=VAT_PREFERENTIAL_R_TOTAL+VAT_STANDARD_R_TOTAL,
                          #  Effective VAT rate by NACE DIVISION
                          EFFECTIVE_VAT_RATE_INDUSTRIES=VAT_R_INDUSTRIES/tax_base_INDUSTRIES,
                          EFFECTIVE_VAT_RATE_HH= VAT_R_HH/tax_base_HH,
                          EFFECTIVE_VAT_RATE_NPISH=VAT_R_NPISH/tax_base_NPISH,
                          EFFECTIVE_VAT_RATE_GOVERMENT=VAT_R_GOVERMENT/tax_base_GOVERMENT,
                          EFFECTIVE_VAT_RATE_TOTAL=VAT_TOTAL_R_TOTAL/tax_base_TOTAL)%>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE)
            
            EFFECTIVE_VAT_RATES[is.na(EFFECTIVE_VAT_RATES)] <- 0
            
            EFFECTIVE_VAT_RATES_HH<-EFFECTIVE_VAT_RATES%>%
              dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_HH,EFFECTIVE_VAT_RATE_HH)    
            
            
            # 2. HBS Analysis ------------------------------------------------------------
                  # 2.1 Applying effective VAT rates with HBS ------------------------------------
                  'In this part, only VAT base from households are used.
                  
                                  These bases come from the SUT table and are distributed by CPA (64 NACE divisions).
                                  Because of this, the COICOP structure was first calculated, and then the same percentages were applied to VAT Revenues from CPA. 
                                  In the end, effective VAT rates are applied.
                                  '
                  Revenue_VAT_TOTAL_HH<-Revenue_VAT_TOTAL_BU%>%
                    dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_HH)
                  
                  VAT_COICOP_PROPORTIONS<-VAT_COICOP_FINAL_RAW %>%
                    dplyr::select(Two_digits,FC,EX,Reduced_Rate_5,Standard_Rate_18) %>%
                    dplyr::group_by(Two_digits) %>%
                    dplyr::summarise(FC = sum(FC, na.rm = T),
                                     VAT_BASE_COICOP_EX= sum(EX, na.rm = T),
                                     VAT_BASE_COICOP_5 = sum(Reduced_Rate_5, na.rm = T),
                                     VAT_BASE_COICOP_18 = sum(Standard_Rate_18, na.rm = T))%>%
                    dplyr::mutate(VAT_BASE_COICOP_5_18=VAT_BASE_COICOP_5+VAT_BASE_COICOP_18)
                  
                  VAT_COICOP_PROPORTIONS<-left_join(VAT_COICOP_PROPORTIONS,CPA_COICOP_CONCORDANCE,by = c("Two_digits"="COICOP_Division"))
                  
                  
                  # Extract only VAT tax base for households
                  EFFECTIVE_VAT_RATES_HH_BASE<-EFFECTIVE_VAT_RATES_HH%>%
                    dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_HH,EFFECTIVE_VAT_RATE_HH)
                  
                  # Concordance
                  VAT_COICOP_PROPORTIONS_CPA_1<-left_join(VAT_COICOP_PROPORTIONS,EFFECTIVE_VAT_RATES_HH_BASE,by = c("CPA_COICOP"="PRODUCT_INDUSTRY_CODE"))%>%
                    dplyr::select(CPA_COICOP,Two_digits,tax_base_HH,VAT_BASE_COICOP_5_18,EFFECTIVE_VAT_RATE_HH)%>%
                    dplyr::group_by(CPA_COICOP) %>%
                    dplyr::mutate(PCT = VAT_BASE_COICOP_5_18/sum(VAT_BASE_COICOP_5_18))%>%
                    ungroup
                  
                  # Sum of VAT new 18-11.2022
                  Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL<-VAT_COICOP_PROPORTIONS_CPA_1%>%
                    dplyr::mutate(PROXY_TAX_BASE_HH=VAT_BASE_COICOP_5_18*PCT)%>%
                    dplyr::mutate(VAT_ESTIMATED=PROXY_TAX_BASE_HH*EFFECTIVE_VAT_RATE_HH)%>%
                    dplyr::group_by(Two_digits)%>%
                    dplyr::summarise(PROXY_TAX_BASE_HH = sum(PROXY_TAX_BASE_HH, na.rm = T),VAT_ESTIMATED = sum(VAT_ESTIMATED, na.rm = T))%>%
                    dplyr::mutate(EFFECTIVE_VAT_RATE_HH=VAT_ESTIMATED/PROXY_TAX_BASE_HH)
                  
                  # Effective VAT rate
                  Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1<-Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL%>%
                    dplyr::mutate(EFFECTIVE_VAT_RATE_HH=round(EFFECTIVE_VAT_RATE_HH,2))
                  
                  Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1[is.na(Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1)] <- 0
                  
                  # 2.2 Merging with HBS Available assets --------------------------------------------------------
                  
                  # Merging with HBS
                  data4_hbs_long_merged<-left_join(data4_hbs_long,Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1,by = c("COICOP_section"="Two_digits"))%>%
                    dplyr::select(-c(PROXY_TAX_BASE_HH,VAT_ESTIMATED))%>%
                    dplyr::mutate(VAT_BASE_HH=Expenditures/(1+EFFECTIVE_VAT_RATE_HH),
                                  VAT_REVENUES_HH=VAT_BASE_HH*EFFECTIVE_VAT_RATE_HH)
                  
                  data4_hbs$Consumption_own<-NULL
                  
                  # Preparing data for estimation of deciles and centiles groups
                  data4_hbs<-data4_hbs%>%
                    dplyr::mutate(total_consumption=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)
                  
                  
                  FINAL_UNWEIGHTED_SAMPLE<-mutate(data4_hbs,
                                                  deciles=qgroup(total_consumption, 10),
                                                  centiles=qgroup(total_consumption, 100),)
                  
                  
                  # # Original
                  # data4_hbs_long_merged_deciles<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
                  #   # View(data4_hbs_long_merged_deciles) 
                  #   dplyr::select(number_hh,COICOP_section,VAT_REVENUES_HH,centiles,deciles,total_consumption)
                   
                  # Pivoting table and extract information about VAT 
                  # data4_hbs_wider_merged_deciles1<-data4_hbs_long_merged_deciles%>%    
                  #   pivot_wider(
                  #               names_from = COICOP_section,
                  #               values_from = c(VAT_REVENUES_HH))%>%
                  #   dplyr::mutate(VAT_TOTAL=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)%>%
                  #   dplyr::select(-c("Consumption_own"))
                  
                  
                  # Estimation of VAT by households
                  
                  data4_hbs_long_merged_deciles_vat<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
                    # View(data4_hbs_long_merged_deciles) 
                    dplyr::select(number_hh,COICOP_section,VAT_REVENUES_HH,centiles,deciles,total_consumption)
                  
                  
                  data4_hbs_wider_merged_deciles_vat_1<-data4_hbs_long_merged_deciles_vat%>%    
                    pivot_wider(
                      names_from = COICOP_section,
                      values_from = c(VAT_REVENUES_HH))%>%
                    dplyr::mutate(VAT_TOTAL=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)%>%
                    dplyr::select(-c("Consumption_own"))
                  
                  # Extract Expenditures  and merging with VAT
                  
                  data4_hbs_long_merged_deciles_exp<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
                    # View(data4_hbs_long_merged_deciles) 
                    dplyr::select(number_hh,COICOP_section,Expenditures)
                  
                  
                  
                  data4_hbs_wider_merged_deciles_exp_1<-data4_hbs_long_merged_deciles_exp%>%    
                    pivot_wider(
                      names_from = COICOP_section,
                      values_from = c(Expenditures))%>%
                    dplyr::select(-c("Consumption_own"))
                  
                  
                  # Merging two bases together
                  data4_hbs_wider_merged_deciles1<-left_join(data4_hbs_wider_merged_deciles_exp_1,data4_hbs_wider_merged_deciles_vat_1,by = c("number_hh"="number_hh"))
                  
                  
                  colnames(data4_hbs_wider_merged_deciles1)[c(2:13)] <- c("Food and Non-Alcoholic Beverages",
                                                                          "Alcoholic Beverages Tobacco and Narcotics",
                                                                          "Clothing and footwear",
                                                                          "Housing Water,Electricity,Gas and Other Fuels",
                                                                          "Furnishings Household Equipment etc.",
                                                                          "Health",
                                                                          "Transport",
                                                                          "Communication",
                                                                          "Recreation and Culture",
                                                                          "Education",
                                                                          "Restaurants and Hotels",
                                                                          "Miscellaneous Goods and Services")
                  
                  colnames(data4_hbs_wider_merged_deciles1)[c(17:28)] <- c("Food and Non-Alcoholic Beverages-VAT",
                                                                          "Alcoholic Beverages Tobacco and Narcotics-VAT",
                                                                          "Clothing and footwear-VAT",
                                                                          "Housing Water,Electricity,Gas and Other Fuels-VAT",
                                                                          "Furnishings Household Equipment etc.-VAT",
                                                                          "Health-VAT",
                                                                          "Transport-VAT",
                                                                          "Communication-VAT",
                                                                          "Recreation and Culture-VAT",
                                                                          "Education-VAT",
                                                                          "Restaurants and Hotels-VAT",
                                                                          "Miscellaneous Goods and Services-VAT")
                  
                  
                  colnames(data4_hbs_wider_merged_deciles1)
                  
                
      
            
                  hbs_bu<-data4_hbs_wider_merged_deciles1  
                
# II.Simulation -----------------------------------------------------------
                  # 1. ESTIMATION OF EFFECTIVE VAT RATE BY SECTORS -------------------------------------
                    # 1.1 Effective rate for : Industries,HH,NPISH and GOVERMENT First approach ----------------------------
                  
                  TAX_BASE_TOTAL<-CPA_PRODUCTS_SIM$Est_Rev%>%
                          dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Total,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government)%>%
                          dplyr::mutate(
                            tax_base_INDUSTRIES=Total_Revenues_from_Intermediate_Inputs/StandardVATRate,
                            tax_base_HH=Final_Demand_HH/StandardVATRate,
                            tax_base_NPISH=Final_Demand_NPISH/StandardVATRate,
                            tax_base_GOVERMENT=Final_Demand_Government/StandardVATRate,
                            tax_base_TOTAL=Final_Demand_Total/StandardVATRate)%>%
                          dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_INDUSTRIES,tax_base_HH,tax_base_NPISH,tax_base_GOVERMENT,tax_base_TOTAL)
                        TAX_BASE_TOTAL[2:6]<-abs(TAX_BASE_TOTAL[2:6])
                  
                  
                  # Estimation of effective VAT rates based on SUT tables
                  # EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIMULATION,by = c("PRODUCT_INDUSTRY_CODE"))%>%
                  # NEW 7.12.2024
                  # EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIMULATION_0,by = c("PRODUCT_INDUSTRY_CODE"))%>%
                  
                  # TEST NEW !!! 23/12/2024
                  
                  EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIM_SCENARIO_EST_CAL_FACTOR_SIM,by = c("PRODUCT_INDUSTRY_CODE"))%>%
                    dplyr::select(PRODUCT_INDUSTRY_CODE,StandardVATRate,PreferentialVATRate_1,tax_base_INDUSTRIES,
                                  tax_base_HH,tax_base_NPISH,tax_base_GOVERMENT,tax_base_TOTAL,Current_Policy_Reduced_Rate,Current_Policy_Fully_Taxable,
                                  Simulated_Policy_Reduced_Rate,Simulated_Policy_Fully_Taxable)%>%
                    dplyr::mutate(
                      # VAT FROM PREFERENTIAL VAT RATES
                      VAT_PREFERENTIAL_R_INDUSTRIES=tax_base_INDUSTRIES*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                      VAT_PREFERENTIAL_R_HH=tax_base_HH*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                      VAT_PREFERENTIAL_R_NPISH=tax_base_NPISH*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                      VAT_PREFERENTIAL_R_GOVERMENT=tax_base_GOVERMENT*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                      VAT_PREFERENTIAL_R_TOTAL=tax_base_TOTAL*Simulated_Policy_Reduced_Rate*PreferentialVATRate_1,
                      # VAT FROM STANDARD RATES
                      VAT_STANDARD_R_INDUSTRIES=tax_base_INDUSTRIES*Simulated_Policy_Fully_Taxable*StandardVATRate,
                      VAT_STANDARD_R_HH=tax_base_HH*Simulated_Policy_Fully_Taxable*StandardVATRate,
                      VAT_STANDARD_R_NPISH=tax_base_NPISH*Simulated_Policy_Fully_Taxable*StandardVATRate,
                      VAT_STANDARD_R_GOVERMENT=tax_base_GOVERMENT*Simulated_Policy_Fully_Taxable*StandardVATRate,
                      VAT_STANDARD_R_TOTAL=tax_base_TOTAL*Simulated_Policy_Fully_Taxable*StandardVATRate,
                      # SUM VAT REVENUES FROM PREFERENTIAL PLUS VAT FROM STANDARD RATE
                      VAT_R_INDUSTRIES= VAT_PREFERENTIAL_R_INDUSTRIES+VAT_STANDARD_R_INDUSTRIES,
                      VAT_R_HH=VAT_PREFERENTIAL_R_HH+VAT_STANDARD_R_HH,
                      VAT_R_NPISH=VAT_PREFERENTIAL_R_NPISH+VAT_STANDARD_R_NPISH,
                      VAT_R_GOVERMENT=VAT_PREFERENTIAL_R_GOVERMENT+VAT_STANDARD_R_GOVERMENT,
                      VAT_TOTAL_R_TOTAL=VAT_PREFERENTIAL_R_TOTAL+VAT_STANDARD_R_TOTAL,
                      #  Effective VAT rate by NACE DIVISION
                      EFFECTIVE_VAT_RATE_INDUSTRIES=VAT_R_INDUSTRIES/tax_base_INDUSTRIES,
                      EFFECTIVE_VAT_RATE_HH= VAT_R_HH/tax_base_HH,
                      EFFECTIVE_VAT_RATE_NPISH=VAT_R_NPISH/tax_base_NPISH,
                      EFFECTIVE_VAT_RATE_GOVERMENT=VAT_R_GOVERMENT/tax_base_GOVERMENT,
                      EFFECTIVE_VAT_RATE_TOTAL=VAT_TOTAL_R_TOTAL/tax_base_TOTAL)%>%
                    dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                  
                  EFFECTIVE_VAT_RATES[is.na(EFFECTIVE_VAT_RATES)] <- 0
                  
                  EFFECTIVE_VAT_RATES_HH<-EFFECTIVE_VAT_RATES%>%
                    dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_HH,EFFECTIVE_VAT_RATE_HH)    
                  
                  # effective_vat_rates<-EFFECTIVE_VAT_RATES
                  # 
                  # effective_vat_rates_bu=effective_vat_rates
                  
                  
                  # 2. HBS Analysis ------------------------------------------------------------
                    # 2.1 Applying effective VAT rates with HBS ------------------------------------
                  'In this part, only VAT base from households are used.
                  
                                  These bases come from the SUT table and are distributed by CPA (64 NACE divisions).
                                  Because of this, the COICOP structure was first calculated, and then the same percentages were applied to VAT Revenues from CPA. 
                                  In the end, effective VAT rates are applied.
                                  '
                  Revenue_VAT_TOTAL_HH<-Revenue_VAT_TOTAL_SIM%>%
                    dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_HH)
                  
                  VAT_COICOP_PROPORTIONS<-VAT_COICOP_FINAL_RAW %>%
                    dplyr::select(Two_digits,FC,EX,Reduced_Rate_5,Standard_Rate_18) %>%
                    dplyr::group_by(Two_digits) %>%
                    dplyr::summarise(FC = sum(FC, na.rm = T),
                                     VAT_BASE_COICOP_EX= sum(EX, na.rm = T),
                                     VAT_BASE_COICOP_5 = sum(Reduced_Rate_5, na.rm = T),
                                     VAT_BASE_COICOP_18 = sum(Standard_Rate_18, na.rm = T))%>%
                    dplyr::mutate(VAT_BASE_COICOP_5_18=VAT_BASE_COICOP_5+VAT_BASE_COICOP_18)
                  
                  VAT_COICOP_PROPORTIONS<-left_join(VAT_COICOP_PROPORTIONS,CPA_COICOP_CONCORDANCE,by = c("Two_digits"="COICOP_Division"))
                  
                  
                  # Extract only VAT tax base for households
                  EFFECTIVE_VAT_RATES_HH_BASE<-EFFECTIVE_VAT_RATES_HH%>%
                    dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_HH,EFFECTIVE_VAT_RATE_HH)
                  
                  # Concordance
                  VAT_COICOP_PROPORTIONS_CPA_1<-left_join(VAT_COICOP_PROPORTIONS,EFFECTIVE_VAT_RATES_HH_BASE,by = c("CPA_COICOP"="PRODUCT_INDUSTRY_CODE"))%>%
                    dplyr::select(CPA_COICOP,Two_digits,tax_base_HH,VAT_BASE_COICOP_5_18,EFFECTIVE_VAT_RATE_HH)%>%
                    dplyr::group_by(CPA_COICOP) %>%
                    dplyr::mutate(PCT = VAT_BASE_COICOP_5_18/sum(VAT_BASE_COICOP_5_18))%>%
                    ungroup
                  
                  # Sum of VAT new 18-11.2022
                  Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL<-VAT_COICOP_PROPORTIONS_CPA_1%>%
                    dplyr::mutate(PROXY_TAX_BASE_HH=VAT_BASE_COICOP_5_18*PCT)%>%
                    dplyr::mutate(VAT_ESTIMATED=PROXY_TAX_BASE_HH*EFFECTIVE_VAT_RATE_HH)%>%
                    dplyr::group_by(Two_digits)%>%
                    dplyr::summarise(PROXY_TAX_BASE_HH = sum(PROXY_TAX_BASE_HH, na.rm = T),VAT_ESTIMATED = sum(VAT_ESTIMATED, na.rm = T))%>%
                    dplyr::mutate(EFFECTIVE_VAT_RATE_HH=VAT_ESTIMATED/PROXY_TAX_BASE_HH)
                  
                  # Effective VAT rate
                  Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1<-Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL%>%
                    dplyr::mutate(EFFECTIVE_VAT_RATE_HH=round(EFFECTIVE_VAT_RATE_HH,2))
                  
                  Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1[is.na(Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1)] <- 0
                  
                    # 2.2 Merging with HBS Available assets --------------------------------------------------------
                  
                  # Merging with HBS
                  data4_hbs_long_merged<-left_join(data4_hbs_long,Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1,by = c("COICOP_section"="Two_digits"))%>%
                    dplyr::select(-c(PROXY_TAX_BASE_HH,VAT_ESTIMATED))%>%
                    dplyr::mutate(VAT_BASE_HH=Expenditures/(1+EFFECTIVE_VAT_RATE_HH),
                                  VAT_REVENUES_HH=VAT_BASE_HH*EFFECTIVE_VAT_RATE_HH)
                  
                  data4_hbs$Consumption_own<-NULL
                  
                  # Preparing data for estimation of deciles and centiles groups
                  data4_hbs<-data4_hbs%>%
                    dplyr::mutate(total_consumption=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)
                  
                  # Adding centiles and deciles groups 
                  # Checking sum
                  # 43152+6960+6528+16512+480 
                  
                  FINAL_UNWEIGHTED_SAMPLE<-mutate(data4_hbs,
                                                  deciles=qgroup(total_consumption, 10),
                                                  centiles=qgroup(total_consumption, 100),)
                  
                  
                  # # Original
                  # data4_hbs_long_merged_deciles<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
                  #   # View(data4_hbs_long_merged_deciles) 
                  #   dplyr::select(number_hh,COICOP_section,VAT_REVENUES_HH,centiles,deciles,total_consumption)
                  
                  # Pivoting table and extract information about VAT 
                  # data4_hbs_wider_merged_deciles1<-data4_hbs_long_merged_deciles%>%    
                  #   pivot_wider(
                  #               names_from = COICOP_section,
                  #               values_from = c(VAT_REVENUES_HH))%>%
                  #   dplyr::mutate(VAT_TOTAL=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)%>%
                  #   dplyr::select(-c("Consumption_own"))
                  
                  
                  # Estimation of VAT by households
                  
                  data4_hbs_long_merged_deciles_vat<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
                    # View(data4_hbs_long_merged_deciles) 
                    dplyr::select(number_hh,COICOP_section,VAT_REVENUES_HH,centiles,deciles,total_consumption)
                  
                  
                  data4_hbs_wider_merged_deciles_vat_1<-data4_hbs_long_merged_deciles_vat%>%    
                    pivot_wider(
                      names_from = COICOP_section,
                      values_from = c(VAT_REVENUES_HH))%>%
                    dplyr::mutate(VAT_TOTAL=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)%>%
                    dplyr::select(-c("Consumption_own"))
                  
                  # Extract Expenditures  and merging with VAT
                  
                  data4_hbs_long_merged_deciles_exp<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
                    # View(data4_hbs_long_merged_deciles) 
                    dplyr::select(number_hh,COICOP_section,Expenditures)
                  
                  
                  
                  data4_hbs_wider_merged_deciles_exp_1<-data4_hbs_long_merged_deciles_exp%>%    
                    pivot_wider(
                      names_from = COICOP_section,
                      values_from = c(Expenditures))%>%
                    dplyr::select(-c("Consumption_own"))
                  
                  
                  # Merging two bases together
                  data4_hbs_wider_merged_deciles1<-left_join(data4_hbs_wider_merged_deciles_exp_1,data4_hbs_wider_merged_deciles_vat_1,by = c("number_hh"="number_hh"))
                  
                  
                  colnames(data4_hbs_wider_merged_deciles1)[c(2:13)] <- c("Food and Non-Alcoholic Beverages",
                                                                          "Alcoholic Beverages Tobacco and Narcotics",
                                                                          "Clothing and footwear",
                                                                          "Housing Water,Electricity,Gas and Other Fuels",
                                                                          "Furnishings Household Equipment etc.",
                                                                          "Health",
                                                                          "Transport",
                                                                          "Communication",
                                                                          "Recreation and Culture",
                                                                          "Education",
                                                                          "Restaurants and Hotels",
                                                                          "Miscellaneous Goods and Services")
                  
                  colnames(data4_hbs_wider_merged_deciles1)[c(17:28)] <- c("Food and Non-Alcoholic Beverages-VAT",
                                                                           "Alcoholic Beverages Tobacco and Narcotics-VAT",
                                                                           "Clothing and footwear-VAT",
                                                                           "Housing Water,Electricity,Gas and Other Fuels-VAT",
                                                                           "Furnishings Household Equipment etc.-VAT",
                                                                           "Health-VAT",
                                                                           "Transport-VAT",
                                                                           "Communication-VAT",
                                                                           "Recreation and Culture-VAT",
                                                                           "Education-VAT",
                                                                           "Restaurants and Hotels-VAT",
                                                                           "Miscellaneous Goods and Services-VAT")
                  
                  
                  colnames(data4_hbs_wider_merged_deciles1)
                  
                  
                  #hbs_bu<-data4_hbs_wider_merged_deciles1  
                  
                  hbs<-data4_hbs_wider_merged_deciles1
                
               
                  
# III. Preparation data for  charts  ---------------------------------------------------------

            # 1.Effective VAT rate by percentiles groups -------------------------------
            
            vat_effective_bu<-hbs_bu%>%
              dplyr::select(centiles,total_consumption,VAT_TOTAL)%>%
              dplyr::group_by(centiles)%>%
              dplyr::summarise(total_consumption=sum(total_consumption),VAT_TOTAL=sum(VAT_TOTAL))%>%
              dplyr::mutate(effective_vat_before_reform=VAT_TOTAL/total_consumption)%>%
              dplyr::select(centiles,effective_vat_before_reform)
            
            
            vat_effective<-hbs%>%
              dplyr::select(centiles,total_consumption,VAT_TOTAL)%>%
              dplyr::group_by(centiles)%>%
              dplyr::summarise(total_consumption=sum(total_consumption),VAT_TOTAL=sum(VAT_TOTAL))%>%
              dplyr::mutate(effective_vat_after_reform=VAT_TOTAL/total_consumption)%>%
              dplyr::select(centiles,effective_vat_after_reform)
            
            
            vat_effective_combined<-left_join(vat_effective_bu,vat_effective,by = c("centiles"))%>%
              data.table()
           
            
            
            # 2.Estimated VAT Revenues by Decile Groups--------------------------------------
              # 2.1 Business as usual ----------------------------------------
            dat_r<-hbs_bu%>%
              dplyr::select(deciles,"Food and Non-Alcoholic Beverages-VAT",
                            "Alcoholic Beverages Tobacco and Narcotics-VAT",
                            "Clothing and footwear-VAT",
                            "Housing Water,Electricity,Gas and Other Fuels-VAT",
                            "Furnishings Household Equipment etc.-VAT",
                            "Health-VAT",
                            "Transport-VAT",
                            "Communication-VAT",
                            "Recreation and Culture-VAT",
                            "Education-VAT",
                            "Restaurants and Hotels-VAT",
                            "Miscellaneous Goods and Services-VAT")
            
            
            dat_r1<-dat_r%>%
              dplyr::group_by(deciles)%>%
              dplyr::summarise(`Food and Non-Alcoholic Beverages` = sum(`Food and Non-Alcoholic Beverages-VAT`),
                               `Alcoholic Beverages Tobacco and Narcotics` = sum(`Alcoholic Beverages Tobacco and Narcotics-VAT`),
                               `Clothing and footwear` = sum(`Clothing and footwear-VAT`),
                               `Housing Water,Electricity,Gas and Other Fuels` = sum(`Housing Water,Electricity,Gas and Other Fuels-VAT`),
                               `Furnishings Household Equipment etc.` = sum(`Furnishings Household Equipment etc.-VAT`),
                               `Health` = sum(`Health-VAT`),
                               `Transport` = sum(`Transport-VAT`),
                               `Communication` = sum(`Communication-VAT`),
                               `Recreation and Culture` = sum(`Recreation and Culture-VAT`),
                               `Education` = sum(`Education-VAT`),
                               `Restaurants and Hotels` = sum(`Restaurants and Hotels-VAT`),
                               `Miscellaneous Goods and Services` = sum(`Miscellaneous Goods and Services-VAT`))
            
            t_df1<-melt(dat_r1, id.vars = c("deciles"))
            t_df1$color <- factor(t_df1$variable, labels = c("royalblue", "cyan", "darkorange", "red", "brown",
                                                             "chartreuse","orange","purple", "gold","tomato",
                                                             "darkturquoise", "forestgreen"))
              # 2.2.Simulation -----------------------------------------------
            
            dat_r<-hbs%>%
              dplyr::select(deciles,"Food and Non-Alcoholic Beverages-VAT",
                            "Alcoholic Beverages Tobacco and Narcotics-VAT",
                            "Clothing and footwear-VAT",
                            "Housing Water,Electricity,Gas and Other Fuels-VAT",
                            "Furnishings Household Equipment etc.-VAT",
                            "Health-VAT",
                            "Transport-VAT",
                            "Communication-VAT",
                            "Recreation and Culture-VAT",
                            "Education-VAT",
                            "Restaurants and Hotels-VAT",
                            "Miscellaneous Goods and Services-VAT")
            
            dat_r1<-dat_r%>%
              dplyr::group_by(deciles)%>%
              dplyr::summarise(`Food and Non-Alcoholic Beverages` = sum(`Food and Non-Alcoholic Beverages-VAT`),
                               `Alcoholic Beverages Tobacco and Narcotics` = sum(`Alcoholic Beverages Tobacco and Narcotics-VAT`),
                               `Clothing and footwear` = sum(`Clothing and footwear-VAT`),
                               `Housing Water,Electricity,Gas and Other Fuels` = sum(`Housing Water,Electricity,Gas and Other Fuels-VAT`),
                               `Furnishings Household Equipment etc.` = sum(`Furnishings Household Equipment etc.-VAT`),
                               `Health` = sum(`Health-VAT`),
                               `Transport` = sum(`Transport-VAT`),
                               `Communication` = sum(`Communication-VAT`),
                               `Recreation and Culture` = sum(`Recreation and Culture-VAT`),
                               `Education` = sum(`Education-VAT`),
                               `Restaurants and Hotels` = sum(`Restaurants and Hotels-VAT`),
                               `Miscellaneous Goods and Services` = sum(`Miscellaneous Goods and Services-VAT`))
            
            
            
            t_df2<-melt(dat_r1, id.vars = c("deciles"))
            t_df2$color <- factor(t_df1$variable, labels = c("royalblue", "cyan", "darkorange", "red", "brown",
                                                             "chartreuse","orange","purple", "gold","tomato",
                                                             "darkturquoise", "forestgreen"))
            
            
            # Merging two databases
            # NEW
            t_df3<-left_join(t_df1,t_df2,by = c("variable"),relationship = "many-to-many")
            
            t_df1<-t_df1%>%
              dplyr:: rename(c("variable.x"= "variable",
                               "value.x"="value",
                               "color.x"="color"
                               
              ))
            
            t_df3<-cbind(t_df1,t_df2)
            
            COICOP_DecileGroups_combined <- pivot_longer(t_df3, cols = c(value.x,value ),names_to = "group") %>% 
                                dplyr::mutate(group = ifelse(group == "value", " After reform",
                                                      "Before reform"))
            

            # 3. HBS structure  ---------------------------------------------------------------------
            
                # 3.1.Business as usual-------------------------
                
                dat_r<-hbs_bu%>%
                            dplyr::select(deciles,"Food and Non-Alcoholic Beverages-VAT",
                                          "Alcoholic Beverages Tobacco and Narcotics-VAT",
                                          "Clothing and footwear-VAT",
                                          "Housing Water,Electricity,Gas and Other Fuels-VAT",
                                          "Furnishings Household Equipment etc.-VAT",
                                          "Health-VAT",
                                          "Transport-VAT",
                                          "Communication-VAT",
                                          "Recreation and Culture-VAT",
                                          "Education-VAT",
                                          "Restaurants and Hotels-VAT",
                                          "Miscellaneous Goods and Services-VAT")
                
                
                
                
                dat_r1<-dat_r%>%
                  dplyr::group_by(deciles)%>%
                  dplyr::summarise(`Food and Non-Alcoholic Beverages` = sum(`Food and Non-Alcoholic Beverages-VAT`),
                                   `Alcoholic Beverages Tobacco and Narcotics` = sum(`Alcoholic Beverages Tobacco and Narcotics-VAT`),
                                   `Clothing and footwear` = sum(`Clothing and footwear-VAT`),
                                   `Housing Water,Electricity,Gas and Other Fuels` = sum(`Housing Water,Electricity,Gas and Other Fuels-VAT`),
                                   `Furnishings Household Equipment etc.` = sum(`Furnishings Household Equipment etc.-VAT`),
                                   `Health` = sum(`Health-VAT`),
                                   `Transport` = sum(`Transport-VAT`),
                                   `Communication` = sum(`Communication-VAT`),
                                   `Recreation and Culture` = sum(`Recreation and Culture-VAT`),
                                   `Education` = sum(`Education-VAT`),
                                   `Restaurants and Hotels` = sum(`Restaurants and Hotels-VAT`),
                                   `Miscellaneous Goods and Services` = sum(`Miscellaneous Goods and Services-VAT`))
                
                
                
        
                
                COICOP_HBS_BU<-melt(dat_r1, id.vars = c("deciles"))
                
                
                COICOP_HBS_BU$colr <- factor(COICOP_HBS_BU$variable, labels = c("royalblue", "cyan", "darkorange", "red", "brown",
                                                                "chartreuse","orange","purple", "gold","tomato",
                                                                "darkturquoise", "forestgreen"))
                
             
                
                
                # 3.2.Simulation---------------------------------
                
                dat_r<-hbs%>%
                  dplyr::select(deciles,"Food and Non-Alcoholic Beverages-VAT",
                                "Alcoholic Beverages Tobacco and Narcotics-VAT",
                                "Clothing and footwear-VAT",
                                "Housing Water,Electricity,Gas and Other Fuels-VAT",
                                "Furnishings Household Equipment etc.-VAT",
                                "Health-VAT",
                                "Transport-VAT",
                                "Communication-VAT",
                                "Recreation and Culture-VAT",
                                "Education-VAT",
                                "Restaurants and Hotels-VAT",
                                "Miscellaneous Goods and Services-VAT")
                
                
                
                
                dat_r1<-dat_r%>%
                  dplyr::group_by(deciles)%>%
                  dplyr::summarise(`Food and Non-Alcoholic Beverages` = sum(`Food and Non-Alcoholic Beverages-VAT`),
                                   `Alcoholic Beverages Tobacco and Narcotics` = sum(`Alcoholic Beverages Tobacco and Narcotics-VAT`),
                                   `Clothing and footwear` = sum(`Clothing and footwear-VAT`),
                                   `Housing Water,Electricity,Gas and Other Fuels` = sum(`Housing Water,Electricity,Gas and Other Fuels-VAT`),
                                   `Furnishings Household Equipment etc.` = sum(`Furnishings Household Equipment etc.-VAT`),
                                   `Health` = sum(`Health-VAT`),
                                   `Transport` = sum(`Transport-VAT`),
                                   `Communication` = sum(`Communication-VAT`),
                                   `Recreation and Culture` = sum(`Recreation and Culture-VAT`),
                                   `Education` = sum(`Education-VAT`),
                                   `Restaurants and Hotels` = sum(`Restaurants and Hotels-VAT`),
                                   `Miscellaneous Goods and Services` = sum(`Miscellaneous Goods and Services-VAT`))
                
                
                COICOP_HBS_SIM<-melt(dat_r1, id.vars = c("deciles"))
                
                COICOP_HBS_SIM$colr <- factor(COICOP_HBS_SIM$variable, labels = c("royalblue", "cyan", "darkorange", "red", "brown",
                                                                "chartreuse","orange","purple", "gold","tomato",
                                                                "darkturquoise", "forestgreen"))
                
             
                
    
    
    
                
    
# 4. Calculation of Kakwani -----------------------------------------------

            # Business as usual
            hbs_bu<-hbs_bu%>%
              dplyr::mutate(net_consumption=total_consumption-VAT_TOTAL)
            
          #  sum(hbs_bu$net_consumption) #732498923
                
                
            # Simualation
            hbs<-hbs%>%
              dplyr::mutate(net_consumption=total_consumption-VAT_TOTAL)
            
            # sum(hbs$net_consumption)  #732498923
            
            # Estimation of GINI 
            
            # Gross income (before reform)
            gx<-data.frame(round(ineq(hbs_bu$total_consumption,type="Gini",na.rm = TRUE),4))   # Gini coefficient of pre-tax income
            
            # Net income (before reform, after taxation)
            gn<-data.frame(ineq(hbs_bu$net_consumption,type="Gini",na.rm = TRUE))# Gini coefficient of after-tax income
            gn<-round(gn,4)
            
            # Net income (after reform, after taxation)
            gx1<-data.frame(ineq(hbs$total_consumption,type="Gini",na.rm = TRUE))# Gini coefficient of pre-tax income
            gx1<-round(gx1,4)
            gn1<-data.frame(ineq(hbs$net_consumption,type="Gini",na.rm = TRUE))# Gini coefficient of after-tax income
            gn1<-round(gn1,4)
            
            # Estimation of re distributive effects
            
            # Before reform
            re<-(gx[1,1]-gn[1,1])
            re<-round(re,4)
            
            # After reform
            re1<-(gx[1,1]- gn1[1,1])
            re1<-round(re1,4)
            
            # Estimation of concentration index 
            
            # Concentration index of after-tax income with respect to pre-tax income DT
            dn_0<-calcSConc(hbs_bu$net_consumption,hbs_bu$total_consumption)
            dn_1<-data.frame(dn_0[[1]])
            dn_1<-round(dn_1,4)
            
            # Concentration index of after-tax income with respect to pre-tax income DT
            dn4<-calcSConc(hbs$net_consumption,hbs$total_consumption)
            dn_5<-data.frame(dn4[[1]])
            dn_5<-round(dn_5,4)
            
            # Concentration coefficient of taxes with respect to pre-tax income DN
            
            # Before reform
            dt_0<-calcSConc(hbs_bu$VAT_TOTAL,hbs_bu$total_consumption)
            dt_1<-data.frame(dt_0[[1]])
            dt_1<-round(dt_1,4)
            
            # After reform
            dt_2<-calcSConc(hbs$VAT_TOTAL,hbs$total_consumption)
            dt_2<-data.frame(dt_2[[1]])
            dt_2<-round(dt_2,4)
            dt_3<-data.frame(dt_2[[1]]) 
            dt_3<-round(dt_3,4)  
            
            # Estimation of Kakwani index of progressiveness
            p=(dt_1[1,1]-gx[1,1])
            p<-round(p,4)
            p1=(dt_3[1,1]-gx[1,1])
            p1<-round(p1,4)
            
            # Average tax rat (witout SSC)
            t0<-data.frame(sum(hbs_bu$VAT_TOTAL)/sum(hbs_bu$total_consumption))
            t0<-round(t0,4)
            t1<-data.frame(sum(hbs$VAT_TOTAL)/sum(hbs$total_consumption))
            t1<-round(t1,4)
            
            # Decomposition of after redistributive effect
            
            # Redistributive effects 1
            h1=(dn_1[1,1]-gn[1,1])/gx[1,1]
            v1=((t0*p)/(1-t0)*gx[1,1])
            v1a<-v1[[1]]
            v1ab<-round(v1a,4)
            r1=h1+v1ab
            r1<-round(r1,4)
            
            
            # Redistributive effects 2
            h2=(dn_5[1,1]-gn1[1,1])/gx1[1,1]
            v2=((t1*p1)/(1-t1)*gx[1,1])
            v2a<-v2[[1]]
            v2ab<-round(v2a,4)
            r2=h2+v2ab
            r2<-round(r2,4)
            
            # Atkinson index 
            
            # Before reform (gross income) 
            at_0<-calcAtkinson(hbs_bu$total_consumption,epsilon = 1)
            at_0<-data.frame(at_0$ineq$index)
            at_0<-round(at_0,4)
            
            # Before reform(net income)
            at_0_1<-calcAtkinson(hbs_bu$net_consumption,epsilon = 1)
            at_0_1<-data.frame(at_0_1$ineq$index)
            at_0_1<-round(at_0_1,4)
            
            # After reform (net income)
            at_1<-calcAtkinson(hbs$net_consumption,epsilon = 1)
            at_1<-data.frame(at_1$ineq$index)
            at_1<-round(at_1,4)
            
            # Coefficient of squared variation
            # Before reform (gross income)
            vc_0<-var.coeff(hbs_bu$total_consumption, square=TRUE) 
            vc_0<-data.frame(vc_0)
            vc_0<-round(vc_0,4)
            
            # Before reform(net income)
            vc_0_1<-var.coeff(hbs_bu$net_consumption, square=TRUE) 
            vc_0_1<-data.frame(vc_0_1)
            vc_0_1<-round(vc_0_1,4)
            
            # After reform (net income)
            vc_1<-var.coeff(hbs$net_consumption, square=TRUE) 
            vc_1<-data.frame(vc_1)
            vc_1<-round(vc_1,4)
            
            # Table with coefficients 
            
            INEQ_TOTAL_INCOME_OUTPUT<-data.frame(
              "Gini coefficient of pre-tax income"=gx[1,1],
              "Gini coefficient of after-tax income"=gn[1,1],
              "RE"= re,
              "Concentration coefficient of taxes with respect to pre-tax"= dn_1[1,1],
              "Concentration index of after-tax income with respect to pre-tax income"= dt_1[1,1],
              "Kakwani"= p,
              "Average tax"= t0[1,1],
              "Redistibutive effect"=r1,
              "Atkinson index (pre tax income)"= at_0[1,1],
              "Atkinson index (after tax income)"= at_0_1[1,1],
              "Squared variation (pre tax income)"=vc_0[1,1],
              "Squared variation (after tax income)"=  vc_0_1[1,1],
              "Gini coefficient of after-tax income1"=   gn1[1,1],
              "RE1"= re1,
              "Concentration coefficient of taxes with respect to pre-tax1"=  dn_5[1,1],
              "Concentration index of after-tax income with respect to pre-tax income1"=  dt_3[1,1],
              "Kakwani1"=  p1,
              "Average tax1"=   t1[1,1],
              "Redistibutive effect1"= r2,
              "Squared variation(after tax income)1"=  vc_1[1,1])
            
            
            INEQ_TOTAL_INCOME_NEW<-t(INEQ_TOTAL_INCOME_OUTPUT)
            colnames(INEQ_TOTAL_INCOME_NEW)<-c("INEQ_TOTAL_INCOME")
            
            rm(gx,gn,re,dn_1,dt_1,p,t0,r1,at_0,
               at_0_1,vc_0,vc_0_1,gn1,re1,dn_5,dt_3,p1,t1,r2,vc_1,at_1,dn_0,dn4,dt_0,dt_2,v1,v2,gx1,h1,h2,
               v1a,v1ab,v2a,v2ab)   
            
            end.time <- proc.time()
            save.time <- end.time - start.time
            cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")
      
            
})                
            print("Script Effective VAT Rates Done !")