'Install packages and importing of data
                                          '



'Step 1. Set your local path to the  model'
rm(list = ls())

path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/MKD-Tax-Model" ##<---PATH


# I.INSTALLING REQUIRED PACKAGES AND SETTING PATH  -------------------------------------------------
          '1.Library installation'

                  list.of.packages <- c(
                                          "shiny",
                                          "shinydashboard",
                                          "shinyjs",
                                          "shinyWidgets",
                                          "DT",
                                          "ineq",
                                          "data.table",
                                          "readxl",
                                          "fontawesome",
                                          "flexdashboard",
                                          "tidyverse",
                                          "plyr",
                                          "shinycssloaders",
                                          "future",
                                          "promises",
                                          "plotly",
                                          "stringr",
                                          "reshape2",
                                          "base64enc",
                                          "parallel",
                                          "purrr",
                                          "tidyr",
                                          "RColorBrewer",
                                          "Hmisc",
                                          "openxlsx",
                                          "sm",
                                          "ks"
                                        
                                        )


          new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
          if(length(new.packages)) install.packages(new.packages)



# Additional installation -------------------------------------------------


        install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
                            repos = NULL, type = "source", method = "wininet")


        
        install.packages(
          "https://cran.r-project.org/src/contrib/Archive/rccmisc/rccmisc_0.3.7.tar.gz",
          repos = NULL,
          type = "source",
          method = "wininet"
        )
        
        
        
          
        library(tidyverse)
        library(readxl)
        library(reshape2)
        library(data.table)
        library(plyr)

# II.IMPORT DATA -----------------------------------------------------------------

      # 1.PIT -------------------------------------------------------------------


        path2 <- paste0(path1, "/Data/PIT")
        setwd(path2)
        getwd()
        
                         
                          
                          #dt<-read_csv("mpin_epdd_nace_final334.csv")%>%data.table()
                          #dt<-read_csv("mpin_epdd_nace_final.csv")%>%data.table()
                            dt<-read_csv("pit_synthetic.csv")%>%data.table()
                          
                          
                          
                          
                          MACRO_FISCAL_INDICATORS<-read_excel("macro_indicators.xlsx")
                          
                          # 2.Growth Factors & Scenario Mapping
                          
                          
                          growth_factors<-read.csv("growfactors_pit_mkd.csv")%>%data.table()
                          
                         
                          # 3.Weights

                          
                          
                          n <- NROW(dt)
                          
                          same_weight<-56
                          
                          weights_pit <- data.table(
                                                    t0 = rep(same_weight, n),
                                                    t1 = rep(1, n),
                                                    t2 = rep(1, n),
                                                    t3 = rep(1, n),
                                                    t4 = rep(1, n),
                                                    t5 = rep(1, n)
                                                  )
                          rm(n)
                          
                          
                          
                  
    # NACE NAMES
    df_nace_names<-structure(list(section = c("A", "B", "C", "D", "E", "F", "G", 
                                              "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
                                              "U", "Other"), description = c("Agriculture, forestry and fishing", 
                                                                             "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply", 
                                                                             "Water supply; sewerage; waste managment and remediation activities", 
                                                                             "Construction", "Wholesale and retail trade; repair of motor vehicles and motorcycles", 
                                                                             "Transporting and storage", "Accommodation and food service activities", 
                                                                             "Information and communication", "Financial and insurance activities", 
                                                                             "Real estate activities", "Professional, scientific and technical activities", 
                                                                             "Administrative and support service activities", "Public administration and defence; compulsory social security", 
                                                                             "Education", "Human health and social work activities", "Arts, entertainment and recreation", 
                                                                             "Other services activities", "Activities of households as employers; undifferentiated goods - and services - producing activities of households for own use", 
                                                                             "Activities of extraterritorial organisations and bodies", "Other"
                                              )), row.names = c(NA, -22L), class = c("tbl_df", "tbl", "data.frame"
                                              ))
    

    

# 2.VAT ---------------------------------------------------------------------

    'DATA PREPROCESSING MODULE          

                       '
    # setwd(path)
    # getwd()
    
    #path3 <- paste0(path1, "/Data/VAT") ## <---Path to VAT Data 
    path3 <- paste0(path1, "/Data/VAT")
    setwd(path3)
    getwd()
    
    # 1. DEFINE FUNCTIONS ----
    
    #  The function creates an ntile group vector:
    qgroup = function(numvec, n, na.rm=TRUE){
      qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  
      out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
      return(out)
    }
    
    #  to extract only English names from SUTs
    trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
    input_output_matrix_to_long_data <- function(matrix){
      
      matrix <- matrix %>%
        dplyr::filter(...2 != "NA")
      
      
      colnames(matrix) <- matrix[1,]
      
      data <- matrix[c(-1,-2),c(-1,-2)] %>% as.matrix() %>% melt()
      
      product_industry_name <- matrix[[2]][c(-1,-2)]
      product_industry_code <- matrix[[1]][c(-1,-2)]
      industry_code <-  matrix[2,c(-1,-2)] %>% as.character()
      
      data$Var1 <- rep(product_industry_name, time = length(industry_code))
      
      data <- data %>% 
        dplyr::rename(PRODUCT_INDUSTRY_NAME = Var1,
                      INDUSTRY_NAME = Var2)
      
      
      data$PRODUCT_INDUSTRY_CODE <- rep(product_industry_code, time = length(industry_code))
      data$INDUSTRY_CODE <- rep(industry_code, each = length(product_industry_code))
      
      data <- data %>% 
        dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value)
      
      
      # Leave the names only in English
      data$PRODUCT_INDUSTRY_NAME<-gsub("^.*\\/", "",  data$PRODUCT_INDUSTRY_NAME) %>% trim()
      data$INDUSTRY_NAME<-gsub("^.*\\/", "",  data$INDUSTRY_NAME) %>% trim()
      
      data$value <- as.numeric(as.character(data$value))
      
      return(data)
      
    }
    
    
    # 2. RAW DATA IMPORT AND PREPROCESS  ----- 
    
    

    CPA_TAX_PROP_SELECTED_WITH_RATES_RAW <- read_excel(
                                                      file.path(path3, "VAT-Data-Template.xlsx"),
                                                       sheet = "CPA_VAT_RATES_old"
    )
    
    
    # RATES_BY_CPA<-read_excel("VAT-Data-Template.xlsx", 
    #                          sheet = "CPA_VAT_RATES")
    # 
    
    RATES_BY_CPA <- read_excel(
      file.path(path3, "VAT-Data-Template.xlsx"),
      sheet = "CPA_VAT_RATES"
    )
    
    
    
    
    # Initialize empty lists to store the tables
    CPA_TAXABLE_PROPORTIONS_BU_list <- list()
    CPA_TAXABLE_PROPORTIONS_SIM_list <- list()
    
    # Created empty dataframe
    CPA_TAXABLE_PROPORTIONS_BASELINE <- data.frame(
                                                  PRODUCT_INDUSTRY_CODE = character(64),
                                                  PRODUCT_INDUSTRY_NAME = character(64),
                                                  Current_Policy_Exempt = numeric(64),
                                                  Current_Policy_Reduced_Rate = numeric(64),
                                                  Current_Policy_Fully_Taxable = numeric(64),
                                                  PreferentialVATRate_1 = numeric(64),
                                                  PreferentialVATRate_2 = numeric(64),
                                                  StandardVATRate = numeric(64),
                                                  stringsAsFactors = FALSE
                                                )
    
    
    
    CPA_TAXABLE_PROPORTIONS_BASELINE_BU<-CPA_TAXABLE_PROPORTIONS_BASELINE
    
    
    ' 
                    In this section data are imported from five files:
                    
                    VAT_Model_v9.16a2.xlsx
                    TaxableProportions-4a.xlsx
                    MACRO_FISCAL_INDICATORS.xlsx
                    Data4_hbs2020.xlsx  <---HBS DATA
                    NACE_SUT_table.xlsx
                    '
    
    # Name of the version of model
    version_vat_model<-c("VAT_Model_v9.16a2.xlsx")
    

    
    macro_fiscal_raw <- read_excel(
      file.path(path3, "MACRO_FISCAL_INDICATORS.xlsx"),
      sheet = "MediumTermForecast"
    )
    
    
    
    # New

    
    taxable_proportions_raw <- read_excel(
      file.path(path3, "TaxableProportions-5.xlsx")
    )
    
    
    
    
    # 2.1 SUTs ------------------------------------
    
    SUPPLY_raw <- read_excel(version_vat_model, sheet = "Supply", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    "Each value from Use_Purchaser are imported here"
    USE_PURCHASER_raw <- read_excel(version_vat_model, sheet = "Use_Purchaser", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    USE_VAT_raw <- read_excel(version_vat_model, sheet = "Use_VAT", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    USE_BASIC_raw <- read_excel(version_vat_model, sheet = "Use_Basic", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    
    # 2.2 COICOP table ------------------------------------------------------------
    # Please import VAT rates that are available at the moment of producing of VAT COICOP
    vat_bu_rate_preferential1<-0.05
    vat_bu_rate_preferential2<-0.05
    vat_bu_rate_standard<-0.18
    RC_prc_of_Constructions_and_construction_works = 0.3
    vat_rate_on_residential_construction = 0.05
    
    
    
    base_year_VAT<-2020 # <-This is the same year as the year from which the data originates.
    
    max_time_horizon<-base_year_VAT+5
    
    time_horizon<-seq(base_year_VAT,max_time_horizon)
    
    
    
    # # Create empty data frame 
    # CPA_BASE_TAXABLE_PROPORTION<-data.frame(Base=numeric())
    
    
    ## Data from COICOP table
    VAT_COICOP_NAMES <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1:2),c(2)]
    VAT_COICOP_NAMES<-VAT_COICOP_NAMES[1:178,1]
    
    
    VAT_COICOP_FC <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1:2),c(4:9)]
    VAT_COICOP_FC<-VAT_COICOP_FC[1:178,1:6]
    
    
    VAT_COICOP_FINAL_RAW<-cbind(VAT_COICOP_NAMES,VAT_COICOP_FC)
    
    VAT_COICOP_FINAL_RAW<-VAT_COICOP_FINAL_RAW%>%
      dplyr:: rename(c("COICOP_Descriptions"= "...2",
                       "FC"="...4",
                       "EX"= "...5",
                       "Reduced_Rate_5"="...6",
                       "Standard_Rate_18"= "...7",
                       "VAT_Revenue_5"="...8",
                       "VAT_Revenue_18"="...9"
                       
      ))
    
    # Select NACE industries on four digits for calculation  
    
    VAT_COICOP_FINAL_RAW<-subset(VAT_COICOP_FINAL_RAW, grepl("^\\d{2}\\.\\d\\.\\d\\s+", COICOP_Descriptions))
    
    # Extract NACE codes
    VAT_COICOP_FINAL_RAW$Four_digits<-substr(VAT_COICOP_FINAL_RAW$COICOP_Descriptions, 1, 6)
    VAT_COICOP_FINAL_RAW$Two_digits<-substr(VAT_COICOP_FINAL_RAW$COICOP_Descriptions, 1, 2)
    VAT_COICOP_FINAL_RAW[is.na(VAT_COICOP_FINAL_RAW)] <- 0
    VAT_COICOP_FINAL_RAW$EX<-as.numeric(VAT_COICOP_FINAL_RAW$EX)
    VAT_COICOP_FINAL_RAW$VAT_Revenue_5<-as.numeric(VAT_COICOP_FINAL_RAW$VAT_Revenue_5)
    
    
    # Input raw concordance table
    ConcordanceVAT_COICOP_CPA <- read_excel(version_vat_model, sheet = "Concordance", col_names = T)
    
    
    
    
    # Merging table <--- This table will be used in GUI NEW 1.6.2024
    VAT_COICOP_FINAL<-left_join(VAT_COICOP_FINAL_RAW,ConcordanceVAT_COICOP_CPA,by = c("COICOP_Descriptions"))
    
    ConcordanceVAT_COICOP_CPA<-ConcordanceVAT_COICOP_CPA %>% filter(!is.na(Four_digits))
    ConcordanceVAT_COICOP_CPA<-ConcordanceVAT_COICOP_CPA %>% filter(!is.na(CPA_CODE))
    
    
    # Adjustment of CPA codes with Concordance table
    COICOP <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1,2),-c(1:19)]
    COICOP[1,1] <- "PRODUCT_INDUSTRY_CODE"
    COICOP[1,5] <- "Negative"
    
    colnames(COICOP) <- c("PRODUCT_INDUSTRY_CODE", "Base",  "Exempt_Levels", "Reduced_Rate_Levels", "Negative", "Exempt_Adjustment", "Reduced_Rate_Adjustment", 
                          "Exempt_Levels_2", "Reduced_Rate_Levels_2",
                          "Exempt_Raw_perc", "Reduced_Rate_Raw_perc", "Exempt_Capped_perc", "Reduced_Rate_Capped_perc")
    
    COICOP <- COICOP[-c(1, 66:nrow(COICOP)),]
    
    COICOP <- COICOP %>%
      dplyr::arrange(PRODUCT_INDUSTRY_CODE)
    
    
    
    # 2.5 Concordance table NACE_SUT ----------------------------------
    
    NACE_SUT_table <- read_excel("NACE_SUT_table.xlsx", sheet = "NACE")
    CPA_COICOP_CONCORDANCE <- read_excel("NACE_SUT_table.xlsx", sheet = "CPA_COICOP_CONCORDANCE")
    
    
    # 2.6 HBS  ----------------------------------------
    # Import data
    data4_hbs <- read_excel("Data4_hbs2020.xlsx")
    
    # Setting columns names
    data4_hbs<-data4_hbs%>%
      dplyr::select(-c('kvartal','Year'))
    
    colnames(data4_hbs)<-c("number_hh","01","02","03","04","05","06","07","08","09","10","11","12","Consumption_own")
    
    # Preparing data for merging 
    data4_hbs_long<-data4_hbs%>%
      pivot_longer(!number_hh, names_to = "COICOP_section", values_to = "Expenditures")
    
    weight_hbs <- read_excel("Weight_hbs2020.xls")
    
    
    # 2.7 MACRO-FISCAL INDICATORS ---------------------------------------------
    MACRO_FISCAL_INDICATORS <- read_excel("MACRO_FISCAL_INDICATORS.xlsx")
    
    FinalConsumption <- read_excel("MACRO_FISCAL_INDICATORS.xlsx", 
                                   sheet = "FinalConsumption")
    
    # 3. INSERT TAXABLE PROPORTIONS SIMULATION PARAMETERS ---------------------------------------------
  
    
    
    taxable_proportion_bu <- taxable_proportions_raw %>%
      dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                    Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                    Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
    
    
    ### NEW 22/12/2024
    
    #CPA_TAXABLE_PROPORTIONS_BU<-read_excel("~/Models/Tax-Modeling-Toolkit/VAT_TaxableProportions.xlsx")
    
    
    CPA_TAXABLE_PROPORTIONS_BU <- read_excel(
                                                  file.path(path3, "VAT_TaxableProportions.xlsx")
                                                )
    
    
    ### NEW 27/12/2024
    
   # growfactors_vat <- read.csv("~/Models/Tax-Modeling-Toolkit/Data/VAT/growfactors_vat.csv")
    
    
    growfactors_vat <- read.csv(
      file.path(path3, "growfactors_vat.csv")
    )
    
        
    
    
# III. SAVE DATA IN R ENVIRONMENT (RDS FILE) --------------------------------------------------------
   
    gc(TRUE)             
                  setwd(path1)
                  save.image(file=".RData") 
          
                  
