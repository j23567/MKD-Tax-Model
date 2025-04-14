'Install packages and importing of data
                                          '



'Step 1. Set your local path to the  model'
rm(list = ls())

path1<-" "


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
                                          "openxlsx"
                                        )


          new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
          if(length(new.packages)) install.packages(new.packages)



        install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
                            repos = NULL, type = "source", method = "wininet")


          
        library(tidyverse)
        library(readxl)
        library(reshape2)
        library(data.table)
        library(plyr)

# II.IMPORT DATA -----------------------------------------------------------------

        path2 <- paste0(path1, "/Data/PIT")
        setwd(path2)
        getwd()
        
                         
                          
                          dt<-read_csv("mpin_epdd_nace_final334.csv")%>%data.table()
                          
                          MACRO_FISCAL_INDICATORS<-read_excel("macro_indicators.xlsx")
                          
                          # 2.Growth Factors & Scenario Mapping
                          
                          
                          growth_factors<-read.csv("growfactors_pit_mkd.csv")%>%data.table()
                          
                         
                          # 3.Weights

                          
                          
                          n <- NROW(dt)
                          
                          weights_pit <- data.table(
                                                    t0 = rep(1, n),
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
    

    
# III. SAVE DATA IN R ENVIRONMENT (RDS FILE) --------------------------------------------------------
   
    gc(TRUE)             
                  setwd(path1)
                  save.image(file=".RData") 
          
                  
