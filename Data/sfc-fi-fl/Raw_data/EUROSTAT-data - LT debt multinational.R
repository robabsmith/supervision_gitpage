###############################################################################################################
###############################################################################################################

# R code to download and process data from EUROSTAT into variables to be 
# used to inform an SFC Model.
# Data is processed as follows
# 1. Data sourced from the Eurostat server
# 2. script written to reduce the data file size
# 3. Additional vectors added in order to compare inflows, outflows, assets 
#       and liabilities
# 4. Variables are named according to:
#       Transaction Flow Matrix variable names (some are similar to Godley and Lavoie) 
#           (from CSV file). These can be edited to your choosing in the csv files, 
#           Itemdataset and Itemdataset2. Several of the variables will not be familiar
#           as they are created in accordance with the Sector National Accounts (SNA) 2008
#           framework. This data can be exported and placed into an excel TFM and BS framework
#           that respects budget and accounting constraints. It is vital to be familiar with
#           the SNA classifications prior to interpreting any information from the TFM or BS
#           matrices. Because the variable names are generic, this can only be done for one
#           country at a time. The positive side is that if EUROSTAT keep the SNA data, both
#           the complete TFM and BS matrices will be generated automatically. Then you will be
#           able to try to aggregate the table into the form that best suits your empirical
#           model's needs.
#       Sector (h, f, nf, g, row)
#       Financial variables include an additional classification for the source of the data, i.e.
#           Balance Sheet (_bs), Transactions (_tr), Revaluations(_rv), Other Changes(_oc)
#       Paid(_P) / Received(_R), Asset(_A) / Liability (_L), or if only one of the two(#blank#)
#           if both P&R or A&L are present an additional variable is created for the net value(_n).
#           
# All variables are exported to the location identified at the end of the document
#   from the data frame called "dat", this dataframe can be exported in whatever 
#   file format you choose.
#   
##################################################################################
#   This code is authored by Jesper Jespersen and Robert Smith, Aalborg University
##################################################################################
#   For any assistance please contact Rob Smith at rs@business.aau.dk
###############################################################################################################

#############################################################################
## If running from R Studio: Check the current running directory of the code
#############################################################################


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls())

sapply(c("knitr", "reshape2", "plm", "countrycode", "tidyr", "plyr", "dplyr", "eurostat", "foreign", 
"readxl", "ecb", "tidyverse", "stringr", "rstudioapi"), require, character.only = TRUE)

# clean_eurostat_cache  # If you want to keep the data in the cache do not run this line.

historical_data_raw <- read_excel("./JSTdatasetR4.xlsx", sheet = "Data")


## Collect data 

#Source Financial balance sheets data
f_bs <- get_eurostat(id = "nasa_10_f_bs",  time_format = "num")
f_bs_Q <- get_eurostat(id = "nasq_10_f_bs",  time_format = "num") # Quarterly

#Source Financial transactions data
f_tr <- get_eurostat(id = "nasa_10_f_tr",  time_format = "num")

#Source Revaluation account data
f_gl <- get_eurostat(id = "nasa_10_f_gl",  time_format = "num")

#Source Other changes in volume data
f_oc <- get_eurostat(id = "nasa_10_f_oc",  time_format = "num")

#Source counterpart data to financial accounts
f_cp <- get_eurostat(id = "nasa_10_f_cp",  time_format = "num")

#Source Non-financial transactions data
nf_tr <- get_eurostat(id = "nasa_10_nf_tr",  time_format = "num")
#Source Balance sheets for non-financial assets data
nfa_bs <- get_eurostat(id = "nama_10_nfa_bs",  time_format = "num")

# Automatically set the working directory to the location of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

unique(f_bs$unit)
    
debt_data <- f_bs %>%
    filter(na_item == "F3", 
           unit == "MIO_NAC",
           co_nco == "NCO",
           finpos == "LIAB",
           values != 0,
           sector == "S14_S15") %>%
    arrange(geo, sector, time) %>%
    mutate(debt = values) %>%
    select(-values, -na_item, -finpos) %>%
    spread(., key = geo, value = debt)

    debt_data <- f_bs %>%
        filter(co_nco == "NCO", 
               na_item == "F3", 
               finpos == "LIAB",
               values != 0,
               sector == "S14") %>%
        arrange(geo, sector, time) %>%
        mutate(debt = values) %>%
        select(-values, -na_item, -finpos)
        
    lt_debt_data <- f_bs %>%
        filter(co_nco == "NCO",
               unit == "MIO_NAC",
               na_item == "F32", 
               finpos == "LIAB",
               values != 0,
               sector == "S14") %>%
        arrange(geo, sector, time) %>%
        mutate(lt_debt = values) %>%
        select(-values, -na_item, -finpos) %>%
        spread(., key = geo, value = lt_debt)
    
    
    debt_prop_data <- debt_data %>%
        left_join(., lt_debt_data, by = c("unit", "co_nco", "geo", "time")) %>%
        mutate(debt_prop = lt_debt / debt)
    
    #Source Non-financial transactions data
    nf_tr <- get_eurostat(id = "nasa_10_nf_tr",  time_format = "num")
    
    Yd_h_data <- nf_tr %>%
        filter(unit == "CP_MNAC",
               na_item == "B6G", 
               direct == "RECV",
               values != 0,
               sector == "S14") %>%
        arrange(geo, sector, time) %>%
        mutate(Yd_hh = values,
               year = time,
               geo = as.character(geo)) %>%
        select(-unit, -values, -na_item, -direct, -time, -sector)
    
    
    
    historical_data <- historical_data_raw %>%
        select(-country, -iso) %>%
        left_join(., Yd_h_data, by = c("geo", "year"))
    
    write.csv2(historical_data, "historical_data_yd.csv")
    
        gather(-c(year, country, iso, un), key = variable, value = values, convert = TRUE)
        
    
    
    
    