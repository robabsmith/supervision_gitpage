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

#   Aspects of several of the packages below are used in the following code. 
#   With these packages installed you should not experience any problems with the
#   data sourcing below.

#Install Packages
# install.packages("pdfetch")
# install.packages("rJava")
# install.packages("xlsxjars")
# install.packages("networkD3")
# install.packages("Formula")
# install.packages("knitr")
# install.packages("reshape")
# install.packages("plm")
# install.packages("countrycode")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("eurostat")
# install.packages("plyr")
# install.packages("foreign")
# install.packages("xlsx")
# install.packages("ecb")
# install.packages("stringr")
# install.packages("rstudioapi")

#Clear list environment and EUROSTAT Cache
rm(list = ls())

#Load packages to the library
#library(pdfetch)
library(rJava)
library(xlsxjars)
#library(networkD3)
#library(Formula)
library(knitr)
library(reshape)
library(plm)
library(countrycode)
library(tidyr)
library(plyr)
library(dplyr)
library(eurostat)
library(foreign)
library(xlsx)
library(ecb)
library(tidyverse)
library(stringr)
library(rstudioapi)

# clean_eurostat_cache  # If you want to keep the data in the cache do not run this line.

## Collect data 

    #Source Financial balance sheets data
    f_bs <- get_eurostat(id = "nasa_10_f_bs",  time_format = "num")
    #Source Financial transactions data
    f_tr <- get_eurostat(id = "nasa_10_f_tr",  time_format = "num")
    #Source Revaluation account data
    f_gl <- get_eurostat(id = "nasa_10_f_gl",  time_format = "num")
    #Source Other changes in volume data
    f_oc <- get_eurostat(id = "nasa_10_f_oc",  time_format = "num")
    #Source Non-financial transactions data
    nf_tr <- get_eurostat(id = "nasa_10_nf_tr",  time_format = "num")
    #Source Balance sheets for non-financial assets data
    nfa_bs <- get_eurostat(id = "nama_10_nfa_bs",  time_format = "num")

    # Automatically set the working directory to the location of the script
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    
    # before continuing to the next step you should ensure that the three CSV
    # files listed below are saved in the same folder location as this R script.
    # Read the dataset that provides the new variable name references for the variables we are interested in
    items <- read.csv("Itemdataset.csv", stringsAsFactors = F)
    items2 <- read.csv("Itemdataset2.csv", stringsAsFactors = F)
    sec <- read.csv("Sectordataset.csv", stringsAsFactors = F)

    
    
## Function development:
    
    # Function to write similar datasets based on eurostat downloaded dataset
    write_datasets <- function(eurostat_data = f_bs, var1 = "bs", item_dataset = items, sec_dataset = sec){
      out_data <- eurostat_data %>% 
        filter(geo == "DK", unit == "MIO_NAC", co_nco == "NCO") %>%
        inner_join(., item_dataset, "na_item") %>%                               
        inner_join(., sec_dataset, "sector") %>%                               
        mutate(finpos_1 = substring(finpos, 1, 1),
               varname = paste(item_new, sec_new, var1, finpos_1, sep = "_")) %>%
        select(varname, time, values) %>%
        arrange(varname, time, values) %>%
        unique() %>%
        spread(key = varname, value = values)
      return(out_data)
    }

    # Function to gather the NFTR dataset
    write_datasets2 <- function(eurostat_data = f_bs, var1 = "bs", item_dataset = items, sec_dataset = sec){
      out_data <- eurostat_data %>% 
        filter(geo == "DK", unit == "CP_MNAC") %>%
        inner_join(., item_dataset, "na_item") %>%                              
        inner_join(., sec_dataset, "sector") %>%                               
        mutate(direct_1 = substring(direct, 1, 1),
               varname = paste(item_new, sec_new, direct_1, sep = "_")) %>%
        select(varname, time, values) %>%
        arrange(varname, time, values) %>%
        unique() %>%
        spread(key = varname, value = values)
      return(out_data)
    }
    
    
    # Function to write similar datasets based on eurostat downloaded dataset
    write_datasets3 <- function(eurostat_data = f_bs, var1 = "bs", item_dataset = items2, sec_dataset = sec){
        out_data <- eurostat_data %>% 
            filter(geo == "DK", unit == "CP_MNAC") %>%
            inner_join(., item_dataset, "asset10") %>%                              
            inner_join(., sec_dataset, "sector") %>%                               
            mutate(direct_1 = substring(asset10, 1, 1),
                   varname = paste(item_new, sec_new, "X", sep = "_")) %>%
            select(varname, time, values) %>%
            arrange(varname, time, values) %>%
            unique() %>%
            spread(key = varname, value = values)
        return(out_data)
    }
    
    
    

## Create the dataset

    # Dataset type 1
    FBS <- write_datasets(eurostat_data = f_bs, var1 = "bs")
    FTR_rv <- write_datasets(eurostat_data = f_gl, var1 = "rv")
    FTR_oc <- write_datasets(eurostat_data = f_oc, var1 = "oc")
    FTR_tr <- write_datasets(eurostat_data = f_tr, var1 = "tr")
    
    # Dataset type 2
    NFTR <- write_datasets2(eurostat_data = nf_tr, var1 = "nftr")
    
    # Dataset type 3
    NFBS <- write_datasets3(eurostat_data = nfa_bs, var1 = "nfbs")

    
    
## Merge all datasets    
    # Write the function that will allow you to merge all dataframes
    cbind.fill <- function(...){
      nm <- list(...) 
      nm <- lapply(nm, as.matrix)
      n <- max(sapply(nm, nrow)) 
      do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n-nrow(x), ncol(x))))) 
    }
    
    # Run the new merge function as written above
    data_raw <- cbind.fill(FTR_oc, FTR_tr, FTR_rv, NFBS, NFTR, FBS)
    
    # Set the raw combined data as a data frame
    merged_dat <- data.frame(data_raw)


    
## Creating new variables in the datasets: Single and Net 
    #Convert all NAs to 0 values in the dataset
     merged_dat[is.na(merged_dat)] <- 0
     
     dat <- merged_dat
     dat_tot <- dat
    
    write_csv(dat_tot, "./dat_raw.csv")
    ## This code block also works, just slower. 
    # Make all NAs into 0 in the dataset
    #dat_tot <- merged_dat %>%
    #    mutate_all( funs(if_else( is.na(.), 0, .)))
    #dat <- dat_tot
    
    # Create 4 different vectors, A, L, R, and P
    (coln <- colnames(dat_tot))
    vec_A <- str_subset(coln, "A$") %>% sort()
    vec_L <- str_subset(coln, "L$") %>% sort()
    vec_R <- str_subset(coln, "R$") %>% sort()
    vec_P <- str_subset(coln, "P$") %>% sort()
    vec_X <- str_subset(coln, "X$") %>% sort()
    
    # Create vector of varnames excluding the last 2 elements
    vec_A_n <- str_sub(vec_A, start = 1L, end = -3L)
    vec_L_n <- str_sub(vec_L, start = 1L, end = -3L)
    vec_R_n <- str_sub(vec_R, start = 1L, end = -3L)
    vec_P_n <- str_sub(vec_P, start = 1L, end = -3L)
    vec_X_n <- str_sub(vec_X, start = 1L, end = -3L)
    
    
    # The Asset and Liability components of each variable, and the paid
    # and received components of each variable are compared here to identify
    # variables that appear in only one of the two categories for each of P/R and A/L.
    # Those variables are recorded separately here in the shorter name format, which is
    # then used further down to overwrite the longer variable name in dat.
    (vec_A_d <- setdiff(vec_A_n, vec_L_n))
    (vec_L_d <- setdiff(vec_L_n, vec_A_n))
    (vec_A_n <- intersect(vec_A_n, vec_L_n))
    (vec_A <- str_c(vec_A_n, "_A"))
    (vec_L <- str_c(vec_A_n, "_L") )   
    (vec_P_d <- setdiff(vec_P_n, vec_R_n))
    (vec_R_d <- setdiff(vec_R_n, vec_P_n))
    vec_P_n <- intersect(vec_P_n, vec_R_n)
    vec_P <- str_c(vec_P_n, "_P")
    vec_R <- str_c(vec_P_n, "_R")    
    vec_X <- str_c(vec_X_n, "_X")  
    
    # Create loop over all variables in the vectors that tests two conditions
    # 1. "is A = L", if yes, then let the single variable exist as values of A
    #                and remove the _A & _L or _P & _R variables, else
    # 2. "is either A or L 0",
    # If yes, create single variable as difference (since liabilities and payments
        # should still register as -ve), and remove extra variables
    # If no, create net as difference, and keep _A & _L and _P & _R variables.
    
    # A/L Assets and Liabilities
    for(c in 1:length(vec_L)){
      if (sum(dat[, vec_A[c]]) == sum(dat[, vec_L[c]])){
          dat[, str_c(vec_A_n[c], "")] = dat[, vec_A[c]]
          dat[, vec_A[c]]=NULL
          dat[, vec_L[c]]=NULL
        } else{
            if (sum(dat[, vec_A[c]]) == 0 | sum(dat[, vec_L[c]]) == 0) {
                dat[, str_c(vec_A_n[c], "")] = dat[, vec_A[c]] - dat[, vec_L[c]]
                dat[, vec_A[c]]=NULL
                dat[, vec_L[c]]=NULL
            } else {
                dat[, str_c(vec_A_n[c], "_n")] = dat[, vec_A[c]] - dat[, vec_L[c]]
            }
        }
    }
    
    
    # P/R Paid and Received
    for(c in 1:length(vec_P)){
      if (sum(dat[, vec_P[c]]) == sum(dat[, vec_R[c]])){
        dat[, str_c(vec_P_n[c], "")] = dat[, vec_P[c]]
        dat[, vec_P[c]]=NULL
        dat[, vec_R[c]]=NULL
      } else{
            if (sum(dat[, vec_P[c]]) == 0 | sum(dat[, vec_R[c]]) == 0) {
                dat[, str_c(vec_P_n[c], "")] = dat[, vec_R[c]] - dat[, vec_P[c]]
                dat[, vec_P[c]]=NULL
                dat[, vec_R[c]]=NULL
            } else {
                dat[, str_c(vec_P_n[c], "_n")] = dat[, vec_R[c]] - dat[, vec_P[c]]
            }
        }
    }
    
    
    # X Fixed Assets
    for(c in 1:length(vec_X)){
      if (sum(dat[, vec_X[c]]) != 0){
        dat[, str_c(vec_X_n[c], "")] = dat[, vec_X[c]]
        dat[, vec_X[c]] = NULL
      } else{
          dat[, vec_X[c]] = NULL
        }
    }
    
    # All single variables that were excluded through setdiff need to be returned to the
    # dataset with the names that exclude the "_?" conponent, i.e. the vec_?_d names.
    # This must be done for both the P and R, and the A and L variables.
    # All values for paid and liability single variables are recorded as negative.

    # P/R Paid and Received - place the single variables into the new dataset
    for(c in 1:length(vec_P_d)){
        dat[, vec_P_d[c]] = - dat[, str_c(vec_P_d[c], "_P")]
        dat[, str_c(vec_P_d[c], "_P")] = NULL
    }
    for(c in 1:length(vec_R_d)){
        dat[, vec_R_d[c]] = dat[, str_c(vec_R_d[c], "_R")]
        dat[, str_c(vec_R_d[c], "_R")] = NULL
    }
    
    # A / L Assets and Liabilities - place the single variables into the new dataset
    for(c in 1:length(vec_A_d)){
        dat[, vec_A_d[c]] = dat[, str_c(vec_A_d[c], "_A")]
        dat[, str_c(vec_A_d[c], "_A")] = NULL
    }

        # The section of code for liabilities is not necessary, as no variables appear in the
        # setdiff function L, A. There are however variables that appear in the above A, L 
        # setdiff function and therefore that part of the code is retained. For a new country it
        # is recommended to run the code with the section below included, to check if an error
        # persists for undefined column names in vec_L_d

    # for(c in 1:length(vec_L_d)){
    #     dat[, vec_L_d[c]] = - dat[, str_c(vec_L_d[c], "_L")]
    #     dat[, str_c(vec_L_d[c], "_L")] = NULL
    # }

    
    # Order dataset alphabetically
    dat <- dat[, order(colnames(dat))]
    
    # The updated and reorded columns (single and net)
    updated_cols = setdiff(colnames(dat), colnames(dat_tot))


######################################################################
    #If you wish to remove all columns that do not contain data
    #Change all zero values to NaN
    dat[dat == 0] <- NA
    sum(is.na(dat))
    
    #Remove all columns that include only NaN values
    dat_clean <- dat[, unlist(lapply(dat, function(x) !all(is.na(x))))]
    
    #Change all NaN values back to zero
    dat_clean[is.na(dat_clean)] <- 0
    sum(is.na(dat_clean))
    
    dat[is.na(dat)] <- 0
    sum(is.na(dat))
    
    write_csv(dat, "./dat.csv")
###########################################################################
    
## Summary: The final file, dat, contains all five original datasets, 
        # and has added net/single columns as well. 

###########################################################################

    ## Additional calculations for analysis of data
    
    ###########################################################################
    ###########################################################################
    # Debt to Disposable Income    
    ###########################################################################
    ###########################################################################
    
    #Calculate household debt to disposable income
    dat$hh_d2yd <- (-(dat$ffl_h_bs - dat$ffd_h_bs_L)/dat$yd_n_h)*100
    # plot(dat$hhd2yd, type= "l")
    
    #Calculate NFC debt to disposable income
    dat$nf_d2yd <- ((dat$ffl_nf_bs_L + dat$ffd_nf_bs_L)/dat$yd_n_nf)*100
    # plot(dat$nfd2yd, type= "l")
    
    #Calculate FC debt to disposable income
    dat$f_d2yd <- ((dat$ffl_f_bs_L + dat$ffd_f_bs_L)/dat$yd_n_f)*100
    # plot(dat$fd2yd, type= "l")
    
    #Calculate G debt to disposable income
    dat$g_d2yd <- ((dat$ffl_g_bs_L + dat$ffd_g_bs_L)/dat$yd_n_g)*100
    # plot(dat$gd2yd, type= "l")

    ###########################################################################
    ###########################################################################
    # Debt to GDP
    ###########################################################################
    ###########################################################################
    
    #Calculate household debt to GDP
    dat$hh_d2gdp <- (-(dat$ffl_h_bs - dat$ffd_h_bs_L)/dat$gnp_s1)*100
    # plot(dat$hhd2gdp, type= "l")
    
    #Calculate NFC debt to GDP
    dat$nf_d2gdp <- ((dat$ffl_nf_bs_L + dat$ffd_nf_bs_L)/dat$gnp_s1)*100
    # plot(dat$nfd2gdp, type= "l")
    
    #Calculate FC debt to GDP
    dat$f_d2gdp <- ((dat$ffl_f_bs_L + dat$ffd_f_bs_L)/dat$gnp_s1)*100
    # plot(dat$fd2gdp, type= "l")
    
    #Calculate G debt to GDP
    dat$g_d2gdp <- ((dat$ffl_g_bs_L + dat$ffd_g_bs_L)/dat$gnp_s1)*100
    # plot(dat$gd2gdp, type= "l")
        
    #Calculate ROW debt to GDP
    dat$row_d2gdp <- ((dat$ffl_row_bs_L + dat$ffd_row_bs_L)/dat$gnp_s1)*100
    # plot(dat$rowd2gdp, type= "l")

    

    
###########################################################################
  
  ## File export: To save the file, select the appropriate option below,
    # or write to a format of your choice. Check the file saving address
    # before running the export.
    
    # The file with the "_c" extention is identical, except that all 
    # columns (variables) with only zero or NA values have been removed.
    
###########################################################################
    
    # # Export data to an Excel Spreadsheet___MAC
    # write.xlsx(dat_clean, "./dat_c.xlsx")
    # 
    # # Export data to an Excel Spreadsheet___MAC
    # write.xlsx(dat, "./dat.xlsx")
    # 
    # # Export data to an Excel Spreadsheet ___Windows
    # write.xlsx(dat_clean, "./dat_c.xlsx")
    # 
    # # Export data to an Excel Spreadsheet ___Windows
    # write.xlsx(dat, "./dat.xlsx")
    # 
###########################################################################
###########################################################################
    # Export dat to CSV file
    write.csv2(dat,"./dat.csv")
    
    # Export dat_clean to CSV file
    # write.csv2(dat_clean,"./dat_c.csv")
###########################################################################
###########################################################################
## Plots using the ggplot2 package for various sectoral balances
## The same can always be sourced for a variety of countries depending on
## the data that is sourced in the begining of the code.
## 
## Remmeber, all data is sourced from the server for all countries, the
## code simply filters out data for just one country. So changing the 
## filter value to be another country code (2 letter code, such as DE for
## Germany, or DK for Denmark) will simply keep all data applicable to
## that country.
###########################################################################
###########################################################################

    # Plot all net lending balances
    ggplot(dat, aes(time.1)) + 
      geom_line(aes(y = dat$nl_nf, color = "nf")) + 
      geom_line(aes(y = dat$nl_row, color = "row")) +
      geom_line(aes(y = dat$nl_h, color = "Hh")) +
      geom_line(aes(y = dat$nl_f, color = "f")) +
      geom_line(aes(y = dat$nl_g, color = "g")) +
      geom_abline("black", size = 0.3, slope = 0, intercept = 0)
    
    #Add new variables for combined sectors.
    dat$nl_allfirm <- (dat$nl_f + dat$nl_nf)
    dat$nl_g_h <- (dat$nl_g + dat$nl_h)
    dat$nl_row_firm <- (dat$nl_allfirm + dat$nl_row)
    
    #Plot the combined variables.    
    ggplot(dat, aes(time.1)) + 
        geom_line(aes(y = dat$nl_g_h, color = "g-h")) + 
        geom_line(aes(y = dat$nl_row_firm, color = "firms-row")) +
        geom_abline("black", size = 0.3, slope = 0, intercept = 0)
    
    # Plot a variety of variables against one another to see if there are any correlations
    ggplot(dat, aes(time.1)) + 
        geom_line(aes(y = dat$nl_nf, color = "nf")) + 
        geom_line(aes(y = dat$nl_row, color = "row")) +
        geom_line(aes(y = dat$nl_allfirm, color = "allfirm")) +
        geom_line(aes(y = dat$nl_f, color = "f")) +
        geom_abline("black", size = 0.3, slope = 0, intercept = 0)
    
    # Create differenced variables and add them to a separate dataframe to be able to plot them.
    nl_h_dif <- diff(dat$nl_h)
    nl_g_dif <- diff(dat$nl_g)
    nl_row_dif <- diff(dat$nl_row)
    nl_nf_dif <- diff(dat$nl_nf)
    nl_f_dif <- diff(dat$nl_f)
    
    # Create new Data.Frame
    dat_1 <- data.frame(nl_h_dif, nl_g_dif, nl_row_dif, nl_nf_dif,nl_f_dif)
    dat_1$time.1 <- (1996:2016)
    # Plot change in net lending of Hh and of Firms
    ggplot(dat_1, aes(time.1)) + 
        geom_line(aes(y = dat_1$nl_h_dif, colour = "Hh")) + 
        geom_line(aes(y = -dat_1$nl_g_dif, colour = "G")) +
        geom_abline("black", size = 0.3, slope = 0, intercept = 0)
    
    # Plot change in net lending of NFC, FC and the rest of the world
    ggplot(dat_1, aes(time.1)) + 
        geom_line(aes(y = dat_1$nl_nf_dif, colour = "nf")) + 
        geom_line(aes(y = -dat_1$nl_row_dif, colour = "row")) +
        geom_line(aes(y = -dat_1$nl_f_dif, colour = "f")) +
        geom_abline("black", size = 0.3, slope = 0, intercept = 0)
    
    # Plot all sectors
    ggplot(dat_1, aes(time.1)) + 
        geom_line(aes(y = dat_1$nl_h_dif, colour = "Hh")) + 
        geom_line(aes(y = -dat_1$nl_g_dif, colour = "G")) +
        geom_line(aes(y = dat_1$nl_nf_dif, colour = "nf")) + 
        geom_line(aes(y = -dat_1$nl_row_dif, colour = "row")) +
        geom_line(aes(y = -dat_1$nl_f_dif, colour = "f")) +
        geom_abline("black", size = 0.3, slope = 0, intercept = 0)
    
    
