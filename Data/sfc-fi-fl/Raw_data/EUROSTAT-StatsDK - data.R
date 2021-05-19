
##################################################################################
#   This code is authored by Robert Smith, Aalborg University
##################################################################################
#   For any assistance please contact Rob Smith at rs@business.aau.dk
##################################################################################

#############################################################################
## If running from R Studio: Check the current running directory of the code
#############################################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#############################################################################

#Clear list environment and EUROSTAT Cache
rm(list = ls())

sapply(c("Formula","knitr","reshape","plm","countrycode","tidyr","dplyr","eurostat","plyr","foreign",
         "ecb","tidyverse","stringr","rstudioapi","ggplot2","corpcor","perturb","randomcoloR",
         "data.table", "rowr", "statsDK"), require, character.only = TRUE)

# Automatically set the working directory to the location of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# clean_eurostat_cache  # If you want to keep the data in the cache do not run this line.
# 
# ## Collect data 
# 
#     #Source Financial balance sheets data
    # f_bs <- get_eurostat(id = "nasa_10_f_bs",  time_format = "num")
    # # f_bs_l <- label_eurostat(f_bs)
    # # label_eurostat_vars(names(f_bs))
    # #Source Financial transactions data
    # f_tr <- get_eurostat(id = "nasa_10_f_tr",  time_format = "num")
    # #Source Revaluation account data
    # f_gl <- get_eurostat(id = "nasa_10_f_gl",  time_format = "num")
    # #Source Other changes in volume data
    # f_oc <- get_eurostat(id = "nasa_10_f_oc",  time_format = "num")
    # #Source Non-financial transactions data
    # nf_tr <- get_eurostat(id = "nasa_10_nf_tr",  time_format = "num")
    # #Source Balance sheets for non-financial assets data
    # nfa_bs <- get_eurostat(id = "nama_10_nfa_bs",  time_format = "num")

#     # 
#     DK_data <- f_tr %>%
#       filter(co_nco == "NCO", geo == "DK", 
#              unit == "MIO_NAC", sector =="S14_S15",
#              na_item == "F62") %>%
#       spread(., key = time, value = values)
#     
#     DK_data <- nf_tr %>%
#       filter(geo == "DK", 
#              unit == "CP_MNAC", sector =="S14_S15") %>%
#       filter(
#              na_item %in% c("D6211","D6212", "D6221", "D6222", "D623")) %>%
#       spread(., key = time, value = values)
    
    # Selection and review of possible data tables to download from DST
    # =================================================================
    # Search for table names
    tables_long_income <- tables %>%
        unnest(variables) %>%
        filter(id == "NAN2")

    # Retrieve the data for the table and adjust all descriptive
    # variables to be usable as variable names in R
    dk_indicators_raw_meta <- retrieve_metadata("NAN2")
    dk_indicators_raw <- retrieve_data("NAN2")
    
    dk_indicators_raw <- retrieve_data("NAN2", TID = "*", )
    
    dk_indicators <- dk_indicators_raw %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\+", "_plus_")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\-", "_minus_")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "B\\.", "B")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "D\\.", "D")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "P\\.", "D")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\*", "--")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = ", real", "--real")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = " ", "_")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "/", "_")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\.", "-")) %>%
        mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = ",", "--")) %>%
        mutate(PRISENHED = str_replace_all(PRISENHED, pattern = "^2", "P_2")) %>%
        mutate(PRISENHED = str_replace_all(PRISENHED, pattern = ", ", "--")) %>%
        mutate(PRISENHED = str_replace_all(PRISENHED, pattern = ",", "--")) %>%
        mutate(PRISENHED = str_replace_all(PRISENHED, pattern = " ", "_"))
    
    # Create lists of unique components of each of the id variables
    TRANSAKT <- data.table(unique(dk_indicators$TRANSAKT))
    PRISENHED <- data.table(unique(dk_indicators$PRISENHED))
    TID <- data.table(unique(dk_indicators$TID))


    # GDP in denmark
    # =================================================================
    # 
    gdp_current <- dk_indicators %>%
        filter(TRANSAKT == "B1--g_Gross_domestic_product", 
               PRISENHED == "Current_prices",
               TID %in% c(min(Benefits$TID):max(Benefits$TID)))%>%
        mutate(gdp_current = as.numeric(INDHOLD)) %>%
        select(-PRISENHED, -TRANSAKT, -INDHOLD)
    
        
    
    # =================================================================
    # =================================================================
    # Distribution of social benefits spending
    # =================================================================
    # 
    # Search for table names
    tables_long_benefits <- tables %>%
        unnest(variables) %>%
      filter(id == "ESSPROS1")
    
    # Retrieve the data for the table and adjust all descriptive
    # variables to be usable as variable names in R
    Benefits <- retrieve_data("ESSPROS1")
    Benefits <- Benefits %>%
      mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = "\\. ", " ")) %>%
      mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = " n.e.c.", "")) %>%
      mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = " ", "_")) %>%
      mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = "/", "_")) %>%
      mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = "\\.", "-")) %>%
      mutate(FORANSTALT = paste0("Cat_", FORANSTALT),
             INDHOLD = as.numeric(INDHOLD))
      
    # Create lists of unique components of each of the id variables
    FORANSTALT <- data.table(unique(Benefits$FORANSTALT))
    YDELSESTYPE <- data.table(unique(Benefits$YDELSESTYPE))
    TID <- data.table(unique(Benefits$TID))
    
    # Create lists for all major totals, and underlying subcomponent groups
    # These are lists of unique descriptors, not the data itself
    Totals <- data.table(c(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_1_")], 
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_2_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_3_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_4_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_5_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_6_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_7_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_8_")],
                         FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_Sch")]))
    # These are the underlying subcomponent groups, same as above
    FORANSTALT_SICKNESS_HEALTH_CARE <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_1")])
    FORANSTALT_DISABILITY <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_2")])
    FORANSTALT_OLD_AGE <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_3")])
    FORANSTALT_SURVIVORS <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_4")])
    FORANSTALT_FAMILY_CHILDREN <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_5")])
    FORANSTALT_UNEMPLOYMENT <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_6")])
    FORANSTALT_HOUSING <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_7")])
    FORANSTALT_SOCIALE_EXCLUSION <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_8")])
    
    # Use the lists and data that was sourced to produce charts
    #
    # The descriptors here are limited to total expenditures -
    #   these are also possible to change to other options such as
    #   gross or net cash benefits, or average incomes etc.

    # Total values for major categories in nominal values
    Totals_plot <- ggplot(data = Benefits %>%
                       filter(FORANSTALT %in% Totals$V1,
                              YDELSESTYPE == "Total social expenditures") %>%
                       filter(FORANSTALT != "Cat_Schemes_total") %>%
                       select(-YDELSESTYPE) %>%
                       group_by(TID), mapping = aes(x = TID, y = INDHOLD, group = FORANSTALT, fill = FORANSTALT, colour = FORANSTALT)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
    Totals_plot

    # Total values as a percentage of overall total
    Totals_plot_pct <- ggplot(data = Benefits %>%
                         filter(FORANSTALT %in% c(Totals$V1),
                                YDELSESTYPE == "Total social expenditures") %>%
                         select(-YDELSESTYPE) %>%
                         spread(., key = FORANSTALT, value = INDHOLD) %>%
                         mutate_at(vars(-TID, -Cat_Schemes_total), ~ (.x %>% (function(x) {x/Cat_Schemes_total}))) %>%
                         select(-Cat_Schemes_total) %>%
                         gather(., -TID, key = Spending_category, value = value) %>%
                         group_by(TID),
                       mapping = aes(x = TID,
                                     y = value,
                                     group = Spending_category,
                                     fill = Spending_category,
                                     colour = Spending_category)) +
      labs(x = "Year", y = "Percentage of total for all categories") +
      geom_line() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(text = element_text(size = 10),
            axis.text.x = element_text(angle=90, vjust=0.5))
    Totals_plot_pct
    
    
    # Total values as a percentage of overall total
    Totals_plot_pct_gdp <- ggplot(data = Benefits %>%
                                      filter(FORANSTALT %in% c(Totals$V1),
                                             YDELSESTYPE == "Total social expenditures") %>%
                                      select(-YDELSESTYPE) %>%
                                      spread(., key = FORANSTALT, value = INDHOLD) %>%
                                      inner_join(., gdp_current, by = "TID") %>%
                                      select(-Cat_Schemes_total) %>%
                                      mutate_at(vars(-TID, -gdp_current), ~ (.x %>% (function(x) {x/gdp_current}))) %>%
                                      select(-gdp_current) %>%
                                      gather(., -TID, key = Spending_category, value = value) %>%
                                      group_by(TID),
                                  mapping = aes(x = as.factor(TID),
                                                y = value,
                                                group = Spending_category,
                                                fill = Spending_category,
                                                colour = Spending_category)) +
        labs(x = "Year", y = "Percentage of GDP (nominal)") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
                                  
    Totals_plot_pct_gdp
    
    
    
    
    # Subcategory: Old age spending, split into underlying components in nominal levels
    Old_age_plot <- ggplot(data = Benefits %>%
                       filter(FORANSTALT %in% FORANSTALT_OLD_AGE$V1,
                              YDELSESTYPE == "Total social expenditures") %>%
                       select(-YDELSESTYPE) %>%
                       group_by(TID),
    mapping = aes(x = TID, y = INDHOLD, group = FORANSTALT, fill = FORANSTALT, colour = FORANSTALT)) +
      geom_line() +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
    Old_age_plot

    # Subcategory: Old age spending, split into underlying components as percentage
    #   of total Old age spending.
    Old_age_plot_pct <- ggplot(data = Benefits %>%
                                 filter(FORANSTALT %in% FORANSTALT_OLD_AGE$V1,
                                        YDELSESTYPE == "Total social expenditures") %>%
                                 select(-YDELSESTYPE) %>%
                                 spread(., key = FORANSTALT, value = INDHOLD) %>%
                                 mutate_at(vars(-TID, -Cat_3_OLD_AGE), ~ (.x %>% (function(x) {x/Cat_3_OLD_AGE}))) %>%
                                 select(-Cat_3_OLD_AGE) %>%
                                 gather(., -TID, key = Spending_category, value = Percent_of_category)%>%
                                 group_by(TID),
                               mapping = aes(x = as.factor(TID),
                                             y = Percent_of_category,
                                             group = Spending_category,
                                             fill = Spending_category,
                                             colour = Spending_category)) +
      labs(x = "Year", y = "Percentage of total in category") +
      geom_line() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(text = element_text(size = 10),
            axis.text.x = element_text(angle=90, vjust=0.5))
    Old_age_plot_pct
    
    
    
    
    
    
    
    
    
    
    
    # =================================================================
    # =================================================================
    # =================================================================
    # =================================================================
    # Types of income in Denmark, split between family groups
    # =================================================================
    # 
    # Search for table names
    # =================================================================
    # tables_long_income <- tables %>%
    #     unnest(variables) %>%
    #     filter(id == "INDKF104")
    # 
    # tables_long_income <- tables %>%
    #     unnest(variables) %>%
    #     filter(id == "INDKP101")
    
    # Retrieve the data for the table and adjust all descriptive
    # variables to be usable as variable names in R
    dk_inc_indiv_count_meta <- retrieve_metadata("INDKP101")
    dk_inc_family_count_meta <- retrieve_metadata("INDKF104")
    
    # Families
    # =================================================================
    
    
    # Income - Families - Count of families - all categories
    dk_inc_family_count_data <- retrieve_data(table_id = "INDKF104", 
                                            ENHED = "100",
                                            OMRÅDE = "000", 
                                            SOCIO = "100",
                                            INDKOMSTTYPE = "*",
                                            Tid = "*") %>%
        select(-c(OMRÅDE, ENHED, SOCIO)) %>%
        mutate(INDHOLD = INDHOLD) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # Income - Families - Count of families - pension categories
    dk_inc_pens_family_count_data <- retrieve_data(table_id = "INDKF104", 
                                              ENHED = "100",
                                              OMRÅDE = "000", 
                                              SOCIO = "100",
                                              INDKOMSTTYPE = paste0(seq(from=190, to = 220, by = 5), collapse=","),
                                              Tid = "*") %>%
        select(-c(OMRÅDE, ENHED, SOCIO)) %>%
        mutate(INDHOLD = INDHOLD) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # Income - Families - total amount for families - all categories
    dk_inc_family_kr_data <- retrieve_data(table_id = "INDKF104", 
                                        ENHED = "110",
                                        OMRÅDE = "000", 
                                        SOCIO = "100",
                                        INDKOMSTTYPE = "*",
                                        Tid = "*") %>%
        select(-c(OMRÅDE, ENHED, SOCIO)) %>%
        mutate(INDHOLD = round(INDHOLD/1000), 0) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # Income - Families - total amount for families - pension categories
    dk_inc_pens_family_kr_data <- retrieve_data(table_id = "INDKF104", 
                                           ENHED = "110",
                                           OMRÅDE = "000", 
                                           SOCIO = "100",
                                           INDKOMSTTYPE = paste0(seq(from=190, to = 220, by = 5), collapse=","),
                                           Tid = "*") %>%
        select(-c(OMRÅDE, ENHED, SOCIO)) %>%
        mutate(INDHOLD = round(INDHOLD/1000), 0) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # =================================================================
    # Income - Persons - Count of individuals - all categories
    dk_inc_count_data <- retrieve_data(table_id = "INDKP101", 
                                            OMRÅDE = "000", 
                                            ENHED = "101",
                                            KOEN = "MOK",
                                            INDKOMSTTYPE = "*",
                                            TID = "*") %>%
        select(-c(OMRÅDE, ENHED, KOEN))%>%
        mutate(INDHOLD = as.numeric(INDHOLD)) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # Income - Persons - Count of individuals - pension categories
    dk_pens_inc_count_data <- retrieve_data(table_id = "INDKP101", 
                                            OMRÅDE = "000", 
                                            ENHED = "101",
                                            KOEN = "MOK",
                                            INDKOMSTTYPE = paste0(seq(from=190, to = 220, by = 5), collapse=","),
                                            TID = "*") %>%
        select(-c(OMRÅDE, ENHED, KOEN))%>%
        mutate(INDHOLD = as.numeric(INDHOLD)) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # Income - Persons - total amounts
    dk_pens_inc_kr_data <- retrieve_data(table_id = "INDKP101", 
                                           OMRÅDE = "000", 
                                           ENHED = "110",
                                           KOEN = "MOK",
                                           INDKOMSTTYPE = paste0(seq(from=190, to = 220, by = 5), collapse=","),
                                           TID = "*") %>%
        select(-c(OMRÅDE, ENHED, KOEN))%>%
        mutate(INDHOLD = str_replace_all(INDHOLD, pattern = "\\.\\.", "0")) %>%
        mutate(INDHOLD = round(as.numeric(INDHOLD)/1000), 0) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    # Income - Persons - average income per category
    dk_pens_inc_kr_mean_data <- retrieve_data(table_id = "INDKP101", 
                                              OMRÅDE = "000", 
                                              ENHED = "121",
                                              KOEN = "MOK",
                                              INDKOMSTTYPE = paste0(seq(from=190, to = 220, by = 5), collapse=","),
                                              TID = "*") %>%
        select(-c(OMRÅDE, ENHED, KOEN))%>%
        mutate(INDHOLD = str_replace_all(INDHOLD, pattern = "\\.\\.", "0")) %>%
        mutate(INDHOLD = round(as.numeric(INDHOLD)), 0) %>%
        filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))
    
    
    # Use the lists and data that was sourced to produce charts
    # 
    # The descriptors here are limited to total expenditures - 
    #   these are also possible to change to other options such as
    #   gross or net cash benefits, or average incomes etc.
    
    # PLOT: Income by category: Count of persons: Pension categories
    dk_pens_inc_count_data_plot <- ggplot(data = dk_pens_inc_count_data %>%
                              group_by(TID), 
                          mapping = aes(x = as.factor(TID), 
                                        y = INDHOLD, 
                                        group = INDKOMSTTYPE, 
                                        fill = INDKOMSTTYPE, 
                                        colour = INDKOMSTTYPE)) +
        labs(x = "Year", y = "Total Persons in each category") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    dk_pens_inc_count_data_plot
    
    # PLOT: Income by category: Income of persons: Pension categories
    dk_pens_inc_kr_data_plot <- ggplot(data = dk_pens_inc_kr_data %>%
                                              group_by(TID), 
                                          mapping = aes(x = as.factor(TID), 
                                                        y = INDHOLD, 
                                                        group = INDKOMSTTYPE, 
                                                        fill = INDKOMSTTYPE, 
                                                        colour = INDKOMSTTYPE)) +
        labs(x = "Year", y = "Total Persons in each category (DKK m)") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    dk_pens_inc_kr_data_plot

    # PLOT: Income by category: Income of persons: Pension categories: Mean
    dk_pens_inc_kr_mean_data_plot <- ggplot(data = dk_pens_inc_kr_mean_data %>%
                                            group_by(TID), 
                                        mapping = aes(x = as.factor(TID), 
                                                      y = INDHOLD, 
                                                      group = INDKOMSTTYPE, 
                                                      fill = INDKOMSTTYPE, 
                                                      colour = INDKOMSTTYPE)) +
        labs(x = "Year", y = "Average Pension Income/p in each category (DKK)") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    dk_pens_inc_kr_mean_data_plot    
    
    
    # PLOT: Income by category: Income of persons: Pension categories: Percentage of total pension income
    dk_pens_inc_kr_pct_pen_inc_plot <- ggplot(data = dk_pens_inc_kr_data %>%
                                                spread(., key = INDKOMSTTYPE, value = INDHOLD) %>%
                                                mutate(total = rowSums(select(.,-TID))) %>%
                                                mutate_at(vars(-TID, -total), ~ (.x %>% (function(x) {x/total}))) %>%
                                                select(-total) %>%
                                                gather(., -TID, key = INDKOMSTTYPE, value = INDHOLD) %>%
                                                group_by(TID),
                                            mapping = aes(x = as.factor(TID), 
                                                          y = INDHOLD, 
                                                          group = INDKOMSTTYPE, 
                                                          fill = INDKOMSTTYPE, 
                                                          colour = INDKOMSTTYPE)) +
        labs(x = "Year", y = "Percentage of total pension incomes") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    dk_pens_inc_kr_pct_pen_inc_plot
    
    
    # PLOT: Income by category: Income of persons: Pension categories: Percentage of GDP
    dk_pens_inc_kr_pct_gdp_plot <- ggplot(data = dk_pens_inc_kr_data %>%
                                                spread(., key = INDKOMSTTYPE, value = INDHOLD) %>%
                                                inner_join(., gdp_current, by = "TID") %>%
                                                mutate_at(vars(-TID, -gdp_current), ~ (.x %>% (function(x) {x/gdp_current}))) %>%
                                                select(-gdp_current) %>%
                                                gather(., -TID, key = INDKOMSTTYPE, value = INDHOLD) %>%
                                                group_by(TID),
                                            mapping = aes(x = as.factor(TID), 
                                                          y = INDHOLD, 
                                                          group = INDKOMSTTYPE, 
                                                          fill = INDKOMSTTYPE, 
                                                          colour = INDKOMSTTYPE)) +
        labs(x = "Year", y = "Percentage of GDP") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    dk_pens_inc_kr_pct_gdp_plot
    
    
    
    
    # Total values as a percentage of overall total
    Totals_plot_pct <- ggplot(data = dk_income_persons_raw %>%
                                  filter(INDKOMSTTYPE %in% c(Totals$V1),
                                         YDELSESTYPE == "Total social expenditures") %>%
                                  select(-YDELSESTYPE) %>%
                                  spread(., key = INDKOMSTTYPE, value = INDHOLD) %>%
                                  mutate_at(vars(-TID, -Cat_Schemes_total), ~ (.x %>% (function(x) {x/Cat_Schemes_total}))) %>%
                                  select(-Cat_Schemes_total) %>%
                                  gather(., -TID, key = Spending_category, value = value) %>%
                                  group_by(TID),
                              mapping = aes(x = as.factor(TID),
                                            y = value,
                                            group = Spending_category,
                                            fill = Spending_category,
                                            colour = Spending_category)) +
        labs(x = "Year", y = "Percentage of total for all categories") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    Totals_plot_pct


    # Subcategory: Old age spending, split into underlying components in nominal levels
    Old_age_plot <- ggplot(data = Benefits %>%
                               filter(INDKOMSTTYPE %in% INDKOMSTTYPE_OLD_AGE$V1,
                                      YDELSESTYPE == "Total social expenditures") %>%
                               select(-YDELSESTYPE) %>%
                               group_by(TID),
                           mapping = aes(x = TID, y = INDHOLD, group = INDKOMSTTYPE, fill = INDKOMSTTYPE, colour = INDKOMSTTYPE)) +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    Old_age_plot

    # Subcategory: Old age spending, split into underlying components as percentage
    #   of total Old age spending.
    Old_age_plot_pct <- ggplot(data = Benefits %>%
                                   filter(INDKOMSTTYPE %in% INDKOMSTTYPE_OLD_AGE$V1,
                                          YDELSESTYPE == "Total social expenditures") %>%
                                   select(-YDELSESTYPE) %>%
                                   spread(., key = INDKOMSTTYPE, value = INDHOLD) %>%
                                   mutate_at(vars(-TID, -Cat_3_OLD_AGE), ~ (.x %>% (function(x) {x/Cat_3_OLD_AGE}))) %>%
                                   select(-Cat_3_OLD_AGE) %>%
                                   gather(., -TID, key = Spending_category, value = Percent_of_category)%>%
                                   group_by(TID),
                               mapping = aes(x = as.factor(TID),
                                             y = Percent_of_category,
                                             group = Spending_category,
                                             fill = Spending_category,
                                             colour = Spending_category)) +
        labs(x = "Year", y = "Percentage of total in category") +
        geom_line() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent_format()) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle=90, vjust=0.5))
    Old_age_plot_pct
    
    
    


    
    
    
    
    # =================================================================
    # =================================================================
    # =================================================================
    # =================================================================
    # Interest rates
    # =================================================================
    # 
    # Search for table names
    # =================================================================
    # 
    # 
    # 
    # 
    # Selection and review of possible data tables to download from DST
    # =================================================================
    # Search for table names
    tables_long_mortgage_interest <- tables %>%
      unnest(variables) %>%
      filter(id == "DNRNURI")
    
    # Retrieve the data for the table and adjust all descriptive
    # variables to be usable as variable names in R
    dk_mortgage_interest_raw_meta <- retrieve_metadata("DNRNURI")
    # ____$variables$id
    # ____$variables$text
    # ____$variables$values
    dk_mortgage_interest_raw_meta$variables$id
    dk_mortgage_interest_raw_meta$variables$values
    
    dk_indicators_raw <- retrieve_data("DNRNURI")
    
    # Families
    # =================================================================
    Tid <- data.table(as.Date(dk_mortgage_interest_raw_data$TID, format='%Y:%m'))
    
    tid <- dk_mortgage_interest_raw_data$TID
    
    date<-as.Date(dk_mortgage_interest_raw_data$TID,format='%Y%m%d')
    
    
    # Interest rates: Household
    dk_mortgage_interest_raw_data <- retrieve_data(table_id = "DNRNURI", 
                                                   DATA = paste0(c("AL51EFFR", "AL51BIDS"),collapse = ","),
                                                   INDSEK = paste0(c("1400"),collapse = ","),
                                                   VALUTA = "z01", 
                                                   LØBETID1 = "ALLE",
                                                   RENTFIX = paste0(c("ALLE", "S10A"),collapse = ","),
                                                   LAANSTR = "ALLE",
                                                   Tid = "*") %>%
      select(-c(VALUTA, LØBETID1, LAANSTR, INDSEK)) %>%
      mutate(INDHOLD = str_replace_all(INDHOLD, pattern = "..", "")) %>%
      mutate(INDHOLD = as.numeric(INDHOLD, na.rm = TRUE)) %>%
      mutate(TID = str_replace_all(TID, pattern = "M", "")) %>%
      mutate(TID = paste0(TID, "01")) %>%
      mutate(TID <- as.Date(Tid,format='%Y%m%d'))  %>%
      filter(INDHOLD !is.na) %>%
      group_by(TID)
    
    # Income - Families - Count of families - pension categories
    dk_inc_pens_family_count_data <- retrieve_data(table_id = "INDKF104", 
                                                   ENHED = "100",
                                                   OMRÅDE = "000", 
                                                   SOCIO = "100",
                                                   INDKOMSTTYPE = paste0(seq(from=190, to = 220, by = 5), collapse=","),
                                                   Tid = "*") %>%
      select(-c(OMRÅDE, ENHED, SOCIO)) %>%
      mutate(INDHOLD = INDHOLD) %>%
      filter(TID %in% c(min(Benefits$TID):max(Benefits$TID)))