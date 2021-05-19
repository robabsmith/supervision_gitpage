###################################################################################

# Plots and data set creator for sector financial assets and liabilities
# ======================================================================
# 
# The purpose of this document is to generate charts for relevant sector and
# national balance sheets.

# This file references data sourced from Eurostat
# ===============================================
# 
# The file is called dat_raw.csv and should be in the same directory,
# this file contains all relevant eurostat data for Denmark with respect
# to the European SNA 2010. Fixed assets, and Financial and non-financial 
# transactions, other changes, revaluations, and balance sheet figures.

# Author information
# ==================
# 
# Robert Smith
# Aalborg University
# Department of Business and Management
# rs@business.aau.dk
# Denmark

###################################################################################

###################################################################################
# Load Packages
###################################################################################
sapply(c("pipeR", "ggplot2", "readr", "lubridate", "data.table", "tidyverse", "reshape2"), 
       require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###################################################################################
# Read data and category file
###################################################################################
dat_tot <- read_csv("./dat_raw.csv")

categories <- read_csv("./categories.csv")

###################################################################################
# GDP and GDP growth variabels
###################################################################################

gdp <-  dat_tot$gnp_s1_P

gdp_growth <- data.frame(diff(dat_tot$gnp_s1_P)/((data.frame(dat_tot$gnp_s1_P))[-1,]))
gdp_growth <- rbind(c(NA,NA), gdp_growth)

gdp_growth <- data.frame(dat_tot$time, gdp_growth)
colnames(gdp_growth) <- c("Year","GDP_growth")

###################################################################################
# Plot Net financial assets for each sector
###################################################################################

DK_NFA <- dat_tot%>%
    mutate(Hh = (fft_h_bs_A - fft_h_bs_L)/gnp_s1_P,
           NFC = (fft_nf_bs_A - fft_nf_bs_L)/gnp_s1_P,
           FC = (fft_f_bs_A - fft_f_bs_L)/gnp_s1_P,
           G = (fft_g_bs_A - fft_g_bs_L)/gnp_s1_P,
           RoW = (fft_row_bs_A - fft_row_bs_L)/gnp_s1_P)%>%
    select(time,Hh,NFC,FC,G,RoW)%>%
    rename(Year = "time")

NFA_5_sector <- melt(DK_NFA, id = c("Year"), na.rm=TRUE)
NFA_5_sector <- NFA_5_sector%>%
    rename(Sector = "variable")
NFA_5_sector <- data.frame(NFA_5_sector)


theme <- theme(legend.position = "bottom",
              panel.grid.minor = element_blank(),
              legend.title = element_blank(),
              text = element_text(size = 6),
              axis.text.x = element_text(angle=90, vjust=0.5))

DK_NFA_plot <- ggplot(NFA_5_sector,
                           aes(Year, 
                               value, 
                               group = Sector)) + 
    geom_line(aes(colour=Sector), linetype=1) + 
    labs(x = "Year",
         y = "NFA as 5 of GDP",
         title = "DK: Sector Net Financial Assets") +
    scale_y_continuous(expand = c(0,0.005),
                       labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    theme_minimal() + 
    theme
DK_NFA_plot

ggsave("./Charts/DK_NFA_plot.pdf", plot = DK_NFA_plot, width = 10, height = 7, units = "cm")

###################################################################################
# Plot Net lending for each sector
###################################################################################

B9_5_sector <- data.frame(dat_tot$time, 
                          dat_tot$nl_h_P/dat_tot$gnp_s1_P, 
                          dat_tot$nl_nf_P/dat_tot$gnp_s1_P, 
                          dat_tot$nl_f_P/dat_tot$gnp_s1_P, 
                          dat_tot$nl_g_P/dat_tot$gnp_s1_P, 
                          dat_tot$nl_row_P/dat_tot$gnp_s1_P)

colnames(B9_5_sector) <- c("Year","Hh", "NFC", "FC", "Govt", "RoW")
B9_5_sector <- melt(B9_5_sector, id = c("Year"), na.rm=TRUE)
B9_5_sector <- B9_5_sector%>%
    rename(Sector = "variable")
B9_5_sector <- data.frame(B9_5_sector)

# Line Plot
###################################################################################
B9_5_sector_plot <- ggplot(B9_5_sector,
                            aes(Year, 
                                value, 
                                group = Sector)) + 
    geom_line(aes(colour=Sector), linetype=1) + 
    labs(x = "Year",
         y = "Sector net lending",
         title = "DK Sector net lending annually") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    theme_minimal() + 
    theme
B9_5_sector_plot

ggsave("./Charts/B9_5_sector_plot.pdf", plot = B9_5_sector_plot, width = 10, height = 7, units = "cm")

###################################################################################
# Plot Net financial assets for each sector
###################################################################################

gross_balance_sheet_totals = data.table(dat_tot$fft_h_bs_A, dat_tot$fft_h_bs_L, dat_tot$fft_g_bs_A, dat_tot$fft_g_bs_L, dat_tot$fft_f_bs_A, dat_tot$fft_f_bs_L, dat_tot$fft_nf_bs_A, dat_tot$fft_nf_bs_L, dat_tot$fft_row_bs_A, dat_tot$fft_row_bs_L)
colnames(gross_balance_sheet_totals) = c("Hh_A", "Hh_L", "Govt_A", "Govt_L", "FC_A", "FC_L", "NFC_A", "NFC_L", "RoW_A", "RoW_L")

A_L_GDP = gross_balance_sheet_totals / gdp
A_L_GDP = data.table(A_L_GDP, dat_tot$time)

colnames(A_L_GDP) = c("Hh_A", "Hh_L", "Govt_A", "Govt_L", "FC_A", "FC_L", "NFC_A", "NFC_L", "RoW_A", "RoW_L", "Year")

A_L_GDP = melt(A_L_GDP, id = c("Year"), na.rm=TRUE)
colnames(A_L_GDP) = c("Year", "Sector_Fin", "Value")

A_L_GDP = A_L_GDP %>%
    separate(Sector_Fin, c("Sector", "A_L"), sep = "_")

# Bar Plot
###################################################################################
plot_S1_FAL_GDP <- ggplot() + 
    geom_bar(mapping=aes(x = factor(Year),
                         y= Value,
                         fill = reorder(Sector,Value)),
             data = A_L_GDP,
             position = "stack",
             stat = "identity") +
    facet_grid(~A_L) +
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "Denmark: Financial assets and liabilities per cent of GDP") +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 6)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
    guides(fill=guide_legend(nrow = 2, byrow = TRUE))
plot_S1_FAL_GDP

ggsave("./Charts/plot_S1_FAL_GDP.pdf", plot = plot_S1_FAL_GDP, width = 10, height = 7, units = "cm")

# Line Plot
###################################################################################
plot_line_S1_FAL_GDP <- ggplot(A_L_GDP,
                           aes(Year, 
                               Value, 
                               group = Sector)) + 
    geom_line(aes(colour=Sector), linetype=1) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "Denmark: Financial assets and liabilities per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    facet_grid(~A_L) +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    theme(legend.title = element_blank()) + 
    theme(text = element_text(size = 6)) + 
    theme(axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_S1_FAL_GDP

ggsave("./Charts/plot_line_S1_FAL_GDP.pdf", plot = plot_line_S1_FAL_GDP, width = 10, height = 7, units = "cm")

###################################################################################
# Household Sector
###################################################################################

BS_comp_h = data.table(dat_tot$ffmg_h_bs_A, dat_tot$ffmd_h_bs_A, dat_tot$ffd_h_bs_A, dat_tot$ffl_h_bs_A, dat_tot$ffeq_h_bs_A, dat_tot$ffins_h_bs_A, dat_tot$ffdop_h_bs_A, dat_tot$ffoa_h_bs_A, dat_tot$ffmg_h_bs_L, dat_tot$ffmd_h_bs_L, dat_tot$ffd_h_bs_L, dat_tot$ffl_h_bs_L, dat_tot$ffeq_h_bs_L, dat_tot$ffins_h_bs_L, dat_tot$ffdop_h_bs_L, dat_tot$ffoa_h_bs_L)

BS_comp_h_GDP = BS_comp_h / gdp
BS_comp_h_GDP = data.table(BS_comp_h_GDP, dat_tot$time)

colnames(BS_comp_h_GDP) = c("mg_h_A", "md_h_A", "d_h_A", "l_h_A", "eq_h_A", "ins_h_A", "dop_h_A", "oa_h_A", "mg_h_L", "md_h_L", "d_h_L", "l_h_L", "eq_h_L", "ins_h_L", "dop_h_L", "oa_h_L", "Year")

BS_comp_h_GDP = melt(BS_comp_h_GDP, id = c("Year"), na.rm=TRUE)
colnames(BS_comp_h_GDP) = c("Year", "Variable", "Value")

BS_comp_h_GDP = BS_comp_h_GDP %>%
    separate(Variable, c("Category","Sector", "A_L"), sep = "_") %>%
    select(-Sector) %>%
    filter(Value != 0) %>%
    inner_join(.,categories, "Category") %>%
    select(-Category) %>%
    rename("Category"= Cat_text)



# Bar Plot
###################################################################################
plot_h_FAL_GDP <- ggplot() + 
    geom_bar(mapping=aes(x = factor(Year),
                         y= Value,
                         fill = Category),
             data = BS_comp_h_GDP,
             position = "stack",
             stat = "identity") +
    facet_grid(~A_L) +
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "Hh: Financial assets and liabilities per cent of GDP") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 6)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
    guides(fill=guide_legend(nrow = 2, byrow = TRUE))
plot_h_FAL_GDP

ggsave("./Charts/plot_h_FAL_GDP.pdf", plot = plot_h_FAL_GDP, width = 10, height = 7, units = "cm")

# Line Plot
###################################################################################
plot_line_h_FAL_GDP <- ggplot(BS_comp_h_GDP,
                               aes(Year, 
                                   Value, 
                                   group = Category)) + 
    geom_line(aes(colour=Category), linetype=1) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "Hh: Financial assets and liabilities per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    facet_grid(~A_L) +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    theme(legend.title = element_blank()) + 
    theme(text = element_text(size = 6)) + 
    theme(axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_h_FAL_GDP

ggsave("./Charts/plot_line_h_FAL_GDP.pdf", plot = plot_line_h_FAL_GDP, width = 10, height = 7, units = "cm")


###################################################################################
# Non-Financial Sector
###################################################################################

BS_comp_nf = data.table(dat_tot$ffmd_nf_bs_A, dat_tot$ffd_nf_bs_A, dat_tot$ffl_nf_bs_A, dat_tot$ffeq_nf_bs_A, dat_tot$ffins_nf_bs_A, dat_tot$ffdop_nf_bs_A, dat_tot$ffoa_nf_bs_A, dat_tot$ffmd_nf_bs_L, dat_tot$ffd_nf_bs_L, dat_tot$ffl_nf_bs_L, dat_tot$ffeq_nf_bs_L, dat_tot$ffins_nf_bs_L, dat_tot$ffdop_nf_bs_L, dat_tot$ffoa_nf_bs_L)

BS_comp_nf_GDP = BS_comp_nf / gdp
BS_comp_nf_GDP = data.table(BS_comp_nf_GDP, dat_tot$time)

colnames(BS_comp_nf_GDP) = c("md_nf_A", "d_nf_A", "l_nf_A", "eq_nf_A", "ins_nf_A", "dop_nf_A", "oa_nf_A", "md_nf_L", "d_nf_L", "l_nf_L", "eq_nf_L", "ins_nf_L", "dop_nf_L", "oa_nf_L", "Year")

BS_comp_nf_GDP = melt(BS_comp_nf_GDP, id = c("Year"), na.rm=TRUE)
colnames(BS_comp_nf_GDP) = c("Year", "Variable", "Value")

BS_comp_nf_GDP = BS_comp_nf_GDP %>%
    separate(Variable, c("Category","Sector", "A_L"), sep = "_") %>%
    select(-Sector) %>%
    filter(Value != 0) %>%
    inner_join(.,categories, "Category") %>%
    select(-Category) %>%
    rename("Category"= Cat_text)

# Bar Plot
###################################################################################
plot_nf_FAL_GDP <- ggplot() + 
    geom_bar(mapping=aes(x = factor(Year),
                         y= Value,
                         fill = Category),
             data = BS_comp_nf_GDP,
             position = "stack",
             stat = "identity") +
    facet_grid(~A_L) +
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "NFC: Financial assets and liabilities per cent of GDP") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 6)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
    guides(fill=guide_legend(nrow = 2, byrow = TRUE))
plot_nf_FAL_GDP

ggsave("./Charts/plot_nf_FAL_GDP.pdf", plot = plot_nf_FAL_GDP, width = 10, height = 7, units = "cm")


# Line Plot
###################################################################################
plot_line_nf_FAL_GDP <- ggplot(BS_comp_nf_GDP,
                              aes(Year, 
                                  Value, 
                                  group = Category)) + 
    geom_line(aes(colour=Category), linetype=1) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "NFC: Financial assets and liabilities per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    facet_grid(~A_L) +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    theme(legend.title = element_blank()) + 
    theme(text = element_text(size = 6)) + 
    theme(axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_nf_FAL_GDP

ggsave("./Charts/plot_line_nf_FAL_GDP.pdf", plot = plot_line_nf_FAL_GDP, width = 10, height = 7, units = "cm")

###################################################################################
# Govt Sector
###################################################################################

BS_comp_g = data.table(dat_tot$ffmg_g_bs_A, dat_tot$ffmd_g_bs_A, dat_tot$ffd_g_bs_A, dat_tot$ffl_g_bs_A, dat_tot$ffeq_g_bs_A, dat_tot$ffins_g_bs_A, dat_tot$ffdop_g_bs_A, dat_tot$ffoa_g_bs_A, dat_tot$ffmg_g_bs_L, dat_tot$ffmd_g_bs_L, dat_tot$ffd_g_bs_L, dat_tot$ffl_g_bs_L, dat_tot$ffeq_g_bs_L, dat_tot$ffins_g_bs_L, dat_tot$ffdop_g_bs_L, dat_tot$ffoa_g_bs_L)

BS_comp_g_GDP = BS_comp_g / gdp
BS_comp_g_GDP = data.table(BS_comp_g_GDP, dat_tot$time)

colnames(BS_comp_g_GDP) = c("mg_g_A", "md_g_A", "d_g_A", "l_g_A", "eq_g_A", "ins_g_A", "dop_g_A", "oa_g_A", "mg_g_L", "md_g_L", "d_g_L", "l_g_L", "eq_g_L", "ins_g_L", "dop_g_L", "oa_g_L", "Year")

BS_comp_g_GDP = melt(BS_comp_g_GDP, id = c("Year"), na.rm=TRUE)
colnames(BS_comp_g_GDP) = c("Year", "Variable", "Value")

BS_comp_g_GDP = BS_comp_g_GDP %>%
    separate(Variable, c("Category","Sector", "A_L"), sep = "_") %>%
    select(-Sector) %>%
    filter(Value != 0) %>%
    inner_join(.,categories, "Category") %>%
    select(-Category) %>%
    rename("Category"= Cat_text)

# Bar Plot
###################################################################################
plot_g_FAL_GDP <- ggplot() + 
    geom_bar(mapping=aes(x = factor(Year),
                         y= Value,
                         fill = Category),
             data = BS_comp_g_GDP,
             position = "stack",
             stat = "identity") +
    facet_grid(~A_L) +
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "Govt: Financial assets and liabilities per cent of GDP") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 6)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
    guides(fill=guide_legend(nrow = 2, byrow = TRUE))
plot_g_FAL_GDP

ggsave("./Charts/plot_g_FAL_GDP.pdf", plot = plot_g_FAL_GDP, width = 10, height = 7, units = "cm")

# Line Plot
###################################################################################
plot_line_g_FAL_GDP <- ggplot(BS_comp_g_GDP,
                              aes(Year, 
                                  Value, 
                                  group = Category)) + 
    geom_line(aes(colour=Category), linetype=1) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "Govt: Financial assets and liabilities per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    facet_grid(~A_L) +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    theme(legend.title = element_blank()) + 
    theme(text = element_text(size = 6)) + 
    theme(axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_g_FAL_GDP

ggsave("./Charts/plot_line_g_FAL_GDP.pdf", plot = plot_line_g_FAL_GDP, width = 10, height = 7, units = "cm")

###################################################################################
# Rest of the World Sector
###################################################################################

BS_comp_row = data.table(dat_tot$ffmg_row_bs_A, dat_tot$ffmd_row_bs_A, dat_tot$ffd_row_bs_A, dat_tot$ffl_row_bs_A, dat_tot$ffeq_row_bs_A, dat_tot$ffins_row_bs_A, dat_tot$ffdop_row_bs_A, dat_tot$ffoa_row_bs_A, dat_tot$ffmg_row_bs_L, dat_tot$ffmd_row_bs_L, dat_tot$ffd_row_bs_L, dat_tot$ffl_row_bs_L, dat_tot$ffeq_row_bs_L, dat_tot$ffins_row_bs_L, dat_tot$ffdop_row_bs_L, dat_tot$ffoa_row_bs_L)

BS_comp_row_GDP = BS_comp_row / gdp
BS_comp_row_GDP = data.table(BS_comp_row_GDP, dat_tot$time)

colnames(BS_comp_row_GDP) = c("mg_row_A", "md_row_A", "d_row_A", "l_row_A", "eq_row_A", "ins_row_A", "dop_row_A", "oa_row_A", "mg_row_L", "md_row_L", "d_row_L", "l_row_L", "eq_row_L", "ins_row_L", "dop_row_L", "oa_row_L", "Year")

BS_comp_row_GDP = melt(BS_comp_row_GDP, id = c("Year"), na.rm=TRUE)
colnames(BS_comp_row_GDP) = c("Year", "Variable", "Value")

BS_comp_row_GDP = BS_comp_row_GDP %>%
    separate(Variable, c("Category","Sector", "A_L"), sep = "_") %>%
    select(-Sector) %>%
    filter(Value != 0) %>%
    inner_join(.,categories, "Category") %>%
    select(-Category) %>%
    rename("Category"= Cat_text)

# Bar Plot
###################################################################################
plot_row_FAL_GDP <- ggplot() + 
    geom_bar(mapping=aes(x = factor(Year),
                         y= Value,
                         fill = Category),
             data = BS_comp_row_GDP,
             position = "stack",
             stat = "identity") +
    facet_grid(~A_L) +
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "RoW: Financial assets and liabilities per cent of GDP") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_row_FAL_GDP

ggsave("./Charts/plot_row_FAL_GDP.pdf", plot = plot_row_FAL_GDP, width = 10, height = 7, units = "cm")

# Line Plot
###################################################################################
plot_line_row_FAL_GDP <- ggplot(BS_comp_row_GDP,
                              aes(Year, 
                                  Value, 
                                  group = Category)) + 
    geom_line(aes(colour=Category), linetype=1) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "RoW: Financial assets and liabilities per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    facet_grid(~A_L) +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_row_FAL_GDP

ggsave("./Charts/plot_line_row_FAL_GDP.pdf", plot = plot_line_row_FAL_GDP, width = 10, height = 7, units = "cm")


###################################################################################
# Financial Corporates Sector
###################################################################################

BS_comp_f = data.table(dat_tot$ffmg_f_bs_A, dat_tot$ffmd_f_bs_A, dat_tot$ffd_f_bs_A, dat_tot$ffl_f_bs_A, dat_tot$ffeq_f_bs_A, dat_tot$ffins_f_bs_A, dat_tot$ffdop_f_bs_A, dat_tot$ffoa_f_bs_A, dat_tot$ffmg_f_bs_L, dat_tot$ffmd_f_bs_L, dat_tot$ffd_f_bs_L, dat_tot$ffl_f_bs_L, dat_tot$ffeq_f_bs_L, dat_tot$ffins_f_bs_L, dat_tot$ffdop_f_bs_L, dat_tot$ffoa_f_bs_L)

BS_comp_f_GDP = BS_comp_f / gdp
BS_comp_f_GDP = data.table(BS_comp_f_GDP, dat_tot$time)

colnames(BS_comp_f_GDP) = c("mg_f_A", "md_f_A", "d_f_A", "l_f_A", "eq_f_A", "ins_f_A", "dop_f_A", "oa_f_A", "mg_f_L", "md_f_L", "d_f_L", "l_f_L", "eq_f_L", "ins_f_L", "dop_f_L", "oa_f_L", "Year")

BS_comp_f_GDP = melt(BS_comp_f_GDP, id = c("Year"), na.rm=TRUE)
colnames(BS_comp_f_GDP) = c("Year", "Variable", "Value")

BS_comp_f_GDP = BS_comp_f_GDP %>%
    separate(Variable, c("Category","Sector", "A_L"), sep = "_") %>%
    select(-Sector) %>%
    filter(Value != 0) %>%
    inner_join(.,categories, "Category") %>%
    select(-Category) %>%
    rename("Category"= Cat_text)

# Bar Plot
###################################################################################
plot_f_FAL_GDP <- ggplot() + 
    geom_bar(mapping=aes(x = factor(Year),
                         y= Value,
                         fill = Category),
             data = BS_comp_f_GDP,
             position = "stack",
             stat = "identity") +
    facet_grid(~A_L) +
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "FC: Financial assets and liabilities per cent of GDP") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_f_FAL_GDP

ggsave("./Charts/plot_f_FAL_GDP.pdf", plot = plot_f_FAL_GDP, width = 10, height = 7, units = "cm")

# Line Plot
###################################################################################
plot_line_f_FAL_GDP <- ggplot(BS_comp_f_GDP,
                              aes(Year, 
                                  Value, 
                                  group = Category)) + 
    geom_line(aes(colour=Category), linetype=1) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "FC: Financial assets and liabilities per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       sec.axis = sec_axis(~(.),
                                           labels = scales::percent_format(accuracy = 2))) +
    geom_hline(yintercept = 0) +
    facet_grid(~A_L) +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_f_FAL_GDP

ggsave("./Charts/plot_line_f_FAL_GDP.pdf", plot = plot_line_f_FAL_GDP, width = 10, height = 7, units = "cm")


###################################################################################
# Fixed Assets
###################################################################################

BS_fixed_all = data.table(dat_tot$fxa_h_X, dat_tot$fxa_nf_X, dat_tot$fxa_f_X, dat_tot$fxa_g_X)

BS_fixed_all = BS_fixed_all / gdp
BS_fixed_all = data.table(BS_fixed_all, dat_tot$time)

colnames(BS_fixed_all) = c("Hh", "NFC", "FC", "Govt", "Year")

BS_fixed_all = melt(BS_fixed_all, id = c("Year"), na.rm=TRUE)
colnames(BS_fixed_all) = c("Year", "Sector", "Value")

# Line Plot
###################################################################################
# Set the limits of the chart and scaling factor for the secondary axis, relative
# to the primary axis
limit_max = 1.8
limit_min = -1.2
break_size = 0.2
secondary_axis_scale = (1/20)
s_axis_accuracy = 0.1


# Create the multi-component plot
plot_line_f_FixedA_GDP <- 
    ggplot() + 
    geom_point(data = gdp_growth, 
               aes(x = Year,
                   y = (GDP_growth)/secondary_axis_scale,
                   fill = c("GDP growth rate"))) +
    geom_line(lty = 4,
              data = gdp_growth, 
              aes(x = Year,
                  y = (GDP_growth)/secondary_axis_scale)) +
    geom_line(lty = 1,
              data = BS_fixed_all, aes(Year, 
                                      Value, 
                                      group = Sector,
                                      colour = Sector),
              show.legend = TRUE) + 
    labs(x = "Year",
         y = "Balance sheet values to GDP",
         title = "DK Fixed assets per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       limits = c(limit_min, limit_max),
                       breaks = seq(limit_min,limit_max,break_size),
                       sec.axis = sec_axis(~(.)*secondary_axis_scale,
                                           labels = scales::percent_format(accuracy = s_axis_accuracy),
                                           breaks = seq(limit_min*secondary_axis_scale,
                                                        limit_max*secondary_axis_scale,
                                                        break_size*secondary_axis_scale),
                                           name = "GDP Growth rate")) +
    geom_hline(yintercept = 0, colour = "black") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_f_FixedA_GDP

ggsave("./Charts/plot_line_f_FixedA_GDP.pdf", plot = plot_line_f_FixedA_GDP, width = 10, height = 7, units = "cm")

###################################################################################
# Disposable income
###################################################################################

Y_D_GDP_all = data.table(dat_tot$yd_g_h_R, dat_tot$yd_g_nf_R, dat_tot$yd_g_f_R, dat_tot$yd_g_g_R)

Y_D_GDP_all = Y_D_GDP_all / gdp
Y_D_GDP_all = data.table(Y_D_GDP_all, dat_tot$time)

colnames(Y_D_GDP_all) = c("Hh", "NFC", "FC", "Govt", "Year")

Y_D_GDP_all = melt(Y_D_GDP_all, id = c("Year"), na.rm=TRUE)
colnames(Y_D_GDP_all) = c("Year", "Sector", "Value")

# Line Plot
###################################################################################
plot_line_Y_D_GDP_All <- 
    ggplot() + 
    geom_point(data = gdp_growth, 
               aes(x = Year,
                   y = (GDP_growth)*10,
                   fill = c("GDP growth rate"))) +
    geom_line(lty = 4,
              data = gdp_growth, 
              aes(x = Year,
                  y = (GDP_growth)*10)) +
    geom_line(lty = 1,
              data = Y_D_GDP_all, aes(Year, 
                                      Value, 
                                      group = Sector,
                                      colour = Sector),
              show.legend = TRUE) + 
    labs(x = "Year",
         y = "Disposable income to GDP",
         title = "DK Disposable income per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       limits = c(-0.5, 0.7),
                       breaks = seq(-0.5,0.7,.1),
                       sec.axis = sec_axis(~(.)/10,
                                           labels = scales::percent_format(accuracy = .1),
                                           breaks = seq(-0.05,0.07,.01),
                                           name = "GDP Growth rate")) +
    geom_hline(yintercept = 0, colour = "black") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_Y_D_GDP_All

ggsave("./Charts/plot_line_Y_D_GDP_All.pdf", plot = plot_line_Y_D_GDP_All, width = 10, height = 7, units = "cm")


###################################################################################
# Investment in fixed capital
###################################################################################


i_fc_all = data.table(dat_tot$i_fc_h_P, 
                      dat_tot$i_fc_nf_P, 
                      dat_tot$i_fc_f_P, 
                      dat_tot$i_fc_g_P)

i_fc_all = i_fc_all / gdp
i_fc_all = data.table(i_fc_all, dat_tot$time)

colnames(i_fc_all) = c("Hh", "NFC", "FC", "Govt", "Year")

i_fc_all = melt(i_fc_all, id = c("Year"), na.rm=TRUE)
colnames(i_fc_all) = c("Year", "Sector", "Value")



# Line Plot
###################################################################################
# Set the limits of the chart and scaling factor for the secondary axis, relative
# to the primary axis
limit_max = 0.15
limit_min = -0.05
break_size = 0.05
secondary_axis_scale = (1)
s_axis_accuracy = 1


plot_line_i_fc_GDP_All <- 
    ggplot() + 
    geom_point(data = gdp_growth, 
               aes(x = Year,
                   y = (GDP_growth)/secondary_axis_scale,
                   fill = c("GDP growth rate"))) +
    geom_line(lty = 4,
              data = gdp_growth, 
              aes(x = Year,
                  y = (GDP_growth)/secondary_axis_scale)) +
    geom_line(lty = 1,
              data = i_fc_all, aes(Year, 
                                       Value, 
                                       group = Sector,
                                       colour = Sector),
              show.legend = TRUE) + 
    labs(x = "Year",
         y = "Investment in fixed capital to GDP",
         title = "DK Investment in fixed capital per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(limit_min, limit_max),
                       breaks = seq(limit_min,limit_max,break_size),
                       sec.axis = sec_axis(~(.)*secondary_axis_scale,
                                           labels = scales::percent_format(accuracy = s_axis_accuracy),
                                           breaks = seq(limit_min*secondary_axis_scale,
                                                        limit_max*secondary_axis_scale,
                                                        break_size*secondary_axis_scale),
                                           name = "GDP Growth rate")) +
    scale_x_continuous(limits = c(min(i_fc_all$Year), max(i_fc_all$Year)),
                       breaks = seq(min(i_fc_all$Year), max(i_fc_all$Year), 1)) + 
    geom_hline(yintercept = 0, colour = "black") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_i_fc_GDP_All

ggsave("./Charts/plot_line_i_fc_GDP_All.pdf", plot = plot_line_i_fc_GDP_All, width = 10, height = 7, units = "cm")

###################################################################################
# Investment in fixed capital less depreciation
###################################################################################

limit_max = 0.07
limit_min = -0.05
break_size = 0.05
secondary_axis_scale = (1)
s_axis_accuracy = 1

i_fc_dep_all = data.table(dat_tot$i_fc_h_P-dat_tot$dep_h_P, 
                      dat_tot$i_fc_nf_P-dat_tot$dep_nf_P, 
                      dat_tot$i_fc_f_P-dat_tot$dep_f_P, 
                      dat_tot$i_fc_g_P-dat_tot$dep_g_P)

i_fc_dep_all = i_fc_dep_all / gdp
i_fc_dep_all = data.table(i_fc_dep_all, dat_tot$time)

colnames(i_fc_dep_all) = c("Hh", "NFC", "FC", "Govt", "Year")

i_fc_dep_all = melt(i_fc_dep_all, id = c("Year"), na.rm=TRUE)
colnames(i_fc_dep_all) = c("Year", "Sector", "Value")



# Line Plot
###################################################################################
# Set the limits of the chart and scaling factor for the secondary axis, relative
# to the primary axis
limit_max = 0.07
limit_min = -0.05
break_size = 0.05
secondary_axis_scale = (1)
s_axis_accuracy = 1


plot_line_i_fc_dep_GDP_All <- 
    ggplot() + 
    geom_point(data = gdp_growth, 
               aes(x = Year,
                   y = (GDP_growth)/secondary_axis_scale,
                   fill = c("GDP growth rate"))) +
    geom_line(lty = 4,
              data = gdp_growth, 
              aes(x = Year,
                  y = (GDP_growth)/secondary_axis_scale)) +
    geom_line(lty = 1,
              data = i_fc_dep_all, aes(Year, 
                                   Value, 
                                   group = Sector,
                                   colour = Sector),
              show.legend = TRUE) + 
    labs(x = "Year",
         y = "Investment in fixed capital to GDP",
         title = "DK Investment in fixed capital per cent of GDP") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(limit_min, limit_max),
                       breaks = seq(limit_min,limit_max,break_size),
                       sec.axis = sec_axis(~(.)*secondary_axis_scale,
                                           labels = scales::percent_format(accuracy = s_axis_accuracy),
                                           breaks = seq(limit_min*secondary_axis_scale,
                                                        limit_max*secondary_axis_scale,
                                                        break_size*secondary_axis_scale),
                                           name = "GDP Growth rate")) +
    scale_x_continuous(limits = c(min(i_fc_dep_all$Year), max(i_fc_dep_all$Year)),
                       breaks = seq(min(i_fc_dep_all$Year), max(i_fc_dep_all$Year), 1)) + 
    geom_hline(yintercept = 0, colour = "black") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 6),
          axis.text.x = element_text(angle=90, vjust=0.5))
plot_line_i_fc_dep_GDP_All

ggsave("./Charts/plot_line_i_fc_dep_GDP_All.pdf", plot = plot_line_i_fc_dep_GDP_All, width = 10, height = 7, units = "cm")

