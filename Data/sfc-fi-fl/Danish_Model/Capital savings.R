##################################################################
# Capital savings ratio: Scenario_0
##################################################################
k_0 <-    (model_data_raw_num$k_f_0+
           model_data_raw_num$k_g_0+
           model_data_raw_num$k_h_0+
           model_data_raw_num$k_nf_0)
s_0 <-    (model_data_raw_num$s_f_0+
           model_data_raw_num$s_g_0+
           model_data_raw_num$s_h_0+
           model_data_raw_num$s_nf_0+
           model_data_raw_num$s_row_0)


S_K_ratio_0 <- data.table(s_0/k_0, model_data_raw_num$Date)
is.na(S_K_ratio_0) <- sapply(S_K_ratio_0, is.infinite)
S_K_ratio_0 <- S_K_ratio_0 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Baseline" = V1) %>%
    select(Date, "Baseline")

rm(k_0, s_0)

##################################################################
# Capital savings ratio: Scenario_1
##################################################################
k_1 <-    (model_data_raw_num$k_f_1+
               model_data_raw_num$k_g_1+
               model_data_raw_num$k_h_1+
               model_data_raw_num$k_nf_1)
s_1 <-    (model_data_raw_num$s_f_1+
               model_data_raw_num$s_g_1+
               model_data_raw_num$s_h_1+
               model_data_raw_num$s_nf_1+
               model_data_raw_num$s_row_1)


S_K_ratio_1 <- data.table(s_1/k_1, model_data_raw_num$Date)
is.na(S_K_ratio_1) <- sapply(S_K_ratio_1, is.infinite)
S_K_ratio_1 <- S_K_ratio_1 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 1" = V1) %>%
    select(Date, "Scenario 1")

rm(k_1, s_1)

##################################################################
# Capital savings ratio: Scenario_2
##################################################################
k_2 <-    (model_data_raw_num$k_f_2+
               model_data_raw_num$k_g_2+
               model_data_raw_num$k_h_2+
               model_data_raw_num$k_nf_2)
s_2 <-    (model_data_raw_num$s_f_2+
               model_data_raw_num$s_g_2+
               model_data_raw_num$s_h_2+
               model_data_raw_num$s_nf_2+
               model_data_raw_num$s_row_2)

S_K_ratio_2 <- data.table(s_2/k_2, model_data_raw_num$Date)
is.na(S_K_ratio_2) <- sapply(S_K_ratio_2, is.infinite)
S_K_ratio_2 <- S_K_ratio_2 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 2" = V1) %>%
    select(Date, "Scenario 2")

rm(k_2, s_2)

##################################################################
# Capital savings ratio: Scenario_3
##################################################################
k_3 <-    (model_data_raw_num$k_f_3+
               model_data_raw_num$k_g_3+
               model_data_raw_num$k_h_3+
               model_data_raw_num$k_nf_3)
s_3 <-    (model_data_raw_num$s_f_3+
               model_data_raw_num$s_g_3+
               model_data_raw_num$s_h_3+
               model_data_raw_num$s_nf_3+
               model_data_raw_num$s_row_3)

S_K_ratio_3 <- data.table(s_3/k_3, model_data_raw_num$Date)
is.na(S_K_ratio_3) <- sapply(S_K_ratio_3, is.infinite)
S_K_ratio_3 <- S_K_ratio_3 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 3" = V1) %>%
    select(Date, "Scenario 3")

rm(k_3, s_3)


##################################################################
##################################################################

S_K <- S_K_ratio_0 %>%
    left_join(., S_K_ratio_1, by = "Date") %>%
    left_join(., S_K_ratio_2, by = "Date") %>%
    left_join(., S_K_ratio_3, by = "Date") %>%
    gather(-Date, key = Scenario, value = Value)

rm(S_K_ratio_0, S_K_ratio_1, S_K_ratio_2, S_K_ratio_3)

##################################################################
# Plot: S/K
##################################################################
plot_S_K_ratio <- ggplot() +
    shock_1_colours +
    shock_2_colours +
    shock_2_legend +
    geom_line(mapping = aes(x = year(Date),
                            y = Value,
                            group = Scenario,
                            colour = Scenario,
                            lty = Scenario),
              data = S_K %>%
                  filter(Date > min_date & Date <max_date),
              lwd = plot_line_width) +
    labs(x = "Year", y = "S-K ratio") +
    #facet_wrap(~Sector, scales = "free") +
    scale_colour_manual(values = blackpalette) +
    scale_y_continuous(labels = pct_scale_settings) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.position = "bottom",
          legend.box = "horizontal") +
    guides(col = guide_legend(nrow = 4, byrow = FALSE))
plot_S_K_ratio


