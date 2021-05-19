##################################################################
# Capital investment ratio: Scenario_0
##################################################################
k_0 <- sum(model_data_raw_num$k_f_0,
           model_data_raw_num$k_g_0,
           model_data_raw_num$k_h_0,
           model_data_raw_num$k_nf_0)
i_0 <- model_data_raw_num$i_0


I_K_ratio_0 <- data.table(i_0/k_0, model_data_raw_num$Date)
is.na(I_K_ratio_0) <- sapply(I_K_ratio_0, is.infinite)
I_K_ratio_0 <- I_K_ratio_0 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Baseline" = V1) %>%
    select(Date, "Baseline")

rm(k_0, i_0)

##################################################################
# Capital investment ratio: Scenario_1
##################################################################
k_1 <- sum(model_data_raw_num$k_f_1,
         model_data_raw_num$k_g_1,
         model_data_raw_num$k_h_1,
         model_data_raw_num$k_nf_1)
i_1 <- model_data_raw_num$i_1


I_K_ratio_1 <- data.table(i_1/k_1, model_data_raw_num$Date)
is.na(I_K_ratio_1) <- sapply(I_K_ratio_1, is.infinite)
I_K_ratio_1 <- I_K_ratio_1 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 1" = V1) %>%
    select(Date, "Scenario 1")

rm(k_1, i_1)

##################################################################
# Capital investment ratio: Scenario_2
##################################################################
k_2 <- sum(model_data_raw_num$k_f_2,
           model_data_raw_num$k_g_2,
           model_data_raw_num$k_h_2,
           model_data_raw_num$k_nf_2)
i_2 <- model_data_raw_num$i_2

I_K_ratio_2 <- data.table(i_2/k_2, model_data_raw_num$Date)
is.na(I_K_ratio_2) <- sapply(I_K_ratio_2, is.infinite)
I_K_ratio_2 <- I_K_ratio_2 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 2" = V1) %>%
    select(Date, "Scenario 2")

rm(k_2, i_2)

##################################################################
# Capital investment ratio: Scenario_3
##################################################################
k_3 <- sum(model_data_raw_num$k_f_3,
           model_data_raw_num$k_g_3,
           model_data_raw_num$k_h_3,
           model_data_raw_num$k_nf_3)
i_3 <- model_data_raw_num$i_3

I_K_ratio_3 <- data.table(i_3/k_3, model_data_raw_num$Date)
is.na(I_K_ratio_3) <- sapply(I_K_ratio_3, is.infinite)
I_K_ratio_3 <- I_K_ratio_3 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 3" = V1) %>%
    select(Date, "Scenario 3")

rm(k_3, i_3)


##################################################################
##################################################################

I_K <- I_K_ratio_0 %>%
    left_join(., I_K_ratio_1, by = "Date") %>%
    left_join(., I_K_ratio_2, by = "Date") %>%
    left_join(., I_K_ratio_3, by = "Date") %>%
    gather(-Date, key = Scenario, value = Value)

rm(I_K_ratio_0, I_K_ratio_1, I_K_ratio_2, I_K_ratio_3)

##################################################################
# Plot
##################################################################
plot_I_K_ratio <- ggplot() +
    shock_1_colours +
    shock_2_colours +
    shock_2_legend +
    geom_line(mapping = aes(x = year(Date),
                            y = Value,
                            group = Scenario,
                            colour = Scenario,
                            lty = Scenario),
              data = I_K %>%
                  filter(Date > min_date & Date <max_date),
              lwd = plot_line_width) +
    labs(x = "Year", y = "$frac{I}{K}$ ratio") +
    #facet_wrap(~Sector, scales = "free") +
    scale_colour_manual(values = blackpalette) +
    scale_y_continuous(labels = pct_scale_settings) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.position = "bottom",
          legend.box = "horizontal") +
    guides(col = guide_legend(nrow = 4, byrow = FALSE))
plot_I_K_ratio


