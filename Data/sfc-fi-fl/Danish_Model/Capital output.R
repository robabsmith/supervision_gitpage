##################################################################
# Capital output ratio: Scenario_1
##################################################################
k_1 <- sum(model_data_raw_num$k_f_1,
         model_data_raw_num$k_g_1,
         model_data_raw_num$k_h_1,
         model_data_raw_num$k_nf_1)
y_1 <- model_data_raw_num$y_1



Capital_output_ratio_1 <- data.table(k_1/y_1, model_data_raw_num$Date)
is.na(Capital_output_ratio_1) <- sapply(Capital_output_ratio_1, is.infinite)
Capital_output_ratio_1 <- Capital_output_ratio_1 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 1" = V1) %>%
    select(Date, "Scenario 1")

rm(k_1, y_1)

##################################################################
# Capital output ratio: Scenario_2
##################################################################
k_2 <- sum(model_data_raw_num$k_f_2,
           model_data_raw_num$k_g_2,
           model_data_raw_num$k_h_2,
           model_data_raw_num$k_nf_2)
y_2 <- model_data_raw_num$y_2

Capital_output_ratio_2 <- data.table(k_2/y_2, model_data_raw_num$Date)
is.na(Capital_output_ratio_2) <- sapply(Capital_output_ratio_2, is.infinite)
Capital_output_ratio_2 <- Capital_output_ratio_2 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 2" = V1) %>%
    select(Date, "Scenario 2")

rm(k_2, y_2)

##################################################################
# Capital output ratio: Scenario_3
##################################################################
k_3 <- sum(model_data_raw_num$k_f_3,
           model_data_raw_num$k_g_3,
           model_data_raw_num$k_h_3,
           model_data_raw_num$k_nf_3)
y_3 <- model_data_raw_num$y_3

Capital_output_ratio_3 <- data.table(k_3/y_3, model_data_raw_num$Date)
is.na(Capital_output_ratio_3) <- sapply(Capital_output_ratio_3, is.infinite)
Capital_output_ratio_3 <- Capital_output_ratio_3 %>%
    replace(is.na(.), 0) %>%
    mutate(Date = V2,
           "Scenario 3" = V1) %>%
    select(Date, "Scenario 3")

rm(k_3, y_3)


##################################################################
##################################################################

capital_output <- Capital_output_ratio_1 %>%
    left_join(., Capital_output_ratio_2, by = "Date") %>%
    left_join(., Capital_output_ratio_3, by = "Date") %>%
    gather(-Date, key = Scenario, value = Value)

rm(Capital_output_ratio_1, Capital_output_ratio_2, Capital_output_ratio_3)

##################################################################
# Plot
##################################################################
plot_cap_out_ratio <- ggplot() +
    shock_1_colours +
    shock_2_colours +
    shock_2_legend +
    geom_line(mapping = aes(x = year(Date),
                            y = Value,
                            group = Scenario,
                            colour = Scenario,
                            lty = Scenario),
              data = capital_output %>%
                  filter(Date > min_date & Date <max_date),
              lwd = plot_line_width) +
    labs(x = "Year", y = "Capital:Output ratio") +
    #facet_wrap(~Sector, scales = "free") +
    scale_colour_manual(values = blackpalette) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.position = "bottom",
          legend.box = "horizontal") +
    guides(col = guide_legend(nrow = 4, byrow = FALSE))
plot_cap_out_ratio


