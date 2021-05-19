library(tidyverse)
library(reshape2)

housedata <- read.csv("/Users/robertayretonbaileysmith/OneDrive - Aalborg Universitet/AAU/06 Current Projects/04 IO loan impact/04 Data/house price.csv", header = TRUE, sep = ";")
house_long = melt(housedata, id=c("Trade.Type", "Category", "Year"), na.rm=TRUE)

house_trade_values = house_long %>%
    filter(Trade.Type=="Total", Category=="Total value of trade (DKK millions)", variable =="Total.sale")%>%
    select(-Trade.Type, -Category)%>%
    group_by(Year)

house_trade_sale = house_long %>%
    filter(Trade.Type=="Total", Category=="Total value of trade (DKK millions)", variable =="Total.sale")%>%
    select(-Trade.Type, -Category)%>%
    group_by(Year)
