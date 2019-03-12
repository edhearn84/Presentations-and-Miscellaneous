
# Packages
library(tidyverse)
library(readxl)

# Data
wi_county_snap = read_csv("Desktop/Collected Data/county_snap.csv" , skip = 1)
wi_county_obes = read_excel("Desktop/Collected Data/Obesity_WI.xlsx" , sheet = "Sheet1")

# Munge data

# Limit to Wisconsin SNAP population
wi_snap = wi_county_snap %>%
  mutate( fips_code = paste0(`State FIPS code` , `County FIPS code`) ) %>%
  select( fips_code , Name , `July 2013`:`July 2004` ) %>%
  filter( as.numeric(fips_code) > 55000 & as.numeric(fips_code) < 56000 )

# Change column names
colnames(wi_snap) = c("fips" , "county" , 2013:2004)

# Cast SNAP to long
wi_snap_long = wi_snap %>%
  gather(-fips,-county , key = "year" , value = "snap_n")

# Cast obesity data to long
wi_obes_long = wi_county_obes %>%
  gather( -`FIPS Code` , -County , key = "year" , value = "obesity_pct")

# Join long formats of WI county obesity and SNAP data
wi_obes_snap = wi_obes_long %>%
  inner_join( wi_snap_long , c("FIPS Code" = "fips" , "year" = "year") ) %>%
  select(fips = `FIPS Code` , county , year , obesity_pct , snap_n)

# Grouped averages of obesity proportion and numbers receiving SNAP by years 2009-2013
wi_obes_avgs = wi_obes_snap %>%
  group_by(year) %>%
  summarize( obs_pct_avg = mean(obesity_pct) ,
             snap_n_avg = mean(snap_n) ) %>%
  ungroup()

png( "Desktop/wi_county_obesity_snap.png" , width = 1200 , height = 900 , res = 100 )
# Plot data for counties (black dots) and state (red dots)
plot( wi_obes_snap$year , wi_obes_snap$obesity_pct , cex = wi_obes_snap$snap_n/100000 , xaxt = "n" , 
      xlab = "" , ylab = "Obesity percentage" , 
      main = "WI Counties' Obesity Percentages by Years and SNAP Recipients\n(Size of dots corresponds to total SNAP recipients)")
axis(1 , at = 2004:2013 , labels = 2004:2013)
points( wi_obes_avgs$year , wi_obes_avgs$obs_pct_avg , cex = wi_obes_avgs$snap_n_avg/10000 , 
        pch = 16 , col = "red" , type = "b" )
text( x = 2004 , y = max(wi_obes_snap$obesity_pct) , labels = "Black is county percentage" , pos=4 )
text( x = 2004 , y = max(wi_obes_snap$obesity_pct)-1 , labels = "Red is state percentage" , pos=4 , col = "red" )
dev.off()