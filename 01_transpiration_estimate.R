# Author J. W. Atkins (jeffrey.atkins@usda.gov)

# this script creates a very rough approximation of increases in latent heat flux from additions of grow lights
# it's going to be way way way overestimated though


# dependencies
require(tidyverse)
require(stringr)
require(ggplot2)
require(lubridate)

# now what we want do is add our addtional Rn 
# this should be an average over the whole area
grow_watts = 150

trans_eff = 0.27 

# bring in 2011-2013 US-Akn Ameriflux data
dat <- read.csv("./data/AMF_US-Akn_BASE_HH_5-5.csv", skip = 2, header = TRUE)

# replace the -9999 values with NA
dat[dat == -9999] <- NA

# change column names
names(dat)[names(dat) == "LE_1_2_1"] <- "LE"

# make the time column
dat$time <- as.numeric(substr(dat$TIMESTAMP_START, 9, 12))

# formatting time stamp
dat$datetime <-ymd_hms(dat$TIMESTAMP_START, truncated = 1)

# now to add to night time hours
dat$LE_proj<- ifelse(dat$time > 2100 | dat$time < 700, dat$LE + grow_watts, dat$LE + 0)

# let's get to daily
### average to month
dat %>% 
    group_by(year = format(datetime, '%Y'), month = format(datetime, '%m'),
             day = format(datetime, '%d')) %>%
    summarise(LE_sum = sum(LE, na.rm = TRUE), LE_proj_sum = sum(LE_proj, na.rm = TRUE)) -> projected
    
# add back in dates
projected$date <- ymd(paste(projected$year, projected$month, projected$day, sep="-"))
    
# convert to mm
# 1 Watt /m2 = 0.0864 MJ /m2/day
# 1 MJ /m2/day  =0.408 mm /day .

projected$LE_sum <- projected$LE_sum * 0.0864 * 0.408
projected$LE_proj_sum <- projected$LE_proj_sum * 0.0864 * 0.408

# formatting
projected$LE_sum[projected$LE_sum == 0] <- NA
projected$LE_proj_sum[projected$LE_proj_sum == 0] <- NA

# data for plot
d2 = data.frame(x = projected$date, y = projected$LE_proj_sum)

####
x11()
ggplot(projected, aes(x = date, y = LE_sum))+
    geom_point(alpha = 0.75, color = "grey")+
    geom_point(x = projected$date, y = projected$LE_proj_sum, color = "red", alpha = 0.5)+
    ylim(c(-5, 12))
    

# ### average to month
# projected %>% 
#     group_by(as.factor(year), as.factor(month)) %>%
#     summarise(LE_mean = mean(LE_sum, na.rm = TRUE), LE_proj_mean = mean(LE_proj_sum, na.rm = TRUE))
    
#### sum to year
projected %>% 
    group_by(as.factor(year)) %>%
    summarise(LE_sum = sum(LE_sum, na.rm = TRUE), LE_proj = sum(LE_proj_sum, na.rm = TRUE)) -> df

# make efficency
df$diff <- ((df$LE_proj - df$LE_sum) / df$LE_sum) * trans_eff

# calculate the yearly
print(df)



