library(readr)
dat <- read_csv("Rention model.csv")


#### Colnames cleaning:

names(dat) = gsub(pattern = "c59320_retnmdl_eda_dataset_v1.", replacement = "", x = names(dat))
head(dat)

#### Overall summary for each variable:

dat <- as.data.frame(unclass(dat))
summary(dat)
str(dat)



dat$Date <- zoo::as.yearmon(paste(dat$close_year, dat$close_month), "%Y %b")
str(dat)


head(dat)

Total_till_the_Date <- aggregate(dat$duration_yrs,by=list(dat$Date),FUN="sum")
colnames(Total_till_the_Date) <- c("Date","Total_Duration")

library(ggplot2)
library(dplyr)
dat %>% group_by(Date) %>% summarise(sum = sum(duration_yrs)) %>% 
  ggplot(aes(x = Date, y = sum)) + geom_line()
