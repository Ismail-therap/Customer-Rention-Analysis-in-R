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


## Summarion of duration by Year-Month
library(ggplot2)
library(dplyr)
library(zoo)
dat %>% group_by(Date) %>% summarise(sum = sum(duration_yrs)) %>% 
  ggplot(aes(x = Date, y = sum)) + geom_line() +
  scale_x_yearmon(format="%Y %b") + 
  labs(x="Date",y="Sum of duration",title="Trend of duration")





##### The dependent variable is binary:

# counts by category of the dependent variable:
table(dat$sold)



# Duration by Sold
p <- ggplot(dat, aes(factor(sold),duration_yrs))
p1 <-p + geom_violin() + labs(x="Sold",y="Duration")
p2 <- p + geom_boxplot() + labs(x="Sold",y="")

library(gridExtra)
grid.arrange(p1, p2, ncol=2)



### Sold by Market

library(tidyr)

dat2 <- dat %>% 
  group_by(sold,market) %>% 
  tally() %>% 
  complete(market, fill = list(n = 0)) %>% 
  mutate(percentage1 = n / sum(n) * 100)
head(dat2)

ggplot(dat2, aes(market, percentage1, fill = sold)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()+
  labs(x="Market",y="Percentage",title="Count of Sold by Market")+
  coord_flip()


### Sold by segment sub


dat3 <- dat %>% 
  group_by(sold,segment_sub) %>% 
  tally() %>% 
  complete(segment_sub, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

ggplot(dat3, aes(segment_sub, percentage, fill = sold)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()+
  labs(x="Segment_sub",y="Percentage",title="Percentage of Sold by Segment_sub")+
  coord_flip()




### Sold by funding type


dat4 <- dat %>% 
  group_by(sold,fundng_ty) %>% 
  tally() %>% 
  complete(fundng_ty, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
dat4 <- na.omit(dat4)
ggplot(dat4, aes(fundng_ty, percentage, fill = sold)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()+
  labs(x="funding type",y="Percentage",title="Percentage of Sold by funding type")+
  coord_flip()


# Sold by Sic_division


dat5 <- dat %>% 
  group_by(sold,sic_division) %>% 
  tally() %>% 
  complete(sic_division, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
dat5 <- na.omit(dat5)

ggplot(dat5, aes(sic_division, percentage, fill = sold)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()+
  labs(x="sic_division",y="Percentage",title="Percentage of Sold by sic_division")+
  coord_flip()


## Sold by renewal fund


dat6 <- dat %>% 
  group_by(sold,rnwl_fund) %>% 
  tally() %>% 
  complete(rnwl_fund, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
dat6 <- na.omit(dat6)

ggplot(dat6, aes(rnwl_fund, percentage, fill = sold)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()+
  labs(x="rnwl_fund",y="Percentage",title="Percentage of Sold by rnwl_fund")+
  coord_flip()


### prebbd_re by Sold

p <- ggplot(dat, aes(factor(sold),prebbd_re))
p1 <-p + geom_violin() + labs(x="Sold",y="prebbd_re")
p2 <- p + geom_boxplot() + labs(x="Sold",y="")
grid.arrange(p1, p2, ncol=2)

# One prebbd_re value is very large compare to other. So, we will recreate the 
# graph after truncating that observation:

p <- ggplot(na.omit(dat[dat$prebbd_re<30,]), aes(factor(sold),prebbd_re))
p1 <-p + geom_violin() + labs(x="Sold",y="prebbd_re")
p2 <- p + geom_boxplot() + labs(x="Sold",y="")
grid.arrange(p1, p2, ncol=2)


#### bbd by sold

p <- ggplot(dat, aes(factor(sold),bbd))
p1 <-p + geom_violin() + labs(x="Sold",y="bbd")
p2 <- p + geom_boxplot() + labs(x="Sold",y="")
grid.arrange(p1, p2, ncol=2)

#### postbbd_re by sold

p <- ggplot(dat, aes(factor(sold),postbbd_re))
p1 <-p + geom_violin() + labs(x="Sold",y="postbbd_re")
p2 <- p + geom_boxplot() + labs(x="Sold",y="")
grid.arrange(p1, p2, ncol=2)


summary(dat$postbbd_re)
# One postbbd_re value is very large compare to other. So, we will recreate the 
# graph after truncating that observation:

p <- ggplot(na.omit(dat[dat$postbbd_re<10,]), aes(factor(sold),postbbd_re))
p1 <-p + geom_violin() + labs(x="Sold",y="postbbd_re")
p2 <- p + geom_boxplot() + labs(x="Sold",y="")
grid.arrange(p1, p2, ncol=2)

