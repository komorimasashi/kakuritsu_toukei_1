#install.packages("tsibble")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("zoo")
#install.packages("forecast")

library(tsibble)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)

#厚生労働省「新型コロナウイルス感染症について 」オープンデータ
#https://www.mhlw.go.jp/stf/covid-19/open-data.html
d <- read.csv("https://www.mhlw.go.jp/content/pcr_positive_daily.csv", header=TRUE)

df <- data.frame(day = as.Date(d$日付), case = d$PCR.検査陽性者数.単日.)

#ライブラリの読み込み
library(tsibble)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)

#作図
df %>% 
  as_tsibble　%>%　
  ggplot()　+ geom_line(aes(x = day, y = case))

#移動平均
df_ts <- df %>% 
  as_tsibble　%>%
  mutate(movave = zoo::rollmean(case, 7, align = "right", fill = NA)) 

df_ts %>%　
  ggplot()　+ geom_line(aes(x = day, y = movave))

#週毎に集計
df %>% 
  as_tsibble　%>%
  mutate(week = yearweek(day))　%>%
  index_by(week) 　%>% 
  summarise(weeklycase = sum(case))  %>%　
  ggplot()　+ geom_line(aes(x = week, y = weeklycase))

#成分の分解
df$case %>% 
  ts(frequency=7) %>%
  stl(s.window="periodic") %>%
  plot()

#移動平均後の感染者数の自己相関関数
acf(na.omit(df_ts$movave))

#auto.arima()による予測
npred <- 60 #60日予測
dts <- ts(df$case, start=df$day[1])
fit <- auto.arima(dts, ic="aic", stepwise=T, trace=T) 
plot(forecast(fit, level = c(50,95), h =npred), shadecols=c("gray", "darkgray"),xaxt="n")
axis.Date(side=1,at=seq(from=min(df$day), to = max(df$day)+npred,by="2 months"),format="%y/%m")