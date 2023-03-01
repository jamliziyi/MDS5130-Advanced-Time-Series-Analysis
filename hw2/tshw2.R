library(fpp3)
#4.6

#1
#Defines a function that calculates the mean and standard deviation of a time series
myfun <- function(ts){
  ###ts is s time series
  mean = mean(ts)
  std = sd(ts)
  return(list(mean=mean,sd = std))
}

#Data PBS
data(PBS)

#Cost

#分别计算不同ATC2类别下，Cost的均值与方差
myresult <- tapply(PBS$Cost,PBS$ATC2,myfun)

#找到均值最大的ATC2类
mymean <- sapply(myresult, function(v) return(v$mean))
which.max(mymean)

#绘制该图像
PBS |>
  filter(ATC2=="C10") |>
  ggplot(aes(x = Month, y =Cost)) +
  geom_line()+
  ggtitle("Cost when ATC2=C10(highest mean)")

#找到标准差最小的ATC2类
mysd <- sapply(myresult, function(v) return(v$sd))
which.min(mysd)

#绘制该图像
PBS |>
  filter(ATC2=="R") |>
  ggplot(aes(x = Month, y =Cost)) +
  geom_line()+
  ggtitle("Cost when ATC2=R(lowest sd)")

#Scripts
#分别计算不同ATC2类别下，Scripts的均值与方差
myresult <- tapply(PBS$Scripts,PBS$ATC2,myfun)

#找到均值最大的ATC2类
mymean <- sapply(myresult, function(v) return(v$mean))
which.max(mymean)

#绘制该图像
PBS |>
  filter(ATC2=="C09") |>
  ggplot(aes(x = Month, y =Scripts)) +
  geom_line()+
  ggtitle("Scripts when ATC2=C09(highest mean)")

#找到标准差最小的ATC2类
mysd <- sapply(myresult, function(v) return(v$sd))
which.min(mysd)

#绘制该图像
PBS |>
  filter(ATC2=="J06") |>
  ggplot(aes(x = Month, y =Cost)) +
  geom_line()+
  ggtitle("Scripts when ATC2=J06(lowest sd)")



#3
#提出所需数据列
PBS_1 = PBS[c(1,6,8,9)]
PBS_1 = as_tsibble(PBS_1,key=ATC2,index = Month,validate = FALSE)

#将数据框转换为tsibble
PBS_features <- PBS_1 |>
  features(Cost, feature_set(pkgs = "feasts"))

#计算feature
PBS_features <- PBS_1 |>features(Scripts, feature_set(pkgs = "feasts"))

#找到值全部相同的列并剔除
library(broom)
apply(PBS_features,2,function(x){length(unique(x))==1})
PBS_features <- select(PBS_features,-pp_pvalue )

#进行主成分分析
pcs <- PBS_features |>
  select(-ATC2) |>
  prcomp(scale = TRUE) |>
  augment(PBS_features)

#绘制分析图像
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col =ATC2)) +
  geom_point() +
  theme(aspect.ratio = 1)

#找出异常序列
outliers <- pcs |>
  filter(.fittedPC1 > 6) |>
  select(ATC2, .fittedPC1, .fittedPC2)
outliers

#绘制异常时间序列图
outliers |>
  left_join(PBS_1, by = c("ATC2"), multiple = "all") |>
  mutate(Series = glue("{ATC2}", .sep = "\n\n")) |>
  ggplot(aes(x = Month, y = Scripts)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying Scripts time series in PC space")

#5.11 
#9
data(hh_budget)
#Find out the maximum year
max(hh_budget$Year)

#Add and integrate data from different cities
mydf <- aggregate(hh_budget[-c(1,2)], by=list(hh_budget$Year), FUN=sum)

#Rename the column name
names(mydf)[1]="Year" 

#Filter the training dataset
myseries = as_tsibble(mydf,index =seq(1,22,1))
myseries_train <-myseries  |>
  filter(Year < 2013) 

#Draw an image
autoplot(myseries, Wealth) +
  autolayer(myseries_train, Wealth,color="red") 

#mean method 
fit <- myseries_train |> model(MEAN(Wealth))
fit |> gg_tsresiduals()
fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))
fc |> autoplot(myseries)
fit |> accuracy()
fc |> accuracy(myseries)

#Naïve method
fit <- myseries_train |> model(NAIVE(Wealth))
fit |> gg_tsresiduals()
fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))
fc |> autoplot(myseries)
fit |> accuracy()
fc |> accuracy(myseries)

#Seasonal naïve method
#do not work

#Drift method
fit <- myseries_train |> model(RW(Wealth ~ drift()))
fit |> gg_tsresiduals()
fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))
fc |> autoplot(myseries)
fit |> accuracy()
fc |> accuracy(myseries)

# Ljung-Box test
augment(fit) |> features(.innov, ljung_box, lag=10)

#12
data(tourism)

#Extract data from the Gold Coast region
gc_tourism <-tourism  |>
  filter( Region=="Gold Coast") 

#aggregate total overnight trips
gc_tourism = summarise(index_by(gc_tourism,Quarter),sum(Trips)) 
gc_tourism = as_tsibble(gc_tourism)
names(gc_tourism)[2]<-"Trips"

#create three training sets for this data excluding the last 1, 2 and 3 years
gc_train_1 <- gc_tourism |> slice(1:(n()-4))
gc_train_2 <- gc_tourism |> slice(1:(n()-4*2))
gc_train_3 <- gc_tourism |> slice(1:(n()-4*3))

#gc_train_1
fit_1 <- gc_train_1 |> model(SNAIVE(Trips))
test_data = gc_tourism |> slice((n()-3):n())
fc_1 <- fit_1 |>
  forecast(new_data = test_data)
fc_1 |> autoplot(gc_tourism)
fc_1 |> accuracy(gc_tourism)

#gc_train_2
fit_2 <- gc_train_2|> model(SNAIVE(Trips))
test_data = gc_tourism |> slice((n()-7):(n()-4))
fc_2 <- fit_2 |>
  forecast(new_data = test_data)
fc_2 |> autoplot(gc_tourism)
fc_2 |> accuracy(gc_tourism)

#gc_train_3
fit_3 <- gc_train_3|> model(SNAIVE(Trips))
test_data = gc_tourism |> slice((n()-11):(n()-8))
fc_3 <- fit_3 |>
  forecast(new_data = test_data)
fc_3 |> autoplot(gc_tourism)
fc_3|> accuracy(gc_tourism)
