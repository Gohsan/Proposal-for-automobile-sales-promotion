#まずデータを読み込みましょう
#setwd("?/statistics-intro/")
#解釈のコツを学ぶ
veh_data<-read.csv("data/data11ku.csv")
head(veh_data)

#仮説検定
#都市と郊外でデータをわけます。データ読み込み
urban<-veh_data[veh_data$urb==1, ]#都市
suburb<-veh_data[veh_data$urb!=1, ]#郊外

# t検定
# 都市と郊外で片側検定
t.test(urban$vehicles_owend,suburb$vehicles_owend, alternative = "greater")
# 検定用軽量t = 3.4872、p-value = 0.000614、0.0614%で棄却できる
boxplot(urban$vehicles_owend,
        suburb$vehicles_owend)

# 都市と市を駅の数で両側検定
unique(veh_data$ward)

cent_urb_data<-veh_data[veh_data$ward=="cent_urb",]
city1_data<-veh_data[veh_data$ward=="city1",]

t.test(cent_urb_data$no_of_sta, 
       city1_data$no_of_sta, 
       var.equal = FALSE,
       alternative = "two.sided")
# 検定用軽量t = 6.1998、p-value = 0.008182、0.82%で棄却できる



# 四分位数グラフで駅数を比較する
boxplot(cent_urb_data$no_of_sta,
        city1_data$no_of_sta)

t.test(cent_urb_data$no_of_sta, 
       city1_data$no_of_sta,
       var.equal = FALSE,
       alternative = "two.sided")

#ヒストグラムを比べてみる
management_hr<-hr_data[hr_data$sales=="management", ]
par(mfrow=c(1,2))
hist(sales_hr$satisfaction_level)
hist(management_hr$satisfaction_level)
par(mfrow=c(1,1))

