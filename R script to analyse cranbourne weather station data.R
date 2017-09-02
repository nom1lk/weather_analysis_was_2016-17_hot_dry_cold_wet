####note missing data is just ignored by 'mean' (same in excel) e.g. the average/mean of 3, <missing>, 5 would be 4



####RAIN ANALYSIS

rain <- read.csv("C:/weather/rain.csv",sep=',', header=TRUE)
colnames(rain)[2] = "Rainfall"
# plot of average daily rainfall (mm)by year 
aggregate(Rainfall ~ Year, rain, mean)
plot(aggregate(Rainfall ~ Year, rain, mean), ylab="Average daily rainfall", xlab="Financial year")
abline(h=mean(aggregate(Rainfall ~ Year, rain, mean)[,2]))
# Average daily rainfall (mm) overall
mean(aggregate(Rainfall ~ Year, rain, mean)[,2])
#2016/17 year % difference from average (of past 27 years)
(aggregate(Rainfall ~ Year, rain, mean)[27,2])/(mean(aggregate(Rainfall ~ Year, rain, mean)[,2]))-1



####TEMPERATURE ANALYSIS

temp <- read.csv("C:/weather/temp.csv",sep=',', header=TRUE)
# plot of average max temp by year
aggregate(Max_temp ~ Year, temp, mean)
plot(aggregate(Max_temp ~ Year, temp, mean), ylab="Average daily max temp", xlab="Financial year")
abline(h=mean(aggregate(Max_temp ~ Year, temp, mean)[,2]))
# Average daily max temp
mean(aggregate(Max_temp ~ Year, temp, mean)[,2])
#2016/17 year % difference from average (of past 27 years)
(aggregate(Max_temp ~ Year, temp, mean)[27,2])/(mean(aggregate(Max_temp ~ Year, temp, mean)[,2]))-1
