####note missing data is just ignored by 'mean' (same in excel) e.g. the average/mean of 3, <missing>, 5 would be 4



####RAIN ANALYSIS

rain <- read.csv("rain.csv",sep=',', header=TRUE)
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






# Adjusting rainfall for missing values (averaging across those NAs)


# Test vectors
# (vector <- c(1, NA, 1.1, NA, NA, 3, 2, NA, NA ,6, 8, NA) %>% rev)
# (expect <- c(1, 0.55, 0.55, 1, 1, 1, 2, 2, 2, 2, 8, 3.516667) %>% rev)




vector <- rain$Rainfall.amount..millimetres. %>% rev
# Or 
# vector <- rain$Rainfall %>% rev

for (i in 1:length(vector)) {
  
  if(i == 1 & is.na(vector[i])) {
    vector[i] <- mean(vector, na.rm=TRUE)
  } else { if(vector[i] %>% is.na(.)) {
    total_rainfall <- vector[i-1]
    
    
    is_it_na_vector <- c()
    for(j in 1:(length(vector)-i+1)) {
      is_it_na_vector <- c(is_it_na_vector, is.na(vector[j+i - 1]))
      if(!is_it_na_vector[j]) {break}
    }
    
    consec_nas <- sum(is_it_na_vector)
    
    vector[(i-1):(i-1+consec_nas)] <- total_rainfall/(consec_nas + 1) 
    
  } # End if
  } # End else 
  
}

# Testing
# vector
# expect


rain$rain_adjusted <- vector %>% rev
head(rain)

# Confirm same totals
colSums(rain, na.rm = TRUE)



