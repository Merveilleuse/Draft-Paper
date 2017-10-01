#Comments
# The only significant test was the comparison between Prophet and... 
#...the hybrid model when I used a 40:60 weighting (ARIMA:Prophet) for h = 1...
#... the ARIMA model didn't differ from the hybrid model for the same case


### Trying to get fraction from error/looking for a trend/pattern
fracs <- abs(error2)/((abs(error1))+(abs(error2)))
summary(fracs)
hist(fracs)
# By the  looks of the histogram, a 40:60 and/or 60:40 Weightings might be worth investigating but note that the mean frac is 50%


#-----------------      a)   Hybrid Model (Equal weighting)      ------------------------

# h = 1
results_1 <- read.table("1 step ahead results.txt")
hybrid_fore_1.a <- 0.5*(results_1[,3]+results_1[,4])   #weighted(hybrid) forecast
error_hybrid_1.a <- hybrid_fore_1.a - results_1[,2]  #hybrid error

dm.test(results_1[,5],error_hybrid_1.a, h = 1)   # Arima vs Hybrid, p = 0.2587   
dm.test(results_1[,6],error_hybrid_1.a, h = 1)   # Prophet vs Hybrid, p = 0.05637 *


# h = 5
results_5 <- read.table("5 step ahead results.txt")
hybrid_fore_5.a <- 0.5*(results_5[,3]+results_5[,4])   #weighted(hybrid) forecast
error_hybrid_5.a <- hybrid_fore_5.a - results_5[,2]  #hybrid error

dm.test(results_5[,5],error_hybrid_5.a, h = 5)   # Arima vs Hybrid, p = 0.6573   
dm.test(results_5[,6],error_hybrid_5.a, h = 5)   # Prophet vs Hybrid, p = 0.1438

# h = 20
results_20 <- read.table("20 step ahead results.txt")
hybrid_fore_20.a <- 0.5*(results_20[,3]+results_20[,4])   #weighted(hybrid) forecast
error_hybrid_20.a <- hybrid_fore_20.a - results_20[,2]  #hybrid error

dm.test(results_20[,5],error_hybrid_20.a, h = 20)   # Arima vs Hybrid, p = 0.3633  
dm.test(results_20[,6],error_hybrid_20.a, h = 20)   # Prophet vs Hybrid, p = 0.2534



# --------------- b) Hybrid Model (ARIMA:PROPHET) = (40:60)  -----------------------------

# h = 1

hybrid_fore_1.b <- 0.4*(results_1[,3]) + 0.6*(results_1[,4])   #weighted(hybrid) forecast
error_hybrid_1.b <- hybrid_fore_1.b - results_1[,2]  #hybrid error

dm.test(results_1[,5],error_hybrid_1.b, h = 1)   # Arima vs Hybrid, p = 0.2268   
dm.test(results_1[,6],error_hybrid_1.b, h = 1)   # Prophet vs Hybrid, p = 0.04722 **


# h = 5
hybrid_fore_5.b <- 0.4*(results_5[,3]) + 0.6*(results_5[,4])   #weighted(hybrid) forecast
error_hybrid_5.b <- hybrid_fore_5.b - results_5[,2]  #hybrid error

dm.test(results_5[,5],error_hybrid_5.b, h = 5)   # Arima vs Hybrid, p = 0.5825  
dm.test(results_5[,6],error_hybrid_5.b, h = 5)   # Prophet vs Hybrid, p = 0.1194


# h = 20
hybrid_fore_20.b <- 0.4*(results_20[,3]) + 0.6*(results_20[,4])   #weighted(hybrid) forecast
error_hybrid_20.b <- hybrid_fore_20.b - results_20[,2]  #hybrid error

dm.test(results_20[,5],error_hybrid_20.b, h = 20)   # Arima vs Hybrid, p = 0.3473  
dm.test(results_20[,6],error_hybrid_20.b, h = 20)   # Prophet vs Hybrid, p = 0.2467



#--------------- C) Hybrid Model (ARIMA:PROPHET) = (60:40)  -----------------------------

# h = 1

hybrid_fore_1.c <- 0.6*(results_1[,3]) + 0.4*(results_1[,4])   # weighted(hybrid) forecast
error_hybrid_1.c <- hybrid_fore_1.c - results_1[,2]  # hybrid error

dm.test(results_1[,5],error_hybrid_1.c, h = 1)   # Arima vs Hybrid, p = 0.2937   
dm.test(results_1[,6],error_hybrid_1.c, h = 1)   # Prophet vs Hybrid, p = 0.067


# h = 5
hybrid_fore_5.c <- 0.6*(results_5[,3]) + 0.4*(results_5[,4])   #weighted(hybrid) forecast
error_hybrid_5.c <- hybrid_fore_5.c - results_5[,2]  #hybrid error

dm.test(results_5[,5],error_hybrid_5.c, h = 5)   # Arima vs Hybrid, p = 0.7363  
dm.test(results_5[,6],error_hybrid_5.c, h = 5)   # Prophet vs Hybrid, p = 0.1724


# h = 20
hybrid_fore_20.c <- 0.6*(results_20[,3]) + 0.4*(results_20[,4])   #weighted(hybrid) forecast
error_hybrid_20.c <- hybrid_fore_20.c - results_20[,2]  #hybrid error

dm.test(results_20[,5],error_hybrid_20.c, h = 20)   # Arima vs Hybrid, p = 0.3809  
dm.test(results_20[,6],error_hybrid_20.c, h = 20)   # Prophet vs Hybrid, p = 0.2606

