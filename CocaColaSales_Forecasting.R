################################# Naive Bayes ########################################

#Objective: Forecast the CocaCola prices data set. Prepare a document for each model explaining
#how many dummy variables you have created and RMSE value for each model. Finally which model you
#will use for Forecasting.

#Data : CocaCola_Sales_Rawdata.csv
######################################################################################

library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth) # forsmoothing and MAPE
install.packages("tseries")
library(tseries)
library(readxl)
CocaCola_Sales_Rawdata <- read_excel("D:\\Shilpa\\Datascience\\Assignments\\Forecasting\\CocaCola_Sales_Rawdata.xlsx")
View(CocaCola_Sales_Rawdata)

# Converting data into time series object
?ts
tssales<-ts(CocaCola_Sales_Rawdata$Sales,frequency = 4)
View(tssales)

# dividing entire data into training and testing data 
train<-tssales[1:38]
train
test<-tssales[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 4)
train
test<-ts(test,frequency = 4)
test

# Plotting time series data
plot(tssales)
# Visualization shows that it has level, trend, seasonality => Additive seasonality


######################################## USING HoltWinters function #############################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
#Holt-Winters exponential smoothing without trend and without seasonal component.

#Call:
#  HoltWinters(x = train, alpha = 0.2, beta = F, gamma = F)

#Smoothing parameters:
#  alpha: 0.2
# beta : FALSE
# gamma: FALSE

#Coefficients:
#  [,1]
#a 4020.406

hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))
?forecast
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape #16.12634
#**********************************************************************************************#

# with alpha = 0.2, beta = 0.15
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
#Holt-Winters exponential smoothing with trend and without seasonal component.

#Call:
#  HoltWinters(x = train, alpha = 0.2, beta = 0.15, gamma = F)

#Smoothing parameters:
#  alpha: 0.2
#beta : 0.15
#gamma: FALSE

#Coefficients:
#  [,1]
#a 4342.1529
#b  113.4152

hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))

# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape #[1] 8.747745
#**********************************************************************************************#

# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
#Holt-Winters exponential smoothing with trend and additive seasonal component.

#Call:
#  HoltWinters(x = train, alpha = 0.2, beta = 0.15, gamma = 0.05)

#Smoothing parameters:
#  alpha: 0.2
#beta : 0.15
#gamma: 0.05

#Coefficients:
#  [,1]
#a  4356.92555
#b   115.94078
#s1  350.06965
#s2   36.69702
#s3 -385.98882
#s4  248.32000

hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape #[1] 3.584105
##################################################################################################

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
#Holt-Winters exponential smoothing without trend and without seasonal component.

#Call:
#  HoltWinters(x = train, beta = F, gamma = F)

#Smoothing parameters:
#  alpha: 0.502
#beta : FALSE
#gamma: FALSE

#Coefficients:
#  [,1]
#a 4456.709

hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
#fit
#1 4456.709
#2 4456.709
#3 4456.709
#4 4456.709
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape #9.093032

#**********************************************************************************************#

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
#Holt-Winters exponential smoothing with trend and without seasonal component.

#Call:
#  HoltWinters(x = train, gamma = F)

#Smoothing parameters:
#  alpha: 0.5747386
#beta : 0.3105725
#gamma: FALSE

#Coefficients:
#  [,1]
#a 4581.1447
#b  182.7749

hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
#fit
#1 4763.920
#2 4946.695
#3 5129.470
#4 5312.244

plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape # 8.62752
#**********************************************************************************************#

hw_nabg<-HoltWinters(train)
hw_nabg
#Holt-Winters exponential smoothing with trend and additive seasonal component.

#Call:
#  HoltWinters(x = train)

#Smoothing parameters:
#  alpha: 0.3784328
#beta : 0.2526015
#gamma: 0.8897278

#Coefficients:
#  [,1]
#a  4200.72210
#b   118.93562
#s1  556.79856
#s2   13.14018
#s3 -204.24618
#s4  732.44912

hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
#fit
#1 4876.456
#2 4451.734
#3 4353.283
#4 5408.914
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape #2.397211

##################################################################################################

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma

new_model <- HoltWinters(tssales)
new_model
#Holt-Winters exponential smoothing with trend and additive seasonal component.

#Call:
#  HoltWinters(x = tssales)

#Smoothing parameters:
#  alpha: 0.3963858
#beta : 0.2321364
#gamma: 0.9921668

#Coefficients:
#  [,1]
#a  4514.40200
#b    94.19669
#s1  606.55136
#s2  -30.22732
#s3 -240.73056
#s4  738.83022
plot(forecast(new_model,n.ahead=4))


# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,n.ahead=4))
forecast_new
#fit
#1 5215.150
#2 4672.568
#3 4556.262
#4 5630.019
########################################################################

############## USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2) 
ses_a
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#10 Q3       4020.476 3420.807 4620.146 3103.361 4937.592
#10 Q4       4020.476 3408.931 4632.022 3085.198 4955.755
#11 Q1       4020.476 3397.281 4643.672 3067.382 4973.571
#11 Q2       4020.476 3385.845 4655.107 3049.892 4991.061
#11 Q3       4020.476 3374.612 4666.341 3032.712 5008.240
#11 Q4       4020.476 3363.571 4677.382 3015.826 5025.126
#12 Q1       4020.476 3352.712 4688.240 2999.219 5041.733
#12 Q2       4020.476 3342.027 4698.925 2982.878 5058.074
#12 Q3       4020.476 3331.508 4709.445 2966.790 5074.162
#12 Q4       4020.476 3321.147 4719.806 2950.945 5090.008

sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape # 16.1243
#**********************************************************************************************#

# with alpha = 0.2, beta = 0.1
holt_ab<-holt(train,alpha = 0.2,beta = 0.15)
holt_ab
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#10 Q3       4632.946 4166.028 5099.864 3918.856 5347.035
#10 Q4       4829.971 4335.280 5324.661 4073.406 5586.535
#11 Q1       5026.995 4479.983 5574.008 4190.413 5863.578
#11 Q2       5224.020 4598.455 5849.586 4267.300 6180.741
#11 Q3       5421.045 4692.443 6149.647 4306.745 6535.345
#11 Q4       5618.070 4765.065 6471.075 4313.512 6922.628
#12 Q1       5815.095 4819.398 6810.792 4292.308 7337.882
#12 Q2       6012.120 4857.972 7166.267 4247.003 7777.236
#12 Q3       6209.145 4882.736 7535.554 4180.577 8237.712
#12 Q4       6406.169 4895.164 7917.174 4095.287 8717.052

holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape #8.267703
#**********************************************************************************************#
# with alpha = 0.2, beta = 0.1, gamma = 0.05 
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg_new
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#10 Q3       4826.538 4563.396 5089.681 4424.097 5228.980
#10 Q4       4725.159 4446.365 5003.953 4298.780 5151.538
#11 Q1       4685.764 4377.482 4994.045 4214.288 5157.239
#11 Q2       5493.996 5141.444 5846.547 4954.814 6033.177
#11 Q3       5581.440 5163.922 5998.959 4942.901 6219.979
#11 Q4       5480.061 4993.425 5966.696 4735.816 6224.305
#12 Q1       5440.665 4874.451 6006.880 4574.715 6306.616
#12 Q2       6248.897 5594.075 6903.720 5247.433 7250.362

hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new #6.149798
#*********************************# With out optimum values#*****************************#

# simple exponential method
ses_na<-ses(train,alpha=NULL)
ses_na
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#10 Q3       4437.383 3930.596 4944.170 3662.319 5212.447
#10 Q4       4437.383 3874.922 4999.844 3577.174 5297.593
#11 Q1       4437.383 3824.283 5050.483 3499.728 5375.038
#11 Q2       4437.383 3777.519 5097.247 3428.209 5446.558
#11 Q3       4437.383 3733.857 5140.909 3361.433 5513.334
#11 Q4       4437.383 3692.750 5182.016 3298.565 5576.201
#12 Q1       4437.383 3653.797 5220.969 3238.992 5635.774
#12 Q2       4437.383 3616.691 5258.075 3182.242 5692.524
#12 Q3       4437.383 3581.191 5293.575 3127.950 5746.816
#12 Q4       4437.383 3547.106 5327.661 3075.821 5798.945

sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
#Point.Forecast    Lo.80    Hi.80    Lo.95    Hi.95
#10 Q3       4437.383 3930.596 4944.170 3662.319 5212.447
#10 Q4       4437.383 3874.922 4999.844 3577.174 5297.593
#11 Q1       4437.383 3824.283 5050.483 3499.728 5375.038
#11 Q2       4437.383 3777.519 5097.247 3428.209 5446.558

plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
#*sesna_mape #9.132635
#*********************************************************************************************#
# Holts winter method 

holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#10 Q3       4358.812 3910.994 4806.629 3673.934 5043.689
#10 Q4       4475.703 4023.788 4927.618 3784.558 5166.848
#1 Q1       4592.595 4131.592 5053.597 3887.552 5297.637
#11 Q2       4709.486 4232.756 5186.215 3980.391 5438.581
#11 Q3       4826.377 4326.063 5326.692 4061.212 5591.542
#11 Q4       4943.269 4410.824 5475.714 4128.965 5757.573
#12 Q1       5060.160 4486.869 5633.452 4183.387 5936.934
#12 Q2       5177.052 4554.432 5799.671 4224.838 6129.266
#12 Q3       5293.943 4614.005 5973.881 4254.067 6333.819
#12 Q4       5410.834 4666.193 6155.476 4272.004 6549.665

holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
#Point.Forecast    Lo.80    Hi.80    Lo.95    Hi.95
#10 Q3       4358.812 3910.994 4806.629 3673.934 5043.689
#10 Q4       4475.703 4023.788 4927.618 3784.558 5166.848
#11 Q1       4592.595 4131.592 5053.597 3887.552 5297.637
#11 Q2       4709.486 4232.756 5186.215 3980.391 5438.581

plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape #8.927388
#*********************************************************************************************#
# Holts winter Exponential method
hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#10 Q3       4896.950 4643.712 5150.187 4509.656 5284.243
#10 Q4       4658.184 4300.150 5016.218 4110.618 5205.750
#11 Q1       4491.605 4053.130 4930.080 3821.015 5162.195
#11 Q2       5167.239 4660.933 5673.544 4392.911 5941.566
#11 Q3       5128.189 4562.039 5694.339 4262.337 5994.041
#11 Q4       4889.423 4269.233 5509.614 3940.923 5837.923
#12 Q1       4722.844 4052.949 5392.740 3698.328 5747.361
#12 Q2       5398.478 4682.311 6114.645 4303.194 6493.762

hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
#Point.Forecast    Lo.80    Hi.80    Lo.95    Hi.95
#10 Q3       4896.950 4643.712 5150.187 4509.656 5284.243
#10 Q4       4658.184 4300.150 5016.218 4110.618 5205.750
#11 Q1       4491.605 4053.130 4930.080 3821.015 5162.195
#11 Q2       5167.239 4660.933 5673.544 4392.911 5941.566

plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape # 2.397211

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 

new_model <- hw(train,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=4))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=4))
print(forecast_new)
#Point.Forecast    Lo.80    Hi.80    Lo.95    Hi.95
#10 Q3       4896.950 4643.712 5150.187 4509.656 5284.243
#10 Q4       4658.184 4300.150 5016.218 4110.618 5205.750
#11 Q1       4491.605 4053.130 4930.080 3821.015 5162.195
#11 Q2       5167.239 4660.933 5673.544 4392.911 5941.566
