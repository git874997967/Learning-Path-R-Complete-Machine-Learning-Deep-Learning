###time  series
set.seed(1000)
vec = round(runif(100, 1, 10), 2)
##frequency=4 means quarterly data
q_ts = ts(vec, frequency = 4, start = c(1959, 2))
###month data
m_ts = ts(vec, frequency = 12, start = 1990)
######
y_ts = ts(vec, start = c(2009), frequency = 1)
d_ts = ts(vec, start = c(2009, 10), frequency = 365.25)
plot(y_ts)
library(xts)
d_xts = xts(vec, as.Date('2009-10-01') + 0:99)
plot(d_xts)

library(lubridate)
dates = seq.Date(ymd('2009-10-01'), length.out = 100, by = 'day')
dates
####
set.seed(4321)
vec1 = sample(1:100)
vec1
vec2 = round(vec1 + runif(100, 10, 20))
df = data.frame(vec1, vec2)
rownames(df) = as.character(dates)
df_xts = as.xts(df)
plot(df_xts$vec1, type = 'o')
lines(df_xts$vec2)
##filter dates
df_xts['2009']
###get all data until the end of that date
df_xts['/2009-12']

#### for stocks   prices
### like the summary   get the low high   open  close  value
to.monthly(df_xts)
to.weekly(df_xts)
to.yearly(df)
typeof(df)
typeof(df_xts)

#### decompose
decomposeRes = decompose(m_ts, type = 'mult')
plot(decomposeRes)

#################
library(tseries)
set.seed(1000)
x=runif(1000)
adf.test(x)
data('wineind',package='forecast')
adf.test(wineind)
adf.test(JohnsonJohnson)
###differencing
library(forecast)

x=AirPassengers 
####season differencing
ns=nsdiffs(x)
if(ns>0){
  de_seas=diff(x,log=frequency(x),difference=ns)
}else{
  de_seas=x
}
plot(de_seas)
ts.stl=stl(AirPassengers,'periodic')
plot(ts.stl)









