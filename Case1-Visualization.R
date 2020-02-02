##################################################################################################### 
## Author: Xuan Wu                Date: 9/25/2019 
##################################################################################################### 
# Load the required libraries/packages for the problems in Chapter 3
# Install the package(s) below once.

install.packages("ggplot2") 
install.packages("forecast") 
library(ggplot2) 
library(forecast)

######################################################################################################
## Case 1 - Shipments of Household Appliances: Line Graphs.
## The fle ApplianceShipments.csv contains the series of quarterly shipments
## (in millions of dollars) of US household appliances between 1985 and 1989. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## 1.a. Create a well-formatted time plot of the data. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load the data
appship.df <- read.csv("ApplianceShipments.csv")

#view the fields in dataset
head(appship.df)   

# Use time series analysis to create time series from the above data frame 
appship.ts <- ts(appship.df$Shipments, 
                 start = c(1985, 1), 
                 end = c(1989, 4), 
                 freq = 4)

#view the data structure of the time series
print(appship.ts)  

# Line chart for the shipments data
plot(appship.ts,xlab='Year',ylab='Shipments (Millions)',main='Line Chart of Shipments Data')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## 1.b Does there appear to be a quarterly pattern? For a closer view of the  
## patterns, zoom in to the range of 3500-5000 on the y-axis.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Answer: The chart does show a quarterly pattern as there is a peak in Q1,Q2 each year.
print('Answer: The chart does show a quarterly pattern as there is a peak in Q1,Q2 each year.')
# zoom in to the range of 3500-5000 on the y-axia
plot(appship.ts,xlab='Year',ylab='Shipments',ylim = c(3500,5000),main='Line Chart of Shipments Data')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## 1.c Using R, create one chart with four separate lines, one line for each of Q1, Q2, Q3, 
## and Q4. In R, this can be achieved by generating a data.frame for each quarter Q1, 
## Q2, Q3, Q4, and then plotting them as separate series on the line graph. Zoom 
## in to the range of 3500-5000 on the y-axis. Does there appear to be a diference 
## between quarters? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# split the Quarter column to seperate quarter and year
library(stringr)
appship.df <- data.frame(str_split_fixed(appship.df$Quarter, "-", 2),appship.df$Shipments)

# reassign names to the columns
names(appship.df) <- c('Quarter','Year','Shipments')

# create a line chart to show the shipments for each quarter as separate series.
ggplot(data=appship.df,mapping=aes(x=as.numeric(Year),y=Shipments,colour = Quarter))+geom_line(size=1,show.legend = TRUE)

#Answer: The shipments of Q2 are significantly higher than the others.

x <- c(1985,1986,1987,1988,1989)
q1 <- subset(appship.ts,cycle(appship.ts) == 1)
q2 <- subset(appship.ts,cycle(appship.ts) == 2)
q3 <- subset(appship.ts,cycle(appship.ts) == 3)
q4 <- subset(appship.ts,cycle(appship.ts) == 4)
df <- data.frame(x,q1,q2,q3,q4)
g <- ggplot(df,aes(x))+geom_line(aes(y=q1), colour="red")+geom_line(aes(y=q2), colour="green") +geom_line(aes(y=q3), colour="blue")+geom_line(aes(y=q4), colour="purple")
g <- g +xlab('Year')+ylab('Shipments (Millions)')+ggtitle('Shipments by Quarter by Year')+ theme(legend.position="right",legend.text=(legend))
g


ggplot(df,mapping = aes(xlab='Year',ylab='Shipments (Millions)'))+ggtitle('Shipments by Quarters by Year')

plot(x,q1,type = "l",xlab='Year',ylab='Shipments (Millions)',main='Shipments by Quarters by Year',ylim = c(3500,5000))
lines(x,q2,type = "l",col='red')
lines(x,q3,type = "l",col='green')
lines(x,q4,type = "l",col='bule')


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## 1.d Using R, create a line graph of the series at a yearly aggregated level (i.e., the total shipments in each year).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# summarize shipments by year
yearship <- aggregate(appship.ts,FUN=sum,nfrequency=1)

# create a line chart for shipments by year
plot(yearship,xlab='Year',ylab='Shipments',main='Total Shipments by Year')

###############################################################################################################################
## Case2: Sales of Riding Mowers: Scatter Plots. 
## A company that manufactures riding 
## mowers wants to identify the best sales prospects for an intensive sales campaign. In 
## particular, the manufacturer is interested in classifying households as prospective owners 
## or nonowners on the basis of Income (in $1000s) and Lot Size (in 1000 ft ). The 
## marketing expert looked at a random sample of 24 households, given in the fle RidingMowers.csv.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## 2.a Using R, create a scatter plot of Lot Size vs. Income, color-coded by the outcome 
## owner/nonowner. Make sure to obtain a well-formatted plot (create legible labels and a legend, etc.).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load data file
ridmowers.df <- read.csv("RidingMowers.csv")

# update column name "Lot Size" to replace blank
colnames(ridmowers.df)[colnames(ridmowers.df)=="Lot Size"] <- "Lot_Size"

# create a scatter plot of Lot Size vs. Income, color-coded by the outcome
ggplot(data=ridmowers.df,mapping=aes(x=Income,y=Lot_Size,colour = Ownership))+geom_point(size=3)+ggtitle('Lot Size vs. Income Scatter Chart')+theme(
  legend.position = c(1,1),
  legend.justification = c("right", "top")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Case 3 Laptop Sales at a London Computer Chain: Bar Charts and Boxplots. 
# The fle LaptopSalesJanuary2008.csv contains data for all sales of laptops at a computer chain 
# in London in January 2008. This is a subset of the full dataset that includes data for 
# the entire year. 
# 3.a. Create a bar chart, showing the average retail price by store. Which store has the 
# highest average? Which has the lowest? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# load data file
LapSales.df <- read.csv("LaptopSalesJanuary2008.csv")

# check if any missing value in retail price and store postcode columns
which(is.na(LapSales.df$Retail.Price))
which(is.na(LapSales.df$Store.Postcode))

# aggregate data calculating the average retail price by store zip code
data.for.plot <- aggregate(LapSales.df$Retail.Price,by=list(LapSales.df$Store.Postcode),FUN=mean)
names(data.for.plot) <- c('Store','Avg_Retail_Price')

# Create a bar chart, showing the average retail price by store
ggplot(data.for.plot, aes(x=data.for.plot$Store, y=data.for.plot$Avg_Retail_Price)) + geom_bar(stat="identity") + 
  labs(x="Store", y="Average Retail Price")+ggtitle('Average Retail Price by Store')+coord_cartesian(ylim=c(450,500))

# Find the stores with highest and lowest prices.
high <- toString(data.for.plot$Store[which.max(data.for.plot$Avg_Retail_Price)])
cat('Store with highest avg. price:',high)

low <- toString(data.for.plot$Store[which.min(data.for.plot$Avg_Retail_Price)])
cat('Sotre with lowest avg. price:',low)

# Answer: The Store N17 6QA has the highest average price. The store W4 3PH has the lowest average price.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 3.b. To better compare retail prices across stores, create side-by-side boxplots of retail 
# price by store. Now compare the prices in the two stores from (a). Does there 
# seem to be a diference between their price distributions?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Create a subset to only include the data for the two stores from (a)
TwoStore <- subset(LapSales.df,LapSales.df$Store.Postcode == c(high,low))

# Create boxplots of retail price by store comparing the two stores.
ggplot(TwoStore)+geom_boxplot(aes(x=as.factor(TwoStore$Store.Postcode), y=TwoStore$Retail.Price)) + labs(x="Store", y="Retail Price")+ggtitle('Retail Price - Highest Avg. Price Store vs. Lowest Avg. Price Store')

# Answer: The retail price concentration of high-price store N17 6QA is higher than the concentration of low-price store W4 3PH.
print('Answer: The retail price concentration of high-price store N17 6QA is higher than the concentration of low-price store W4 3PH.')
