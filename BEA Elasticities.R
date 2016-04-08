
"""
The following code reads in price indexes and quantity indexes of personal consumption
expenditures by type of product from the BEA. It cleans the data and then calculates 
annual elasticities for each consumption type annually from 1981 to 2015. 

"""

# Reads in data from csv's 
price_index <- read.csv('Price Index.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
price_index[,3] <- NULL
quantity_index <- read.csv('Quantity Index.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

# Tranforms and cleans price and quantity indices 
new_price <- t(price_index)
colnames(new_price) <- new_price[2,]
new_price <- new_price[-c(1:2),]
new_price <- as.data.frame(new_price)
indx <- sapply(new_price, is.factor)
new_price[indx] <- lapply(new_price[indx], function(x) as.numeric(as.character(x)))
new_price_clean <- new_price[indx]

new_quantity <- t(quantity_index)
colnames(new_quantity) <- new_quantity[2,]
new_quantity <- new_quantity[-c(1:2),]
new_quantity <- as.data.frame(new_quantity)
new_quantity[indx] <- lapply(new_quantity[indx], function(x) as.numeric(as.character(x)))
new_quantity_clean <- new_quantity[indx]

# Computes new dataframes of the yoy growth rates for the price and quantity indexes
library(data.table)

quantity_growth <- as.data.table(new_quantity_clean)[, lapply(.SD, function(x)x/shift(x) - 1), .SDcols = 1:401]
quantity_growth <- as.data.frame(quantity_growth)
rownames(quantity_growth) <- rownames(new_quantity_clean)

price_growth <- as.data.table(new_price_clean)[, lapply(.SD, function(x)x/shift(x) - 1), .SDcols = 1:401]
price_growth <- as.data.frame(price_growth)
rownames(price_growth) <- rownames(new_price_clean)

# Calculate Elasticities
elasticities_raw <- quantity_growth / price_growth
rownames(elasticities_raw) <- rownames(price_growth)
elasticities_raw <- elasticities_raw[-1,]

elasticities_clean <- elasticities_raw
elasticities_clean[is.infinite(elasticities_clean)] <- NA 
elasticities_clean[is.na(elasticities_clean)] <- 1

# Produce some summary statistics and exploratory graphics
means <- sapply(elasticities_clean, mean)
summary(means)
barplot(table(means))

boxplot(elasticities_clean$`    Durable goods`, elasticities_clean$`    Nondurable goods`,
        elasticities_clean$Services)




