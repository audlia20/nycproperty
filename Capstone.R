
library(readxl)
manhattan <- read_excel("/Users/audreyliang/Downloads/rollingsales_manhattan.xlsx", skip = 4)
bronx <- read_excel("/Users/audreyliang/Downloads/rollingsales_bronx.xlsx", skip = 4)
brooklyn <- read_excel("/Users/audreyliang/Downloads/rollingsales_brooklyn.xlsx", skip = 4)
queens <- read_excel("/Users/audreyliang/Downloads/rollingsales_queens.xlsx", skip = 4)
statenisland <- read_excel("/Users/audreyliang/Downloads/rollingsales_statenisland.xlsx", skip = 4)


# View the structure of the data frame
str(manhattan)

# View the first few rows of the data frame
head(manhattan)
nrow(manhattan)
head(bronx)
nrow(bronx)
head(brooklyn)
nrow(brooklyn)
head(queens)
nrow(queens)
head(statenisland)
nrow(statenisland)

housing <- rbind(manhattan, bronx, brooklyn, queens, statenisland)
nrow(housing)

colnames(housing)
table(housing$`BUILDING CLASS CATEGORY`)
housing1 <- housing[housing$`BUILDING CLASS CATEGORY` == "01 ONE FAMILY DWELLINGS",]
table(housing1$`TAX CLASS AT TIME OF SALE`)

summary(housing1)
table(housing1$`BUILDING CLASS CATEGORY`)

housing1$NEIGHBORHOOD <- NULL
housing1$`BUILDING CLASS CATEGORY` <- NULL
housing1$`TAX CLASS AT PRESENT` <- NULL
housing1$BLOCK <- NULL
housing1$LOT <- NULL
housing1$EASEMENT <- NULL
housing1$`BUILDING CLASS AT PRESENT` <- NULL
housing1$ADDRESS <- NULL
housing1$`APARTMENT NUMBER` <- NULL
housing1$`ZIP CODE` <- NULL
housing1 <- housing1[housing1$`RESIDENTIAL UNITS`==1,]
housing1 <- housing1[housing1$`COMMERCIAL UNITS`==0,]
housing1$`RESIDENTIAL UNITS` <- NULL
housing1$`COMMERCIAL UNITS` <- NULL
housing1$`TOTAL UNITS` <- NULL
housing1$`TAX CLASS AT TIME OF SALE` <- NULL
housing1$`BUILDING CLASS AT TIME OF SALE` <- NULL
housing1
summary(housing1)
housing1 <- housing1[!is.na(housing1$`YEAR BUILT`), ]
table(housing1$BOROUGH)

boxplot(`SALE PRICE` ~ BOROUGH, data = housing1,
        main = "Box Plot of Sale Prices Across Boroughs",
        xlab = "Borough",
        ylab = "Sale Price", ylim = c(0, 2e+07))

# Scatter plot of Land Square Feet vs. Sale Price
plot(housing1$`LAND SQUARE FEET`, housing1$`SALE PRICE`,
     main = "Scatter Plot of Land Square Feet vs. Sale Price",
     xlab = "Land Square Feet", ylab = "Sale Price", xlim = c(0, 20000), ylim = c(0, 2e+07))

# Scatter plot of Gross Square Feet vs. Sale Price
plot(housing1$`GROSS SQUARE FEET`, housing1$`SALE PRICE`,
     main = "Scatter Plot of Gross Square Feet vs. Sale Price",
     xlab = "Gross Square Feet", ylab = "Sale Price", xlim = c(0, 8000), ylim = c(0, 2e+07))

# Scatter plot of Year Built vs. Sale Price
plot(housing1$`YEAR BUILT`, housing1$`SALE PRICE`,
     main = "Scatter Plot of Year Built vs. Sale Price",
     xlab = "Year Built", ylab = "Sale Price", xlim = c(1890, 2023), ylim = c(0, 1e+07))



# Create a bar chart of the distribution of properties across different boroughs
borough_counts <- table(housing1$BOROUGH)
barplot(borough_counts, 
        main = "Boroughs",
        xlab = "Borough",
        ylab = "Count")

# Assuming 'data' is your data frame containing numeric variables

housing1$BOROUGH <- as.numeric(housing1$BOROUGH)
housing1$`SALE DATE` <- as.numeric(housing1$`SALE DATE`)
correlation <- cor(housing1)
heatmap(correlation, 
        main = "Heatmap of Correlation Matrix",
        xlab = "Variables", ylab = "Variables",
)



                     