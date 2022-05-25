## median debt-to-income ratio for each county, courtesy of the Fed
library(readr)
household_debt_by_county <- read_csv("household-debt-by-county.csv")
hd <- household_debt_by_county
#head(hd)

# only need a subset of it
# understood: 2019, fiscal year quarter 4
library(dplyr) # for subset
hd2019 <- subset(hd, (year == 2019) & (qtr == 4))[,3:5]
#head(hd2019)
#dim(hd2019) # 3139 x 3   # merge this onto haf

## real estate data
library(readxl)
haf <- read_excel("haf_data_all_counties (4).xlsx")
#head(haf)

# get rid of row 1, since it duplicates the headers of haf
haf <- haf[-c(1), ]
small_haf <- cbind(rev(haf[,c(2:4)]), haf[,8], haf[,12]) # Homeowners less 100 AMI, County, State (full name), total homeowners, Predicted Foreclosure Rate; perhaps merge by state then county
colnames(small_haf) <- c("Homeowners_less_100_AMI", "county", "state_name", "total_homeowners", "pred_fc_rate")
head(small_haf)


## pre-processing fips codes: make a column that makes a code out of state_code + county_code
  # one way to get tables with columns in common

library(tigris)
fips_codes <- transform(fips_codes, area_fips=paste0(state_code, county_code))
#head(fips_codes) # this is a data frame

## THIS IS IT ##
# Perform a right-join. On left: fips_codes. On right: hd2019

rj1 <- merge(x =fips_codes, y = hd2019, all.y = TRUE)

# haf: 'State' 2 full-blown name, 'County' 3, Homeowners less 100 AMI' 4
# rj1: area_fips 1, state_name 4 full-blown name, county 6
rj1 <- rj1[,-c(2,3,5)]
#head(rj1) # this is just area_fips, state_name (full name), county, low, high


# I need a way to connect state and county together...
dtiMedData <- merge(x = rj1, y = small_haf, by = c("state_name", "county"))
head(dtiMedData, 50) # FINALLY!!! :D

# save this data
write.csv(dtiMedData, "dtiMedData.csv")
# Manually deleted a column of observation numbers.
## ^^^THIS IS IT. THIS IS OUR DATA. LET'S REGRESS!!!^^^ ##










### START FROM HERE ###

# Regress lower DTI ratios onto the proportion of homeowners with homeowners with less than 100% AMI. This model looks at all counties.

# This command would give a multiple regression model with 3135 beta values, including another beta value for the intercept.
# I need to do this differently...
lowFit <- lm(dtiMedData[,'low'] ~ (dtiMedData[,'Homeowners_less_100_AMI']/dtiMedData[,'total_homeowners']))
summary(lowFit)

# a 217 x 7 matrix, so 217 counties in the Northeast
Northeast <- dtiMedData[dtiMedData[,1]=="Connecticut" | dtiMedData[,1]== "Maine" | dtiMedData[,1]== "Massachusetts" | dtiMedData[,1]== "New Hampshire" | dtiMedData[,1]== "Rhode Island"| dtiMedData[,1]== "Vermont" | dtiMedData[,1]=="New Jersey"| dtiMedData[,1]=="New York" | dtiMedData[,1]== "Pennsylvania",]

# a 1052 x 7 matrix, so 1,052 counties in the Midwest
Midwest <- dtiMedData[dtiMedData[,1]=="Illinois"| dtiMedData[,1]=="Indiana"| dtiMedData[,1]=="Michigan"| dtiMedData[,1]=="Ohio"| dtiMedData[,1]=="Wisconsin"| dtiMedData[,1]=="Iowa"| dtiMedData[,1]=="Kansas"| dtiMedData[,1]=="Minnesota"| dtiMedData[,1]=="Missouri"| dtiMedData[,1]=="Nebraska"| dtiMedData[,1]=="North Dakota"| dtiMedData[,1]=="South Dakota",]

# a 1419 x 7 matrix, so 1,419 counties in the South
South <- dtiMedData[dtiMedData[,1]=="Delaware" | dtiMedData[,1]=="Florida"| dtiMedData[,1]=="Georgia"| dtiMedData[,1]=="Maryland"| dtiMedData[,1]=="North Carolina"| dtiMedData[,1]=="South Carolina"| dtiMedData[,1]=="Virginia"| dtiMedData[,1]=="District of Columbia" | dtiMedData[,1]=="West Virginia"| dtiMedData[,1]=="Alabama"| dtiMedData[,1]=="Kentucky"| dtiMedData[,1]=="Mississippi"| dtiMedData[,1]=="Tennessee"| dtiMedData[,1]=="Arkansas"| dtiMedData[,1]=="Louisiana"| dtiMedData[,1]=="Oklahoma"| dtiMedData[,1]=="Texas",]

# a 445 x 7, so 445 counties in the West
West <- dtiMedData[dtiMedData[,1]=="Arizona"| dtiMedData[,1]=="Colorado"| dtiMedData[,1]=="Idaho"| dtiMedData[,1]=="Montana"| dtiMedData[,1]=="Nevada"| dtiMedData[,1]=="New Mexico"| dtiMedData[,1]=="Utah"| dtiMedData[,1]=="Wyoming"| dtiMedData[,1]=="Alaska"| dtiMedData[,1]=="California"| dtiMedData[,1]=="Hawaii"| dtiMedData[,1]=="Oregon"| dtiMedData[,1]=="Washington",]

