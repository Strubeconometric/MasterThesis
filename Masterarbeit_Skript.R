# Instructions for Replication ####

# please create a parent directory (name does not matter) with the following child directory:

# data_raw <- download raw data sets and save them here
# data_tidy <- merged and cleaned data sets will be saved as a single tidy panel data set here
# output <- illustrations as well as descriptive and result tables will be saved here

# the directory containing the three folders must be set as the current working directory or contain a here() file
# not all econometric results will be saved in the output folder
# if you are interested in robustness and statistical assumption checks please run the code from chunk to chunk and read the commented results

# Settings and Packages #####

# set error message language to english
Sys.setenv(LANG = "en")


# install packages
# packages have to be installed only once - remove #'s in RStudio with -> Code -> Comment/Uncomment Lines

#install.packages("tidyverse")
#install.packages("here")
#install.packages("readxl")
#install.packages("data.table")
#install.packages("magrittr")
#install.packages("countrycode")
#install.packages("plm")
#install.packages("stargazer")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("tseries")
#install.packages("systemfit")
#install.packages("ggeffects")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("patchwork")
#install.packages("cowplot")
#install.packages("ggrepel")

#packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.2.0.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

#packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggpubr/ggpubr_0.4.0.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

# load packages

# packages need to be loaded for every new session
library(tidyverse)
library(here)
library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(countrycode)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(tseries)
library(systemfit)
library(ggeffects)
library(ggpubr)
library(cowplot)
library(patchwork)
library(ggrepel)

# test if here.package works getwd() = here() -> path should link to parent folders containing the three mentioned folders

here()

# set new plot device to keep aspect ratio of graphics
windows()

# Import of Raw-Data and Creation of a Tidy-Panel-Dataset ####

# import oecd_database

oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")

oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))

oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]

# subset for high income oecd countries

oecd_database = oecd_database %>% subset(., Country %in% c("Australia", "Austria", "Belgium", "Canada","Chile", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Israel", "Italy", "Japan","Korea", "Latvia", "Lithuania","Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic","Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
))


#subset for all 33 high income oecd countries

#"Australia", "Austria", "Belgium", "Canada","Chile", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Israel", "Italy", "Japan","Korea", "Latvia", "Lithuania","Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic","Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"

# subset for 29 countries for more common observations of variables/better data

#"Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan", "Korea, Rep.", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"



# create panel format

oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)

# create a list of high income oecd countries as reference to filter other data sets

oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))

oecd_database[,c(2):=NULL]
oecd_database = oecd_database %>%  
  mutate(Year = as.character(Year))

# import missing oecd variables

oecd_cpi=fread("data_raw/oecd_cpi.csv", colClasses = "double", na.strings = "..")

oecd_cpi %>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Inflation"))

oecd_cpi[,c(2,3,4,5,8):=NULL]

oecd_cpi = oecd_cpi %>% subset(., Country_Code %in% country_codes_vector)

oecd_cpi = oecd_cpi %>%  
  mutate(Year = as.character(Year))

# create condensed code with pipes and import the remaining single variable oecd data sets

# import oecd_disposable_income

oecd_disposable_income=fread("data_raw/oecd_disposable_income.csv", colClasses = "double", na.strings = "..")

oecd_disposable_income<-oecd_disposable_income[FREQUENCY!="Q",]
oecd_disposable_income<-oecd_disposable_income[SUBJECT!="GROSS",]

oecd_disposable_income %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Disposable_Income")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))



# import oecd_fertility

oecd_fertility=fread("data_raw/oecd_fertility.csv", colClasses = "double", na.strings = "..")
oecd_fertility %<>% setnames(., old = c(1,15), new = c("Country_Code", "Fertility")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,6,7,9,10,11,12,13,14,16,17 ):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_house_prices

oecd_house_prices=fread("data_raw/oecd_house_prices.csv", colClasses = "double", na.strings = "..")
oecd_house_prices %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","House_Prices")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_household_debt

oecd_household_debt=fread("data_raw/oecd_household_debt.csv", colClasses = "double", na.strings = "..")
oecd_household_debt %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Household_Debt")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_household_savings

oecd_household_savings=fread("data_raw/oecd_household_savings.csv", colClasses = "double", na.strings = "..")
oecd_household_savings %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Household_Savings_Rate")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_net_savings

oecd_net_savings=fread("data_raw/oecd_net_savings.csv", colClasses = "double", na.strings = "..")
oecd_net_savings %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Net_National_Savings")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_private_education_gdp

oecd_private_education_gdp=fread("data_raw/oecd_private_education_gdp.csv", colClasses = "double", na.strings = "..")
oecd_private_education_gdp %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Educ_Spending_GDP")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_private_education_total

oecd_private_education_total=fread("data_raw/oecd_private_education_total.csv", colClasses = "double", na.strings = "..")
oecd_private_education_total %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Educ_Spending_Total")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_public_social_expenditure

oecd_public_social_expenditure=fread("data_raw/oecd_public_social_expenditure.csv", colClasses = "double", na.strings = "..")
oecd_public_social_expenditure %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Social_Expenditure")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import oecd_share_prices

oecd_share_prices=fread("data_raw/oecd_share_prices.csv", colClasses = "double", na.strings = "..")
oecd_share_prices %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Share_Prices")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import finreform dataset

finreform=fread("data_raw/finreform.csv", colClasses = "double", na.strings = "..")
finreform[,"Country_Code"]=countrycode(finreform[[1]], origin = "country.name", destination = "iso3c")
finreform <- select(finreform, Country_Code, year, finreform, finreform_n)
finreform <- setnames(finreform, "year", "Year")
finreform=mutate(finreform, Year = as.character(Year))
finreform = finreform %>% subset(., Country_Code %in% country_codes_vector)

# import gdf_data -> muss noch ausgewählt werden welche indikatoren

gfd_data=fread("data_raw/gfd_data.csv", colClasses = "double", na.strings = "..")
gfd_data %<>% setnames(., old = c(1,7), new = c("Country_Code","Year")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,6,30:38):=NULL] %>%
  mutate(Year = as.character(Year))

# imf_financial_development

imf_financial_development=fread("data_raw/imf_financial_development.csv", colClasses = "double", na.strings = "..", header= TRUE)

imf_financial_development %<>% 
  pivot_longer(cols=c(3:43),
               names_to='Year',
               values_to='Value') 

imf_financial_development %<>% 
  pivot_wider(names_from = Variable, values_from = Value)

imf_financial_development[,"Country_Code"]=countrycode(imf_financial_development[[1]], origin = "country.name", destination = "iso3c")

imf_financial_development = imf_financial_development %>% subset(., Country_Code %in% country_codes_vector)

imf_financial_development = setcolorder(imf_financial_development, c(12,2:11,1))

imf_financial_development = as.data.table(imf_financial_development)

imf_financial_development[,c(12):=NULL]

imf_financial_development=mutate(imf_financial_development, Year = as.character(Year))

# import pwt10_labour_share

pwt10_labour_share=fread("data_raw/pwt10_labour_share.csv", colClasses = "double", na.strings = "..")
pwt10_labour_share %<>% setnames(., old = c(2,3,4), new = c("Country_Code","Year","Labour_Share")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(1):=NULL] %>%
  mutate(Labour_Share = Labour_Share*100) %>%
  mutate(Year = as.character(Year)) 

# import swiid9_3

swiid9_3=fread("data_raw/swiid9_3.csv", colClasses = "double", na.strings = "..")

swiid9_3[,"Country_Code"]=countrycode(swiid9_3[[1]], origin = "country.name", destination = "iso3c")

swiid9_3 = swiid9_3 %>% subset(., Country_Code %in% country_codes_vector)

swiid9_3 = setcolorder(swiid9_3, c(11,2:10,1))

swiid9_3 = as.data.table(swiid9_3)

swiid9_3[,c(11):=NULL]

swiid9_3 %<>% setnames(., old = c(2), new = c("Year"))

swiid9_3=mutate(swiid9_3, Year = as.character(Year))

# import union_data

union_data=fread("data_raw/union_data.csv", colClasses = "double", na.strings = "..")
union_data[,"Country_Code"]=countrycode(union_data[[1]], origin = "country.name", destination = "iso3c")
union_data = setcolorder(union_data, c(9,2:8))
union_data = as.data.table(union_data)
union_data %<>% setnames(., old = c(2), new = c("Year")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(9):=NULL] %>%
  mutate(Year = as.character(Year))

# import wdi_data

wdi_data=fread("data_raw/wdi_data.csv", colClasses = "double", na.strings = "..")
wdi_data %<>% setnames(., old = c(1,4), new = c("Year","Country_Code")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3):=NULL] %>%
  setcolorder(., c(2,1,3:18))

wdi_data <- wdi_data[, Year:=as.character(Year)]


# import wid_shares

wid_shares=fread("data_raw/wid_shares.csv", colClasses = "double", na.strings = "..")
wid_shares[,"Country_Code"]=countrycode(wid_shares[[1]], origin = "country.name", destination = "iso3c")

wid_shares = setcolorder(wid_shares, c(6,4,2,3,5))

wid_shares %<>%
  mutate(Variable = str_sub(Variable, start = 21L))

wid_shares = wid_shares %>% subset(., Country_Code %in% country_codes_vector)

wid_shares %<>% 
  pivot_wider(names_from = Variable, values_from = Value)

wid_shares = as.data.table(wid_shares)

wid_shares[,c(3,4):=NULL]

wid_shares %<>% setnames(., old = c(3,4,5,6), new = c("Top_1_Wealth", "Top_10_Wealth", "Top_1_Income","Top_10_Income"))

wid_shares <- wid_shares[, Year:=as.character(Year)]

wid_w1 <- wid_shares[, list(Country_Code, Year, Top_1_Wealth)]
wd_w10 <- wid_shares[, list(Country_Code, Year,Top_10_Wealth)]
wd_i1 <- wid_shares[, list(Country_Code, Year,Top_1_Income)]
wd_i10 <- wid_shares[, list(Country_Code, Year,Top_10_Income)]

wid_w1<-wid_w1[-which(is.na(wid_w1$Top_1_Wealth)),]
wd_w10<-wd_w10[-which(is.na(wd_w10$Top_10_Wealth)),]
wd_i1<-wd_i1[-which(is.na(wd_i1$Top_1_Income)),]
wd_i10<-wd_i10[-which(is.na(wd_i10$Top_10_Income)),]

wid_shares = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(
  wid_w1, wd_w10, wd_i1, wd_i10 ))

# merge all data sets into one tidy panel data table

tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(
  finreform, gfd_data, imf_financial_development, oecd_cpi, oecd_database, oecd_disposable_income, oecd_fertility, oecd_house_prices, oecd_household_debt,
  oecd_household_savings, oecd_net_savings, oecd_private_education_gdp, oecd_private_education_total, oecd_public_social_expenditure, oecd_share_prices, 
  pwt10_labour_share,  swiid9_3, union_data, wdi_data, wid_shares ))

# save data set

write.csv(tidy_data, "data_tidy/tidy_data.csv", row.names = FALSE )

# clean environment 

#rm(list = ls())


# Tidy Data Preparation ####

# clean environment 

# import function 

tidy_data=fread("data_tidy/tidy_data.csv", colClasses = "double", na.strings = "..")

# set object type to data.frame
tidy_data=setDF(tidy_data)

# set data type to numeric
tidy_data[,3:87] = sapply(tidy_data[,3:87],as.numeric)

# set object type to data.table
tidy_data = as.data.table(tidy_data)

# change variable names
tidy_data = setnames(tidy_data, old = c(3, 4, 36, 37, 38, 39, 40, 42, 44, 47,
                                        52, 54, 68, 69, 84, 85, 86, 87
), 
new = c("Finreform", "Finreform_n","Inflation", "Current_Account", "Fiscal_Account_Balance", "CPI", "Nominal_Interest_Rate", "GDP", "Birth_Rate", "Household_Savings",
        "Nominal_Share_Prices", "Gini_SWIID9", "Private_Credit", "Dependency_Ratio", "Top_1_Wealth", "Top_10_Wealth", "Top_1_Income", "Top_10_Income"
))

# create factors / idcode / numbers for each country
tidy_data[, idcode := .GRP, by=.(Country_Code)]

# rearrange variable order
tidy_data = setcolorder(tidy_data, c(88,2:87,1))

# change idcode datatype to factor
tidy_data[,1] = lapply(tidy_data[,1],as.factor)


# create growth rates / deltas
tidy_data$D_Disposable_Income <- with(tidy_data, ave(Disposable_Income , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
tidy_data$D_House_Prices <- with(tidy_data, ave(House_Prices , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))

# create first differences and percentage based variables
# function to create first differences 
# dat %>%  group_by(id) %>% mutate(time.difference = time - lag(time))
tidy_data %<>% 
  group_by(idcode) %>% 
  mutate(Interest_Rate = Nominal_Interest_Rate - Inflation)
tidy_data %<>% 
  group_by(idcode) %>% 
  mutate(Share_Prices = Nominal_Share_Prices - Inflation)
tidy_data %<>% 
  group_by(idcode) %>% 
  mutate(Top_1Percent_Wealth = Top_1_Wealth*100) %>%
  mutate(Top_10Percent_Wealth = Top_10_Wealth*100) %>%
  mutate(Top_1Percent_Income = Top_1_Income*100) %>%
  mutate(Top_10Percent_Income = Top_10_Income*100) %>%
  mutate(Delta_Disposable_Income = D_Disposable_Income*100) %>%
  mutate(Delta_House_Prices = D_House_Prices*100)

# set object type to data.table
tidy_data = as.data.table(tidy_data)

# change variable names
tidy_data = setnames(tidy_data, old = c(70, 71, 72, 73, 74, 75,
                                        76, 77, 78, 79, 80, 81,
                                        82, 83
), 
new = c("Pop_Growth", "Pop_Density", "Pop_Urban", "Market_Cap", "Inflation_WDI", "GDP_Deflator",
        "Net_National_Savings_GNI", "Unemployment_Benefits", "Social_Insurance", "Health_Expenditure", "Public_Educ_Spending_Tertiary", "Public_Educ_Spending_Total",
        "Current_Account_WDI", "Fiscal_Account_Balance_WDI"
        
))

tidy_data = setnames(tidy_data, old = c(36,37,38
                                        
), 
new = c("Inflation_OECD","Current_Account_OECD", "Fiscal_Account_Balance_OECD" 
        
))

tidy_data = setcolorder(tidy_data, c(1:2, 88, 3:35, 36, 74, 37, 82, 38, 83, 39:73, 75:81, 84:87, 89:98))

#expand oecd data with wdi data

tidy_data %<>%  
  mutate(Inflation = coalesce(Inflation_OECD, Inflation_WDI))
tidy_data = setcolorder(tidy_data, c(1:36, 99, 37:98))

tidy_data %<>%  
  mutate(Fiscal_Account_Balance = coalesce(Fiscal_Account_Balance_OECD, Fiscal_Account_Balance_WDI))
tidy_data = setcolorder(tidy_data, c(1:41, 100, 42:99))

tidy_data %<>%  
  mutate(Current_Account_Balance = coalesce(Current_Account_OECD, Current_Account_WDI))
tidy_data = setcolorder(tidy_data, c(1:39, 101, 40:100))

#fix inflation calculations
tidy_data %<>% 
  group_by(idcode) %>% 
  mutate(Interest_Rate = Nominal_Interest_Rate - Inflation)
tidy_data %<>% 
  group_by(idcode) %>% 
  mutate(Share_Prices = Nominal_Share_Prices - Inflation)

#add missing data from israel

isr=fread("data_raw/israel_household_savings_rate.csv", colClasses = "double", na.strings = "..")

isr = isr %>%  
  mutate(idcode = as.factor(idcode))

isr <- isr[-41, ]

#merge isr data 
# merge all data sets into one tidy panel data table

tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("idcode", "Country_Code", "Year"), all = TRUE), list(tidy_data, isr) )

tidy_data %<>%  
  mutate(Household_Savings = coalesce(Household_Savings.x, Household_Savings.y))

tidy_data <- tidy_data[, -c(54,102)]

tidy_data <- tidy_data[-1, ]

# save data set
#rm(list = ls())

write.csv(tidy_data, "data_tidy/tidy_data.csv", row.names = FALSE )

tidy_data=fread("data_tidy/tidy_data.csv", colClasses = "double", na.strings = "..")

# set object type to data.frame
tidy_data=setDF(tidy_data)

# set data type to numeric
tidy_data[,3:101] = sapply(tidy_data[,3:101],as.numeric)

# set object type to data.table
tidy_data = as.data.table(tidy_data)

# change idcode datatype to factor
tidy_data[,1] = lapply(tidy_data[,1],as.factor)

tidy_data <- setcolorder(tidy_data, c(1,3,2,4:101))

#create gdp growth rate
tidy_data$D_GDP <- with(tidy_data, ave(GDP , idcode, FUN=function(x) c(NA, (diff(x)/x[-length(x)])*100) ))

#replace na from union data with latest non na value

tidy_data <- tidy_data[, Cent_Union := CENTunion[1], .(idcode, cumsum(!is.na(CENTunion)))]

tidy_data <- tidy_data[, Cent_Bargaining := CENT[1], .(idcode, cumsum(!is.na(CENT)))]

#replace educ data with latest non value

tidy_data <- tidy_data[, Educ_Spending_GDP := Educ_Spending_GDP[1], .(idcode, cumsum(!is.na(Educ_Spending_GDP)))]

tidy_data <- tidy_data[, Educ_Spending_Total := Educ_Spending_Total[1], .(idcode, cumsum(!is.na(Educ_Spending_Total)))]

tidy_data <- tidy_data[, Public_Educ_Spending_Tertiary := Public_Educ_Spending_Tertiary[1], .(idcode, cumsum(!is.na(Public_Educ_Spending_Tertiary)))]

tidy_data <- tidy_data[, Public_Educ_Spending_Total := Public_Educ_Spending_Total[1], .(idcode, cumsum(!is.na(Public_Educ_Spending_Total)))]


# import missing oecd variables

# import Social_Transfers_Cash
oecd_transfers_cash=fread("data_raw/oecd_transfers_cash.csv", colClasses = "double", na.strings = "..")

oecd_transfers_cash %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Social_Transfers_Cash")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.numeric(Year))
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, oecd_transfers_cash) )

# import Social_Transfers_Kind
oecd_transfers_kind=fread("data_raw/oecd_transfers_kind.csv", colClasses = "double", na.strings = "..")

oecd_transfers_kind %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Social_Transfers_Kind")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.numeric(Year))
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, oecd_transfers_kind) )

# import Unemployment_Spending
oecd_unemployment_spending=fread("data_raw/oecd_unemployment_spending.csv", colClasses = "double", na.strings = "..")

oecd_unemployment_spending %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Unemployment_Spending")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.numeric(Year))
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, oecd_unemployment_spending) )

# import Health_Spending
oecd_health_spending=fread("data_raw/oecd_health_spending.csv", colClasses = "double", na.strings = "..")

oecd_health_spending %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Health_Spending")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.numeric(Year))
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, oecd_health_spending) )

# import Pension_Spending_Public
oecd_pension_public=fread("data_raw/oecd_pension_public.csv", colClasses = "double", na.strings = "..")

oecd_pension_public %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","oecd_pension_public")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.numeric(Year))
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, oecd_pension_public) )

# import Pension_Spending_Private
oecd_pension_private=fread("data_raw/oecd_pension_private.csv", colClasses = "double", na.strings = "..")

oecd_pension_private %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Pension_Spending_Private")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.numeric(Year))
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, oecd_pension_private) )

#import missing union data
union_density=fread("data_raw/union_density.csv", colClasses = "double", na.strings = "..")
union_density[,"Country_Code"]=countrycode(union_density[[1]], origin = "country.name", destination = "iso3c")
union_density = setcolorder(union_density, c(4,2,3,1))
union_density = as.data.table(union_density)
union_density %<>% setnames(., old = c(2), new = c("Year")) %>%
  subset(Country_Code %in% country_codes_vector) %>%
  .[,c(4):=NULL]
#merge
tidy_data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(tidy_data, union_density) )

# rearrange variable order
tidy_data = setcolorder(tidy_data, c(3,2,1,4:111))

# set object type to data.frame
tidy_data=setDF(tidy_data)

# set data type to numeric
tidy_data[,4:111] = sapply(tidy_data[,4:111],as.numeric)

# set object type to data.table
tidy_data = as.data.table(tidy_data)

# drop old Union Variables # drop unnecessary gini variables # drop old cpi variable
tidy_data <- tidy_data[ ,c(48,64:67,69:73) := NULL]

# fix oecd_pension_name
tidy_data = setnames(tidy_data, old = c(99
                                        
), 
new = c("Pension_Spending_Public" 
        
))

#replace na from union data with latest non na value
tidy_data <- tidy_data[, Cent_Union := UD[1], .(idcode, cumsum(!is.na(UD)))]

#replace na from unemployment spending data with latest non na value
tidy_data <- tidy_data[, Unemployment_Spending := Unemployment_Spending[1], .(idcode, cumsum(!is.na(Unemployment_Spending)))]

#replace na from health spending data with latest non na value
tidy_data <- tidy_data[, Health_Spending := Health_Spending[1], .(idcode, cumsum(!is.na(Health_Spending)))]

#replace na from pension spending  data with latest non na value
tidy_data <- tidy_data[, Pension_Spending_Public := Pension_Spending_Public[1], .(idcode, cumsum(!is.na(Pension_Spending_Public)))]
tidy_data <- tidy_data[, Pension_Spending_Private := Pension_Spending_Private[1], .(idcode, cumsum(!is.na(Pension_Spending_Private)))]

# drop old variables
tidy_data <- tidy_data[ ,c(63,101) := NULL]

#replace gaps in inequality data
tidy_data <- tidy_data[, Top_1Percent_Wealth := Top_1Percent_Wealth[1], .(idcode, cumsum(!is.na(Top_1Percent_Wealth)))]
tidy_data <- tidy_data[, Top_10Percent_Wealth := Top_10Percent_Wealth[1], .(idcode, cumsum(!is.na(Top_10Percent_Wealth)))]
tidy_data <- tidy_data[, Top_1Percent_Income := Top_1Percent_Income[1], .(idcode, cumsum(!is.na(Top_1Percent_Income)))]
tidy_data <- tidy_data[, Top_10Percent_Income := Top_10Percent_Income[1], .(idcode, cumsum(!is.na(Top_10Percent_Income)))]

#create missing growth rate variables
tidy_data$D_Household_Savings <- with(tidy_data, ave(Household_Savings , idcode, FUN=function(x) c(NA, (diff(x)/x[-length(x)])*100) ))
tidy_data$D_Top_1Percent_Income <- with(tidy_data, ave(Top_1Percent_Income , idcode, FUN=function(x) c(NA, (diff(x)/x[-length(x)])*100) ))

#add missing real share price index growth rate
tidy_data$Nominal_Delta_Share_Prices <- with(tidy_data, ave(Nominal_Share_Prices , idcode, FUN=function(x) c(NA, (diff(x)/x[-length(x)])*100) ))
tidy_data %<>% 
  group_by(idcode) %>% 
  mutate(Delta_Share_Prices = Nominal_Delta_Share_Prices - Inflation)






#save tidy data
write.csv(tidy_data, "data_tidy/tidy_data.csv", row.names = FALSE )

# clean environment 
rm(list = ls())

# import function 
tidy_data=fread("data_tidy/tidy_data.csv", colClasses = "double", na.strings = "..")

# set object type to data.frame
tidy_data=setDF(tidy_data)

# set data types
tidy_data[,4:103] = sapply(tidy_data[,4:103],as.numeric)
tidy_data[,1] = sapply(tidy_data[,1],as.factor)

# set object type to data.table
tidy_data = as.data.table(tidy_data)



#delete empty duplicates
#tidy_data = as.data.table(tidy_data)
#tidy_data[,c(83:87):=NULL]
# optional conversion function to pdata.frame class for plm tests
# Data = pdata.frame(Data, index=c("idcode", "Year"))

# End
# Import Tidy Data ####

# clean environment 
rm(list = ls())

# import function 
tidy_data=fread("data_tidy/tidy_data.csv", colClasses = "double", na.strings = "..")

# set object type to data.frame
tidy_data=setDF(tidy_data)

# set data types
tidy_data[,4:103] = sapply(tidy_data[,4:103],as.numeric)
tidy_data[,1] = sapply(tidy_data[,1],as.factor)

# set object type to data.table
tidy_data = as.data.table(tidy_data)

#delete years before 1980 and after 2019
tidy_data <- tidy_data[-which(Year < 1980)] 
tidy_data <- tidy_data[-which(Year > 2019)]

tidy_data = setnames(tidy_data, old = c(28),new = c("Financial_Development_Index"))

# Subsets ####
# change subsets
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for 29 high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c(
  "Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan", "Korea", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data = tidy_data %>% subset(., Country_Code %in% country_codes_vector)

#create subset for financial crisis period (1980-2007) 
#delete years before 1980 and after 2019
tidy_data_pre_crisis <- tidy_data[-which(Year > 2007)]

#create subset for financial crisis period (2007-2019) 
tidy_data_post_crisis <- tidy_data[-which(Year < 2007)] 

# varities of capitalism subsets

# original lme classification
#"Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States"
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c("Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States"))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data_original_lme= tidy_data %>% subset(., Country_Code %in% country_codes_vector)

# original cme classification - without following countries, due to them not being part of the oecd: iceland
#"Austria","Belgium", "Denmark", "Finland", "Germany", "Japan", "Netherlands", "Norway", "Sweden", "Switzerland"
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c("Austria","Belgium", "Denmark", "Finland", "Germany", "Japan", "Netherlands", "Norway", "Sweden", "Switzerland"))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data_original_cme = tidy_data %>% subset(., Country_Code %in% country_codes_vector)

# original mediterranean market economies / mixed market economies- without following countries, due to them not beeing part of the oecd: turkey
#"France", "Italy", "Spain", "Portugal", "Greece"
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c("France", "Italy", "Spain", "Portugal", "Greece"))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data_original_mediterranean = tidy_data %>% subset(., Country_Code %in% country_codes_vector)

# schneider and paunescu 2012 lme classification
#"Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States", "Denmark", "Switzerland", "Finland", "Netherlands"
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c("Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States", "Denmark", "Switzerland", "Finland", "Netherlands"))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data_schneider_lme = tidy_data %>% subset(., Country_Code %in% country_codes_vector)

# schneider and paunescu 2012 cme classification
#"Austria", "Belgium", "France", "Germany", "Czech Republic", "Italy" , "Finland"
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c("Austria", "Belgium", "France", "Germany", "Czech Republic", "Italy" , "Finland"))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data_schneider_cme = tidy_data %>% subset(., Country_Code %in% country_codes_vector)

# schneider and paunescu 2012 hybrid classification
#"Hungary", "Japan", "Korea", "Poland", "Norway" ,  "Czech Republic", "Italy"
oecd_database=fread("data_raw/oecd_database.csv", colClasses = "double", na.strings = "..")
oecd_database %>% setnames(., old = c(1,7), new = c("Country_Code","Year"))
oecd_database[,c(3,5,6,8,9,10,11,12,13,14,16,17):=NULL]
# subset for high income oecd countries
oecd_database = oecd_database %>% subset(., Country %in% c("Hungary", "Japan", "Korea", "Poland", "Norway" , "Czech Republic", "Italy"))
# create panel format
oecd_database %<>% 
  pivot_wider(names_from = Variable, values_from = Value)
# create a list of high income oecd countries as reference to filter other data sets
oecd_database = as.data.table(oecd_database)
country_codes_vector = oecd_database[, Country_Code]
country_codes_vector = unique(unlist(strsplit(country_codes_vector, " ")))
tidy_data_schneider_hybrid = tidy_data %>% subset(., Country_Code %in% country_codes_vector)


# varities of capitalism pre and post financial crisis subsets

tidy_data_original_cme_pre_crisis <- tidy_data_original_cme[-which(Year > 2007)]


tidy_data_original_cme_post_crisis <- tidy_data_original_cme[-which(Year < 2007)] 


tidy_data_original_lme_pre_crisis <- tidy_data_original_lme[-which(Year > 2007)]


tidy_data_original_lme_post_crisis <- tidy_data_original_lme[-which(Year < 2007)] 


tidy_data_original_mediterranean_pre_crisis <- tidy_data_original_mediterranean[-which(Year > 2007)]

tidy_data_original_mediterranean_post_crisis <- tidy_data_original_mediterranean[-which(Year < 2007)] 

# Descriptive Statistics ####

# count number of observations for specific variables/countries

#count variables per country
tidy_data[ , N := sum(!is.na(Market_Cap)), by = idcode]
print(tidy_data[idcode == 28, 100])

#count variables for all countries
tidy_data[ , N := sum(!is.na(Top_1Percent_Wealth))]
print(tidy_data[1, 100])

#na ersetzern mit vor value: Educ_Spending_GDP Educ_Spending_Total Public_Educ_Spending_Tertiary, Public_Educ_Spending_Total

# create descriptive table 

# select variables
descriptive_data <- select(Data, "idcode", "Year", "Household_Savings", "Gini_SWIID9", "Top_1Percent", "Top_10Percent",
                           "Dependency_Ratio", "Disposable_Income", "Interest_Rate",
                           "Fiscal_Account_Balance", "GDP", "Inflation", "Share_Prices",
                           "House_Prices", "Private_Credit", "Finreform")
# store selected variables
descriptive_data<-as.data.frame(descriptive_data)
# descriptive_data<-na.omit(descriptive_data)
descriptive_data[,1] = sapply(descriptive_data[,1],as.numeric)

# create and save descriptive statistics table in word format
stargazer(descriptive_data,
          digits=2,
          title="Descriptive Statistics",
          type = "html",
          out="output/descriptive.doc",
          
          
          covariate.labels = c("ID Code (Countries)", "Year", "Household Savings Rate", "Gini Coefficient", "Top 1 %", "Top 10%",
                               "Old-Age Dependency", "Disposable Income", "Interest Rate", "Fiscal Account Balance", "GDP",
                               "Inflation", "Share Prices", "House Prices", "Private Credit", "Finreform") 
)

ggplot(Data, aes(x=Household_Savings)) +
  geom_histogram()

# show max value 

max(tidy_data$Inflation, na.rm = TRUE)


  # table of summary statistics
# select variables
descriptive_data <- select(tidy_data, "idcode", "Year", "Household_Savings", "Top_1Percent_Income", "Top_10Percent_Income", "Gini_SWIID9", "Top_1Percent_Wealth", "Top_10Percent_Wealth",
                           "Dependency_Ratio", "Disposable_Income", "GDP", "Interest_Rate", "Inflation", "Nominal_Share_Prices", "House_Prices", "Private_Credit",
                           "Financial_Development_Index", "Fiscal_Account_Balance", "Current_Account_Balance", "Social_Expenditure", "Cent_Union", "Cent_Bargaining",
                           "Finreform", "Market_Cap")

# store selected variables
descriptive_data<-as.data.frame(descriptive_data)
# descriptive_data<-na.omit(descriptive_data)
descriptive_data[,1] = sapply(descriptive_data[,1],as.numeric)

# create and save descriptive statistics table in word format
stargazer(descriptive_data,
          digits=2,
          title="Descriptive Statistics",
          type = "html",
          out="output/descriptive.doc",
          covariate.labels = c("Countries", "Year", "Household Savings Rate", "Top 1% Income Share", "Top 10% Income Share", "Gini Coefficient", "Top 1% Wealth Share",
                               "Top 10% Wealth Share", "Old Age Dependency Ratio", "Disposable Income", "Gross Domestic Product", "Interest Rate", "Inflation",
                               "Share Prices", "House Prices", "Private Credit", "Financial Development Index", "Fiscal Account Balance", "Current Account Balance",
                               "Social Transfers", "Union Density", "Centralization of Wage Bargaining", "Financial Reform Database", "Market Capizalization")
)

  # line charts for variables over time for all countries####
# set theme 
windows()
theme_set(
  theme_minimal()
)
# graphic device needs to be sligthly expanded to the right in order to make room for missing legend
# Top 1% 1980-2019
one<- ggplot(subset(tidy_data, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                                   ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                            "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

two<- ggplot(subset(tidy_data, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

three<- ggplot(subset(tidy_data, Country_Code %in% c( "ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c( "ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                            "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

four<- ggplot(subset(tidy_data, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                            ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen","SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")


top1_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Top 1% Income Share from 1980 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
top1_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_over_time.jpeg', top1_over_time, dpi = 600)

# Top 10% 1980-2019
one<- ggplot(subset(tidy_data, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                                   ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

two<- ggplot(subset(tidy_data, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

three<- ggplot(subset(tidy_data, Country_Code %in% c( "ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

four<- ggplot(subset(tidy_data, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                            ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")


top10_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Top 10% Income Share from 1980 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
top10_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top10_over_time.jpeg', top10_over_time, dpi = 600)

# Gini Coefficient 1980-2019
one<- ggplot(subset(tidy_data, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                                   ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

two<- ggplot(subset(tidy_data, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

three<- ggplot(subset(tidy_data, Country_Code %in% c("ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

four<- ggplot(subset(tidy_data, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                            ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")


gini_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Gini Coefficient from 1980 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
gini_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/gini_over_time.jpeg', gini_over_time, dpi = 600)

# Household Savings Rate 1980-2019
one<- ggplot(subset(tidy_data, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                                   ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                            "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

two<- ggplot(subset(tidy_data, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

three<- ggplot(subset(tidy_data, Country_Code %in% c("ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")

four<- ggplot(subset(tidy_data, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                            ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")


savings_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Household Savings Rate from 1980 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
savings_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/savings_over_time.jpeg', savings_over_time, dpi = 600)

# Top 1% 1980-2007
one<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c( "ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                              ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c( "ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                            "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen","SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


top1_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Top 1% Income Share from 1980 to 2007") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
top1_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_over_time_pre_crisis.jpeg', top1_over_time, dpi = 600)

# Top 10% 1980-2007
one<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c( "ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                              ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


top10_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Top 10% Income Share from 1980 to 2007") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
top10_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top10_over_time_pre_crisis.jpeg', top10_over_time, dpi = 600)

# Gini Coefficient 1980-2007
one<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                            ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


gini_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Gini Coefficient from 1980 to 2007") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
gini_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/gini_over_time_pre_crisis.jpeg', gini_over_time, dpi = 600)

# Household Savings Rate 1980-2007
one<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_pre_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Household_Savings
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


savings_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Household Savings Rate from 1980 to 2007") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
savings_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/savings_over_time_pre_crisis.jpeg', savings_over_time, dpi = 600)

# Top 1% 2007-2019
one<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c( "ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                                              ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c( "ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                            "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Top_1Percent_Income
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen","SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Top 1% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


top1_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Top 1% Income Share from 2007 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
top1_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_over_time_post_crisis.jpeg', top1_over_time, dpi = 600)

# Top 10% 2007-2019
one<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c( "ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                                              ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Top_10Percent_Income
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Top 10% Income Share in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


top10_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Top 10% Income Share from 2007 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
top10_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top10_over_time_post_crisis.jpeg', top10_over_time, dpi = 600)

# Gini Coefficient 2007-2019
one<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                            ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Gini_SWIID9
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Gini Coefficient in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


gini_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Gini Coefficient from 2007 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
gini_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/gini_over_time_post_crisis.jpeg', gini_over_time, dpi = 600)

# Household Savings Rate 2007-2019
one<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE" , "DEU" , "DNK")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("AUS" = "brown", "AUT" = "blueviolet", "BEL" = "red", "CAN" = "blue", "CHE" = "black",
                                           "CZE" = "aquamarine", "DEU" = "darkorange", "DNK" = "darkolivegreen")) +
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

two<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("ESP", "EST", "FIN", "FRA", "GBR","GRC", "HUN" , "IRL")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                           ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ESP" = "darkmagenta", "EST" = "darkgreen", "FIN" = "cyan", "FRA" = "cadetblue", "GBR" = "gold",
                                           "GRC" = "firebrick1", "HUN" = "blue", "IRL" = "deeppink"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

three<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("ITA", "JPN", "KOR","LUX" , "NLD", "NOR", "NZL", "POL")), mapping = aes(x = Year, y = Household_Savings
                                                                                                                             ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("ITA" = "green", "JPN" = "blue", "KOR" = "lightcoral","LUX" = "midnightblue", 
                                           "NLD" = "red", "NOR" = "blueviolet", "NZL" = "lightsteelblue3", "POL" = "khaki"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

four<- ggplot(subset(tidy_data_post_crisis, Country_Code %in% c("PRT", "SVK","SVN", "SWE" , "USA")), mapping = aes(x = Year, y = Household_Savings
                                                                                                       ,group = Country_Code, colour = Country_Code)) +
  geom_line(size = 1)+
  scale_color_manual(name = "", values = c("PRT" = "lightseagreen",
                                           "SVK" = "peru", "SVN" = "seashell4", "SWE" = "yellow", "USA" = "red"))+
  xlab("Years") + ylab("Household Savings Rate in %") +
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.position="bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


savings_over_time <- (one + two) / (three + four) +
  plot_annotation(title = "Development of the Household Savings Rate from 2007 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
savings_over_time


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/savings_over_time_post_crisis.jpeg', savings_over_time, dpi = 600)

  # scatter plots for  all countries with trend lines ####
windows()
theme_set(
  theme_minimal()
)



# household savings
Household_Savings <- ggplot(data = tidy_data, aes(x=Year, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Household Savings Rate" ,
    #subtitle = "Correlation of Top 1% Income Share with Household Savings Rate",
    x = "Years",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/household_savings.jpeg', Household_Savings, dpi = 600)

# gini
Gini <- ggplot(data = tidy_data, aes(x=Year, y=Gini_SWIID9)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Gini Coefficient" ,
    x = "Years",
    y = "Gini Coefficient in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/gini_swiid9.jpeg', Gini, dpi = 600)

# top 1%
Top1 <- ggplot(data = tidy_data, aes(x=Year, y=Top_1Percent_Income)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Top 1% Income Share" ,
    x = "Years",
    y = "Top 1% Income Share in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1.jpeg', Top1, dpi = 600)

# top 10%
Top10 <- ggplot(data = tidy_data, aes(x=Year, y=Top_10Percent_Income)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Top 10% Income Share" ,
    x = "Years",
    y = "Top 10% Income Share in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top10.jpeg', Top10, dpi = 600)

# old age dependency ratio
dependency_ratio <- ggplot(data = tidy_data, aes(x=Year, y=Dependency_Ratio)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Old Age Dependency Ratio" ,
    x = "Years",
    y = "Old Age Dependency Ratio in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/dependency_ratio.jpeg', dependency_ratio, dpi = 600)

# disposable income
disposable_income <- ggplot(data = tidy_data, aes(x=Year, y=Delta_Disposable_Income)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for Real Disposable Income Growth" ,
    x = "Years",
    y = "Growth Rate of Real Disposable Income in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/disposable_income.jpeg', disposable_income, dpi = 600)

# log(GDP)
log_gdp <- ggplot(data = tidy_data, aes(x=Year, y=log(GDP))) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Natural Logarithm of GDP" ,
    x = "Years",
    y = "Natural Logarithm of GDP"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/log_gdp.jpeg', log_gdp, dpi = 600)

# growth rate of gdp
gdp_growth <- ggplot(data = tidy_data, aes(x=Year, y=D_GDP)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Growth Rate of Real GDP" ,
    x = "Years",
    y = "Growth Rate of Real GDP in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/gdp_growth.jpeg', gdp_growth, dpi = 600)

# interest rate
interest_rate <- ggplot(data = tidy_data, aes(x=Year, y=Interest_Rate)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Real Interest Rate" ,
    x = "Years",
    y = "Real Interest Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/interest_rate.jpeg', interest_rate, dpi = 600)

# inflation
inflation <- ggplot(tidy_data[which(tidy_data$Inflation<50)], aes(x=Year, y=Inflation)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Inflation Rate" ,
    x = "Years",
    y = "Inflation Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/inflation.jpeg', inflation, dpi = 600)

# fiscal balance
fiscal_balance <- ggplot(data = tidy_data, aes(x=Year, y=Fiscal_Account_Balance)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Fiscal Account Balance" ,
    x = "Years",
    y = "Fiscal Account Balance in % of GDP"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/fiscal_balance.jpeg', fiscal_balance, dpi = 600)

# current balance
current_balance <- ggplot(data = tidy_data, aes(x=Year, y=Current_Account_Balance)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Current Account Balance" ,
    x = "Years",
    y = "Current Account Balance in % of GDP"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/current_balance.jpeg', current_balance, dpi = 600)

# share prices
share_prices <- ggplot(data = tidy_data[which(tidy_data$Delta_Share_Prices<600)], aes(x=Year, y=Delta_Share_Prices)) + 
  geom_point(colour = "black", size = 1.5)+
  ylim(-50, 125)+
  labs(
    title = "Data Points for Real Share Price Growth Rate",
    x = "Years",
    y = "Growth Rate of Real Share Prices in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/share_prices.jpeg', share_prices, dpi = 600)

# house prices
house_prices <- ggplot(data = tidy_data, aes(x=Year, y=Delta_House_Prices)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for Growth of Real House Prices" ,
    x = "Years",
    y = "Growth Rate of Real House Prices in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/house_prices.jpeg', house_prices, dpi = 600)

# private credit
private_credit <- ggplot(data = tidy_data, aes(x=Year, y=Private_Credit)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Private Credit" ,
    x = "Years",
    y = "Private Credit in % of GDP"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/private_credit.jpeg', private_credit, dpi = 600)

# financial development
financial_development <- ggplot(data = tidy_data, aes(x=Year, y=Financial_Development_Index)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Financial Development Index" ,
    x = "Years",
    y = "Financial Development Index in Points"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/financial_development.jpeg', financial_development, dpi = 600)

# cent union
union_density <- ggplot(data = tidy_data, aes(x=Year, y=Cent_Union)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Union Density" ,
    x = "Years",
    y = "Union Density in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/union_density.jpeg', union_density, dpi = 600)

# cent bargaining
cent_bargaining <- ggplot(data = tidy_data, aes(x=Year, y=Cent_Bargaining)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for Centralization of Wage Bargaining" ,
    x = "Years",
    y = "Centralization of Wage Bargaining in Points"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/cent_bargaining.jpeg', cent_bargaining, dpi = 600)

# social transfers
social_expenditure <- ggplot(data = tidy_data, aes(x=Year, y=Social_Expenditure)) + 
  geom_point(colour = "black", size = 1.5)+
  labs(
    title = "Data Points for the Social Transfers" ,
    x = "Years",
    y = "Social Transfers in % of GDP"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=18, face = "bold"))+theme(axis.title = element_text(size = 16)) + theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1.5)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/social_expenditure.jpeg', social_expenditure, dpi = 600)



  # scatter plot for correlation between top 1% income share and household savings rate with trend line for all countries combined ####
# set theme
theme_set(
  theme_minimal()
)
windows()
# top 1% income and household savings from 1980 to 2019
one <- ggplot(tidy_data[which(tidy_data$Household_Savings>-30)], aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  labs(
    title = "Level - Level" ,
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# Delta top 1% income and household savings from 1980 to 2019
two <- ggplot(tidy_data[which(tidy_data$Household_Savings>-30)], aes(x=D_Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  labs(
    title = "Growth Rate - Level" ,
    x = "Growth Rate of Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# top 1% income and Delta household savings from 1980 to 2019
three <- ggplot(tidy_data[which(tidy_data$D_Household_Savings>-2000)], aes(x=Top_1Percent_Income, y=D_Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  ylim(-1500,2000)+
  labs(
    title = "Level - Growth Rate" ,
    x = "Top 1% Income Share in %",
    y = "Growth Rate of Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# Delta top 1% income and Delta household savings from 1980 to 2019
four <- ggplot(tidy_data[which(tidy_data$D_Household_Savings>-2000)], aes(x=D_Top_1Percent_Income, y=D_Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  ylim(-1500,2000)+
  labs(
    title = "Growth Rate - Growth Rate" ,
    x = "Growth Rate of Top 1% Income Share in %",
    y = "Growth Rate of Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

correlation_29_countries <- (one + two) / (three + four) +
  plot_annotation(title = "Correlation of Top 1% Income Share with Household Savings Rate from 1980 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
correlation_29_countries

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/correlation_29_countries.jpeg', correlation_29_countries, dpi = 600)

# top 1% income and household savings from 1980 to 2007
one <- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$Household_Savings>-30)], aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  labs(
    title = "Level - Level" ,
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# Delta top 1% income and household savings from 1980 to 2007
two <- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$Household_Savings>-30)], aes(x=D_Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  labs(
    title = "Growth Rate - Level" ,
    x = "Growth Rate of Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# top 1% income and Delta household savings from 1980 to 2007
three <- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$D_Household_Savings>-2000)], aes(x=Top_1Percent_Income, y=D_Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  ylim(-1500,2000)+
  labs(
    title = "Level - Growth Rate" ,
    x = "Top 1% Income Share in %",
    y = "Growth Rate of Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# Delta top 1% income and Delta household savings from 1980 to 2007
four <- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$D_Household_Savings>-2000)], aes(x=D_Top_1Percent_Income, y=D_Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  ylim(-1500,2000)+
  labs(
    title = "Growth Rate - Growth Rate" ,
    x = "Growth Rate of Top 1% Income Share in %",
    y = "Growth Rate of Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

correlation_29_countries <- (one + two) / (three + four) +
  plot_annotation(title = "Correlation of Top 1% Income Share with Household Savings Rate from 1980 to 2007") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
correlation_29_countries

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/correlation_29_countries_pre_crisis.jpeg', correlation_29_countries, dpi = 600)

# top 1% income and household savings from 2007 to 2019
one <- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$Household_Savings>-30)], aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  labs(
    title = "Level - Level" ,
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# Delta top 1% income and household savings from 2007to 2019
two <- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$Household_Savings>-30)], aes(x=D_Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  labs(
    title = "Growth Rate - Level" ,
    x = "Growth Rate of Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# top 1% income and Delta household savings from 2007 to 2019
three <- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$D_Household_Savings>-2000)], aes(x=Top_1Percent_Income, y=D_Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  ylim(-1500,2000)+
  labs(
    title = "Level - Growth Rate" ,
    x = "Top 1% Income Share in %",
    y = "Growth Rate of Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

# Delta top 1% income and Delta household savings from 2007 to 2019
four <- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$D_Household_Savings>-2000)], aes(x=D_Top_1Percent_Income, y=D_Household_Savings)) + 
  geom_point(colour = "black", size = 0.8)+
  ylim(-1500,2000)+
  labs(
    title = "Growth Rate - Growth Rate" ,
    x = "Growth Rate of Top 1% Income Share in %",
    y = "Growth Rate of Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    plot.subtitle = element_text(color = "black", size=7,face="bold"))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=1)

correlation_29_countries <- (one + two) / (three + four) +
  plot_annotation(title = "Correlation of Top 1% Income Share with Household Savings Rate from 2007 to 2019") &
  theme(plot.title = element_text(color = "royalblue4", size=12, face = "bold"))
correlation_29_countries

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/correlation_29_countries_post_crisis.jpeg', correlation_29_countries, dpi = 600)




  # scatter plots for correlation between top 1% income share and household savings rate with trend line for individual countries ####
# set theme
windows()
theme_set(
  theme_minimal()
)

# caption = "Data from OECD and WID (1980-2019), Trend Line created with OLS"

AUS <- ggplot(data = subset(tidy_data, Country_Code == "AUS"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Australia" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))
AUS


ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Australia.jpeg', AUS, dpi = 600)

AUT <- ggplot(data = subset(tidy_data, Country_Code == "AUT"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Austria" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Austria.jpeg', AUT, dpi = 600)

BEL <- ggplot(data = subset(tidy_data, Country_Code == "BEL"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Belgium" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Belgium.jpeg', BEL, dpi = 600)


CAN <- ggplot(data = subset(tidy_data, Country_Code == "CAN"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Canada" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Canada.jpeg', CAN, dpi = 600)

CHE <- ggplot(data = subset(tidy_data, Country_Code == "CHE"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Switzerland" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Switzerland.jpeg', CHE, dpi = 600)

# CHL <- ggplot(data = subset(tidy_data, Country_Code == "CHE"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
#   geom_point(colour = "black", size = 3)+
#   labs(
#     title = "Chile" ,
#     subtitle = "Observations from 1980 to 2019",
#     x = "Top 1% Income Share in %",
#     y = "Household Savings Rate in %"
#   )+ 
#   theme(
#     plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
#   geom_smooth(method=lm, se=FALSE, col='blue', size=2)
#   #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))
# 
# ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Chile.jpeg', CHL, dpi = 600)

CZE <- ggplot(data = subset(tidy_data, Country_Code == "CZE"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3) +
  xlim(10, 11.6) +
  labs(
    title = "Czech Republic" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Czech Republic.jpeg', CZE, dpi = 600)

DEU <- ggplot(data = subset(tidy_data, Country_Code == "DEU"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Germany" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Germany.jpeg', DEU, dpi = 600)

DNK <- ggplot(data = subset(tidy_data, Country_Code == "DNK"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(9,13) +
  labs(
    title = "Denmark" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Denmark.jpeg', DNK, dpi = 600)

ESP <- ggplot(data = subset(tidy_data, Country_Code == "ESP"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Spain" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Spain.jpeg', ESP, dpi = 600)

EST <- ggplot(data = subset(tidy_data, Country_Code == "EST"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(9,20)+
  labs(
    title = "Estonia" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Estonia.jpeg', EST, dpi = 600)

FIN <- ggplot(data = subset(tidy_data, Country_Code == "FIN"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Finland" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Finland.jpeg', FIN, dpi = 600)

FRA <- ggplot(data = subset(tidy_data, Country_Code == "FRA"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "France" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_France.jpeg', FRA, dpi = 600)

GBR <- ggplot(data = subset(tidy_data, Country_Code == "GBR"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(10,15)+
  labs(
    title = "United Kingdom" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_United Kingdom.jpeg', GBR, dpi = 600)

GRC <- ggplot(data = subset(tidy_data, Country_Code == "GRC"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Greece" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Greece.jpeg', GRC, dpi = 600)

HUN <- ggplot(data = subset(tidy_data, Country_Code == "HUN"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(7, 13)+
  labs(
    title = "Hungary" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Hungary.jpeg', HUN, dpi = 600)

IRL <- ggplot(data = subset(tidy_data, Country_Code == "IRL"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Ireland" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Ireland.jpeg', IRL, dpi = 600)

# ISR <- ggplot(data = subset(tidy_data, Country_Code == "ISR"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
#   geom_point(colour = "black", size = 3)+
#   labs(
#     title = "Israel" ,
#     subtitle = "Observations from 1980 to 2019",
#     x = "Top 1% Income Share in %",
#     y = "Household Savings Rate in %"
#   )+ 
#   theme(
#     plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
#   geom_smooth(method=lm, se=FALSE, col='blue', size=2)
#   #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))
# 
# ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Israel.jpeg', ISR, dpi = 600)

ITA <- ggplot(data = subset(tidy_data, Country_Code == "ITA"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(7,9.5)+
  labs(
    title = "Italy" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Italy.jpeg', ITA, dpi = 600)

JPN <- ggplot(data = subset(tidy_data, Country_Code == "JPN"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Japan" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Japan.jpeg', JPN, dpi = 600)

KOR <- ggplot(data = subset(tidy_data, Country_Code == "KOR"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(12, 15.5)+
  labs(
    title = "Korea" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Korea.jpeg', KOR, dpi = 600)

# LTU <- ggplot(data = subset(tidy_data, Country_Code == "LTU"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
#   geom_point(colour = "black", size = 3)+
#   xlim(8,15)+
#   labs(
#     title = "Lithuania" ,
#     subtitle = "Observations from 1980 to 2019",
#     x = "Top 1% Income Share in %",
#     y = "Household Savings Rate in %"
#   )+ 
#   theme(
#     plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
#   geom_smooth(method=lm, se=FALSE, col='blue', size=2)
#   #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))
# 
# ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Lithuania.jpeg', LTU, dpi = 600)

LUX <- ggplot(data = subset(tidy_data, Country_Code == "LUX"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Luxembourg" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Luxembourg.jpeg', LUX, dpi = 600)

# LVA <- ggplot(data = subset(tidy_data, Country_Code == "LVA"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
#   geom_point(colour = "black", size = 3)+
#   xlim(8, 12.5)+
#   labs(
#     title = "Latvia" ,
#     subtitle = "Observations from 1980 to 2019",
#     x = "Top 1% Income Share in %",
#     y = "Household Savings Rate in %"
#   )+ 
#   theme(
#     plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
#   geom_smooth(method=lm, se=FALSE, col='blue', size=2)
#   #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))
# 
# ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Latvia.jpeg', LVA, dpi = 600)

NLD <- ggplot(data = subset(tidy_data, Country_Code == "NLD"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(5.75, 8)+
  labs(
    title = "Netherlands" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Netherlands.jpeg', NLD, dpi = 600)

NOR <- ggplot(data = subset(tidy_data, Country_Code == "NOR"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "Norway" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Norway.jpeg', NOR, dpi = 600)

NZL <- ggplot(data = subset(tidy_data, Country_Code == "NZL"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "New Zealand" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_New Zealand.jpeg', NZL, dpi = 600)

POL <- ggplot(data = subset(tidy_data, Country_Code == "POL"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(9,16)+
  labs(
    title = "Poland" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Poland.jpeg', POL, dpi = 600)

PRT <- ggplot(data = subset(tidy_data, Country_Code == "PRT"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(9.5,12)+
  labs(
    title = "Portugal" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Portugal.jpeg', PRT, dpi = 600)

SVK <- ggplot(data = subset(tidy_data, Country_Code == "SVK"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(5,11.5)+
  labs(
    title = "Slovakia" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Slovakia.jpeg', SVK, dpi = 600)

SVN <- ggplot(data = subset(tidy_data, Country_Code == "SVN"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(6.25,8.5)+
  labs(
    title = "Slovenia" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Slovenia.jpeg', SVN, dpi = 600)

SWE <- ggplot(data = subset(tidy_data, Country_Code == "SWE"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  xlim(8.5,13)+
  labs(
    title = "Sweden" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_Sweden.jpeg', SWE, dpi = 600)

USA <- ggplot(data = subset(tidy_data, Country_Code == "USA"), aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 3)+
  labs(
    title = "United States" ,
    subtitle = "Observations from 1980 to 2019",
    x = "Top 1% Income Share in %",
    y = "Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size= 22, face = "bold"))+ theme(axis.title = element_text(size = 18)) + theme(plot.subtitle=element_text(size=18))+theme(axis.text = element_text(size=14))+
  geom_smooth(method=lm, se=FALSE, col='blue', size=2)
  #theme(plot.background = element_rect(colour = "gray80", fill=NA, size=1))

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/top1_savings_United States.jpeg', USA, dpi = 600)

# create 29 graphs in one file 
ggplot(tidy_data, aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~idcode)


#sapply(descriptive_data, max, na.rm = TRUE)


  # scatter plots with functional form trend lines ####

# Top 1% functional forms from 1980 to 2019
top1<- ggplot(tidy_data[which(tidy_data$Household_Savings>-30)], aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 1980 to 2019", fill="Possible Functional Forms from 1980 to 2019") +
  xlab("Top 1% Income Share in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Top 1% Income Share") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
top1


# Top 10% functional forms from 1980 to 2019
top10<- ggplot(tidy_data[which(tidy_data$Household_Savings>-30)], aes(x=Top_10Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 1980 to 2019", fill= "Possible Functional Forms from 1980 to 2019") +
  xlab("Top 10% Income Share in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Top 10% Income Share") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
top10

# Gini functional forms from 1980 to 2019
gini<- ggplot(tidy_data[which(tidy_data$Household_Savings>-30)], aes(x=Gini_SWIID9, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 1980 to 2019", fill="Possible Functional Forms from 1980 to 2019") +
  xlab("Gini Coefficient in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Gini Coefficient") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
gini

functional_forms <- ggarrange(top1, top10, gini,
                              common.legend = TRUE, legend = "top",
                              ncol = 2, nrow = 2)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/functional_forms.jpeg', functional_forms, dpi = 600)

# Top 1% functional forms from 1980 to 2007
top1<- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$Household_Savings>-30)], aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 1980 to 2007", fill="Possible Functional Forms from 1980 to 2007") +
  xlab("Top 1% Income Share in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Top 1% Income Share") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
top1


# Top 10% functional forms from 1980 to 2007
top10<- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$Household_Savings>-30)], aes(x=Top_10Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 1980 to 2007", fill= "Possible Functional Forms from 1980 to 2007") +
  xlab("Top 10% Income Share in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Top 10% Income Share") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
top10

# Gini functional forms from 1980 to 2007
gini<- ggplot(tidy_data_pre_crisis[which(tidy_data_pre_crisis$Household_Savings>-30)], aes(x=Gini_SWIID9, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 1980 to 2007", fill="Possible Functional Forms from 1980 to 2007") +
  xlab("Gini Coefficient in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Gini Coefficient") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
gini

functional_forms <- ggarrange(top1, top10, gini,
                              common.legend = TRUE, legend = "top",
                              ncol = 2, nrow = 2)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/functional_forms_pre_crisis.jpeg', functional_forms, dpi = 600)

# Top 1% functional forms from 2007 to 2019
top1<- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$Household_Savings>-30)], aes(x=Top_1Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 2007 to 2019", fill="Possible Functional Forms from 2007 to 2019") +
  xlab("Top 1% Income Share in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Top 1% Income Share") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
top1


# Top 10% functional forms from 2007  to 2019
top10<- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$Household_Savings>-30)], aes(x=Top_10Percent_Income, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 2007  to 2019", fill= "Possible Functional Forms from 2007  to 2019") +
  xlab("Top 10% Income Share in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Top 10% Income Share") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
top10

# Gini functional forms from 2007  to 2019
gini<- ggplot(tidy_data_post_crisis[which(tidy_data_post_crisis$Household_Savings>-30)], aes(x=Gini_SWIID9, y=Household_Savings)) + 
  geom_point(colour = "black", size = 1) + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Possible Functional Forms from 2007  to 2019", fill="Possible Functional Forms from 2007  to 2019") +
  xlab("Gini Coefficient in %") + ylab("Household Savings Rate in %") +
  labs(
    title = "Gini Coefficient") +
  theme(
    plot.title = element_text(color = "royalblue4", size=8, face = "bold"),
    axis.title = element_text(color = "black", size=7,face="bold"),
    legend.title = element_text(color = "royalblue4", size=12, face = "bold"),
    legend.text = element_text(color = "royalblue4", size=12, face = "bold"))
gini

functional_forms <- ggarrange(top1, top10, gini,
                              common.legend = TRUE, legend = "top",
                              ncol = 2, nrow = 2)

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/functional_forms_post_crisis.jpeg', functional_forms, dpi = 600)

# visualise possible functional forms of single groups
# m1 <- lm(Household_Savings ~ Top_1Percent_Income + idcode, data=tidy_data )
# m2 <- lm(Household_Savings ~ Top_1Percent_Income + I(Top_1Percent_Income^2) + idcode, data=tidy_data )
# m3 <- lm(Household_Savings ~ Top_1Percent_Income + I(Top_1Percent_Income^2) + I(Top_1Percent_Income^3) + idcode, data=tidy_data )
# 
# p1 <- ggpredict(m1, terms=c("Top_1Percent_Income", "idcode")) %>% 
#   mutate(form="linear") %>% 
#   rename("Top_1Percent_Income" = "x", 
#          "idcode" = "group", 
#          "Household_Savings" = "predicted")
# p2 <- ggpredict(m2, terms=c("Top_1Percent_Income", "idcode")) %>% 
#   mutate(form="quadratic") %>% 
#   rename("Top_1Percent_Income" = "x", 
#          "idcode" = "group", 
#          "Household_Savings" = "predicted")
# p3 <- ggpredict(m3, terms=c("Top_1Percent_Income", "idcode")) %>% 
#   mutate(form="cubic") %>% 
#   rename("Top_1Percent_Income" = "x", 
#          "idcode" = "group", 
#          "Household_Savings" = "predicted")
# 
# ggplot() + 
#   geom_line(data=p1, aes(x=Top_1Percent_Income, y=Household_Savings, colour="linear")) + 
#   geom_line(data=p2, aes(x=Top_1Percent_Income, y=Household_Savings, colour="quadratic")) + 
#   geom_line(data=p3, aes(x=Top_1Percent_Income, y=Household_Savings, colour="cubic")) + 
#   geom_point(data=tidy_data, aes(x=Top_1Percent_Income, y=Household_Savings)) + 
#   facet_wrap(~idcode) + 
#   theme_bw() + 
#   labs(colour="Functional\nForm")






  # scatter plots for average change of household savings and average change of top 1% income share with trend line  ####

# create average values for 1980 to 2019

na_omit_tidy_data<-na.omit(tidy_data, cols=c("Household_Savings", "Top_1Percent_Income"))

average_savings <- na_omit_tidy_data[ , .(mean_savings = mean(Household_Savings)), by = Country_Code]

average_top1 <- na_omit_tidy_data[ , .(mean_top1 = mean(Top_1Percent_Income)), by = Country_Code]

average_tidy_data <- merge(average_savings, average_top1, by = "Country_Code")

# plot average values from 1980 to 2019

#[which(tidy_data$Household_Savings>-30)]
theme_set(
  theme_minimal()
)
average_levels <- ggplot(average_tidy_data, aes(x=mean_top1, y=mean_savings)) + 
  geom_point(colour = "black", size = 1.2)+
  labs(
    title = "Average Household Savings Rate and Top 1% Income Share from 1980 to 2019" ,
    x = "Average Top 1% Income Share in %",
    y = "Average Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=9,face="bold"),
    plot.subtitle = element_text(color = "black", size=9,face="bold"))+
  geom_line(stat = "smooth",method=lm, se=FALSE, col='blue', size=1.5, alpha = 0.2)+
  geom_text_repel(aes(label = Country_Code, size = 2))+
  theme(legend.position = "none")  
#geom_smooth(method=lm, se=FALSE, col='blue', size=1) +
average_levels

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/average_levels.jpeg', average_levels, dpi = 600)

# create average values for 1980 to 2007

na_omit_tidy_data<-na.omit(tidy_data_pre_crisis, cols=c("Household_Savings", "Top_1Percent_Income"))

average_savings <- na_omit_tidy_data[ , .(mean_savings = mean(Household_Savings)), by = Country_Code]

average_top1 <- na_omit_tidy_data[ , .(mean_top1 = mean(Top_1Percent_Income)), by = Country_Code]

average_tidy_data <- merge(average_savings, average_top1, by = "Country_Code")

# plot average values from 1980 to 2007

#[which(tidy_data$Household_Savings>-30)]
theme_set(
  theme_minimal()
)
average_levels <- ggplot(average_tidy_data, aes(x=mean_top1, y=mean_savings)) + 
  geom_point(colour = "black", size = 1.2)+
  labs(
    title = "Average Household Savings Rate and Top 1% Income Share from 1980 to 2007" ,
    x = "Average Top 1% Income Share in %",
    y = "Average Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=9,face="bold"),
    plot.subtitle = element_text(color = "black", size=9,face="bold"))+
  geom_line(stat = "smooth",method=lm, se=FALSE, col='blue', size=1.5, alpha = 0.2)+
  geom_text_repel(aes(label = Country_Code, size = 2))+
  theme(legend.position = "none")  
#geom_smooth(method=lm, se=FALSE, col='blue', size=1) +
average_levels

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/average_levels_pre_crisis.jpeg', average_levels, dpi = 600)

# create average values from 1980 to 2007

na_omit_tidy_data<-na.omit(tidy_data_post_crisis, cols=c("Household_Savings", "Top_1Percent_Income"))

average_savings <- na_omit_tidy_data[ , .(mean_savings = mean(Household_Savings)), by = Country_Code]

average_top1 <- na_omit_tidy_data[ , .(mean_top1 = mean(Top_1Percent_Income)), by = Country_Code]

average_tidy_data <- merge(average_savings, average_top1, by = "Country_Code")

# plot average values from 1980 to 2007

#[which(tidy_data$Household_Savings>-30)]
theme_set(
  theme_minimal()
)
average_levels <- ggplot(average_tidy_data, aes(x=mean_top1, y=mean_savings)) + 
  geom_point(colour = "black", size = 1.2)+
  labs(
    title = "Average Household Savings Rate and Top 1% Income Share from 2007 to 2019" ,
    x = "Average Top 1% Income Share in %",
    y = "Average Household Savings Rate in %"
  )+ 
  theme(
    plot.title = element_text(color = "royalblue4", size=12, face = "bold"),
    axis.title = element_text(color = "black", size=9,face="bold"),
    plot.subtitle = element_text(color = "black", size=9,face="bold"))+
  geom_line(stat = "smooth",method=lm, se=FALSE, col='blue', size=1.5, alpha = 0.2)+
  geom_text_repel(aes(label = Country_Code, size = 2))+
  theme(legend.position = "none")  
#geom_smooth(method=lm, se=FALSE, col='blue', size=1) +
average_levels

ggsave('C:/Users/pauls/OneDrive/Studium/Master/4. Semester/Masterarbeit/output/average_levels_post_crisis.jpeg', average_levels, dpi = 600)









# Results ####



  # base line models: ####
    # bofinger scheuermeyer replication ####

# pooled linear baseline gini
po_linear_gini <- plm(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                 , data=tidy_data, model = "pooling")
po_linear_gini_robust_se <- vcovHC(po_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic baseline gini
po_quadratic_gini <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
                      , data=tidy_data, model = "pooling")
po_quadratic_gini_robust_se <- vcovHC(po_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear baseline gini
fe_linear_gini <- plm(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                 , data=tidy_data, model = "within")
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic baseline gini
fe_quadratic_gini <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                 , data=tidy_data, model = "within")
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear extended baseline gini
fe_linear_gini_extended <- plm(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                        Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit
                      , data=tidy_data, model = "within")
fe_linear_gini_extended_robust_se <- vcovHC(fe_linear_gini_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic extened baseline gini
fe_quadratic_gini_extended <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                           Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit
                         , data=tidy_data, model = "within")
fe_quadratic_gini_extended_robust_se <- vcovHC(fe_quadratic_gini_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# generate output table in word
stargazer(
  
  po_linear_gini, po_quadratic_gini, fe_linear_gini, fe_quadratic_gini,fe_linear_gini_extended,fe_quadratic_gini_extended,  # choose models
  
  se = list(po_linear_gini_robust_se, po_quadratic_gini_robust_se, fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_gini_extended_robust_se, fe_quadratic_gini_extended_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Replication of Income Inequality and Aggregated Saving by Bofinger and Scheuermeyer", # name table
  
  out="output/results_bofinger_table3_replication.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "POLS", "FE", "FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Gini", "Gini Quadratic", "Age Dependency Ratio", "Growth Rate of Real Disposable Income" , "Real Interest Rates", "Fiscal Account Balance",
                       "ln(GDP)", "Inflation", "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", "Private Credit"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)



    # top 10% ####

# pooled linear baseline top 10%
po_linear_top1 <- plm(Household_Savings~Top_10Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                      , data=tidy_data, model = "pooling")
po_linear_top1_robust_se <- vcovHC(po_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic baseline top 10%
po_quadratic_top1 <- plm(Household_Savings~Top_10Percent_Income+ I(Top_10Percent_Income^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
                         , data=tidy_data, model = "pooling")
po_quadratic_top1_robust_se <- vcovHC(po_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear baseline top 10%
fe_linear_top1 <- plm(Household_Savings~Top_10Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                      , data=tidy_data, model = "within")
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic baseline top 10%
fe_quadratic_top1 <- plm(Household_Savings~Top_10Percent_Income+ I(Top_10Percent_Income^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                         , data=tidy_data, model = "within")
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear extended baseline top 10%
fe_linear_top1_extended <- plm(Household_Savings~Top_10Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                                 Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit
                               , data=tidy_data, model = "within")
fe_linear_top1_extended_robust_se <- vcovHC(fe_linear_top1_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic extened baseline top 10%
fe_quadratic_top1_extended <- plm(Household_Savings~Top_10Percent_Income+ I(Top_10Percent_Income^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                                    Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit
                                  , data=tidy_data, model = "within")
fe_quadratic_top1_extended_robust_se <- vcovHC(fe_quadratic_top1_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()



# top 10% baseline regressions

stargazer(
  
  po_linear_top1, po_quadratic_top1, fe_linear_top1, fe_quadratic_top1, fe_linear_top1_extended,  fe_quadratic_top1_extended, # choose models
  
  se = list(po_linear_top1_robust_se, po_quadratic_top1_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se, fe_linear_top1_extended_robust_se, fe_quadratic_top1_extended_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger replication", # name table
  
  out="output/results_bofinger_table3_replication_top_income_10.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "POLS", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 10%", "Top 10% Quadratic","Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", " log(GDP)", "Inflation", "Growth Rate of Real Share Prices",
                       "Growth Rate of Real House Prices", "Private Credit"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # top 1% ####

# pooled linear baseline top 1%
po_linear_top1 <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                      , data=tidy_data, model = "pooling")
po_linear_top1_robust_se <- vcovHC(po_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic baseline top 1%
po_quadratic_top1 <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
                         , data=tidy_data, model = "pooling")
po_quadratic_top1_robust_se <- vcovHC(po_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear baseline top 1%
fe_linear_top1 <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                      , data=tidy_data, model = "within")
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic baseline top 1%
fe_quadratic_top1 <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                         , data=tidy_data, model = "within")
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear extended baseline top 1%
fe_linear_top1_extended <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                                 Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit
                               , data=tidy_data, model = "within")
fe_linear_top1_extended_robust_se <- vcovHC(fe_linear_top1_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic extened baseline top 1%
fe_quadratic_top1_extended <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                                    Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit
                                  , data=tidy_data, model = "within")
fe_quadratic_top1_extended_robust_se <- vcovHC(fe_quadratic_top1_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()



# top 1% baseline regressions

stargazer(
  
  po_linear_top1, po_quadratic_top1, fe_linear_top1, fe_quadratic_top1, fe_linear_top1_extended,  fe_quadratic_top1_extended, # choose models
  
  se = list(po_linear_top1_robust_se, po_quadratic_top1_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se, fe_linear_top1_extended_robust_se, fe_quadratic_top1_extended_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger replication", # name table
  
  out="output/results_bofinger_table3_replication_top_income_1.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "POLS", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic","Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", " log(GDP)", "Inflation", "Growth Rate of Real Share Prices",
                       "Growth Rate of Real House Prices", "Private Credit"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # top 1% - varities of capitalism ####
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                 #+Current_Account_Balance +
                  #D_GDP +
                 # log(GDP) +
                 # Inflation +  
                 # Delta_Share_Prices+ Delta_House_Prices + 
                 # Private_Credit 
                #+ Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                  #Cent_Union + 
                 # Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                , data=tidy_data_original_cme, model = "pooling")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
             # +Current_Account_Balance +
                #D_GDP +
              #  log(GDP) +
              #  Inflation +  
               # Delta_Share_Prices+ Delta_House_Prices + 
               # Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
               # Cent_Union + 
                #Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
              , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+ Current_Account_Balance +
                  #D_GDP +
                  #log(GDP) +
                 # Inflation +  
                 #Delta_Share_Prices+ Delta_House_Prices + 
                 # Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                 # Cent_Union + 
                  #Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                , data=tidy_data_original_lme, model = "pooling")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance +
                #D_GDP +
                #log(GDP) +
                #Inflation +  
               # Delta_Share_Prices+ Delta_House_Prices + 
               # Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
               # Cent_Union + 
               # Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+ Current_Account_Balance +
                  #D_GDP +
                  #log(GDP) +
                 # Inflation +  
                 # Delta_Share_Prices+ Delta_House_Prices + 
                 # Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                  #Cent_Union + 
                  #Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                , data=tidy_data_original_mediterranean, model = "pooling")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+ Current_Account_Balance +
                #D_GDP +
               # log(GDP) +
               # Inflation +  
              #  Delta_Share_Prices+ Delta_House_Prices + 
               # Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                #Cent_Union + 
               # Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
              , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - Bofinger Treeck Behringer Variables", # name table
  
  out="output/results_bofinger_varities_of_capitalism.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS CME", "FE CME", "POLS LME","FE LME", "POLS MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" 
                      # "Current Account Balance",
                       #"Growth Rate of Real GDP", "Inflation", 
                      # "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                      # "Private Credit", "Financial Development Index", "Top 1% x Financial Development Index",
                      # "Union Density",
                     #  "Centralization of Wage Bargaining", "Top 1% x Centralization of Wage Bargaining", "Social Transfers"
                     ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


  # extended models: ####
    # trade unions and social transfers ####

# pooled linear Top 1% Income 
po_linear <- plm(Household_Savings~Top_1Percent_Income + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                   Cent_Union + 
                   Cent_Bargaining + Social_Expenditure
                 , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_linear_robust_se <- vcovHC(po_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear Top 1% Income
fe_linear <- plm(Household_Savings~Top_1Percent_Income  + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                   Cent_Union +
                   Cent_Bargaining + Social_Expenditure
                 , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic Top 1% Income 
po_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                      Cent_Union + 
                      Cent_Bargaining + Social_Expenditure
                    , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_quadratic_robust_se <- vcovHC(po_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic Top 1% Income
fe_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                      Cent_Union + 
                      Cent_Bargaining + Social_Expenditure
                    , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  po_linear, fe_linear, po_quadratic, fe_quadratic, # choose models
  
  se = list(po_linear_robust_se, fe_linear_robust_se, po_quadratic_robust_se, fe_quadratic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger Treeck", # name table
  
  out="output/results_bofinger_treeck.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "FE", "POLS","FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", " Growth Rate of Real GDP", "Inflation", "Growth Rate of Real Share Prices",
                       "Growth Rate of Real House Prices", "Private Credit", "Union Density",
                       "Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # trade unions and social transfers - interaction variables ####

# pooled linear Top 1% Income 
po_linear <- plm(Household_Savings~Top_1Percent_Income + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                   #Cent_Union + Top_1Percent_Income*Cent_Union + 
                   Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining + Social_Expenditure
                 , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_linear_robust_se <- vcovHC(po_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear Top 1% Income
fe_linear <- plm(Household_Savings~Top_1Percent_Income  + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                  # Cent_Union + Top_1Percent_Income*Cent_Union +
                   Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining + Social_Expenditure
                 , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic Top 1% Income 
po_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                     # Cent_Union + Top_1Percent_Income*Cent_Union + 
                      Cent_Bargaining+ Top_1Percent_Income*Cent_Bargaining + Social_Expenditure
                    , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_quadratic_robust_se <- vcovHC(po_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic Top 1% Income
fe_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + D_GDP +Inflation +  Delta_Share_Prices+ Delta_House_Prices + Private_Credit + 
                      #Cent_Union + Top_1Percent_Income*Cent_Union + 
                      Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining+ Social_Expenditure
                    , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  po_linear, fe_linear, po_quadratic, fe_quadratic, # choose models
  
  se = list(po_linear_robust_se, fe_linear_robust_se, po_quadratic_robust_se, fe_quadratic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger Treeck", # name table
  
  out="output/results_bofinger_treeck_interaction.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "FE", "POLS","FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", " Growth Rate of Real GDP", "Inflation", "Growth Rate of Real Share Prices",
                       "Growth Rate of Real House Prices", "Private Credit",   #"Union Density", "Top 1% x Union Density",
                       "Centralization of Wage Bargaining", "Top 1% x Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # trade unions and social transfers - varities of capitalism ####
# pooled ols cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_cme, model = "pooling")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_lme, model = "pooling")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_mediterranean, model = "pooling")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - Bofinger/Treeck", # name table
  
  out="output/results_bofinger_treeck_varities_of_capitalism.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS CME", "FE CME", "POLS LME","FE LME", "POLS MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" ,
                      # "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit",
                       # "Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density","Centralization of Wage Bargaining", 
                     # "Top 1% x Centralization of Wage Bargaining", 
                      "Social Transfers"
                       ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

    # trade unions and social transfers - varities of capitalism - interaction terms ####
# pooled ols cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_cme, model = "pooling")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_lme, model = "pooling")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_mediterranean, model = "pooling")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - Bofinger/Treeck", # name table
  
  out="output/results_bofinger_treeck_varities_of_capitalism_interaction.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS CME", "FE CME", "POLS LME","FE LME", "POLS MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" ,
                       # "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit",
                       # "Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density","Centralization of Wage Bargaining", 
                        "Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"
  ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)




    # financial development and integration  ####
po_linear <- plm(Household_Savings~Top_1Percent_Income + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index  +
                   Cent_Union + 
                   Cent_Bargaining + Social_Expenditure
                 , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_linear_robust_se <- vcovHC(po_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear Top 1% Income
fe_linear <- plm(Household_Savings~Top_1Percent_Income  + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining   + Social_Expenditure
                 , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic Top 1% Income 
po_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + 
                      Cent_Union + 
                      Cent_Bargaining  + Social_Expenditure
                    , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_quadratic_robust_se <- vcovHC(po_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic Top 1% Income
fe_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + 
                      Cent_Union + 
                      Cent_Bargaining  + Social_Expenditure
                    , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  po_linear, fe_linear, po_quadratic, fe_quadratic, # choose models
  
  se = list(po_linear_robust_se, fe_linear_robust_se, po_quadratic_robust_se, fe_quadratic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger Treeck Behringer Alvarez-Cuadrado Rocher Variables", # name table
  
  out="output/results_bofinger_treeck_cuadrado_rocher.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "FE", "POLS","FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", "Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)



    # financial development and integration  - interaction variables  ####
po_linear <- plm(Household_Savings~Top_1Percent_Income + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                 , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_linear_robust_se <- vcovHC(po_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear Top 1% Income
fe_linear <- plm(Household_Savings~Top_1Percent_Income  + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                 , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic Top 1% Income 
po_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                      Cent_Union + 
                      Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                    , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_quadratic_robust_se <- vcovHC(po_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic Top 1% Income
fe_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                      Cent_Union + 
                      Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                    , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  po_linear, fe_linear, po_quadratic, fe_quadratic, # choose models
  
  se = list(po_linear_robust_se, fe_linear_robust_se, po_quadratic_robust_se, fe_quadratic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger Treeck Behringer Alvarez-Cuadrado Rocher Variables", # name table
  
  out="output/results_bofinger_treeck_cuadrado_rocher_interaction.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "FE", "POLS","FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", "Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", "Top 1% x Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)



    # financial development and integration  - varities of capitalism ####

# pooled ols cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index + 
                  #Top_1Percent_Income*Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  + 
                  Social_Expenditure
                 , data=tidy_data_original_cme, model = "pooling")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index + 
                #Top_1Percent_Income*Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  + 
                Social_Expenditure
                 , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                  Current_Account_Balance +
                  D_GDP +
                  #log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit + Financial_Development_Index + 
                  #Top_1Percent_Income*Financial_Development_Index +
                  Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  + 
                  Social_Expenditure
                , data=tidy_data_original_lme, model = "pooling")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                Current_Account_Balance +
                D_GDP +
                #log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit + Financial_Development_Index + 
                #Top_1Percent_Income*Financial_Development_Index +
                Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  + 
                Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + 
                      #Top_1Percent_Income*Financial_Development_Index +
                      Cent_Union + 
                      Cent_Bargaining + 
                      #Top_1Percent_Income*Cent_Bargaining  + 
                      Social_Expenditure
                    , data=tidy_data_original_mediterranean, model = "pooling")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + 
                      #Top_1Percent_Income*Financial_Development_Index +
                      Cent_Union + 
                      Cent_Bargaining + 
                      #Top_1Percent_Income*Cent_Bargaining  + 
                      Social_Expenditure
                    , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - Bofinger Treeck Behringer Alvarez-Cuadrado Rocher Variables", # name table
  
  out="output/results_bofinger_treeck_cuadrado_varities_of_capitalism.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS CME", "FE CME", "POLS LME","FE LME", "POLS MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", "Financial Development Index", 
                       #"Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       #"Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # financial development and integration  - varities of capitalism - interaction terms ####

# pooled ols cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                  Current_Account_Balance +
                  D_GDP +
                  #log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                  Cent_Union + 
                  Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                , data=tidy_data_original_cme, model = "pooling")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                Current_Account_Balance +
                D_GDP +
                #log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                Cent_Union + 
                Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
              , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                  Current_Account_Balance +
                  D_GDP +
                  #log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                  Cent_Union + 
                  Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                , data=tidy_data_original_lme, model = "pooling")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                Current_Account_Balance +
                D_GDP +
                #log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                Cent_Union + 
                Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                  Current_Account_Balance +
                  D_GDP +
                  #log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                  Cent_Union + 
                  Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                , data=tidy_data_original_mediterranean, model = "pooling")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                Current_Account_Balance +
                D_GDP +
                #log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                Cent_Union + 
                Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
              , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - Bofinger Treeck Behringer Alvarez-Cuadrado Rocher Variables", # name table
  
  out="output/results_bofinger_treeck_cuadrado_varities_of_capitalism_interaction.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS CME", "FE CME", "POLS LME","FE LME", "POLS MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", "Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", "Top 1% x Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)






    # fe estimations pre crisis 2007 ####
fe_linear_gini <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        #Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit
                      #Financial_Development_Index + 
                      # Top_1Percent_Income*Financial_Development_Index +
                      # Cent_Union + 
                      # Cent_Bargaining + 
                      #Top_1Percent_Income*Cent_Bargaining  + 
                      #Social_Expenditure
                      , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_gini <- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         #Cent_Union + 
                         #Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         #Social_Expenditure
                         , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top10 <- plm(Household_Savings~Top_1Percent_Income + 
                         Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                         Fiscal_Account_Balance + 
                         #Current_Account_Balance +
                         D_GDP +
                         #log(GDP) +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Private_Credit + 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         Cent_Union + 
                         Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         Social_Expenditure
                       , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_top10_robust_se <- vcovHC(fe_linear_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top10<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit + 
                           #Financial_Development_Index + 
                           # Top_1Percent_Income*Financial_Development_Index +
                           Cent_Union + 
                           Cent_Bargaining + 
                           #Top_1Percent_Income*Cent_Bargaining  + 
                           Social_Expenditure
                         , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_top10_robust_se <- vcovHC(fe_quadratic_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top1 <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit + 
                        Financial_Development_Index + 
                        # Top_1Percent_Income*Financial_Development_Index +
                        Cent_Union + 
                        Cent_Bargaining + 
                        #Top_1Percent_Income*Cent_Bargaining  + 
                        Social_Expenditure
                      , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top1<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                          Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                          Fiscal_Account_Balance + 
                          Current_Account_Balance +
                          D_GDP +
                          #log(GDP) +
                          Inflation +  
                          Delta_Share_Prices+ Delta_House_Prices + 
                          Private_Credit + 
                          Financial_Development_Index + 
                          # Top_1Percent_Income*Financial_Development_Index +
                          Cent_Union + 
                          Cent_Bargaining + 
                          #Top_1Percent_Income*Cent_Bargaining  + 
                          Social_Expenditure
                        , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  fe_linear_gini, fe_quadratic_gini, fe_linear_top10, fe_quadratic_top10, fe_linear_top1, fe_quadratic_top1, # choose models
  
  se = list(fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_top10_robust_se, fe_quadratic_top10_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Household Savings Rate", # name table
  
  out="output/results_fe_estimations_pre_crisis.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", 
                       "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", 
                       "Financial Development Index", 
                       #"Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       #"Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)
#within ols cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_cme, model = "pooling")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# within ols lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_lme, model = "pooling")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# within ols mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+Current_Account_Balance 
                + D_GDP +
                  #  log(GDP) +
                  Inflation +  
                  Delta_Share_Prices+ Delta_House_Prices + 
                  Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                + Cent_Union + 
                  Cent_Bargaining + 
                  #Top_1Percent_Income*Cent_Bargaining  
                  + Social_Expenditure
                , data=tidy_data_original_mediterranean, model = "pooling")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
              #+Current_Account_Balance 
              + D_GDP +
                #  log(GDP) +
                Inflation +  
                Delta_Share_Prices+ Delta_House_Prices + 
                Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              + Cent_Union + 
                Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                + Social_Expenditure
              , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - Bofinger/Treeck", # name table
  
  out="output/results_bofinger_treeck_varities_of_capitalism.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS CME", "FE CME", "POLS LME","FE LME", "POLS MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" ,
                       # "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit",
                       # "Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density","Centralization of Wage Bargaining", 
                       # "Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"
  ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)
    # fe esstimations post crisis 2007 ####
fe_linear_gini <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        #Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit
                      #Financial_Development_Index + 
                      # Top_1Percent_Income*Financial_Development_Index +
                      # Cent_Union + 
                      # Cent_Bargaining + 
                      #Top_1Percent_Income*Cent_Bargaining  + 
                      #Social_Expenditure
                      , data=tidy_data_post_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_gini <- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         #Cent_Union + 
                         #Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         #Social_Expenditure
                         , data=tidy_data_post_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top10 <- plm(Household_Savings~Top_1Percent_Income + 
                         Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                         Fiscal_Account_Balance + 
                         #Current_Account_Balance +
                         D_GDP +
                         #log(GDP) +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Private_Credit + 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         Cent_Union + 
                         Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         Social_Expenditure
                       , data=tidy_data_post_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_top10_robust_se <- vcovHC(fe_linear_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top10<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit + 
                           #Financial_Development_Index + 
                           # Top_1Percent_Income*Financial_Development_Index +
                           Cent_Union + 
                           Cent_Bargaining + 
                           #Top_1Percent_Income*Cent_Bargaining  + 
                           Social_Expenditure
                         , data=tidy_data_post_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_top10_robust_se <- vcovHC(fe_quadratic_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top1 <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit + 
                        Financial_Development_Index + 
                        # Top_1Percent_Income*Financial_Development_Index +
                        Cent_Union + 
                        Cent_Bargaining + 
                        #Top_1Percent_Income*Cent_Bargaining  + 
                        Social_Expenditure
                      , data=tidy_data_post_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top1<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                          Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                          Fiscal_Account_Balance + 
                          Current_Account_Balance +
                          D_GDP +
                          #log(GDP) +
                          Inflation +  
                          Delta_Share_Prices+ Delta_House_Prices + 
                          Private_Credit + 
                          Financial_Development_Index + 
                          # Top_1Percent_Income*Financial_Development_Index +
                          Cent_Union + 
                          Cent_Bargaining + 
                          #Top_1Percent_Income*Cent_Bargaining  + 
                          Social_Expenditure
                        , data=tidy_data_post_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  fe_linear_gini, fe_quadratic_gini, fe_linear_top10, fe_quadratic_top10, fe_linear_top1, fe_quadratic_top1, # choose models
  
  se = list(fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_top10_robust_se, fe_quadratic_top10_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Household Savings Rate", # name table
  
  out="output/results_fe_estimations_post_crisis.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", 
                       "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", 
                       "Financial Development Index", 
                       #"Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       #"Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

    # fe stimations varities of capitalism quadratic linear ####
# fixed effects cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                #+ D_GDP +
                #  log(GDP) +
                #Inflation 
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                # Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_cme, model = "within")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance 
              # + D_GDP +
              #  log(GDP) +
              #Inflation  
              #Delta_Share_Prices+ Delta_House_Prices + 
              # Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              # + Cent_Union + 
              # Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_cme, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                # + D_GDP +
                #  log(GDP) +
                #Inflation  
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                #Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_lme, model = "within")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
              #+Current_Account_Balance 
              #+ D_GDP +
              #  log(GDP) +
              #Inflation   
              #Delta_Share_Prices+ Delta_House_Prices + 
              #Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              #+ Cent_Union + 
              # Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_lme, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+Current_Account_Balance 
                # + D_GDP +
                #  log(GDP) +
                # Inflation  
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                # Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_mediterranean, model = "within")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
              #+Current_Account_Balance 
              #+ D_GDP +
              #  log(GDP) +
              #Inflation 
              #Delta_Share_Prices+ Delta_House_Prices + 
              #Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              #+ Cent_Union + 
              #  Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_mediterranean, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - base line - quadratic linear", # name table
  
  out="output/results_base_line_varities_of_capitalism_quadratic.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE CME", "FE CME", "FE LME","FE LME", "FE MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic",
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" 
                       # "Current Account Balance",
                       #"Growth Rate of Real GDP", "Inflation"
                       #"Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       #"Private Credit",
                       # "Financial Development Index", "Top 1% x Financial Development Index",
                       #"Union Density","Centralization of Wage Bargaining", 
                       # "Top 1% x Centralization of Wage Bargaining", 
                       #"Social Transfers"
  ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)
    # varities of capitalism pre crisis 2007 ####
# fixed effects cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                #+ D_GDP +
                #  log(GDP) +
                #Inflation 
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                # Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_cme_pre_crisis, model = "within")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance 
              # + D_GDP +
              #  log(GDP) +
              #Inflation  
              #Delta_Share_Prices+ Delta_House_Prices + 
              # Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              # + Cent_Union + 
              # Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_cme_pre_crisis, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                # + D_GDP +
                #  log(GDP) +
                #Inflation  
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                #Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_lme_pre_crisis, model = "within")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
              #+Current_Account_Balance 
              #+ D_GDP +
              #  log(GDP) +
              #Inflation   
              #Delta_Share_Prices+ Delta_House_Prices + 
              #Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              #+ Cent_Union + 
              # Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_lme_pre_crisis, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+Current_Account_Balance 
                # + D_GDP +
                #  log(GDP) +
                # Inflation  
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                # Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_mediterranean_pre_crisis, model = "within")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
              #+Current_Account_Balance 
              #+ D_GDP +
              #  log(GDP) +
              #Inflation 
              #Delta_Share_Prices+ Delta_House_Prices + 
              #Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              #+ Cent_Union + 
              #  Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_mediterranean_pre_crisis, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - base line - Pre Crisis", # name table
  
  out="output/results_base_line_varities_of_capitalism_pre_crisis.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE CME", "FE CME", "FE LME","FE LME", "FE MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic",
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" 
                       # "Current Account Balance",
                       #"Growth Rate of Real GDP", "Inflation"
                       #"Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       #"Private Credit",
                       # "Financial Development Index", "Top 1% x Financial Development Index",
                       #"Union Density","Centralization of Wage Bargaining", 
                       # "Top 1% x Centralization of Wage Bargaining", 
                       #"Social Transfers"
  ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

    # varities of capitalism post crisis 2007 ####
# fixed effects cme
pols_cme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                #+ D_GDP +
                #  log(GDP) +
                #Inflation 
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                # Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_cme_post_crisis, model = "within")
pols_cme_robust_se <- vcovHC(pols_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects cme
fe_cme <- plm(Household_Savings~Top_1Percent_Income  + I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              #+Current_Account_Balance 
              # + D_GDP +
              #  log(GDP) +
              #Inflation  
              #Delta_Share_Prices+ Delta_House_Prices + 
              # Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              # + Cent_Union + 
              # Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_cme_post_crisis, model = "within")
fe_cme_robust_se <- vcovHC(fe_cme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
pols_lme <- plm(Household_Savings~Top_1Percent_Income + 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
                #+Current_Account_Balance 
                # + D_GDP +
                #  log(GDP) +
                #Inflation  
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                #Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_lme_post_crisis, model = "within")
pols_lme_robust_se <- vcovHC(pols_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects lme
fe_lme <- plm(Household_Savings~Top_1Percent_Income  + I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance
              #+Current_Account_Balance 
              #+ D_GDP +
              #  log(GDP) +
              #Inflation   
              #Delta_Share_Prices+ Delta_House_Prices + 
              #Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              #+ Cent_Union + 
              # Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_lme_post_crisis, model = "within")
fe_lme_robust_se <- vcovHC(fe_lme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
pols_mme <- plm(Household_Savings~Top_1Percent_Income+ 
                  Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                #+Current_Account_Balance 
                # + D_GDP +
                #  log(GDP) +
                # Inflation  
                #Delta_Share_Prices+ Delta_House_Prices + 
                #Private_Credit 
                # + Financial_Development_Index 
                # + Top_1Percent_Income*Financial_Development_Index 
                #+ Cent_Union + 
                # Cent_Bargaining + 
                #Top_1Percent_Income*Cent_Bargaining  
                #+ Social_Expenditure
                , data=tidy_data_original_mediterranean_post_crisis, model = "within")
pols_mme_robust_se <- vcovHC(pols_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fixed effects mme
fe_mme <- plm(Household_Savings~Top_1Percent_Income+I(Top_1Percent_Income^2)  + 
                Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance  
              #+Current_Account_Balance 
              #+ D_GDP +
              #  log(GDP) +
              #Inflation 
              #Delta_Share_Prices+ Delta_House_Prices + 
              #Private_Credit 
              # + Financial_Development_Index 
              # + Top_1Percent_Income*Financial_Development_Index 
              #+ Cent_Union + 
              #  Cent_Bargaining + 
              #Top_1Percent_Income*Cent_Bargaining  
              #+ Social_Expenditure
              , data=tidy_data_original_mediterranean_post_crisis, model = "within")
fe_mme_robust_se <- vcovHC(fe_mme,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# saving output table in word format
stargazer(
  
  pols_cme, fe_cme, pols_lme, fe_lme, pols_mme, fe_mme, # choose models
  
  se = list(pols_cme_robust_se, fe_cme_robust_se, pols_lme_robust_se, fe_lme_robust_se,pols_mme_robust_se, fe_mme_robust_se ), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Varities of Capitalism - base line - post Crisis", # name table
  
  out="output/results_base_line_varities_of_capitalism_post_crisis.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE CME", "FE CME", "FE LME","FE LME", "FE MME", "FE MME"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic",
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance" 
                       # "Current Account Balance",
                       #"Growth Rate of Real GDP", "Inflation"
                       #"Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       #"Private Credit",
                       # "Financial Development Index", "Top 1% x Financial Development Index",
                       #"Union Density","Centralization of Wage Bargaining", 
                       # "Top 1% x Centralization of Wage Bargaining", 
                       #"Social Transfers"
  ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

# Robustness Checks ####

  # alternative variables and models: ####
    # extended models - trade unions and social transfers - different inequality indicators ####
fe_linear_gini <- plm(Household_Savings~Gini_SWIID9 + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        #Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit + 
                        #Financial_Development_Index + 
                        # Top_1Percent_Income*Financial_Development_Index +
                        Cent_Union + 
                        Cent_Bargaining + 
                        #Top_1Percent_Income*Cent_Bargaining  + 
                        Social_Expenditure
                      , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_gini <- plm(Household_Savings~Gini_SWIID9 +  I(Gini_SWIID9^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit + 
                           #Financial_Development_Index + 
                           # Top_1Percent_Income*Financial_Development_Index +
                           Cent_Union + 
                           Cent_Bargaining + 
                           #Top_1Percent_Income*Cent_Bargaining  + 
                           Social_Expenditure
                         , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top10 <- plm(Household_Savings~Top_10Percent_Income + 
                         Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                         Fiscal_Account_Balance + 
                         #Current_Account_Balance +
                         D_GDP +
                         #log(GDP) +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Private_Credit + 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         Cent_Union + 
                         Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         Social_Expenditure
                       , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_top10_robust_se <- vcovHC(fe_linear_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top10<- plm(Household_Savings~Top_10Percent_Income +  I(Top_10Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit + 
                           #Financial_Development_Index + 
                           # Top_1Percent_Income*Financial_Development_Index +
                           Cent_Union + 
                           Cent_Bargaining + 
                           #Top_1Percent_Income*Cent_Bargaining  + 
                           Social_Expenditure
                         , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_top10_robust_se <- vcovHC(fe_quadratic_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top1 <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        #Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit + 
                        #Financial_Development_Index + 
                        # Top_1Percent_Income*Financial_Development_Index +
                        Cent_Union + 
                        Cent_Bargaining + 
                        #Top_1Percent_Income*Cent_Bargaining  + 
                        Social_Expenditure
                      , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top1<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                          Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                          Fiscal_Account_Balance + 
                          #Current_Account_Balance +
                          D_GDP +
                          #log(GDP) +
                          Inflation +  
                          Delta_Share_Prices+ Delta_House_Prices + 
                          Private_Credit + 
                          #Financial_Development_Index + 
                          # Top_1Percent_Income*Financial_Development_Index +
                          Cent_Union + 
                          Cent_Bargaining + 
                          #Top_1Percent_Income*Cent_Bargaining  + 
                          Social_Expenditure
                        , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  fe_linear_gini, fe_quadratic_gini, fe_linear_top10, fe_quadratic_top10, fe_linear_top1, fe_quadratic_top1, # choose models
  
  se = list(fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_top10_robust_se, fe_quadratic_top10_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Household Savings Rate", # name table
  
  out="output/results_fe_estimations_with_all_three_inequality_indicators.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Gini", "Gini Quadratic", "Top 10%", "Top 10% Quadratic", "Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", 
                       "Fiscal Account Balance", 
                       #"Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", 
                       #"Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       #"Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # extended models - fixed effects models - test with wealth inequality ####
fe_linear_gini <- plm(Household_Savings~Top_1Percent_Wealth + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        #Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit
                      #Financial_Development_Index + 
                      # Top_1Percent_Wealth*Financial_Development_Index +
                      # Cent_Union + 
                      # Cent_Bargaining + 
                      #Top_1Percent_Wealth*Cent_Bargaining  + 
                      #Social_Expenditure
                      , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_gini <- plm(Household_Savings~Top_1Percent_Wealth +  I(Top_1Percent_Wealth^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit 
                         #Financial_Development_Index + 
                         # Top_1Percent_Wealth*Financial_Development_Index +
                         #Cent_Union + 
                         #Cent_Bargaining + 
                         #Top_1Percent_Wealth*Cent_Bargaining  + 
                         #Social_Expenditure
                         , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top10 <- plm(Household_Savings~Top_1Percent_Wealth + 
                         Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                         Fiscal_Account_Balance + 
                         #Current_Account_Balance +
                         D_GDP +
                         #log(GDP) +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Private_Credit + 
                         #Financial_Development_Index + 
                         # Top_1Percent_Wealth*Financial_Development_Index +
                         Cent_Union + 
                         Cent_Bargaining + 
                         #Top_1Percent_Wealth*Cent_Bargaining  + 
                         Social_Expenditure
                       , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_top10_robust_se <- vcovHC(fe_linear_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top10<- plm(Household_Savings~Top_1Percent_Wealth +  I(Top_1Percent_Wealth^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit + 
                           #Financial_Development_Index + 
                           # Top_1Percent_Income*Financial_Development_Index +
                           Cent_Union + 
                           Cent_Bargaining + 
                           #Top_1Percent_Wealth*Cent_Bargaining  + 
                           Social_Expenditure
                         , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_top10_robust_se <- vcovHC(fe_quadratic_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top1 <- plm(Household_Savings~Top_1Percent_Wealth + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit + 
                        Financial_Development_Index + 
                        # Top_1Percent_Income*Financial_Development_Index +
                        Cent_Union + 
                        Cent_Bargaining + 
                        #Top_1Percent_Income*Cent_Bargaining  + 
                        Social_Expenditure
                      , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top1<- plm(Household_Savings~Top_1Percent_Wealth +  I(Top_1Percent_Wealth^2)  + 
                          Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                          Fiscal_Account_Balance + 
                          Current_Account_Balance +
                          D_GDP +
                          #log(GDP) +
                          Inflation +  
                          Delta_Share_Prices+ Delta_House_Prices + 
                          Private_Credit + 
                          Financial_Development_Index + 
                          # Top_1Percent_Income*Financial_Development_Index +
                          Cent_Union + 
                          Cent_Bargaining + 
                          #Top_1Percent_Income*Cent_Bargaining  + 
                          Social_Expenditure
                        , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  fe_linear_gini, fe_quadratic_gini, fe_linear_top10, fe_quadratic_top10, fe_linear_top1, fe_quadratic_top1, # choose models
  
  se = list(fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_top10_robust_se, fe_quadratic_top10_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Household Savings Rate", # name table
  
  out="output/results_fe_estimations_wealth_inequality.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1% Wealth", "Top 1% Wealth Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", 
                       "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", 
                       "Financial Development Index", 
                       #"Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       #"Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # baseline models - top 1% - lagged ####

test <-plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
           , data=tidy_data, model = "pooling")
test_lag<- plm(Household_Savings~Top_1Percent_Income + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) 
               , data=tidy_data, model = "pooling")

summary(test)
summary(test_lag)

# pooled linear baseline gini
po_linear_top1 <- plm(Household_Savings~plm::lag(Top_1Percent_Income) + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) 
                      , data=tidy_data, model = "pooling")
po_linear_top1_robust_se <- vcovHC(po_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic baseline gini
po_quadratic_top1 <- plm(Household_Savings~plm::lag(Top_1Percent_Income) +  plm::lag(I(Top_1Percent_Income^2)) + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) 
                         , data=tidy_data, model = "pooling")
po_quadratic_top1_robust_se <- vcovHC(po_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear baseline gini
fe_linear_top1 <- plm(Household_Savings~plm::lag(Top_1Percent_Income) + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) 
                      , data=tidy_data, model = "within")
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic baseline gini
fe_quadratic_top1 <- plm(Household_Savings~plm::lag(Top_1Percent_Income) +  plm::lag(I(Top_1Percent_Income^2)) + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance)
                         , data=tidy_data, model = "within")
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear extended baseline gini
fe_linear_top1_extended <- plm(Household_Savings~plm::lag(Top_1Percent_Income) + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) 
                               + plm::lag(log(GDP)) +  plm::lag(Inflation) +  plm::lag(Delta_Share_Prices) + plm::lag(Delta_House_Prices) + plm::lag(Private_Credit)
                               , data=tidy_data, model = "within")
fe_linear_top1_extended_robust_se <- vcovHC(fe_linear_top1_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic extened baseline gini
fe_quadratic_top1_extended <- plm(Household_Savings~plm::lag(Top_1Percent_Income) +plm::lag(I(Top_1Percent_Income^2)) + plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance)
                                  + plm::lag(log(GDP)) +  plm::lag(Inflation) +  plm::lag(Delta_Share_Prices) + plm::lag(Delta_House_Prices) + plm::lag(Private_Credit)
                                  , data=tidy_data, model = "within")
fe_quadratic_top1_extended_robust_se <- vcovHC(fe_quadratic_top1_extended,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


# generate output table in word
stargazer(
  
  po_linear_top1, po_quadratic_top1, fe_linear_top1, fe_quadratic_top1, fe_linear_top1_extended,fe_quadratic_top1_extended,  # choose models
  
  se = list(po_linear_top1_robust_se, po_quadratic_top1_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se, fe_linear_top1_extended_robust_se, fe_quadratic_top1_extended_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Lagged Replication of Income Inequality and Aggregated Saving by Bofinger and Scheuermeyer", # name table
  
  out="output/results_bofinger_table3_replication_baseline_top_income_lagged.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "POLS", "FE", "FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", "Age Dependency Ratio", "Growth Rate of Real Disposable Income" , "Real Interest Rates", "Fiscal Account Balance",
                       "ln(GDP)", "Inflation", "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", "Private Credit"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

# 


    # extended models - trade unions and social transfers - lagged ####

# pooled linear Top 1% Income 
po_linear <- plm(Household_Savings~plm::lag(Top_1Percent_Income) + 
                   plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) + 
                   plm::lag(D_GDP) + plm::lag(Inflation) + plm::lag(Delta_Share_Prices) + plm::lag(Delta_House_Prices) + plm::lag(Private_Credit) + 
                   plm::lag(Cent_Union) + plm::lag(Cent_Bargaining) + plm::lag(Social_Expenditure)
                 , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_linear_robust_se <- vcovHC(po_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear Top 1% Income
fe_linear <- plm(Household_Savings~plm::lag(Top_1Percent_Income) + 
                   plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) + 
                   plm::lag(D_GDP) + plm::lag(Inflation) + plm::lag(Delta_Share_Prices) + plm::lag(Delta_House_Prices) + plm::lag(Private_Credit) + 
                   plm::lag(Cent_Union) + plm::lag(Cent_Bargaining) + plm::lag(Social_Expenditure)
                 , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic Top 1% Income 
po_quadratic <- plm(Household_Savings~ plm::lag(Top_1Percent_Income) + plm::lag(I(Top_1Percent_Income^2)) +
                      plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) + 
                      plm::lag(D_GDP) + plm::lag(Inflation) + plm::lag(Delta_Share_Prices) + plm::lag(Delta_House_Prices) + plm::lag(Private_Credit) + 
                      plm::lag(Cent_Union) + plm::lag(Cent_Bargaining) + plm::lag(Social_Expenditure)
                    , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_quadratic_robust_se <- vcovHC(po_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic Top 1% Income
fe_quadratic <- plm(Household_Savings~plm::lag(Top_1Percent_Income) + plm::lag(I(Top_1Percent_Income^2)) +
                      plm::lag(Dependency_Ratio) + plm::lag(Delta_Disposable_Income) + plm::lag(Interest_Rate) + plm::lag(Fiscal_Account_Balance) + 
                      plm::lag(D_GDP) + plm::lag(Inflation) + plm::lag(Delta_Share_Prices) + plm::lag(Delta_House_Prices) + plm::lag(Private_Credit) + 
                      plm::lag(Cent_Union) + plm::lag(Cent_Bargaining) + plm::lag(Social_Expenditure)
                    , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  po_linear, fe_linear, po_quadratic, fe_quadratic, # choose models
  
  se = list(po_linear_robust_se, fe_linear_robust_se, po_quadratic_robust_se, fe_quadratic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger Treeck", # name table
  
  out="output/results_bofinger_treeck_lagged.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "FE", "POLS","FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", " Growth Rate of Real GDP", "Inflation", "Growth Rate of Real Share Prices",
                       "Growth Rate of Real House Prices", "Private Credit", "Union Density","Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)



    # extended models - financial development and integration  - lagged  ####
po_linear <- plm(Household_Savings~Top_1Percent_Income + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                 , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_linear_robust_se <- vcovHC(po_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe linear Top 1% Income
fe_linear <- plm(Household_Savings~Top_1Percent_Income  + 
                   Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                   Current_Account_Balance +
                   D_GDP +
                   #log(GDP) +
                   Inflation +  
                   Delta_Share_Prices+ Delta_House_Prices + 
                   Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                   Cent_Union + 
                   Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                 , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled quadratic Top 1% Income 
po_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance +
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                      Cent_Union + 
                      Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                    , data=tidy_data, model = "pooling")
# saving robust standard errors for output table
po_quadratic_robust_se <- vcovHC(po_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# fe quadratic Top 1% Income
fe_quadratic <- plm(Household_Savings~Top_1Percent_Income+ I(Top_1Percent_Income^2)  + 
                      Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                      Current_Account_Balance +
                      D_GDP +
                      #log(GDP) +
                      Inflation +  
                      Delta_Share_Prices+ Delta_House_Prices + 
                      Private_Credit + Financial_Development_Index + Top_1Percent_Income*Financial_Development_Index +
                      Cent_Union + 
                      Cent_Bargaining + Top_1Percent_Income*Cent_Bargaining  + Social_Expenditure
                    , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  po_linear, fe_linear, po_quadratic, fe_quadratic, # choose models
  
  se = list(po_linear_robust_se, fe_linear_robust_se, po_quadratic_robust_se, fe_quadratic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Bofinger Treeck Behringer Alvarez-Cuadrado Rocher Variables", # name table
  
  out="output/results_bofinger_treeck_cuadrado_lagged.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("POLS", "FE", "POLS","FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", "Financial Development Index", "Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", "Top 1% x Centralization of Wage Bargaining", "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)


    # base line models - comparison of different financial market indicators ####
  
 
fe_linear_gini <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        Current_Account_Balance +
                        D_GDP +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Cent_Union + 
                        Cent_Bargaining + 
                        Social_Expenditure +
                        Private_Credit
                      , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_gini <- plm(Household_Savings~Top_1Percent_Income +
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           Current_Account_Balance +
                           D_GDP +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Cent_Union + 
                           Cent_Bargaining + 
                           Social_Expenditure +
                           Financial_Development_Index
                         , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top10 <- plm(Household_Savings~Top_1Percent_Income + 
                         Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                         Fiscal_Account_Balance + 
                         Current_Account_Balance +
                         D_GDP +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Cent_Union + 
                         Cent_Bargaining + 
                         Social_Expenditure +
                         Finreform
                       , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_linear_top10_robust_se <- vcovHC(fe_linear_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top10<- plm(Household_Savings~Top_1Percent_Income +
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           Current_Account_Balance +
                           D_GDP +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Cent_Union + 
                           Cent_Bargaining + 
                           Social_Expenditure +
                           Market_Cap 
                         , data=tidy_data, model = "within")
# saving robust standard errors for output table
fe_quadratic_top10_robust_se <- vcovHC(fe_quadratic_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()


stargazer(
  
  fe_linear_gini, fe_quadratic_gini, fe_linear_top10, fe_quadratic_top10, # choose models
  
  se = list(fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_top10_robust_se, fe_quadratic_top10_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Comaparison of Financial Development Indicators", # name table
  
  out="output/results_fe_estimations_financial_indicators.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE"), # name columns with model types
  
  covariate.labels = c("Top 1% Income", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", 
                       "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       "Social Transfers",
                       "Private Credit",
                       "Financial Market Index",
                       "Financial Reform Database",
                       "Market Capitalization"
                       ), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)
    # estimations for 1980-2007
fe_linear_gini <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        #Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit
                      #Financial_Development_Index + 
                      # Top_1Percent_Income*Financial_Development_Index +
                      # Cent_Union + 
                      # Cent_Bargaining + 
                      #Top_1Percent_Income*Cent_Bargaining  + 
                      #Social_Expenditure
                      , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_gini_robust_se <- vcovHC(fe_linear_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_gini <- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         #Cent_Union + 
                         #Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         #Social_Expenditure
                         , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_gini_robust_se <- vcovHC(fe_quadratic_gini,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top10 <- plm(Household_Savings~Top_1Percent_Income + 
                         Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                         Fiscal_Account_Balance + 
                         #Current_Account_Balance +
                         D_GDP +
                         #log(GDP) +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Private_Credit + 
                         #Financial_Development_Index + 
                         # Top_1Percent_Income*Financial_Development_Index +
                         Cent_Union + 
                         Cent_Bargaining + 
                         #Top_1Percent_Income*Cent_Bargaining  + 
                         Social_Expenditure
                       , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_top10_robust_se <- vcovHC(fe_linear_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top10<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                           Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                           Fiscal_Account_Balance + 
                           #Current_Account_Balance +
                           D_GDP +
                           #log(GDP) +
                           Inflation +  
                           Delta_Share_Prices+ Delta_House_Prices + 
                           Private_Credit + 
                           #Financial_Development_Index + 
                           # Top_1Percent_Income*Financial_Development_Index +
                           Cent_Union + 
                           Cent_Bargaining + 
                           #Top_1Percent_Income*Cent_Bargaining  + 
                           Social_Expenditure
                         , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_top10_robust_se <- vcovHC(fe_quadratic_top10,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_linear_top1 <- plm(Household_Savings~Top_1Percent_Income + 
                        Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                        Fiscal_Account_Balance + 
                        Current_Account_Balance +
                        D_GDP +
                        #log(GDP) +
                        Inflation +  
                        Delta_Share_Prices+ Delta_House_Prices + 
                        Private_Credit + 
                        Financial_Development_Index + 
                        # Top_1Percent_Income*Financial_Development_Index +
                        Cent_Union + 
                        Cent_Bargaining + 
                        #Top_1Percent_Income*Cent_Bargaining  + 
                        Social_Expenditure
                      , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_linear_top1_robust_se <- vcovHC(fe_linear_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

fe_quadratic_top1<- plm(Household_Savings~Top_1Percent_Income +  I(Top_1Percent_Income^2)  + 
                          Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + 
                          Fiscal_Account_Balance + 
                          Current_Account_Balance +
                          D_GDP +
                          #log(GDP) +
                          Inflation +  
                          Delta_Share_Prices+ Delta_House_Prices + 
                          Private_Credit + 
                          Financial_Development_Index + 
                          # Top_1Percent_Income*Financial_Development_Index +
                          Cent_Union + 
                          Cent_Bargaining + 
                          #Top_1Percent_Income*Cent_Bargaining  + 
                          Social_Expenditure
                        , data=tidy_data_pre_crisis, model = "within")
# saving robust standard errors for output table
fe_quadratic_top1_robust_se <- vcovHC(fe_quadratic_top1,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

stargazer(
  
  fe_linear_gini, fe_quadratic_gini, fe_linear_top10, fe_quadratic_top10, fe_linear_top1, fe_quadratic_top1, # choose models
  
  se = list(fe_linear_gini_robust_se, fe_quadratic_gini_robust_se, fe_linear_top10_robust_se, fe_quadratic_top10_robust_se, fe_linear_top1_robust_se, fe_quadratic_top1_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Household Savings Rate", # name table
  
  out="output/results_fe_estimations_pre_crisis.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", 
                       "Old Age Dependency Ratio", "Growth Rate of Real Disposable Income",
                       "Real Interest Rates", 
                       "Fiscal Account Balance", 
                       "Current Account Balance",
                       "Growth Rate of Real GDP", "Inflation", 
                       "Growth Rate of Real Share Prices", "Growth Rate of Real House Prices", 
                       "Private Credit", 
                       "Financial Development Index", 
                       #"Top 1% x Financial Development Index",
                       "Union Density",
                       "Centralization of Wage Bargaining", 
                       #"Top 1% x Centralization of Wage Bargaining", 
                       "Social Transfers"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.10**p<0.05***p<0.01", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

    # estimations for 2007-2019
    # extended models - growth rate of top 1% income share 
  # gauss-markov-/statistical assumption tests for panel and time series data and methods: ####
    # tests for deterministic/stochastic trends, weak dependency, stationary process ####

# Household savings 

  # prepare and isolate data for tests
  Data_without_NA<-tidy_data[-which(is.na(tidy_data$Household_Savings)),]
  Data_without_NA<-Data_without_NA[, c("idcode","Year","Household_Savings")]
  Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)

  # Levin-Lin-Chu Unit-Root Test
  #HO: there is unit root in variable or variable is non stationary
  #H1: there is no unit root and variable is stationary 
  purtest(Data_without_NA, pmax=2, exo = "intercept", test = "levinlin")
  # stationarity can be confirmed for Household Savings

  # Augmented Dickey-Fuller Test
  #H0: no stationary process
  #H1: stationary process
  #k defines number of lags tested
  adf.test(Data_without_NA$Household_Savings, k = 6)
  # stationarity can be confirmed

# Gini
  
  # prepare and isolate data for tests
  Data_without_NA<-tidy_data[-which(is.na(tidy_data$Gini_SWIID9)),]
  Data_without_NA<-Data_without_NA[, c("idcode","Year","Gini_SWIID9")]
  Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)


  # Levin-Lin-Chu Unit-Root Test for Gini
  #HO: there is unit root in variable or variable is non stationary
  #H1: there is no unit root and variable is stationary 
  purtest(Data_without_NA, pmax=2, exo = "intercept", test = "levinlin")
  # stationarity can be confirmed for Gini

  # Augmented Dickey-Fuller Test for Gini
  #H0: no stationary process
  #H1: stationary process
  #k defines number of lags tested
  adf.test(Data_without_NA$Gini_SWIID9, k = 6)
  # stationarity can be confirmed

# Top 1%
  
  # prepare and isolate data for tests
  # tidy_data was already cleaned for NAs for Top Income Variables
  Data_without_NA<-tidy_data[, c("idcode","Year","Top_1Percent_Income")]
  Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)
  
  # Levin-Lin-Chu Unit-Root Test for Top 1 %
  #HO: there is unit root in variable or variable is non stationary
  #H1: there is no unit root and variable is stationary 
  purtest(Data_without_NA, pmax=2, exo = "intercept", test = "levinlin")
  # stationarity can not be confirmed

  # Augmented Dickey-Fuller Test for Top 1 %
  #H0: no stationary
  #H1: stationary
  #k defines number of lags tested
  adf.test(Data_without_NA$Top_1Percent, k = 6)
  # stationarity can not be confirmed

# Growth Rate for Top 1%
  
  # prepare and isolate data for tests
  Data_without_NA<-tidy_data[-which(is.na(tidy_data$D_Top_1Percent_Income)),]
  Data_without_NA<-Data_without_NA[, c("idcode","Year","D_Top_1Percent_Income")]
  Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)
  
  # Levin-Lin-Chu Unit-Root Test for Top 1 %
  #HO: there is unit root in variable or variable is non stationary
  #H1: there is no unit root and variable is stationary 
  purtest(Data_without_NA, pmax=2, exo = "intercept", test = "levinlin")
  # stationarity be confirmed
  
  # Augmented Dickey-Fuller Test for Top 1 %
  #H0: no stationary
  #H1: stationary
  #k defines number of lags tested
  adf.test(Data_without_NA$D_Top_1Percent_Income, k = 6)
  # stationarity be confirmed
  
# Top 10%
  
  # prepare and isolate data for tests
  # tidy_data was already cleaned for NAs for Top Income Variables
  Data_without_NA<-tidy_data[, c("idcode","Year","Top_10Percent_Income")]
  Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)

  # Levin-Lin-Chu Unit-Root Test for Top 10 %
  # test requires a dataset where the tested variable is stored in different lists for each group
  #HO: there is unit root in variable or variable is non stationary
  #H1: there is no unit root and variable is stationary 
  purtest(Data_without_NA, pmax=2, exo = "intercept", test = "levinlin")
  # stationarity can be confirmed

  # Augmented Dickey-Fuller Test for Top 10 %
  # test requires dataset without NA's
  #H0: no stationary process
  #H1: stationary process
  #k defines number of lags tested
  adf.test(Data_without_NA$Top_10Percent, k = 6)
  # stationarity can be confirmed

  
    # tests for serial correlation / autocorrelation ####

#estimate model for test input
fe_linear <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                        , data=tidy_data, model = "within")

  #Durbin Watson test
  #H0: There is no autocorrelation / serial correlation in error term.
  #H1: There is autocorrelation / serial correlation in error term.
  pdwtest(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
          , data=tidy_data, model = "within")

  #wooldridge test 
  #H0: There is no autocorrelation / serial correlation in error term.
  #H1: There is autocorrelation / serial correlation in error term.
  pbgtest(fe_linear)
  
  # both tests confirm autocorrelation
  # robust standard errors are important
  
    # tests for homoskedasticity / heteroskedasticity ####
  
  #Breusch-Pagan Test
  #HO: There is homoskedasticity
  #H1: There is heteroskedasticity
  bptest(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
         , data=tidy_data, studentize = F)
  
  # test confirms heteroskedasticity
  
  # robust standard errors are necessary
  
  #Robust Standard Erros for heteroskedasticity and autocorrelation
  #if there is heteroscedasticity
  coeftest(fe_linear, vcovHC)
  #if there is heteroskedascticity and autocorrelation
  coeftest(fe_linear, vcovHC(fe_linear, method = "arellano"))
  # or use cluster robust standard errors
  
    # tests for multicollinearity and cross sectional dependence ####
  
  # variance inflation factor function needs pooled ols model as input for panel data test
  pooled_linear <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                       , data=tidy_data, model = "pooling")
  vif(pooled_linear)
  
  # low variance inflation factor (no factor over 2, no multicollinearity induced problems expected
  
  pooled_linear <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + 
                         Current_Account_Balance +
                         D_GDP +
                         #log(GDP) +
                         Inflation +  
                         Delta_Share_Prices+ Delta_House_Prices + 
                         Private_Credit + Financial_Development_Index  +
                         Cent_Union + 
                         Cent_Bargaining + Social_Expenditure
                       , data=tidy_data, model = "pooling")
  vif(pooled_linear)

  # dependency ratio and social expenditure over 2.5 but below 5 -> some research indicated multicollinearity induced problems for this model, while some research indicated no problem for values under value 5 or 10 
  
  #Tests for cross sectional dependence in Panel data
  # problem for macro panels with long time series
  
  #Breusch-Pagan LM test for cross sectional dependence
  #H0: there is no cross sectional dependence
  #H1: there is cross sectional dependence
  
  pcdtest(fe_linear, test = c("lm"))
  
  #Pesaran CD test for cross sectional dependence
  #H0: there is no cross sectional dependence
  #H1: there is cross sectional dependence
  
  # cross sectional dependence is confirmed by both tests
  
  pcdtest(fe_linear, test = c("cd"))
  
  # cluster dependent robust standard errors are needed
  # test model with cross sectional dependence robust standard errors
  
  # PCSE if N>T
  summary(fe_linear, vcov = function(x) vcovBK(x, type="HC1", cluster = c("group")))
  
  # FGLS if T>N
  test<-pggls(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
              , data=tidy_data, model = "within")
  summary(test)
  
  # explanatory coefficient is still statistically significant
  # cluster robust standard errors are used for output tables
    # model comparison tests, random effects, time fixed effects, individual fixed effects, etc. ####
  
  # the following estimations are needed as inputs 
  fe_linear <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                   , data=tidy_data, model = "within")
  re_linear <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                   , data=tidy_data, model = "random")
  pooled_linear <- plm(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
                       , data=tidy_data, model = "pooling")        
  #Hausman test
  #H0: random effect model is consistent
  #H1: fixed effect model is consistent
  phtest(fe_linear,re_linear)   
  # fixed model is consistent
  
  #pooled ols test
  #HO: the same coefficients apply to each individual (pooled ols is stable)
  #h1: the same coefficients do not apply to each individual (pooled ols is unstable)
  pooltest(Household_Savings~Top_1Percent_Income, data = tidy_data,  model = "within")
  # pooled ols is unstable
  
  #test for individual effects and time effects
  #H0: no significant individual effects and time effects
  #H1: significant individual effects and time effects
  plmtest(Household_Savings~Top_1Percent_Income, data=tidy_data, effect = "twoways", type="ghm")
  # significant individual and time effects have to be assumed
  
  #test for consistency for pooled ols and fixed effects models
  #H0: pooled ols is consistent
  #H1: fixed effect model is consistent
  pFtest(fe_linear, pooled_linear)
  # fixed effects model is consistent
  
  #test for individual effects and time effects
  #H0: no significant individual effects and time effects
  #H1: significant individual effects and time effects
  plmtest(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
          , data=tidy_data,
          effect = "twoways", type="ghm")
  # significant individual and time effects have to be assumed
  
  #test for individual effects
  #H0: no significant individual effects
  #H1: significant individual effects
  plmtest(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
          , data=tidy_data,
          effect = "individual", type="honda")
  # significant individual effects have to be assumed
  
  #test for time effects
  #H0: no significant time effects
  #H1: significant time effects
  plmtest(Household_Savings~Top_1Percent_Income + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance 
          , data=tidy_data,
          effect = "time", type="honda")
  # no significant time effects can be assumed
  
# all test results point towards group fixed effects models, assumptions for fixed effects method can be confirmed
  
#tukey anscombe plot
  ggplot(
    data.frame(
      GefitteteWerte= fitted.values(fe_linear),
      Residuen= residuals(fe_linear)
    ),
    aes(x=GefitteteWerte, y=Residuen)
  ) +
    ggtitle("Tukey-Anscombe-Plot") +
    geom_hline(yintercept = 0) + 
    geom_point()
  
# qqplot
  ggplot(
    data.frame(
      GefitteteWerte= fitted.values(fe_linear),
      Residuen= residuals(fe_linear)
    ),
    aes(sample=Residuen)
  ) +
    stat_qq() + stat_qq_line()+
    labs(title="Q-Q-Plot", x = "Theoretical Quantiles", y="Residuals")
  
  ggplot(fe_linear, aes(sample=residuals)) +
    stat_qq() + stat_qq_line()
  # code end - check output folder for results ####






