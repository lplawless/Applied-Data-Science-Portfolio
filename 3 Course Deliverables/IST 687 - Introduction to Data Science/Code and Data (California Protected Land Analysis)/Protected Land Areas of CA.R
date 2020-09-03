#Index - Double click on the bracketed string and press Ctrl+F and then Enter to jump to that section.
# Load and prep [i001]
# Data Munging [i002]
# Descriptive Analysis [i003]
# Descriptive Charts [i003a]
# Descriptive Maps [i003b]
# Advanced Analysis [i004]

#--------------------------------------Load and prep [i001]--------------------------------------------------------------

#Activate libraries.
require(arulesViz)
require(caret)
require(crayon)
require(dplyr)
require(e1071)
require(GGally)
require(ggcorrplot)
require(ggiraph)
require(ggiraphExtra)
require(ggplot2)
require(ggpubr)
require(gridExtra)
require(kernlab)
require(mapproj)
require(maps)
require(mice)
require(readxl)
require(reshape2)
require(VIM)

#Load the Protected Land Data & Voter Data .csv file.
# ProtectedLand <- read.csv("~/Protected Land.csv")
# Voting <- read.csv("~/Voting.csv")
# Party <- read.csv("~/Party.csv")
# Centers <- read.csv("~/Centers.csv")

ProtectedLand <- read.csv(file.choose())
Voting <- read.csv(file.choose())
Party <- read.csv(file.choose())
Centers <- read.csv(file.choose())
mappingData <- read_excel(file.choose())

#--------------------------------------Data Munging [i002]--------------------------------------------------------------

#Clean Data - Rename columns to make them more understandable.
names(ProtectedLand)[2] <- "year"
names(ProtectedLand)[3] <- "county"
names(ProtectedLand)[4] <- "tract_id"
names(ProtectedLand)[5] <- "census_tract_population"
names(ProtectedLand)[6] <- "average_income"
names(ProtectedLand)[7] <- "median_housing_price"
names(ProtectedLand)[8] <- "county_number"
names(ProtectedLand)[10] <- "distance_to_tract"
names(ProtectedLand)[11] <- "county_population"
names(ProtectedLand)[12] <- "population_density_per_square_mile"
names(ProtectedLand)[18] <- "share_native_american"
names(ProtectedLand)[20] <- "share_pacific_islander"
names(Party)[1] <- "county"
names(Voting)[5] <- "voting_age_pop"

#Clean Data - Remove " County" from the county field. It's redundant and takes up space on visualizations.
#     gsub() replaces the 1st arguement with the 2nd arguement while looking through the 3rd arguement.

ProtectedLand$county <- gsub(" County", "", ProtectedLand$county)

#Merge Party Data
ProtectedLand <- merge(ProtectedLand, Voting, by = "gisjoin")
ProtectedLand <- merge(ProtectedLand, Party, by.x = "county", by.y = "county")
ProtectedLand <- merge(ProtectedLand, Centers, by = "gisjoin")

# ####Using R Mice Package to impute missing values [average_income column]####
# 
# ###Step 1: Confirm the existence of 'NA's within a column
# 
# any(is.na(ProtectedLand$average_income)) ###Returns 'TRUE'  Specifically, rows 1918, 2234, 2248, 2819, 4103 and 4213 are 'NA's.
# 
# ###Step 2: Create a data.frame of at least two columns to work with
# 
# miceAverage_Income <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$average_income)
# 
# #####Step 3: Probably redundant of Step 1 above, but interesting nonetheless.  This function creates a 'pattern' graphic
# ######that illustrates the number of rows in our dummy df with 0 missing values and the rows with 1 missing values.
# 
# md.pattern(miceAverage_Income)
# 
# #####Step 4:  The R mice function develops potential imputed values for 'NA's using various types of regression.
# imputed_miceAverage_Income <- mice(miceAverage_Income, m=5, method = 'pmm', seed = 101)
# 
# ######Step 5:  We use the R Mice's 'compete' function to choose the results regression method (here, method '3')
# ######to be imputed in place of the 'NA's
# 
# miceAverage_Income <- complete(imputed_miceAverage_Income,3)
# 
# ###### Step 6: We replace the old average_income column values with the column that contains the six imputed values to replace 'NA's
# ProtectedLand$average_income <- miceAverage_Income$ProtectedLand.average_income
# 
# #####Step 7: We confirm the absence of 'NA's within the replacement column
# 
# any(is.na(ProtectedLand$average_income))
# ProtectedLand$average_income [1918] ##### For example, we see that the 'NA' formerly in row 1918 has been replaced with###
# ### with '31418'
# #Clearing unneeded object
# rm(imputed_miceAverage_Income)
# rm(miceAverage_Income)
# 
# ######Using R Mice Package to impute missing values [median_housing_price]
# 
# any(is.na(ProtectedLand$median_housing_price))
# 
# miceMedianHousingPrice <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$median_housing_price)
# md.pattern(miceMedianHousingPrice)
# imputed_miceMedianHousingPrice <- mice(miceMedianHousingPrice, m=5, method = 'pmm', seed = 101)
# miceMedianHousingPrice <- complete(imputed_miceMedianHousingPrice,3)
# ProtectedLand$median_housing_price <- miceMedianHousingPrice$ProtectedLand.median_housing_price
# 
# any(is.na(ProtectedLand$median_housing_price))
# 
# #Clearing unneeded object
# rm(imputed_miceMedianHousingPrice)
# rm(miceMedianHousingPrice)
# 
# ######Using R Mice Package to impute missing values [share_white]
# 
# any(is.na(ProtectedLand$share_white))
# 
# miceshare_white <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_white)
# md.pattern(miceshare_white)
# imputed_miceshare_white <- mice(miceshare_white, m=5, method = 'pmm', seed = 101)
# miceshare_white <- complete(imputed_miceshare_white,3)
# ProtectedLand$share_white <- miceshare_white$ProtectedLand.share_white
# 
# any(is.na(ProtectedLand$share_white))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_white)
# rm(miceshare_white)
# 
# ######Using R Mice Package to impute missing values [share_black]
# 
# any(is.na(ProtectedLand$share_black))
# 
# miceshare_black <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_black)
# md.pattern(miceshare_black)
# imputed_miceshare_black <- mice(miceshare_black, m=5, method = 'pmm', seed = 101)
# miceshare_black <- complete(imputed_miceshare_black,3)
# ProtectedLand$share_black <- miceshare_black$ProtectedLand.share_black
# 
# any(is.na(ProtectedLand$share_black))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_black)
# rm(miceshare_black)
# 
# ######Using R Mice Package to impute missing values [share_native_american]
# 
# any(is.na(ProtectedLand$share_native_american))
# 
# miceshare_native_american <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_native_american)
# md.pattern(miceshare_native_american)
# imputed_miceshare_native_american <- mice(miceshare_native_american, m=5, method = 'pmm', seed = 101)
# miceshare_native_american <- complete(imputed_miceshare_native_american,3)
# ProtectedLand$share_native_american <- miceshare_native_american$ProtectedLand.share_native_american
# 
# any(is.na(ProtectedLand$share_native_american))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_native_american)
# rm(miceshare_native_american)
# 
# ######Using R Mice Package to impute missing values [share_asian]
# 
# any(is.na(ProtectedLand$share_asian))
# 
# miceshare_asian <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_asian)
# md.pattern(miceshare_asian)
# imputed_miceshare_asian <- mice(miceshare_asian, m=5, method = 'pmm', seed = 101)
# miceshare_asian <- complete(imputed_miceshare_asian,3)
# ProtectedLand$share_asian <- miceshare_asian$ProtectedLand.share_asian
# 
# any(is.na(ProtectedLand$share_asian))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_asian)
# rm(miceshare_asian)
# 
# ######Using R Mice Package to impute missing values [share_pacific_islander]
# 
# any(is.na(ProtectedLand$share_pacific_islander))
# 
# miceshare_pacific_islander <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_pacific_islander)
# md.pattern(miceshare_pacific_islander)
# imputed_miceshare_pacific_islander <- mice(miceshare_pacific_islander, m=5, method = 'pmm', seed = 101)
# miceshare_pacific_islander <- complete(imputed_miceshare_pacific_islander,3)
# ProtectedLand$share_pacific_islander <- miceshare_pacific_islander$ProtectedLand.share_pacific_islander
# 
# any(is.na(ProtectedLand$share_pacific_islander))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_pacific_islander)
# rm(miceshare_pacific_islander)
# 
# ######Using R Mice Package to impute missing values [share_other]
# 
# any(is.na(ProtectedLand$share_other))
# 
# miceshare_other <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_other)
# md.pattern(miceshare_other)
# imputed_miceshare_other <- mice(miceshare_other, m=5, method = 'pmm', seed = 101)
# miceshare_other <- complete(imputed_miceshare_other,3)
# ProtectedLand$share_other <- miceshare_other$ProtectedLand.share_other
# 
# any(is.na(ProtectedLand$share_other))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_other)
# rm(miceshare_other)
# 
# ######Using R Mice Package to impute missing values [share_2plus]
# 
# any(is.na(ProtectedLand$share_2plus))
# 
# miceshare_2plus <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_2plus)
# md.pattern(miceshare_2plus)
# imputed_miceshare_2plus <- mice(miceshare_2plus, m=5, method = 'pmm', seed = 101)
# miceshare_2plus <- complete(imputed_miceshare_2plus,3)
# ProtectedLand$share_2plus <- miceshare_2plus$ProtectedLand.share_2plus
# 
# any(is.na(ProtectedLand$share_2plus))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_2plus)
# rm(miceshare_2plus)
# 
# ######Using R Mice Package to impute missing values [share_hispanic]
# 
# any(is.na(ProtectedLand$share_hispanic))
# 
# miceshare_hispanic <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$share_hispanic)
# md.pattern(miceshare_hispanic)
# imputed_miceshare_hispanic <- mice(miceshare_hispanic, m=5, method = 'pmm', seed = 101)
# miceshare_hispanic <- complete(imputed_miceshare_hispanic,3)
# ProtectedLand$share_hispanic <- miceshare_hispanic$ProtectedLand.share_hispanic
# 
# any(is.na(ProtectedLand$share_hispanic))
# 
# #Clearing unneeded object
# rm(imputed_miceshare_hispanic)
# rm(miceshare_hispanic)
# 
# ######Using R Mice Package to impute missing values [lessthanHS]
# 
# any(is.na(ProtectedLand$lessthanHS))
# 
# micelessthanHS <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$lessthanHS)
# md.pattern(micelessthanHS)
# imputed_micelessthanHS <- mice(micelessthanHS, m=5, method = 'pmm', seed = 101)
# micelessthanHS <- complete(imputed_micelessthanHS,3)
# ProtectedLand$lessthanHS <- micelessthanHS$ProtectedLand.lessthanHS
# 
# any(is.na(ProtectedLand$lessthanHS))
# 
# #Clearing unneeded object
# rm(imputed_micelessthanHS)
# rm(micelessthanHS)
# 
# ######Using R Mice Package to impute missing values [HSdiploma]
# 
# any(is.na(ProtectedLand$HSdiploma))
# 
# miceHSdiploma <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$HSdiploma)
# md.pattern(miceHSdiploma)
# imputed_miceHSdiploma <- mice(miceHSdiploma, m=5, method = 'pmm', seed = 101)
# miceHSdiploma <- complete(imputed_miceHSdiploma,3)
# ProtectedLand$HSdiploma <- miceHSdiploma$ProtectedLand.HSdiploma
# 
# any(is.na(ProtectedLand$HSdiploma))
# 
# #Clearing unneeded object
# rm(imputed_miceHSdiploma)
# rm(miceHSdiploma)
# 
# ######Using R Mice Package to impute missing values [someCollege]
# 
# any(is.na(ProtectedLand$someCollege))
# 
# micesomeCollege <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$someCollege)
# md.pattern(micesomeCollege)
# imputed_someCollege <- mice(micesomeCollege, m=5, method = 'pmm', seed = 101)
# micesomeCollege <- complete(imputed_someCollege,3)
# ProtectedLand$someCollege <- micesomeCollege$ProtectedLand.someCollege
# 
# any(is.na(ProtectedLand$someCollege))
# 
# #Clearing unneeded object
# rm(imputed_someCollege)
# rm(micesomeCollege)
# 
# ######Using R Mice Package to impute missing values [Associates]
# 
# any(is.na(ProtectedLand$Associates))
# 
# miceAssociates <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Associates)
# md.pattern(miceAssociates)
# imputed_Associates <- mice(miceAssociates, m=5, method = 'pmm', seed = 101)
# miceAssociates <- complete(imputed_Associates,3)
# ProtectedLand$Associates <- miceAssociates$ProtectedLand.Associates
# 
# any(is.na(ProtectedLand$Associates))
# 
# #Clearing unneeded object
# rm(imputed_Associates)
# rm(miceAssociates)
# 
# ######Using R Mice Package to impute missing values [Bachelors]
# 
# any(is.na(ProtectedLand$Bachelors))
# 
# miceBachelors <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Bachelors)
# md.pattern(miceBachelors)
# imputed_Bachelors <- mice(miceBachelors, m=5, method = 'pmm', seed = 101)
# miceBachelors <- complete(imputed_Bachelors,3)
# ProtectedLand$Bachelors <- miceBachelors$ProtectedLand.Bachelors
# 
# any(is.na(ProtectedLand$Bachelors))
# 
# #Clearing unneeded object
# rm(imputed_Bachelors)
# rm(miceBachelors)
# 
# ######Using R Mice Package to impute missing values [Masters]
# 
# any(is.na(ProtectedLand$Masters))
# 
# miceMasters <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Masters)
# md.pattern(miceMasters)
# imputed_Masters <- mice(miceMasters, m=5, method = 'pmm', seed = 101)
# miceMasters <- complete(imputed_Masters,3)
# ProtectedLand$Masters <- miceMasters$ProtectedLand.Masters
# 
# any(is.na(ProtectedLand$Masters))
# 
# #Clearing unneeded object
# rm(imputed_Masters)
# rm(miceMasters)
# 
# ######Using R Mice Package to impute missing values [Professional]
# 
# any(is.na(ProtectedLand$Professional))
# 
# miceProfessional <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Professional)
# md.pattern(miceProfessional)
# imputed_Professional <- mice(miceProfessional, m=5, method = 'pmm', seed = 101)
# miceProfessional <- complete(imputed_Professional,3)
# ProtectedLand$Professional <- miceProfessional$ProtectedLand.Professional
# 
# any(is.na(ProtectedLand$Professional))
# 
# #Clearing unneeded object
# rm(imputed_Professional)
# rm(miceProfessional)
# 
# ######Using R Mice Package to impute missing values [Doctorate]
# 
# any(is.na(ProtectedLand$Doctorate))
# 
# miceDoctorate <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Doctorate)
# md.pattern(miceDoctorate)
# imputed_Doctorate <- mice(miceDoctorate, m=5, method = 'pmm', seed = 101)
# miceDoctorate <- complete(imputed_Doctorate,3)
# ProtectedLand$Doctorate <- miceDoctorate$ProtectedLand.Doctorate
# 
# any(is.na(ProtectedLand$Doctorate))
# 
# #Clearing unneeded object
# rm(imputed_Doctorate)
# rm(miceDoctorate)
# 
# ######Using R Mice Package to impute missing values [Voted]
# any(is.na(ProtectedLand$Voted))
# 
# miceVoted <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Voted)
# md.pattern(miceVoted)
# imputed_Voted <- mice(miceVoted, m=5, method = 'pmm', seed = 101)
# miceVoted <- complete(imputed_Voted,3)
# ProtectedLand$Voted <- miceVoted$ProtectedLand.Voted
# 
# any(is.na(ProtectedLand$Voted))
# 
# #Clearing unneeded object
# rm(imputed_Voted)
# rm(miceVoted)
# 
# ######Using R Mice Package to impute missing values [Registered]
# any(is.na(ProtectedLand$Registered))
# 
# miceRegistered <- data.frame(ProtectedLand$census_tract_population, ProtectedLand$Registered)
# md.pattern(miceRegistered)
# imputed_Registered <- mice(miceRegistered, m=5, method = 'pmm', seed = 101)
# miceRegistered <- complete(imputed_Registered,3)
# ProtectedLand$Registered <- miceRegistered$ProtectedLand.Registered
# 
# any(is.na(ProtectedLand$Registered))
# 
# #Clearing unneeded object
# rm(imputed_Registered)
# rm(miceRegistered)
# 
# #     Clear the "Values" section
# rm(list=ls.str(mode='numeric'))

#--------------------------------------Descriptive Analysis [i003]--------------------------------------------------------------

#Generate a vector of unique counties
uniqueCounty <- unique(ProtectedLand$county)

#     Create new fields calculating the population of the tract id based on ethnicity
ProtectedLand$number_white <- round(ProtectedLand$census_tract_population * ProtectedLand$share_white,0)
ProtectedLand$number_black <- round(ProtectedLand$census_tract_population * ProtectedLand$share_black,0)
ProtectedLand$number_native_american <- round(ProtectedLand$census_tract_population * ProtectedLand$share_native_american,0)
ProtectedLand$number_asian <- round(ProtectedLand$census_tract_population * ProtectedLand$share_asian,0)
ProtectedLand$number_pacific_islander <- round(ProtectedLand$census_tract_population * ProtectedLand$share_pacific_islander,0)
ProtectedLand$number_other <- round(ProtectedLand$census_tract_population * ProtectedLand$share_other,0)
ProtectedLand$number_2plus <- round(ProtectedLand$census_tract_population * ProtectedLand$share_2plus,0)
ProtectedLand$number_hispanic <- round(ProtectedLand$census_tract_population * ProtectedLand$share_hispanic,0)

#     Create new fields calculating the population of the tract id based on education
ProtectedLand$number_lessthanHS <- round(ProtectedLand$census_tract_population * ProtectedLand$lessthanHS,0)
ProtectedLand$number_hsDiploma <- round(ProtectedLand$census_tract_population * ProtectedLand$HSdiploma,0)
ProtectedLand$number_someCollege <- round(ProtectedLand$census_tract_population * ProtectedLand$someCollege,0)
ProtectedLand$number_Associates <- round(ProtectedLand$census_tract_population * ProtectedLand$Associates,0)
ProtectedLand$number_Bachelors <- round(ProtectedLand$census_tract_population * ProtectedLand$Bachelors,0)
ProtectedLand$number_Masters <- round(ProtectedLand$census_tract_population * ProtectedLand$Masters,0)
ProtectedLand$number_Professional <- round(ProtectedLand$census_tract_population * ProtectedLand$Professional,0)
ProtectedLand$number_Doctorate <- round(ProtectedLand$census_tract_population * ProtectedLand$Doctorate,0)

#     Create new fields calculating the population of the tract id based on education
ProtectedLand$number_Registered <- round(ProtectedLand$Registered * ProtectedLand$Registered....of.Eligible.,0)
ProtectedLand$number_Democrat <- round(ProtectedLand$Registered * ProtectedLand$Democratic....of.Eligible.,0)
ProtectedLand$number_Republican <- round(ProtectedLand$Registered * ProtectedLand$Republican....of.Eligible.,0)
ProtectedLand$number_Independent <- round(ProtectedLand$Registered * ProtectedLand$American.Independent....of.Eligible.,0)
ProtectedLand$number_Green <- round(ProtectedLand$Registered * ProtectedLand$Green....of.Eligible.,0)
ProtectedLand$number_Libertarian <- round(ProtectedLand$Registered * ProtectedLand$Libertarian....of.Eligible.,0)
ProtectedLand$number_PeaceAndFreedom <- round(ProtectedLand$Registered * ProtectedLand$Peace.and.Freedom....of.Eligible.,0)
ProtectedLand$number_Party_Other <- round(ProtectedLand$Registered * ProtectedLand$Other....of.Eligible.,0)
ProtectedLand$number_Party_Declined <- round(ProtectedLand$Registered * ProtectedLand$Decline.to.State....of.Eligible.,0)

#     Create a function that generates a new data frame that houses all descriptive stats
myDescriptiveStats <- data.frame()

getDescriptiveStats <- function(){
  for ( iter in 1:length(uniqueCounty)) {
    myCounty <- uniqueCounty[iter]
    
    mean_ct_pop <- round(mean(ProtectedLand$census_tract_population[ProtectedLand$county==myCounty]),0)
    median_ct_pop <- round(median(ProtectedLand$census_tract_population[ProtectedLand$county==myCounty]),0)
    min_ct_pop <- round(min(ProtectedLand$census_tract_population[ProtectedLand$county==myCounty]),0)
    max_ct_pop <- round(max(ProtectedLand$census_tract_population[ProtectedLand$county==myCounty]),0)  
    
    mean_income <- round(mean(ProtectedLand$average_income[ProtectedLand$county==myCounty]),0)
    median_income <- round(median(ProtectedLand$average_income[ProtectedLand$county==myCounty]),0)
    min_income <- round(min(ProtectedLand$average_income[ProtectedLand$county==myCounty]),0)
    max_income <- round(max(ProtectedLand$average_income[ProtectedLand$county==myCounty]),0)
    
    mean_housing_price <- round(mean(ProtectedLand$median_housing_price[ProtectedLand$county==myCounty]),0)
    median_housing_price <- round(median(ProtectedLand$median_housing_price[ProtectedLand$county==myCounty]),0)
    min_housing_price <- round(min(ProtectedLand$median_housing_price[ProtectedLand$county==myCounty]),0)
    max_housing_price <- round(max(ProtectedLand$median_housing_price[ProtectedLand$county==myCounty]),0)
    
    mean_white_pop <- round(mean(ProtectedLand$number_white[ProtectedLand$county==myCounty]),0)
    median_white_pop <- round(median(ProtectedLand$number_white[ProtectedLand$county==myCounty]),0)
    min_white_pop <- round(min(ProtectedLand$number_white[ProtectedLand$county==myCounty]),0)
    max_white_pop <- round(max(ProtectedLand$number_white[ProtectedLand$county==myCounty]),0)   
    
    mean_black_pop <- round(mean(ProtectedLand$number_black[ProtectedLand$county==myCounty]),0)
    median_black_pop <- round(median(ProtectedLand$number_black[ProtectedLand$county==myCounty]),0)
    min_black_pop <- round(min(ProtectedLand$number_black[ProtectedLand$county==myCounty]),0)
    max_black_pop <- round(max(ProtectedLand$number_black[ProtectedLand$county==myCounty]),0)       
    
    mean_native_american_pop <- round(mean(ProtectedLand$number_native_american[ProtectedLand$county==myCounty]),0)
    median_native_american_pop <- round(median(ProtectedLand$number_native_american[ProtectedLand$county==myCounty]),0)
    min_native_american_pop <- round(min(ProtectedLand$number_native_american[ProtectedLand$county==myCounty]),0)
    max_native_american_pop <- round(max(ProtectedLand$number_native_american[ProtectedLand$county==myCounty]),0)          
    
    mean_asian_pop <- round(mean(ProtectedLand$number_asian[ProtectedLand$county==myCounty]),0)
    median_asian_pop <- round(median(ProtectedLand$number_asian[ProtectedLand$county==myCounty]),0)
    min_asian_pop <- round(min(ProtectedLand$number_asian[ProtectedLand$county==myCounty]),0)
    max_asian_pop <- round(max(ProtectedLand$number_asian[ProtectedLand$county==myCounty]),0)    
    
    mean_pacific_islander_pop <- round(mean(ProtectedLand$number_pacific_islander[ProtectedLand$county==myCounty]),0)
    median_pacific_islander_pop <- round(median(ProtectedLand$number_pacific_islander[ProtectedLand$county==myCounty]),0)
    min_pacific_islander_pop <- round(min(ProtectedLand$number_pacific_islander[ProtectedLand$county==myCounty]),0)
    max_pacific_islander_pop <- round(max(ProtectedLand$number_pacific_islander[ProtectedLand$county==myCounty]),0)      
    
    mean_other_pop <- round(mean(ProtectedLand$number_other[ProtectedLand$county==myCounty]),0)
    median_other_pop <- round(median(ProtectedLand$number_other[ProtectedLand$county==myCounty]),0)
    min_other_pop <- round(min(ProtectedLand$number_other[ProtectedLand$county==myCounty]),0)
    max_other_pop <- round(max(ProtectedLand$number_other[ProtectedLand$county==myCounty]),0)     
    
    mean_2plus_pop <- round(mean(ProtectedLand$number_2plus[ProtectedLand$county==myCounty]),0)
    median_2plus_pop <- round(median(ProtectedLand$number_2plus[ProtectedLand$county==myCounty]),0)
    min_2plus_pop <- round(min(ProtectedLand$number_2plus[ProtectedLand$county==myCounty]),0)
    max_2plus_pop <- round(max(ProtectedLand$number_2plus[ProtectedLand$county==myCounty]),0)   
    
    mean_lessthanHS <- round(mean(ProtectedLand$number_lessthanHS[ProtectedLand$county==myCounty]),0)
    median_lessthanHS <- round(mean(ProtectedLand$number_lessthanHS[ProtectedLand$county==myCounty]),0)
    min_lessthanHS <- round(min(ProtectedLand$number_lessthanHS[ProtectedLand$county==myCounty]),0)
    max_lessthanHS <- round(max(ProtectedLand$number_lessthanHS[ProtectedLand$county==myCounty]),0)    
    
    mean_hsDiploma <- round(mean(ProtectedLand$number_hsDiploma[ProtectedLand$county==myCounty]),0)
    median_hsDiploma <- round(mean(ProtectedLand$number_hsDiploma[ProtectedLand$county==myCounty]),0)
    min_hsDiploma <- round(min(ProtectedLand$number_hsDiploma[ProtectedLand$county==myCounty]),0)
    max_hsDiploma <- round(max(ProtectedLand$number_hsDiploma[ProtectedLand$county==myCounty]),0)    
    
    mean_someCollege <- round(mean(ProtectedLand$number_someCollege[ProtectedLand$county==myCounty]),0)
    median_someCollege <- round(mean(ProtectedLand$number_someCollege[ProtectedLand$county==myCounty]),0)
    min_someCollege <- round(min(ProtectedLand$number_someCollege[ProtectedLand$county==myCounty]),0)
    max_someCollege <- round(max(ProtectedLand$number_someCollege[ProtectedLand$county==myCounty]),0)  
    
    mean_Associates <- round(mean(ProtectedLand$number_Associates[ProtectedLand$county==myCounty]),0)
    median_Associates <- round(mean(ProtectedLand$number_Associates[ProtectedLand$county==myCounty]),0)
    min_Associates <- round(min(ProtectedLand$number_Associates[ProtectedLand$county==myCounty]),0)
    max_Associates <- round(max(ProtectedLand$number_Associates[ProtectedLand$county==myCounty]),0)    
    
    mean_Bachelors <- round(mean(ProtectedLand$number_Bachelors[ProtectedLand$county==myCounty]),0)
    median_Bachelors <- round(mean(ProtectedLand$number_Bachelors[ProtectedLand$county==myCounty]),0)
    min_Bachelors <- round(min(ProtectedLand$number_Bachelors[ProtectedLand$county==myCounty]),0)
    max_Bachelors <- round(max(ProtectedLand$number_Bachelors[ProtectedLand$county==myCounty]),0)     
    
    mean_Masters <- round(mean(ProtectedLand$number_Masters[ProtectedLand$county==myCounty]),0)
    median_Masters <- round(mean(ProtectedLand$number_Masters[ProtectedLand$county==myCounty]),0)
    min_Masters <- round(min(ProtectedLand$number_Masters[ProtectedLand$county==myCounty]),0)
    max_Masters <- round(max(ProtectedLand$number_Masters[ProtectedLand$county==myCounty]),0)     
    
    mean_Professional <- round(mean(ProtectedLand$number_Professional[ProtectedLand$county==myCounty]),0)
    median_Professional <- round(mean(ProtectedLand$number_Professional[ProtectedLand$county==myCounty]),0)
    min_Professional <- round(min(ProtectedLand$number_Professional[ProtectedLand$county==myCounty]),0)
    max_Professional <- round(max(ProtectedLand$number_Professional[ProtectedLand$county==myCounty]),0)     
    
    mean_Doctorate <- round(mean(ProtectedLand$number_Doctorate[ProtectedLand$county==myCounty]),0)
    median_Doctorate <- round(mean(ProtectedLand$number_Doctorate[ProtectedLand$county==myCounty]),0)
    min_Doctorate <- round(min(ProtectedLand$number_Doctorate[ProtectedLand$county==myCounty]),0)
    max_Doctorate <- round(max(ProtectedLand$number_Doctorate[ProtectedLand$county==myCounty]),0)      
    
    mean_Voted <- round(mean(ProtectedLand$Voted[ProtectedLand$county==myCounty]),0)
    median_Voted <- round(mean(ProtectedLand$Voted[ProtectedLand$county==myCounty]),0)
    min_Voted <- round(min(ProtectedLand$Voted[ProtectedLand$county==myCounty]),0)
    max_Voted <- round(max(ProtectedLand$Voted[ProtectedLand$county==myCounty]),0)    
    
    mean_Registered <- round(mean(ProtectedLand$Registered[ProtectedLand$county==myCounty]),0)
    median_Registered <- round(mean(ProtectedLand$Registered[ProtectedLand$county==myCounty]),0)
    min_Registered <- round(min(ProtectedLand$Registered[ProtectedLand$county==myCounty]),0)
    max_Registered <- round(max(ProtectedLand$Registered[ProtectedLand$county==myCounty]),0)   
    
    mean_voting_age_pop <- round(mean(ProtectedLand$voting_age_pop[ProtectedLand$county==myCounty]),0)
    median_voting_age_pop <- round(mean(ProtectedLand$voting_age_pop[ProtectedLand$county==myCounty]),0)
    min_voting_age_pop <- round(min(ProtectedLand$voting_age_pop[ProtectedLand$county==myCounty]),0)
    max_voting_age_pop <- round(max(ProtectedLand$voting_age_pop[ProtectedLand$county==myCounty]),0)     
    
    mean_number_democrat <- round(mean(ProtectedLand$number_Democrat[ProtectedLand$county==myCounty]),0)
    median_number_democrat <- round(mean(ProtectedLand$number_Democrat[ProtectedLand$county==myCounty]),0)
    min_number_democrat <- round(min(ProtectedLand$number_Democrat[ProtectedLand$county==myCounty]),0)
    max_number_democrat <- round(max(ProtectedLand$number_Democrat[ProtectedLand$county==myCounty]),0)
    
    mean_number_republican <- round(mean(ProtectedLand$number_Republican[ProtectedLand$county==myCounty]),0)
    median_number_republican <- round(mean(ProtectedLand$number_Republican[ProtectedLand$county==myCounty]),0)
    min_number_republican <- round(min(ProtectedLand$number_Republican[ProtectedLand$county==myCounty]),0)
    max_number_republican <- round(max(ProtectedLand$number_Republican[ProtectedLand$county==myCounty]),0)   
    
    mean_number_independent <- round(mean(ProtectedLand$number_Independent[ProtectedLand$county==myCounty]),0)
    median_number_independent <- round(mean(ProtectedLand$number_Independent[ProtectedLand$county==myCounty]),0)
    min_number_independent <- round(min(ProtectedLand$number_Independent[ProtectedLand$county==myCounty]),0)
    max_number_independent <- round(max(ProtectedLand$number_Independent[ProtectedLand$county==myCounty]),0)       
    
    mean_number_green <- round(mean(ProtectedLand$number_Green[ProtectedLand$county==myCounty]),0)
    median_number_green <- round(mean(ProtectedLand$number_Green[ProtectedLand$county==myCounty]),0)
    min_number_green <- round(min(ProtectedLand$number_Green[ProtectedLand$county==myCounty]),0)
    max_number_green <- round(max(ProtectedLand$number_Green[ProtectedLand$county==myCounty]),0)    
    
    mean_number_libertarian <- round(mean(ProtectedLand$number_Libertarian[ProtectedLand$county==myCounty]),0)
    median_number_libertarian <- round(mean(ProtectedLand$number_Libertarian[ProtectedLand$county==myCounty]),0)
    min_number_libertarian <- round(min(ProtectedLand$number_Libertarian[ProtectedLand$county==myCounty]),0)
    max_number_libertarian <- round(max(ProtectedLand$number_Libertarian[ProtectedLand$county==myCounty]),0)    
    
    mean_number_peaceandfreedom <- round(mean(ProtectedLand$number_PeaceAndFreedom[ProtectedLand$county==myCounty]),0)
    median_number_peaceandfreedom <- round(mean(ProtectedLand$number_PeaceAndFreedom[ProtectedLand$county==myCounty]),0)
    min_number_peaceandfreedom <- round(min(ProtectedLand$number_PeaceAndFreedom[ProtectedLand$county==myCounty]),0)
    max_number_peaceandfreedom <- round(max(ProtectedLand$number_PeaceAndFreedom[ProtectedLand$county==myCounty]),0)    
    
    mean_number_party_other <- round(mean(ProtectedLand$number_Party_Other[ProtectedLand$county==myCounty]),0)
    median_number_party_other <- round(mean(ProtectedLand$number_Party_Other[ProtectedLand$county==myCounty]),0)
    min_number_party_other <- round(min(ProtectedLand$number_Party_Other[ProtectedLand$county==myCounty]),0)
    max_number_party_other <- round(max(ProtectedLand$number_Party_Other[ProtectedLand$county==myCounty]),0)    
    
    mean_number_party_declined <- round(mean(ProtectedLand$number_Party_Declined[ProtectedLand$county==myCounty]),0)
    median_number_party_declined <- round(mean(ProtectedLand$number_Party_Declined[ProtectedLand$county==myCounty]),0)
    min_number_party_declined <- round(min(ProtectedLand$number_Party_Declined[ProtectedLand$county==myCounty]),0)
    max_number_party_declined <- round(max(ProtectedLand$number_Party_Declined[ProtectedLand$county==myCounty]),0) 
    
    newRow <- data.frame(county=myCounty,
                         mean_ct_pop, median_ct_pop, min_ct_pop, max_ct_pop,
                         mean_income, median_income, min_income, max_income,
                         mean_housing_price, median_housing_price, min_housing_price, max_housing_price,
                         mean_white_pop,median_white_pop,min_white_pop,max_white_pop,
                         mean_black_pop,median_black_pop,min_black_pop,max_black_pop,
                         mean_native_american_pop,median_native_american_pop,min_native_american_pop,max_native_american_pop,
                         mean_asian_pop,median_asian_pop,min_asian_pop,max_asian_pop,
                         mean_pacific_islander_pop,median_pacific_islander_pop,min_pacific_islander_pop,max_pacific_islander_pop,
                         mean_other_pop,median_other_pop,min_other_pop,max_other_pop,
                         mean_2plus_pop,median_2plus_pop,min_2plus_pop,max_2plus_pop,
                         mean_lessthanHS, median_lessthanHS, min_lessthanHS, max_lessthanHS,
                         mean_hsDiploma, median_hsDiploma, min_hsDiploma, max_hsDiploma,
                         mean_someCollege, median_someCollege, min_someCollege, max_someCollege,
                         mean_Associates, median_Associates, min_Associates, max_Associates,
                         mean_Bachelors, median_Bachelors, min_Bachelors, max_Bachelors,
                         mean_Masters, median_Masters, min_Masters, max_Masters,
                         mean_Professional, median_Professional, min_Professional, max_Professional,
                         mean_Doctorate, median_Doctorate, min_Doctorate, max_Doctorate,
                         mean_Voted, median_Voted, min_Voted, max_Voted,
                         mean_Registered, median_Registered, min_Registered, max_Registered,
                         mean_voting_age_pop, median_voting_age_pop, min_voting_age_pop, max_voting_age_pop,
                         mean_number_democrat, median_number_democrat, min_number_democrat, max_number_democrat,
                         mean_number_republican, median_number_republican, min_number_republican, max_number_republican,
                         mean_number_independent, median_number_independent, min_number_independent, max_number_independent,
                         mean_number_green, median_number_green, min_number_green, max_number_green,
                         mean_number_libertarian, median_number_libertarian, min_number_libertarian, max_number_libertarian,
                         mean_number_peaceandfreedom, median_number_peaceandfreedom, min_number_peaceandfreedom, max_number_peaceandfreedom,
                         mean_number_party_other, median_number_party_other, min_number_party_other, max_number_party_other,
                         mean_number_party_declined, median_number_party_declined, min_number_party_declined, max_number_party_declined
    )
    
    myDescriptiveStats <<- rbind(myDescriptiveStats,newRow)
  }
}

getDescriptiveStats()

#----------------------------------------Descriptive Charts [i003a]----------------------------------------
#Generate lolipop chart to show population distribution by county
#     Create a subset of data and save it to a dataframe

myPopTotalsByCounty <- ProtectedLand[,c("county","county_population")]

#     Deduplicate the rows
myPopTotalsByCounty <- myPopTotalsByCounty[!duplicated(myPopTotalsByCounty[, c("county","county_population")]), ]

#     Reset the row names
row.names(myPopTotalsByCounty) <- NULL

#     Plot the data
theme_set(theme_bw())
ggplot(myPopTotalsByCounty, aes(x=county, y=county_population)) +
  geom_point(size=3) +
  geom_segment(aes(x=county, xend=county, y=0, yend=county_population)) +
  labs(title="Population vs County", caption="LA county accounts for ~27% of Californias population.") +
  theme(axis.text.x=element_text(angle=65, hjust=1, vjust=1))


#Generate box charts to visualize ethnic and education dispersion in California
#     Create a subset of data and save it to a dataframe
myEthnicData <- ProtectedLand[,c("number_white","number_black",
                                 "number_native_american","number_asian",
                                 "number_pacific_islander","number_other",
                                 "number_2plus","number_hispanic")]

myEducationData <- ProtectedLand[,c("number_lessthanHS","number_hsDiploma",
                                    "number_someCollege","number_Associates",
                                    "number_Bachelors","number_Masters",
                                    "number_Professional","number_Doctorate")]


#     Melt the data for easier plotting
myMeltedEthnicData <- melt(data=myEthnicData,measure.vars = c("number_white","number_black",
                                                              "number_native_american","number_asian",
                                                              "number_pacific_islander","number_other",
                                                              "number_2plus","number_hispanic"))

myMeltedEducationData <- melt(data=myEducationData,measure.vars = c("number_lessthanHS","number_hsDiploma",
                                                                    "number_someCollege","number_Associates",
                                                                    "number_Bachelors","number_Masters",
                                                                    "number_Professional","number_Doctorate"))

#     Relabel column headers and remove "number_"
names(myMeltedEthnicData)[1] <- "Ethnicity"
names(myMeltedEthnicData)[2] <- "Population"
myMeltedEthnicData$Ethnicity <- gsub("number_", "", myMeltedEthnicData$Ethnicity)
names(myMeltedEducationData)[1] <- "Education"
names(myMeltedEducationData)[2] <- "Population"
myMeltedEducationData$Education <- gsub("number_", "", myMeltedEducationData$Education)

#     Make additional column so plot order mimics dataframe column order
myMeltedEthnicData$Ethnicity2 <- factor(myMeltedEthnicData$Ethnicity, c("white","black",
                                                                        "native_american","asian",
                                                                        "pacific_islander","other",
                                                                        "2plus","hispanic"))

myMeltedEducationData$Education2 <- factor(myMeltedEducationData$Education, c("lessthanHS","hsDiploma",
                                                                              "someCollege","Associates",
                                                                              "Bachelors","Masters",
                                                                              "Professional","Doctorate"))

#     Box-plot  ethnicity, education and income in california
ethnicBoxData <- ggplot(data=myMeltedEthnicData, aes(x=Ethnicity2,y=Population,color=Ethnicity))
ethnicBoxData + geom_boxplot(size=1) + theme(legend.position = "none", plot.title=element_text(hjust=0.5)) +
  labs(x="race",y="population") + ggtitle("population vs race")

print("White and hispanic population distributions are very similar with white groups having a slightly
      higher median with one notable outlier enclave (Los Angeles).")

educationBoxData <- ggplot(data=myMeltedEducationData, aes(x=Education2,y=Population,color=Education))
educationBoxData + geom_boxplot(size=1) + theme(legend.position = "none", plot.title=element_text(hjust=0.5)) +
  labs(x="education",y="population") + ggtitle("population vs education level")

print("Educational dispersion is very similar between high school diploma and some college, with Bachelors
      and less than high school showing greater dispersion and less consistancy around the median.")

meanAverageIncome <- data.frame(round(mean(ProtectedLand$average_income, na.rm = T),0))
names(meanAverageIncome)[1] <- "average_income"

incomeBoxData <- ggplot(data=ProtectedLand, aes(x="",y=average_income))
incomeBoxData + geom_boxplot(size=1) + 
  stat_summary(fun.y = mean, geom = "point", shape=23, size=4) +              
  geom_text(data = meanAverageIncome, aes(label = average_income), nudge_x = .03, vjust=-0.5) +               
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) + 
  labs(x="",y="average income") + ggtitle("average income dispersion in california")

print("Most people in California make between $20,000 and $40,000 on average. There is a considerable spread of average salaries.")


#----------------------------------------Descriptive Maps [i003b]----------------------------------------
mappingData<-data.frame(mappingData)

###Registered to eligible voters [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$registered_to_eligible)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$registered_to_eligible))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Registered to Eligible Voters (Dist2Tract)",fill="% Registered to Eligible Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Registered to eligible voters [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$registered_to_eligible)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$registered_to_eligible))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+labs(title = "Registered to Eligible Voters (ldist)", fill="% Registered to Eligible Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Democratic to registered voters [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$democratic_to_registered)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$democratic_to_registered))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Democratic to Registered Voters (Dist2Tract)",fill="% Democratic to Registered Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Democratic to registered voters [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$democratic_to_registered)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$democratic_to_registered))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+labs(title = "Democratic to Registered Voters (ldist)", fill="% Democratic to Registered Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Republican to registered voters [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$republican_to_registered)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$republican_to_registered))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Republican to Registered Voters (Dist2Tract)",fill="% Republican to Registered Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Republican to registered voters [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$republican_to_registered)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$republican_to_registered))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Republican to Registered Voters (ldist)", fill="% Republican to Registered Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###No Party to registered voters [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$noparty_to_registered)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$noparty_to_registered))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "No Party to Registered Voters (Dist2Tract)",fill="% No Party to Registered Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###No Party to registered voters [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$noparty_to_registered)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$noparty_to_registered))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "No Party to Registered Voters (ldist)", fill="% No Party to Registered Voters")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Average Income  [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$avgincome)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$avgincome))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Average Income (Dist2Tract)",fill="Average Income")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Average Income [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$avgincome)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$avgincome))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Average Income (ldist)", fill="Average Income")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Median Housing Price  [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$med_housing)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$med_housing))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Median Housing Price (Dist2Tract)",fill="Median Housing Price")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))


###Median Housing Price[county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$med_housing)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$med_housing))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Median Housing Price (ldist)", fill="Median Housing Price")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###County Population [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$Cpop)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$Cpop))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "County Population (Dist2Tract)",fill="Median Housing Price")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###County Population [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$Cpop)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$Cpop))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "County Population (ldist)", fill="County Population")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Population Density [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$popdens)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$popdens))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Population Density (Dist2Tract)",fill="Population Density")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Population Density [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$popdens)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$popdens))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Population Density (ldist)", fill="Population Density")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))


###Share White [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_white)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_white))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share White (Dist2Tract)",fill="Share White")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Share White [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_white)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_white))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share White (ldist)", fill="Share White")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Share Black [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_black)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_black))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share Black  (Dist2Tract)",fill="Share Black")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Share Black [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_black)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_black))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share Black (ldist)", fill="Share Black")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Share Hispanic [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_hispanic)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_hispanic))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share Hispanic  (Dist2Tract)",fill="Share Hispanic")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Share Hispanic [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_hispanic)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_hispanic))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share Hispanic (ldist)", fill="Share Hispanic")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Share Asian [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_asian)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_asian))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share Asian  (Dist2Tract)",fill="Share Asian")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

#Share Asian[county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$share_asian)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$share_asian))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Share Asian (ldist)", fill="Share Asian")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Less than HS Education [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$lessthanHS)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$lessthanHS))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Less than HS Education  (Dist2Tract)",fill="Less than HS Education")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

#Less than HS Education [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$lessthanHS)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$lessthanHS))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Less than HS Education (ldist)", fill="Less than HS Education")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Professional Degrees [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$Professional)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$Professional))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Professional Degrees  (Dist2Tract)",fill="Professional Degrees")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

#Professional Degrees [county_ldist]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$Professional)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$Professional))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Professional Degrees (ldist)", fill="Professional Degrees")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Doctorate [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$Doctorate)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$Doctorate))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_dist2tract, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_dist2tract", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Doctorate  (Dist2Tract)",fill="Doctorate")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))

###Doctorate [county_dist2tract]

ww <- ggplot(mappingData, aes(x=long, y=lat, group=group, fill = mappingData$Doctorate)) + geom_polygon(colour="black")+ coord_map('polyconic')

xx <- ww+scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(mappingData$Doctorate))

yy <- xx +geom_point( data=mappingData, aes(x=long, y=lat, size = mappingData$county_ldist, color="hotpink2", alpha = 0.1)) + scale_size_continuous(name="mappingData$county_ldist", range = c(1,10)) + guides(colour=FALSE, alpha=FALSE) #dots

zz <- yy+scale_fill_viridis_c()+labs(title = "Doctorate  (ldist)",fill="Doctorate")

zz + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),rect = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), plot.title=element_text(hjust=0.5))


#--------------------------------------Advanced Analysis [i004]--------------------------------------------------------------
hist(ProtectedLand$distance_to_tract)
hist(ProtectedLand$ldist)
print("Using the log of distance instead of the regular distance variable normalizes the data, allowing for more accurate linear models.")

# 1. Can we show inequality in access to protected land areas?
# 1a. Is there a difference in access for people in rural versus suburban versus urban counties?
model1a <- lm(ldist ~ urban + suburb, ProtectedLand)
summary(model1a)
print("1a. Urban and suburban counties have increased access to protected land areas compared to rural areas. The model has a near-0 p-value and an adjusted R-squared of 11.21%. Suburban county census tracts see a exp(-1.13181)-1 = 67.75% increase in access and urban county census tracts see a 78.48% increase in access.")

model1a_urban <- ggplot(model1a$model, aes_string(x = names(model1a$model)[2], y = names(model1a$model)[1])) + geom_point(colour="lightblue", alpha = 0.01) + geom_abline(intercept = coef(model1a)[1], slope = coef(model1a)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1a_suburb <- ggplot(model1a$model, aes_string(x = names(model1a$model)[3], y = names(model1a$model)[1])) + geom_point(colour="springgreen4", alpha = 0.01) + geom_abline(intercept = coef(model1a)[1], slope = coef(model1a)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1a_urban, model1a_suburb, nrow=2, top="Increased access for urban and suburban areas compared to rural areas")


# 1ai. Is there a relationship between rural, suburban, and urban incomes?
model1ai <- lm(average_income ~ urban + suburb, ProtectedLand)
summary(model1ai)
print("1ai. The model is significant and has an adjusted R-squared value of 2.784%. Both coefficients are significant, showing an increase of approximately $10,000 for urban and $4,500 for suburban in average income over rural incomes.")

model1ai_urban <- ggplot(model1ai$model, aes_string(x = names(model1ai$model)[2], y = names(model1ai$model)[1])) + geom_point(colour="lightblue", alpha = 0.01) + geom_abline(intercept = coef(model1ai)[1], slope = coef(model1ai)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1ai_suburb <- ggplot(model1ai$model, aes_string(x = names(model1ai$model)[3], y = names(model1ai$model)[1])) + geom_point(colour="springgreen4", alpha = 0.01) + geom_abline(intercept = coef(model1ai)[1], slope = coef(model1ai)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1ai_urban, model1ai_suburb, nrow=2, top="Increased income for urban and suburban areas compared to rural areas")


# 1aia. Does that factor into median housing price?
model1aia <- lm(median_housing_price ~ average_income + urban + suburb, ProtectedLand)
summary(model1aia)
print("1aia. The model is significant and has an adjusted R-squared of 66.23%. Average income and urban locations seem to be the primary drivers of housing prices in California, although suburban housing prices are also much higher.")

model1aia_avgInc <- ggplot(model1aia$model, aes_string(x = names(model1aia$model)[2], y = names(model1aia$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1aia)[1], slope = coef(model1aia)[2], colour="black", size=1)
model1aia_urban <- ggplot(model1aia$model, aes_string(x = names(model1aia$model)[3], y = names(model1aia$model)[1])) + geom_point(colour="springgreen4", alpha = 0.01) + geom_abline(intercept = coef(model1aia)[1], slope = coef(model1aia)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1aia_suburb <- ggplot(model1aia$model, aes_string(x = names(model1aia$model)[4], y = names(model1aia$model)[1])) + geom_point(colour="firebrick", alpha = 0.01) + geom_abline(intercept = coef(model1aia)[1], slope = coef(model1aia)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1aia_avgInc, model1aia_urban, model1aia_suburb, nrow=3, top="Increased median housing price for urban and suburban areas compared to rural areas")


# 1b. Is there a relationship between race and access?
model1b <- lm(ldist ~ share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other, ProtectedLand)
summary(model1b)
print("1b. The model is significant and has an adjusted R-squared of 9.437%. Most race/ethnicity coefficients are statistically significant and show slightly increased access compared to white populations. Of notable exception is that Native American populations see highly decreased access (increased distance), which is expected since Native land is not included in the group of designated protected lands.")

model1b_black <- ggplot(model1b$model, aes_string(x = names(model1b$model)[2], y = names(model1b$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1b_hispanic <- ggplot(model1b$model, aes_string(x = names(model1b$model)[3], y = names(model1b$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1b_asian <- ggplot(model1b$model, aes_string(x = names(model1b$model)[4], y = names(model1b$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1b_na <- ggplot(model1b$model, aes_string(x = names(model1b$model)[5], y = names(model1b$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1b_pi <- ggplot(model1b$model, aes_string(x = names(model1b$model)[6], y = names(model1b$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1b_2plus <- ggplot(model1b$model, aes_string(x = names(model1b$model)[7], y = names(model1b$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1b_other <- ggplot(model1b$model, aes_string(x = names(model1b$model)[8], y = names(model1b$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model1b)[1], slope = coef(model1b)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1b_black, model1b_hispanic, model1b_asian, model1b_na, model1b_pi, model1b_2plus, model1b_other, nrow=4, top="Most racial groups show slightly increased access to protected lands compared to white people")


# 1bi. Is there a relationship between race and income?
model1bi <- lm(average_income ~ share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other, ProtectedLand)
summary(model1bi)
print("1bi. The model is significant and has an adjusted R-squared of 54.05%. All minority racial/ethnic populations have lower average incomes compared to white populations except those that identify as other. The coefficients can be interpreted as a 1 percentage point (0.01) increase in the population share of a racial/ethnic minority in a census tract will show a $[coefficient value] change in census tract average income.")

model1bi_black <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[2], y = names(model1bi$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1bi_hispanic <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[3], y = names(model1bi$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1bi_asian <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[4], y = names(model1bi$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1bi_na <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[5], y = names(model1bi$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1bi_pi <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[6], y = names(model1bi$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1bi_2plus <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[7], y = names(model1bi$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1bi_other <- ggplot(model1bi$model, aes_string(x = names(model1bi$model)[8], y = names(model1bi$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model1bi)[1], slope = coef(model1bi)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
grid.arrange(model1bi_black,model1bi_hispanic,model1bi_asian,model1bi_na,model1bi_pi,model1bi_2plus,model1bi_other, nrow=4, top="All minority groups have lower average incomes compared to their white counterparts")


# 1bii. Is there a relationship between race and housing price?
model1bii <- lm(median_housing_price ~ share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other, ProtectedLand)
summary(model1bii)
print("1bii. The model is significant and has an adjusted R-squared of 39.59%. All minority racial/ethnic populations have lower median housing prices compared to white populations except those that identify as Asian or other. The coefficients can be interpreted as a 1 percentage point (0.01) increase in the population share of a racial/ethnic minority in a census tract will show a $[coefficient value] change in census tract median housing price.")

model1bii_black <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[2], y = names(model1bii$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1bii_hispanic <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[3], y = names(model1bii$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1bii_asian <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[4], y = names(model1bii$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1bii_na <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[5], y = names(model1bii$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1bii_pi <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[6], y = names(model1bii$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1bii_2plus <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[7], y = names(model1bii$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1bii_other <- ggplot(model1bii$model, aes_string(x = names(model1bii$model)[8], y = names(model1bii$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model1bii)[1], slope = coef(model1bii)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
grid.arrange(model1bii_black,model1bii_hispanic,model1bii_asian,model1bii_na,model1bii_pi,model1bii_2plus,model1bii_other, nrow=4, top="Most minority groups have lower median housing prices compared to white populations")


# 1c. Is there a relationship between education and access?
model1c <- lm(ldist ~ HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLand)
summary(model1c)
print("1c. The model is significant and has an adjusted R-squared of 4.446%. All of the coefficients are positive (decreased access compared to less than High School) except Bachelors and Doctorate, and most are significant except Masters and Doctorate. Notably, professional degree holders have significantly decreased access compared to other education groups.")

model1c_HSdiploma <- ggplot(model1c$model, aes_string(x = names(model1c$model)[2], y = names(model1c$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1c_someCollege <- ggplot(model1c$model, aes_string(x = names(model1c$model)[3], y = names(model1c$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1c_Associates <- ggplot(model1c$model, aes_string(x = names(model1c$model)[4], y = names(model1c$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1c_Bachelors <- ggplot(model1c$model, aes_string(x = names(model1c$model)[5], y = names(model1c$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1c_Masters <- ggplot(model1c$model, aes_string(x = names(model1c$model)[6], y = names(model1c$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1c_Professional <- ggplot(model1c$model, aes_string(x = names(model1c$model)[7], y = names(model1c$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1c_Doctorate <- ggplot(model1c$model, aes_string(x = names(model1c$model)[8], y = names(model1c$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model1c)[1], slope = coef(model1c)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1c_HSdiploma,model1c_someCollege,model1c_Associates,model1c_Bachelors,model1c_Masters,model1c_Professional,model1c_Doctorate, nrow=4, top="Most education levels have decreased access to protected land")


# 1ci. Is there a relationship between education and income?
model1ci <- lm(average_income ~ HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLand)
summary(model1ci)
print("1c. The model is significant and has an adjusted R-squared of 78.06%. Most of the coefficients are significant except Associates, with all showing an increase in income (compared to less than High School) except Doctorate.")

model1ci_HSdiploma <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[2], y = names(model1ci$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1ci_someCollege <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[3], y = names(model1ci$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1ci_Associates <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[4], y = names(model1ci$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1ci_Bachelors <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[5], y = names(model1ci$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1ci_Masters <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[6], y = names(model1ci$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1ci_Professional <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[7], y = names(model1ci$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
model1ci_Doctorate <- ggplot(model1ci$model, aes_string(x = names(model1ci$model)[8], y = names(model1ci$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model1ci)[1], slope = coef(model1ci)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("avg_income")
grid.arrange(model1ci_HSdiploma,model1ci_someCollege,model1ci_Associates,model1ci_Bachelors,model1ci_Masters,model1ci_Professional,model1ci_Doctorate, nrow=4, top="Most education levels show increased average income compared to less than HS education")


# 1cii. Is there a relationship between education and housing price?
model1cii <- lm(median_housing_price ~ HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLand)
summary(model1cii)
print("1c. The model is significant and has an adjusted R-squared of 65.71%. All coefficients are significant except Doctorate, which has a p-value of 0.2902. Compared to less the High School, Bachelors, Masters and Professional dregrees see increases in median housing price, while high school diploma, some college, Associates, and Doctorate show decreases in median housing price.")

model1cii_HSdiploma <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[2], y = names(model1cii$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1cii_someCollege <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[3], y = names(model1cii$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1cii_Associates <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[4], y = names(model1cii$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1cii_Bachelors <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[5], y = names(model1cii$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1cii_Masters <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[6], y = names(model1cii$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1cii_Professional <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[7], y = names(model1cii$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
model1cii_Doctorate <- ggplot(model1cii$model, aes_string(x = names(model1cii$model)[8], y = names(model1cii$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model1cii)[1], slope = coef(model1cii)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1)) + ylab("med_hse_pr")
grid.arrange(model1cii_HSdiploma,model1cii_someCollege,model1cii_Associates,model1cii_Bachelors,model1cii_Masters,model1cii_Professional,model1cii_Doctorate, nrow=4, top="Bachelors, Masters and Professional levels of education show increased median housing prices")


# 1d. What are primary indicators of access, all else constant?
model1d <- lm(ldist ~ average_income + median_housing_price + urban + suburb + share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other + HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLand)
summary(model1d)
print("1d. The model is significant and has an adjusted R-squared of 15.88%. The significant variables are average income, median housing price, urban, suburban, all race/ethnicity variables except other, high school diploma, Bachelors, and Doctorate degrees.")

model1d_average_income <- ggplot(model1d$model, aes_string(x = names(model1d$model)[2], y = names(model1d$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[2], colour="black", size=1)
model1d_median_housing_price <- ggplot(model1d$model, aes_string(x = names(model1d$model)[3], y = names(model1d$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[3], colour="black", size=1)
model1d_urban <- ggplot(model1d$model, aes_string(x = names(model1d$model)[4], y = names(model1d$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_suburb <- ggplot(model1d$model, aes_string(x = names(model1d$model)[5], y = names(model1d$model)[1])) + geom_point(colour="chartreuse4", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_black <- ggplot(model1d$model, aes_string(x = names(model1d$model)[6], y = names(model1d$model)[1])) + geom_point(colour="cadetblue", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_hispanic <- ggplot(model1d$model, aes_string(x = names(model1d$model)[7], y = names(model1d$model)[1])) + geom_point(colour="burlywood", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_asian <- ggplot(model1d$model, aes_string(x = names(model1d$model)[8], y = names(model1d$model)[1])) + geom_point(colour="brown2", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_na <- ggplot(model1d$model, aes_string(x = names(model1d$model)[9], y = names(model1d$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_pi <- ggplot(model1d$model, aes_string(x = names(model1d$model)[10], y = names(model1d$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_2plus <- ggplot(model1d$model, aes_string(x = names(model1d$model)[11], y = names(model1d$model)[1])) + geom_point(colour="bisque3", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_other <- ggplot(model1d$model, aes_string(x = names(model1d$model)[12], y = names(model1d$model)[1])) + geom_point(colour="aquamarine3", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_HSdiploma <- ggplot(model1d$model, aes_string(x = names(model1d$model)[13], y = names(model1d$model)[1])) + geom_point(colour="dodgerblue", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[13], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_someCollege <- ggplot(model1d$model, aes_string(x = names(model1d$model)[14], y = names(model1d$model)[1])) + geom_point(colour="goldenrod1", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[14], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_Associates <- ggplot(model1d$model, aes_string(x = names(model1d$model)[15], y = names(model1d$model)[1])) + geom_point(colour="forestgreen", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[15], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_Bachelors <- ggplot(model1d$model, aes_string(x = names(model1d$model)[16], y = names(model1d$model)[1])) + geom_point(colour="indianred1", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[16], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_Masters <- ggplot(model1d$model, aes_string(x = names(model1d$model)[17], y = names(model1d$model)[1])) + geom_point(colour="khaki", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[17], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_Professional <- ggplot(model1d$model, aes_string(x = names(model1d$model)[18], y = names(model1d$model)[1])) + geom_point(colour="tomato", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[18], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1d_Doctorate <- ggplot(model1d$model, aes_string(x = names(model1d$model)[19], y = names(model1d$model)[1])) + geom_point(colour="thistle", alpha = 0.1) + geom_abline(intercept = coef(model1d)[1], slope = coef(model1d)[19], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1d_average_income, model1d_median_housing_price, model1d_urban, model1d_suburb, model1d_black, model1d_hispanic, model1d_asian, model1d_na, model1d_pi, model1d_2plus, model1d_other, model1d_HSdiploma, model1d_someCollege, model1d_Associates, model1d_Bachelors, model1d_Masters, model1d_Professional, model1d_Doctorate, nrow=4, top="Access to protected land with all variables accounted for")


model1dRace <- lm(ldist ~ average_income + median_housing_price + urban + suburb + share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other, ProtectedLand)
summary(model1dRace)

model1dRace_average_income <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[2], y = names(model1dRace$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[2], colour="black", size=1)
model1dRace_median_housing_price <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[3], y = names(model1dRace$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[3], colour="black", size=1)
model1dRace_urban <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[4], y = names(model1dRace$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_suburb <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[5], y = names(model1dRace$model)[1])) + geom_point(colour="chartreuse4", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_black <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[6], y = names(model1dRace$model)[1])) + geom_point(colour="cadetblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_hispanic <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[7], y = names(model1dRace$model)[1])) + geom_point(colour="burlywood", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_asian <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[8], y = names(model1dRace$model)[1])) + geom_point(colour="brown2", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_na <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[9], y = names(model1dRace$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_pi <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[10], y = names(model1dRace$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_2plus <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[11], y = names(model1dRace$model)[1])) + geom_point(colour="bisque3", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRace_other <- ggplot(model1dRace$model, aes_string(x = names(model1dRace$model)[12], y = names(model1dRace$model)[1])) + geom_point(colour="aquamarine3", alpha = 0.1) + geom_abline(intercept = coef(model1dRace)[1], slope = coef(model1dRace)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1dRace_average_income, model1dRace_median_housing_price, model1dRace_urban, model1dRace_suburb, model1dRace_black, model1dRace_hispanic, model1dRace_asian, model1dRace_na, model1dRace_pi, model1dRace_2plus, model1dRace_other, nrow=4, top="Access to protected land with all variables accounted for except education level")


model1dEdu <- lm(ldist ~ average_income + median_housing_price + urban + suburb + HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLand)
summary(model1dEdu)

model1dEdu_average_income <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[2], y = names(model1dEdu$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[2], colour="black", size=1)
model1dEdu_median_housing_price <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[3], y = names(model1dEdu$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[3], colour="black", size=1)
model1dEdu_urban <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[4], y = names(model1dEdu$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_suburb <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[5], y = names(model1dEdu$model)[1])) + geom_point(colour="chartreuse4", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_HSdiploma <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[6], y = names(model1dEdu$model)[1])) + geom_point(colour="dodgerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_someCollege <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[7], y = names(model1dEdu$model)[1])) + geom_point(colour="goldenrod1", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_Associates <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[8], y = names(model1dEdu$model)[1])) + geom_point(colour="forestgreen", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_Bachelors <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[9], y = names(model1dEdu$model)[1])) + geom_point(colour="indianred1", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_Masters <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[10], y = names(model1dEdu$model)[1])) + geom_point(colour="khaki", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_Professional <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[11], y = names(model1dEdu$model)[1])) + geom_point(colour="tomato", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dEdu_Doctorate <- ggplot(model1dEdu$model, aes_string(x = names(model1dEdu$model)[12], y = names(model1dEdu$model)[1])) + geom_point(colour="thistle", alpha = 0.1) + geom_abline(intercept = coef(model1dEdu)[1], slope = coef(model1dEdu)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1dEdu_average_income, model1dEdu_median_housing_price, model1dEdu_urban, model1dEdu_suburb, model1dEdu_HSdiploma, model1dEdu_someCollege, model1dEdu_Associates, model1dEdu_Bachelors, model1dEdu_Masters, model1dEdu_Professional, model1dEdu_Doctorate, nrow=4, top="Access to protected land with all variables accounted for except race")


print("What if we only look at urban/suburban areas, where unused green land areas--whether protected or not--are more scarce?")
ProtectedLandNonRural <- ProtectedLand[ProtectedLand$rural != 1,]
ProtectedLandUrban <- ProtectedLand[ProtectedLand$urban == 1,]
ProtectedLandSuburban <- ProtectedLand[ProtectedLand$suburb == 1,]
ProtectedLandRural <- ProtectedLand[ProtectedLand$rural == 1,]

model1dNonRural <- lm(ldist ~ average_income + median_housing_price + urban + share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other + HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLandNonRural)
summary(model1dNonRural)

model1dNonRural_average_income <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[2], y = names(model1dNonRural$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[2], colour="black", size=1)
model1dNonRural_median_housing_price <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[3], y = names(model1dNonRural$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[3], colour="black", size=1)
model1dNonRural_urban <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[4], y = names(model1dNonRural$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_black <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[5], y = names(model1dNonRural$model)[1])) + geom_point(colour="cadetblue", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_hispanic <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[6], y = names(model1dNonRural$model)[1])) + geom_point(colour="burlywood", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_asian <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[7], y = names(model1dNonRural$model)[1])) + geom_point(colour="brown2", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_na <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[8], y = names(model1dNonRural$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_pi <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[9], y = names(model1dNonRural$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_2plus <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[10], y = names(model1dNonRural$model)[1])) + geom_point(colour="bisque3", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_other <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[11], y = names(model1dNonRural$model)[1])) + geom_point(colour="aquamarine3", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_HSdiploma <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[12], y = names(model1dNonRural$model)[1])) + geom_point(colour="dodgerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_someCollege <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[13], y = names(model1dNonRural$model)[1])) + geom_point(colour="goldenrod1", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[13], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_Associates <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[14], y = names(model1dNonRural$model)[1])) + geom_point(colour="forestgreen", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[14], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_Bachelors <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[15], y = names(model1dNonRural$model)[1])) + geom_point(colour="indianred1", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[15], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_Masters <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[16], y = names(model1dNonRural$model)[1])) + geom_point(colour="khaki", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[16], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_Professional <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[17], y = names(model1dNonRural$model)[1])) + geom_point(colour="tomato", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[17], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dNonRural_Doctorate <- ggplot(model1dNonRural$model, aes_string(x = names(model1dNonRural$model)[18], y = names(model1dNonRural$model)[1])) + geom_point(colour="thistle", alpha = 0.1) + geom_abline(intercept = coef(model1dNonRural)[1], slope = coef(model1dNonRural)[18], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1dNonRural_average_income, model1dNonRural_median_housing_price, model1dNonRural_urban, model1dNonRural_black, model1dNonRural_hispanic, model1dNonRural_asian, model1dNonRural_na, model1dNonRural_pi, model1dNonRural_2plus, model1dNonRural_other, model1dNonRural_HSdiploma, model1dNonRural_someCollege, model1dNonRural_Associates, model1dNonRural_Bachelors, model1dNonRural_Masters, model1dNonRural_Professional, model1dNonRural_Doctorate, nrow=4, top="Access to protected land with all variables accounted for except rural areas")


model1dUrban <- lm(ldist ~ average_income + median_housing_price + share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other + HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLandUrban)
summary(model1dUrban)

model1dUrban_average_income <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[2], y = names(model1dUrban$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[2], colour="black", size=1)
model1dUrban_median_housing_price <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[3], y = names(model1dUrban$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[3], colour="black", size=1)
model1dUrban_black <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[4], y = names(model1dUrban$model)[1])) + geom_point(colour="cadetblue", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_hispanic <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[5], y = names(model1dUrban$model)[1])) + geom_point(colour="burlywood", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_asian <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[6], y = names(model1dUrban$model)[1])) + geom_point(colour="brown2", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_na <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[7], y = names(model1dUrban$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_pi <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[8], y = names(model1dUrban$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_2plus <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[9], y = names(model1dUrban$model)[1])) + geom_point(colour="bisque3", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_other <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[10], y = names(model1dUrban$model)[1])) + geom_point(colour="aquamarine3", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_HSdiploma <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[11], y = names(model1dUrban$model)[1])) + geom_point(colour="dodgerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_someCollege <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[12], y = names(model1dUrban$model)[1])) + geom_point(colour="goldenrod1", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_Associates <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[13], y = names(model1dUrban$model)[1])) + geom_point(colour="forestgreen", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[13], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_Bachelors <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[14], y = names(model1dUrban$model)[1])) + geom_point(colour="indianred1", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[14], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_Masters <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[15], y = names(model1dUrban$model)[1])) + geom_point(colour="khaki", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[15], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_Professional <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[16], y = names(model1dUrban$model)[1])) + geom_point(colour="tomato", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[16], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dUrban_Doctorate <- ggplot(model1dUrban$model, aes_string(x = names(model1dUrban$model)[17], y = names(model1dUrban$model)[1])) + geom_point(colour="thistle", alpha = 0.1) + geom_abline(intercept = coef(model1dUrban)[1], slope = coef(model1dUrban)[17], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1dUrban_average_income, model1dUrban_median_housing_price, model1dUrban_black, model1dUrban_hispanic, model1dUrban_asian, model1dUrban_na, model1dUrban_pi, model1dUrban_2plus, model1dUrban_other, model1dUrban_HSdiploma, model1dUrban_someCollege, model1dUrban_Associates, model1dUrban_Bachelors, model1dUrban_Masters, model1dUrban_Professional, model1dUrban_Doctorate, nrow=4, top="Access to protected land with all variables accounted for in urban areas")


model1dSuburban <- lm(ldist ~ average_income + median_housing_price + share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other + HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLandSuburban)
summary(model1dSuburban)

model1dSuburban_average_income <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[2], y = names(model1dSuburban$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[2], colour="black", size=1)
model1dSuburban_median_housing_price <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[3], y = names(model1dSuburban$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[3], colour="black", size=1)
model1dSuburban_black <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[4], y = names(model1dSuburban$model)[1])) + geom_point(colour="cadetblue", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_hispanic <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[5], y = names(model1dSuburban$model)[1])) + geom_point(colour="burlywood", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_asian <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[6], y = names(model1dSuburban$model)[1])) + geom_point(colour="brown2", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_na <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[7], y = names(model1dSuburban$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_pi <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[8], y = names(model1dSuburban$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_2plus <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[9], y = names(model1dSuburban$model)[1])) + geom_point(colour="bisque3", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_other <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[10], y = names(model1dSuburban$model)[1])) + geom_point(colour="aquamarine3", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_HSdiploma <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[11], y = names(model1dSuburban$model)[1])) + geom_point(colour="dodgerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_someCollege <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[12], y = names(model1dSuburban$model)[1])) + geom_point(colour="goldenrod1", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_Associates <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[13], y = names(model1dSuburban$model)[1])) + geom_point(colour="forestgreen", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[13], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_Bachelors <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[14], y = names(model1dSuburban$model)[1])) + geom_point(colour="indianred1", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[14], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_Masters <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[15], y = names(model1dSuburban$model)[1])) + geom_point(colour="khaki", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[15], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_Professional <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[16], y = names(model1dSuburban$model)[1])) + geom_point(colour="tomato", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[16], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dSuburban_Doctorate <- ggplot(model1dSuburban$model, aes_string(x = names(model1dSuburban$model)[17], y = names(model1dSuburban$model)[1])) + geom_point(colour="thistle", alpha = 0.1) + geom_abline(intercept = coef(model1dSuburban)[1], slope = coef(model1dSuburban)[17], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1dSuburban_average_income, model1dSuburban_median_housing_price, model1dSuburban_black, model1dSuburban_hispanic, model1dSuburban_asian, model1dSuburban_na, model1dSuburban_pi, model1dSuburban_2plus, model1dSuburban_other, model1dSuburban_HSdiploma, model1dSuburban_someCollege, model1dSuburban_Associates, model1dSuburban_Bachelors, model1dSuburban_Masters, model1dSuburban_Professional, model1dSuburban_Doctorate, nrow=4, top="Access to protected land with all variables accounted for in suburban areas")


model1dRural <- lm(ldist ~ average_income + median_housing_price + share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other + HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLandRural)
summary(model1dRural)

model1dRural_average_income <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[2], y = names(model1dRural$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[2], colour="black", size=1)
model1dRural_median_housing_price <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[3], y = names(model1dRural$model)[1])) + geom_point(colour="coral", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[3], colour="black", size=1)
model1dRural_black <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[4], y = names(model1dRural$model)[1])) + geom_point(colour="cadetblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_hispanic <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[5], y = names(model1dRural$model)[1])) + geom_point(colour="burlywood", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_asian <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[6], y = names(model1dRural$model)[1])) + geom_point(colour="brown2", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_na <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[7], y = names(model1dRural$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_pi <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[8], y = names(model1dRural$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_2plus <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[9], y = names(model1dRural$model)[1])) + geom_point(colour="bisque3", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[9], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_other <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[10], y = names(model1dRural$model)[1])) + geom_point(colour="aquamarine3", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[10], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_HSdiploma <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[11], y = names(model1dRural$model)[1])) + geom_point(colour="dodgerblue", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[11], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_someCollege <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[12], y = names(model1dRural$model)[1])) + geom_point(colour="goldenrod1", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[12], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_Associates <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[13], y = names(model1dRural$model)[1])) + geom_point(colour="forestgreen", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[13], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_Bachelors <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[14], y = names(model1dRural$model)[1])) + geom_point(colour="indianred1", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[14], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_Masters <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[15], y = names(model1dRural$model)[1])) + geom_point(colour="khaki", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[15], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_Professional <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[16], y = names(model1dRural$model)[1])) + geom_point(colour="tomato", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[16], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model1dRural_Doctorate <- ggplot(model1dRural$model, aes_string(x = names(model1dRural$model)[17], y = names(model1dRural$model)[1])) + geom_point(colour="thistle", alpha = 0.1) + geom_abline(intercept = coef(model1dRural)[1], slope = coef(model1dRural)[17], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model1dRural_average_income, model1dRural_median_housing_price, model1dRural_black, model1dRural_hispanic, model1dRural_asian, model1dRural_na, model1dRural_pi, model1dRural_2plus, model1dRural_other, model1dRural_HSdiploma, model1dRural_someCollege, model1dRural_Associates, model1dRural_Bachelors, model1dRural_Masters, model1dRural_Professional, model1dRural_Doctorate, nrow=4, top="Access to protected land with all variables accounted for in rural areas")

print("Looking at non-rural areas (urban and suburban combined), we see that a combination of median housing price and average income, when taken with appropriate magnitudes, would oftentimes indicate increased access for higher-income communities (overall negative effect on distance) over lower-income communities. We also see mostly negative coefficients (decreased distance and thus increased access) for more educated communities, compared to mostly positive coefficients (increased distance and thus decreased access) for less educated communities. Therefore, while we cannot establish racial inequality in access to protected lands in urban and suburban areas, we can reasonably state that there is income and education-related inequality.")


# 2. If there is inequality, what might reduce it?
print("2. Since protected land areas are commonly established through local and state government groups, departments, or officials, we will investigate the relationship between voter participation, party affiliation, and protected land areas.")
ProtectedLand$registration <- ProtectedLand$Registered/ProtectedLand$voting_age_pop
ProtectedLand$participation <- ProtectedLand$Voted/ProtectedLand$voting_age_pop

ProtectedLandNonRural$registration <- ProtectedLandNonRural$Registered/ProtectedLandNonRural$voting_age_pop
ProtectedLandNonRural$participation <- ProtectedLandNonRural$Voted/ProtectedLandNonRural$voting_age_pop

# 2a. Does access increase with voter registration?
model2a <- lm(ldist ~ registration, ProtectedLandNonRural)
summary(model2a)
print("2a. The coefficient indicates that in non-rural California, as voter registration (proportion of voting age population that is registered to vote) increases, distance to protected land decreases (increased access).")

ggplot(ProtectedLandNonRural, aes(x=registration,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Distance to protected land decreases as voter registration increases") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 1.5, y = 2.5, label = paste("Slope =", round(coef(model2a)[2],5), "\nP =", round(summary(model2a)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2a)$adj.r.squared,7)), colour="red")


# 2b. Does access increase with voter participation?
model2b <- lm(ldist ~ participation, ProtectedLandNonRural)
summary(model2b)
print("2b. This coefficient indicates that in non-rural California, as voter participation (proportion of voting age population that voted in the last election) increases, distance to protected land also increases (decreased access).")

ggplot(ProtectedLandNonRural, aes(x=participation,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Distance to protected land increases as voter participation increases") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 1, y = 2.5, label = paste("Slope =", round(coef(model2b)[2],5), "\nP =", round(summary(model2b)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2b)$adj.r.squared,7)), colour="red")


model2bi <- lm(ldist ~ registration + participation, ProtectedLandNonRural)
summary(model2bi)
print("This model shows the same coefficient signs from previous models at high levels of significance, indicating that higher voter registration and lower voter participation would be associated with increased access to protected land areas. Speculatively, this may indicate that voter engagement (interacting with local officials and politics in ways other than voting) may impact local protected areas more so than voter just participation.")

model2bi_registration <- ggplot(model2bi$model, aes_string(x = names(model2bi$model)[2], y = names(model2bi$model)[1])) + geom_point(colour="gold", alpha = 0.05) + geom_abline(intercept = coef(model2bi)[1], slope = coef(model2bi)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,2))
model2bi_participation <- ggplot(model2bi$model, aes_string(x = names(model2bi$model)[3], y = names(model2bi$model)[1])) + geom_point(colour="skyblue", alpha = 0.05) + geom_abline(intercept = coef(model2bi)[1], slope = coef(model2bi)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,2))
grid.arrange(model2bi_registration,model2bi_participation,nrow=2, top="Increased registration and less participation show an decreased distance to protected land")


# 2c. Does access depend on county-wide party affiliation?
model2c <- lm(ldist ~ Ratio.Dem.Rep, ProtectedLand)
summary(model2c)
print("2c. The variable Ratio.Dem.Rep represents the ratio of the proportion of registered Democrats to the proportion of registered Republicans at the county level. This model indicates that as the ratio increases by county (more registered Democrats compared to registered Republicans), the distance to protected land decreases for census tracts in that county.")

ggplot(ProtectedLand, aes(x=Ratio.Dem.Rep,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Distance to protected land decreases with increased registered Democrats") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 4.5, y = 2.5, label = paste("Slope =", round(coef(model2c)[2],5), "\nP =", round(summary(model2c)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2c)$adj.r.squared,7)), colour="red")


# 2ci. If there is a relationship, does it still exist when considering population density classification (rural, suburban, urban)?
model2ciNonRural <- lm(ldist ~ Ratio.Dem.Rep, ProtectedLandNonRural)
summary(model2ciNonRural)
model2ciUrban <- lm(ldist ~ Ratio.Dem.Rep, ProtectedLandUrban)
summary(model2ciUrban)
model2ciSuburban <- lm(ldist ~ Ratio.Dem.Rep, ProtectedLandSuburban)
summary(model2ciSuburban)
model2ciRural <- lm(ldist ~ Ratio.Dem.Rep, ProtectedLandRural)
summary(model2ciRural)
print("2ci. This relationship persists through all population density segments we have examined in this data set.")

model2ciNonRuralPlot <- ggplot(ProtectedLandNonRural, aes(x=Ratio.Dem.Rep,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + xlab("Ratio.Dem.Rep (Nonrural)")
model2ciUrbanPlot <- ggplot(ProtectedLandUrban, aes(x=Ratio.Dem.Rep,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + xlab("Ratio.Dem.Rep (Urban)")
model2ciSuburbanPlot <- ggplot(ProtectedLandSuburban, aes(x=Ratio.Dem.Rep,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + xlab("Ratio.Dem.Rep (Suburban)")
model2ciRuralPlot <- ggplot(model2ciRural, aes(x=Ratio.Dem.Rep,y=ldist)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + xlab("Ratio.Dem.Rep (Rural)")
grid.arrange(model2ciNonRuralPlot,model2ciUrbanPlot,model2ciSuburbanPlot,model2ciRuralPlot,nrow=2, top="Irrespective of population density classification, distance to protected land decreases with increased registered Democrats")

# 2d. Are there relationships between voter participation/registration and other demographic characteristics?
# 2di. Race/ethnicity?
model2diReg <- lm(registration ~ share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other, ProtectedLandNonRural)
summary(model2diReg)
model2diPart <- lm(participation ~ share_black + share_hispanic + share_asian + share_native_american + share_pacific_islander + share_2plus + share_other, ProtectedLandNonRural)
summary(model2diPart)
print("2di. All minority populations show decreased voter registration and decreased voter participation compared to white populations in non-rural counties of California.")

model2diReg_black <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[2], y = names(model2diReg$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diReg_hispanic <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[3], y = names(model2diReg$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diReg_asian <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[4], y = names(model2diReg$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diReg_na <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[5], y = names(model2diReg$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diReg_pi <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[6], y = names(model2diReg$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diReg_2plus <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[7], y = names(model2diReg$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diReg_other <- ggplot(model2diReg$model, aes_string(x = names(model2diReg$model)[8], y = names(model2diReg$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model2diReg)[1], slope = coef(model2diReg)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model2diReg_black, model2diReg_hispanic, model2diReg_asian, model2diReg_na, model2diReg_pi, model2diReg_2plus, model2diReg_other, nrow=4, top="All minority populations show decreased voter registration compared to white populations")

model2diPart_black <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[2], y = names(model2diPart$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diPart_hispanic <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[3], y = names(model2diPart$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diPart_asian <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[4], y = names(model2diPart$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diPart_na <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[5], y = names(model2diPart$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diPart_pi <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[6], y = names(model2diPart$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diPart_2plus <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[7], y = names(model2diPart$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diPart_other <- ggplot(model2diPart$model, aes_string(x = names(model2diPart$model)[8], y = names(model2diPart$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model2diPart)[1], slope = coef(model2diPart)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model2diPart_black, model2diPart_hispanic, model2diPart_asian, model2diPart_na, model2diPart_pi, model2diPart_2plus, model2diPart_other, nrow=4, top="All minority populations show decreased voter participation compared to white populations")


# 2dii. Education?
model2diiReg <- lm(registration ~ HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLandNonRural)
summary(model2diiReg)
model2diiPart <- lm(participation ~ HSdiploma + someCollege + Associates + Bachelors + Masters + Professional + Doctorate, ProtectedLandNonRural)
summary(model2diiPart)
print("2dii. As education level increases, so does voter registration and voter turnout, except for communities with higher proportions of Doctorates, where we see turnout and participation levels much lower than all other education levels.")

model2diiReg_HSdiploma <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[2], y = names(model2diiReg$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiReg_someCollege <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[3], y = names(model2diiReg$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiReg_Associates <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[4], y = names(model2diiReg$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiReg_Bachelors <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[5], y = names(model2diiReg$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiReg_Masters <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[6], y = names(model2diiReg$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiReg_Professional <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[7], y = names(model2diiReg$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiReg_Doctorate <- ggplot(model2diiReg$model, aes_string(x = names(model2diiReg$model)[8], y = names(model2diiReg$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model2diiReg)[1], slope = coef(model2diiReg)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model2diiReg_HSdiploma,model2diiReg_someCollege,model2diiReg_Associates,model2diiReg_Bachelors,model2diiReg_Masters,model2diiReg_Professional,model2diiReg_Doctorate, nrow=4, top="Higher levels of education typically lead to higher levels of voter registration")

model2diiPart_HSdiploma <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[2], y = names(model2diiPart$model)[1])) + geom_point(colour="pink", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[2], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiPart_someCollege <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[3], y = names(model2diiPart$model)[1])) + geom_point(colour="cornflowerblue", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[3], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiPart_Associates <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[4], y = names(model2diiPart$model)[1])) + geom_point(colour="chocolate", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[4], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiPart_Bachelors <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[5], y = names(model2diiPart$model)[1])) + geom_point(colour="slateblue", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[5], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiPart_Masters <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[6], y = names(model2diiPart$model)[1])) + geom_point(colour="firebrick", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[6], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiPart_Professional <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[7], y = names(model2diiPart$model)[1])) + geom_point(colour="lightblue", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[7], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
model2diiPart_Doctorate <- ggplot(model2diiPart$model, aes_string(x = names(model2diiPart$model)[8], y = names(model2diiPart$model)[1])) + geom_point(colour="springgreen4", alpha = 0.1) + geom_abline(intercept = coef(model2diiPart)[1], slope = coef(model2diiPart)[8], colour="black", size=1) + coord_cartesian(xlim=c(0,1))
grid.arrange(model2diiPart_HSdiploma,model2diiPart_someCollege,model2diiPart_Associates,model2diiPart_Bachelors,model2diiPart_Masters,model2diiPart_Professional,model2diiPart_Doctorate, nrow=4, top="Higher levels of education typically lead to higher levels of voter participation")


# 2diii. Income?
model2diiiReg <- lm(registration ~ average_income, ProtectedLandNonRural)
summary(model2diiiReg)
model2diiiPart <- lm(participation ~ average_income, ProtectedLandNonRural)
summary(model2diiiPart)
print("2diii. As income increases, so does voter registration and voter participation.")

ggplot(ProtectedLandNonRural, aes(x=average_income,y=registration)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Increased average income shows increased voter registration") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 100000, y = 1.5, label = paste("Slope =", coef(model2diiiReg)[2], "\nP =", round(summary(model2diiiReg)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2diiiReg)$adj.r.squared,7)), colour="red")
ggplot(ProtectedLandNonRural, aes(x=average_income,y=participation)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Increased average income shows increased voter participation") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 100000, y = 1.1, label = paste("Slope =", coef(model2diiiPart)[2], "\nP =", round(summary(model2diiiPart)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2diiiPart)$adj.r.squared,7)), colour="red")

# 2div. Housing price?
model2divReg <- lm(registration ~ median_housing_price, ProtectedLandNonRural)
summary(model2divReg)
model2divPart <- lm(participation ~ median_housing_price, ProtectedLandNonRural)
summary(model2divPart)
print("2div. As median housing price increases, so does voter registration and voter participation.")

ggplot(ProtectedLandNonRural, aes(x=median_housing_price,y=registration)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Increased median housing price shows increased voter registration") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 750000, y = 1.5, label = paste("Slope =", coef(model2divReg)[2], "\nP =", round(summary(model2divReg)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2divReg)$adj.r.squared,7)), colour="red")
ggplot(ProtectedLandNonRural, aes(x=median_housing_price,y=participation)) + geom_point(alpha = 0.05) + geom_smooth(method="lm") + labs(title="Increased median housing price shows increased voter participation") + theme(plot.title = element_text(hjust = 0.5), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + annotate("text", x = 750000, y = 1, label = paste("Slope =", coef(model2divPart)[2], "\nP =", round(summary(model2divPart)$coef[2,4],4), "\nAdj. R2 = ", round(summary(model2divPart)$adj.r.squared,7)), colour="red")

print("Voter registration, a significant indicator of access to protected land areas by census tract, is reduced in certain communities of color, less educated communities, and lower income communities. Voter participation is similarly impacted in disadvantaged communities, but we do not see increased voter participation have the same association with access. With this knowledge, and without direct intervention to designate protected land areas in disadvantaged communities, we would suggest focusing on increasing voter registration in those communities as a gateway for increased voter engagement. That being said, direct intervention may be necessary, as disadvantaged communities often have more difficulty engaging with politics (lack of flexible working hours, inaccessibility to engagement opportunities).")

# 3. Can we use a small set of variables to identify non-rural census tracts where voter engagement (registration) is low and efforts to improve it would be most beneficial to the community?
model3Data <- na.omit(ProtectedLandNonRural)
randIndex <- sample(1:dim(model3Data)[1])
cutpoint <- floor(2*dim(model3Data)[1]/3)
trainData <- model3Data[randIndex[1:cutpoint],]
testData <- model3Data[randIndex[(cutpoint+1):dim(model3Data)[1]],]

trainData$lowReg[trainData$registration < mean(model3Data$registration)] <- 1
trainData$lowReg[trainData$registration >= mean(model3Data$registration)] <- 0
testData$lowReg[testData$registration < mean(model3Data$registration)] <- 1
testData$lowReg[testData$registration >= mean(model3Data$registration)] <- 0

trainData$lowReg <- as.factor(trainData$lowReg)
testData$lowReg <- as.factor(testData$lowReg)

model3 <- ksvm(lowReg ~ median_housing_price + average_income + lessthanHS + Ratio.Dem.Rep, data = trainData, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
model3

model3Pred <- predict(model3, testData)

testComp <- data.frame(testData, model3Pred)
correctPercent <- length(which(testComp$lowReg == testComp$model3Pred))/length(testComp$lowReg)
correctPercent

testComp$PredWrong[testComp$lowReg == testComp$model3Pred] <- 0
testComp$PredWrong[testComp$lowReg != testComp$model3Pred] <- 1

testComp$PredWrong <- as.factor(testComp$PredWrong)

model3plot <- ggplot(testComp) + geom_point(aes(y=average_income, x=lessthanHS, size=PredWrong, color=lowReg, shape=model3Pred)) + labs(title="Identifying non-rural areas where voter registration is low") + theme(plot.title = element_text(hjust = 0.5))
model3plot
print("Using ksvm, we can use the data we have available to identify non-rural census tracts where voter engagement (registration) is low. Efforts to improve it would be most beneficial to the community and improve access to protected lands.")
