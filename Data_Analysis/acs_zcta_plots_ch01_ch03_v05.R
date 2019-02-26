#####################################################################
#
# this program takes the summary data created
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_census_zcta_ch01_ch03_v04.py
# and tries to make plots from it
# for presentation purposes
#
# february 24, 2019
#
# acs_zcta_2012_2016_plots.R
#
##############################################################################
# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(RColorBrewer)
library(sf)

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

# data and output directories
data_dir <- paste0("/Data_Collection/")
out_dir <- paste0("/Graphs/")


# load the data
acs_zcta_2012_2016 <- read.csv(paste0(data_dir,"20190123_acs_zcta_2012_2016.csv"),stringsAsFactors = F)


head(acs_zcta_2012_2016)



zctas <- st_read(paste0(groupDir,"/maps/united_states/census2010/zcta/2017_census_zcta_shapefile"),
                 layer="cb_2017_us_zcta510_500k",stringsAsFactors = F)

names(zctas)[1] <- 'ZCTA'

zcta_2016 <- merge(zctas,acs_zcta_2012_2016,by='ZCTA')

# clean up the column names having "X" or "." in the begining or "." in the end
# colnames(zcta_2016) <- gsub("^X", "",  colnames(zcta_2016))
# colnames(zcta_2016) <- gsub("^\\.", "",  colnames(zcta_2016))
# colnames(zcta_2016) <- gsub("\\.$", "",  colnames(zcta_2016))

head(zcta_2016)


data <- zcta_2016

st_geometry(data) <- NULL



data %>% select(matches('^B25127_8_|B25127_15_|B25127_51_|B25127_58_')) %>% head(2)


data %>% select(matches('^B25127_18_|B25127_25_|B25127_32_|B25127_39_|B25127_61_|B25127_68_|B25127_75_|B25127_82_')) %>% head(2)


data <- data %>% mutate('total_single_family_households_since_2000'= 
                          select(.,matches('^B25127_4_|B25127_11_|B25127_47_|B25127_54_')) %>% apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_single_family_households_before_2000'=
        select(.,matches('^B25127_18_|B25127_25_|B25127_32_|B25127_39_|B25127_61_|B25127_68_|B25127_75_|B25127_82_')) %>% apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_2_to_4_family_households_since_2000'=select(.,matches('^B25127_5_|B25127_12_|B25127_48_|B25127_55_')) %>% 
                          apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_2_to_4_family_households_before_2000'=
        select(.,matches('^B25127_19_|B25127_26_|B25127_33_|B25127_40_|B25127_62_|B25127_69_|B25127_76_|B25127_83_')) %>% apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_5_to_19_family_households_since_2000'=select(.,matches('^B25127_6_|B25127_13_|B25127_49_|B25127_56_')) %>% 
                          apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_5_to_19_family_households_before_2000'=
        select(.,matches('^B25127_20_|B25127_27_|B25127_34_|B25127_41_|B25127_63_|B25127_70_|B25127_77_|B25127_84_')) %>% apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_20_to_49_family_households_since_2000'=select(.,matches('^B25127_7_|B25127_14_|B25127_50_|B25127_57_')) %>% 
                          apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_20_to_49_family_households_before_2000'=
        select(.,matches('^B25127_21_|B25127_28_|B25127_35_|B25127_42_|B25127_64_|B25127_71_|B25127_78_|B25127_85_')) %>% apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_50_or_more_family_households_since_2000'=select(.,matches('^B25127_8_|B25127_15_|B25127_51_|B25127_58_')) %>% 
                          apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_50_or_more_family_households_before_2000'=
      select(.,matches('^B25127_22_|B25127_29_|B25127_36_|B25127_43_|B25127_65_|B25127_72_|B25127_79_|B25127_86_')) %>% apply(1, sum, na.rm=TRUE))




#data %>% select(matches('*since_2000$')) %>% head(2)

data %>% select(matches('*before_2000$')) %>% head(2)

data <- data %>% mutate('new_constructions'=select(.,matches('*since_2000$'))
                        %>% apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('existing_constructions'=select(.,matches('*before_2000$'))
                        %>% apply(1, sum, na.rm=TRUE))


#data <- data %>% mutate('new_existing_constructions_ratio'=round(new_constructions/existing_constructions,2)*100)

data <- data %>% mutate('new_existing_constructions_ratio'=round(new_constructions/existing_constructions,2))
#data %>% select(matches('^total_single_family*')) %>% head(2)

data$new_existing_constructions_ratio <-  ifelse(is.na(data$new_existing_constructions_ratio)==TRUE,-1,data$new_existing_constructions_ratio)

data <- data %>% mutate('total_single_family_households'=select(.,matches('^total_single_family*'))
                        %>% apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('share_single_family_new_constructions'=round(total_single_family_households_since_2000/new_constructions,2))

data$share_single_family_new_constructions <-  ifelse(is.na(data$share_single_family_new_constructions)==TRUE,-1,
                                                             data$share_single_family_new_constructions)




print(head(data[,c(242,243,253)]))


zcta_2016$new_constructions <- data$new_constructions

zcta_2016$existing_constructions <- data$existing_constructions

zcta_2016$total_single_family_households_since_2000 <- data$total_single_family_households_since_2000

zcta_2016$total_single_family_households <- data$total_single_family_households

zcta_2016$new_existing_constructions_ratio <- data$new_existing_constructions_ratio

zcta_2016$share_single_family_new_constructions <- data$share_single_family_new_constructions



head(zcta_2016)

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- stringr::str_pad(x,3,side ="left",pad="0")
  return(y)
}

# apply the function to get countyfips column as length 3
zcta_2016[['COUNTY']] <- sapply(zcta_2016[["COUNTY"]], padzero)

zcta_2016 <- zcta_2016 %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                                       (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                                     ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                                              (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                                            "Exurban")))


zcta_2016[,c(240:249)]


