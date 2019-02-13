#####################################################################

# this program takes the summary data created in
# /Data_Collection/download_acs_county_data.py
# and performs analysis on it to produce required graphs
# for presentation purposes

# Januray 26, 2019

# acs_county_plots_ch01_ch03_v03.R
##############################################################################
##############################################################################
# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(RColorBrewer)
library(sf)
library(stringr)

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

# data and output directories
data_dir <- paste0("/Data_Collection/")
out_dir <- paste0("/Graphs/")


# load the data
acs_cnt_2007_2011 <- read.csv(paste0(data_dir,"20190125_cnty_acs_2007_2011_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2008_2012 <- read.csv(paste0(data_dir,"20190125_cnty_acs_2008_2012_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2009_2013 <- read.csv(paste0(data_dir,"20190125_cnty_acs_2009_2013_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2010_2014 <- read.csv(paste0(data_dir,"20190125_cnty_acs_2010_2014_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2011_2015 <- read.csv(paste0(data_dir,"20190125_cnty_acs_2011_2015_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,"20190125_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)

#df <- acs_cnt_2012_2016

# write a function to reformat the data to bring it to county level and subset it for requried columns
reformat_subset_data <- function(df) {
  
  # transpose the data to show attributes as per county level
  data <- as.data.frame(t(df))
  
  # remove unnecessary columns
  data <- data[-c(1,2),]
  
  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index
  
  # print the df to check if the format is appropriate
  print(head(data))
  

  #names of columns in data frame
  cols <- colnames(data)
  
  # character variables
  cols.char <- c("FILEID","FILETYPE","STATE","GEO_NAME","GEO_ID")
  
  #numeric variables
  cols.num <- cols[!cols %in% cols.char]
  
  
  
  # write a function to convert the required columns to numeric
  make_num <- function(x)
  {
    return(as.numeric(as.character(x)))
  }
  
  # make all the required columns numeric
  data[cols.num] <- lapply(data[cols.num],make_num)
  
  # print the dataframe to check the data types
  print(str(data))
  
  # create column state country code
  data["state_county_code"] <- rownames(data)
  
  # split the column GEO NAME to extract county and state name
  data <- as.data.frame(cSplit(data,c("GEO_NAME"),',',drop = F))
  
  # split the column country code to get state and county codes
  data <- as.data.frame(cSplit(data,c("state_county_code"),'_',drop = F,type.convert = FALSE))
  
  # rename the splitted columns
  names(data)[names(data)=="GEO_NAME_1"] <- "county_name"
  names(data)[names(data)=="GEO_NAME_2"] <- "state_name"
  names(data)[names(data)=="state_county_code_1"] <- "state_code"
  names(data)[names(data)=="state_county_code_2"] <- "county_code"
  
  data$FILETYPE <- as.character(data$FILETYPE)
  
  # get the year column
  data <- data %>% mutate(year=substr(FILETYPE,1,4))
  
  
  #data %>% select(matches('^B11003_7_|B11003_14_|B11003_20_')) %>% head(2)
  
  # get the percent of families with children
  data <- data %>% mutate('family_with_no_childern'=select(.,matches('B11003_7_|B11003_14_|B11003_20_')) %>%
                            apply(1, sum, na.rm=TRUE)) %>%
    mutate("percent_families_with_children"=round((B11003_1_total-family_with_no_childern)/B11003_1_total*100,2))
  

  print(unique(data$year))
  
  # get different type of family households (single, 2 to 4 units)
  # because 2015 and 2016 have addintional column of Built 2010 or later we need to put it in else loop
  if(unique(data$year)<2015){
    data <- data %>% mutate('total_single_family_households_since_2000'= 
                     select(.,matches('^B25127_4_|B25127_40_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_single_family_households_before_2000'=
        select(.,matches('^B25127_11_|B25127_18_|B25127_25_|B25127_32_|B25127_47_|B25127_54_|B25127_61_|B25127_68_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_2_to_4_family_households_since_2000'=select(.,matches('^B25127_5_|B25127_41_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_2_to_4_family_households_before_2000'=
                     select(.,matches('^B25127_12_|B25127_19_|B25127_26_|B25127_33_|B25127_48_|B25127_55_|B25127_62_|B25127_69_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_5_to_19_family_households_since_2000'=select(.,matches('^B25127_6_|B25127_42_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_5_to_19_family_households_before_2000'=
       select(.,matches('^B25127_13_|B25127_20_|B25127_27_|B25127_34_|B25127_49_|B25127_56_|B25127_63_|B25127_70_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_since_2000'=select(.,matches('^B25127_7_|B25127_43_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_before_2000'=
                     select(.,matches('^B25127_14_|B25127_21_|B25127_28_|B25127_35_|B25127_50_|B25127_57_|B25127_64_|B25127_71_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_since_2000'=select(.,matches('^B25127_8_|B25127_44_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_before_2000'=
                     select(.,matches('^B25127_15_|B25127_22_|B25127_29_|B25127_36_|B25127_51_|B25127_58_|B25127_65_|B25127_72_')) %>% apply(1, sum, na.rm=TRUE))
    
    
  }else{
    ## for year 2015-2016
    
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
    
    
  }
  
  
  
  # select column with household income
  data_median_col <- data %>% select(contains("B19013_1"))
  
  # get name of household income column
  median_col_name <- colnames(data_median_col)
  
  print(median_col_name)
  
  # rename the median household income to get consistent name across years
  colnames(data)[which(colnames(data) == median_col_name)] <- 'median_household_income'
  
  # rename other columns to later merge with 1950-2010 data
  colnames(data)[which(colnames(data) == "B01003_1_total")] <- 'total_population'
  colnames(data)[which(colnames(data) == "B25001_1_total")] <- 'total_housing_units'
  colnames(data)[which(colnames(data) == "B25077_1_median_value_(dollars)")] <- 'median_housing_value'
  colnames(data)[which(colnames(data) == "B25003_2_owner_occupied")] <- 'total_owner_occupied'
  colnames(data)[which(colnames(data) == "B25003_3_renter_occupied")] <- 'total_renter_occupied'
  
  # subset for all the relevant columns
  data_subset <- data %>% select(STATE_FIPS, county_code, year, total_population, total_housing_units, median_housing_value,
                                 median_household_income, percent_families_with_children, county_name, state_name, state_code,
                                 total_single_family_households_since_2000,total_single_family_households_before_2000,
                                 total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
                                 total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
                                 total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
                                 total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000,
                                 total_owner_occupied,total_renter_occupied)
  
 return (data_subset)

}

# get the data for all years 2011 to 2016 with relevant columns by calling the function
acs_cnt_2007_2011_subset <- reformat_subset_data(acs_cnt_2007_2011)
acs_cnt_2008_2012_subset <- reformat_subset_data(acs_cnt_2008_2012)
acs_cnt_2009_2013_subset <- reformat_subset_data(acs_cnt_2009_2013)
acs_cnt_2010_2014_subset <- reformat_subset_data(acs_cnt_2010_2014)
acs_cnt_2011_2015_subset <- reformat_subset_data(acs_cnt_2011_2015)
acs_cnt_2012_2016_subset <- reformat_subset_data(acs_cnt_2012_2016)

# stack data for all the years together
acs_cnt_2011_2016 <- rbind(acs_cnt_2007_2011_subset,acs_cnt_2008_2012_subset, acs_cnt_2009_2013_subset, acs_cnt_2010_2014_subset,
                           acs_cnt_2011_2015_subset, acs_cnt_2012_2016_subset)



# load data for years before 2011
acs_cnt_1910_2010 <- read.csv(paste0(data_dir,"was_msas_1910_2010_20190115.csv"),stringsAsFactors = F)

colnames(acs_cnt_1910_2010)

# rename the columns
acs_cnt_1910_2010 <- plyr::rename(acs_cnt_1910_2010, c("statefips" = "statefips",
                                                       "countyfips" = "countyfips",
                                                       "year" = "year",
                                                       "cv1" = "total_population",
                                                       "cv2" = "foreign-born_population",
                                                       "cv3" = "total_black_pop",
                                                       "cv4" = "total_other_races_pop",
                                                       "cv5" = "num_of_manuf_establishments",
                                                       "cv6" = "avg_num_of_manufacturing_wage_earners",
                                                       "cv7" = "total_manufacturing_wages",
                                                       "cv8" = "males > 25, no yrs of school",
                                                       "cv9" = "males > 25, 1-4 years elem school",
                                                       "cv10" = "males > 25, 5-6 years elem school",
                                                       "cv11" = "males > 25, 7 years elem school",
                                                       "cv12" = "males > 25, 8 years elem school",
                                                       "cv13" = "males > 25, 1-3 years hs",
                                                       "cv14" = "males > 25, 4 years hs",
                                                       "cv15" = "males > 25, 1-3 years college",
                                                       "cv16" = "males > 25, 4 years college",
                                                       "cv17" = "males > 25, 5+ years college",
                                                       "cv18" = "females > 25, no yrs of school",
                                                       "cv19" = "females > 25, 1-4 years elem school",
                                                       "cv20" = "females > 25, 5-6 years elem school",
                                                       "cv21" = "females > 25, 7 years elem school",
                                                       "cv22" = "females > 25, 8 years elem school",
                                                       "cv23" = "females > 25, 1-3 years hs",
                                                       "cv24" = "females > 25, 4 years hs",
                                                       "cv25" = "females > 25, 1-3 years college",
                                                       "cv26" = "females > 25, 4 years college",
                                                       "cv27" = "females > 25, 5+ years college",
                                                       "cv28" = "total_housing_units",
                                                       "cv29" = "single_family_housing_units",
                                                       "cv30" = "median_housing_value",
                                                       "cv31" = "median_household_income",
                                                       "cv58" = "population_65+",
                                                       "cv59" = "employed, various defns by year",
                                                       "cv60" = "gini_coefficient",
                                                       "cv87" = "land area (sq mi, ccdb, 1950; sq m, 2010)"))

colnames(acs_cnt_1910_2010)

# subset for required years
acs_cnt_1950_2010 <- acs_cnt_1910_2010 %>% filter(year>1949)

# subset for required columns
acs_cnt_1950_2010_subset <- acs_cnt_1950_2010 %>% select(statefips, countyfips, year, total_population, total_housing_units,
                                                         median_housing_value, median_household_income)

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- str_pad(x,3,side ="left",pad="0")
  return(y)
}

# apply the function to get countyfips column as length 3
acs_cnt_1950_2010_subset[['countyfips']] <- sapply(acs_cnt_1950_2010_subset[["countyfips"]], padzero)

acs_cnt_1950_2010_subset$year <- as.character(acs_cnt_1950_2010_subset$year)

head(acs_cnt_1950_2010_subset)

# make column names same before stacking dataframes
colnames(acs_cnt_2011_2016)[1:7] <- colnames(acs_cnt_1950_2010_subset)[1:7]

# stack both dataframes to get pop and housing for year 1950-2016
acs_cnt_1950_2016 <- bind_rows(acs_cnt_1950_2010_subset, acs_cnt_2011_2016)

write.csv(acs_cnt_1950_2016,paste0(out_dir,dateo,"_acs_cnt_merged_data_1950_2016.csv"))

##################################### CHAPTER 1 ##############################################################

###################### P1.G1. Plot Housing Units Area Wise ###################################################

# make the area type column
acs_cnt_1950_2016 <- acs_cnt_1950_2016 %>% mutate("area_type"=ifelse(countyfips %in% c("001","013") & statefips %in% c("11","51"),"Urban",
                                                  ifelse(countyfips %in% c("033","031","059","600","610") & statefips %in% c("24","51"),"SubUrban",
                                                                            "ExUrban")))

# replace NA with values of county, state and state code within groups
acs_cnt_1950_2016 <- acs_cnt_1950_2016 %>% group_by(statefips, countyfips) %>% mutate(county_name=unique(county_name[!is.na(county_name)]),
                                                                                      state_name=unique(state_name[!is.na(state_name)]),
                                                                                      state_code=unique(state_code[!is.na(state_code)]))



# convert tibble to dataframe
acs_cnt_1950_2016 <- acs_cnt_1950_2016 %>% as.data.frame()

# filter for the relevant columns
acs_cnt_2016_subset <- acs_cnt_1950_2016 %>%
                                          filter(year=='2016') %>%
                            select(county_name,total_single_family_households_since_2000,total_single_family_households_before_2000,
                                          total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
                                          total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
                                          total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
                                          total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000,area_type) %>%
                                          as.data.frame()

# melt the dataframe
acs_cnt_2016_subset_melt <- melt(acs_cnt_2016_subset, id.var=c("county_name","area_type"))

# create a column unit_type to assign new or exsting for housing units
acs_cnt_2016_subset_melt <- acs_cnt_2016_subset_melt %>%
                                          mutate("unit_type" = ifelse(grepl('since_2000$', variable),"New","Existing"))


acs_cnt_2016_area_wise_new_ext_housing_melt <- acs_cnt_2016_subset_melt %>% 
                                                        group_by(county_name,area_type,unit_type) %>% 
                                                                  summarise("total_housing_units"=sum(value)) %>% as.data.frame()

p <- ggplot(acs_cnt_2016_area_wise_new_ext_housing_melt, aes(x = area_type, y = total_housing_units, fill = unit_type)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  labs(x = "area type", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.15),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=F), size=FALSE)



# make the barplot horizontal
p1 <- p + coord_flip() #+ scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))

# save the plot
ggsave(paste0(out_dir_ch01,"p1.g1_",dateo,"_acs_cnt_1950_2016_housing_units_area_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

###################### P1.G2. Plot Housing Units Area Wise - County Level ###################################################

# filter for new housing units
acs_cnt_2016_area_wise_new_housing_melt <- acs_cnt_2016_area_wise_new_ext_housing_melt %>% filter(unit_type=="New")

# plot the graph
p <- ggplot(acs_cnt_2016_area_wise_new_housing_melt, aes(x = area_type, y = total_housing_units, fill = county_name)) +
  geom_bar(stat = "identity")+
  #scale_y_continuous(labels = comma,  limits = c(0, 8000000), breaks = c(seq(0,8000000,2000000)))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "area type", y = "num of new housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.95,0.95),
        legend.justification = c(1,1),
        legend.text = element_text(size=15),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=5),reverse=F), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


# make the barplot horizontal
p1 <- p + coord_flip() #+ scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))

p1

# save the plot
ggsave(paste0(out_dir_ch01,"p1.g2_",dateo,"_acs_cnt_1950_2016_housing_units_after_2000_area_county_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


###################### P1.G3.1 Plot Housing Units Percent change from 200 to 2016 vs Median Housing Value 2000 ###################################################

# filter the data for years 2000 and 2016
acs_cnt_2000_2016_comp <- acs_cnt_1950_2016 %>% filter(year=="2000" | year=="2016")

# calculate the percent change from 2000
acs_cnt_2000_2016_comp_pct <- acs_cnt_2000_2016_comp %>%
  group_by(statefips, countyfips) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(housing_units_pct_change_from_2000=(total_housing_units/lag(total_housing_units)-1) * 100) %>%
  mutate(housing_units_pct_change_from_2000=round(housing_units_pct_change_from_2000,2))


# fill the NA with values for percent change from 2000
# (please note we are just doing it for filling the columns for year 2000 as we will subset it later for median housing value 2000 )
acs_cnt_2000_2016_comp_pct <- acs_cnt_2000_2016_comp_pct %>%
  group_by(statefips, countyfips) %>%
  mutate(housing_units_pct_change_from_2000=unique(housing_units_pct_change_from_2000[!is.na(housing_units_pct_change_from_2000)]))

# arrange by year and make tibble as dataframe
acs_cnt_2000_2016_comp_pct <- acs_cnt_2000_2016_comp_pct %>% arrange(year) %>% as.data.frame()


# plot the graph
p1 <- ggplot(data = subset(acs_cnt_2000_2016_comp_pct, year %in% c("2000")), aes(x = median_housing_value, y = housing_units_pct_change_from_2000)) +
  #geom_point(aes(color = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none")),
  #                fill = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none"))),size=7) +
  #geom_point(aes(color = factor(STATE)),shape = 16, size = 3)+
  geom_point(shape = 16, size = 5)+geom_text(aes(label=county_name),hjust=-0.1, vjust=0.15)+
  scale_x_continuous(labels = comma,limits= c(0, 300000), breaks = c(seq(0,300000,50000))) +
  #scale_y_continuous(limits= c(0, 30000), breaks = c(seq(0,30000,10000))) +
  labs(x = "median housing value 2000", y="% change in no. of housing units from 2000 to 2016")+
  #scale_color_manual(values = c("none" = damage_scale[4], "minimal" = damage_scale[3],"extensive" = damage_scale[2],"irreparable" = damage_scale[1]))+
  geom_abline(intercept = 0, slope = 0.4, color = "grey")+  # 45 degree line
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #legend.position = "right",
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.2),
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10)))

p1

# save the graph
ggsave(paste0(out_dir_ch01,"p1.g3.1_",dateo,"_acs_cnt_comp_2000_2016_scatter_house_median_value_percent_change_housing_units.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


###################### P1.G3.2 Plot Housing Units regional share vs Median Housing Value 2000 ###################################################

# calculate absolute change in housing units from 2000
acs_cnt_2000_2016_comp_abs <- acs_cnt_2000_2016_comp %>%
  group_by(statefips, countyfips) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(housing_units_abs_change_from_2000=(total_housing_units-lag(total_housing_units)))

# arrange by year and make tibble as dataframe
acs_cnt_2000_2016_comp_abs <- acs_cnt_2000_2016_comp_abs %>% arrange(year) %>% as.data.frame()

# calculate the proportion of housing units for each county in new housing units
acs_cnt_2000_2016_comp_abs <- acs_cnt_2000_2016_comp_abs %>%
  mutate(housing_units_abs_change_from_2000_prop=round(housing_units_abs_change_from_2000/sum(housing_units_abs_change_from_2000, na.rm = T)*100,2))

# fill NAs with the values
acs_cnt_2000_2016_comp_abs <- acs_cnt_2000_2016_comp_abs %>%
  group_by(statefips, countyfips) %>%
  mutate(housing_units_abs_change_from_2000_prop=unique(housing_units_abs_change_from_2000_prop[!is.na(housing_units_abs_change_from_2000_prop)]))

# arrange by year and make tibble as dataframe
acs_cnt_2000_2016_comp_abs <- acs_cnt_2000_2016_comp_abs %>% arrange(year) %>% as.data.frame()

# plot the graph
p1 <- ggplot(data = subset(acs_cnt_2000_2016_comp_abs, year %in% c("2000")), aes(x = median_housing_value, 
                                                                                 y = housing_units_abs_change_from_2000_prop)) +
  #geom_point(aes(color = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none")),
  #                fill = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none"))),size=7) +
  #geom_point(aes(color = factor(STATE)),shape = 16, size = 3)+
  geom_point(shape = 16, size = 5)+geom_text(aes(label=county_name),hjust=-0.1, vjust=0.15)+
  scale_x_continuous(labels = comma,limits= c(0, 300000), breaks = c(seq(0,300000,50000))) +
  #scale_y_continuous(limits= c(0, 30000), breaks = c(seq(0,30000,10000))) +
  labs(x = "median housing value 2000", y="% regional share in no. of housing units from 2000 to 2016")+
  #scale_color_manual(values = c("none" = damage_scale[4], "minimal" = damage_scale[3],"extensive" = damage_scale[2],"irreparable" = damage_scale[1]))+
  geom_abline(intercept = 0, slope = 0.4, color = "grey")+  # 45 degree line
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #legend.position = "right",
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.2),
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10)))

p1

# save the graph
ggsave(paste0(out_dir_ch01,"p1.g3.2_",dateo,"_acs_cnt_comp_2000_2016_scatter_house_median_value_regional_share_housing_units.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


##################################### CHAPTER 2 ##############################################################

###################### P2.G1 Plot Housing Units by unit type and count ###################################################

# filter for the relevant columns
acs_cnt_2016_new_ext_housing <- acs_cnt_1950_2016 %>%
  filter(year=='2016') %>%
  select(county_name,total_single_family_households_since_2000,total_single_family_households_before_2000,
         total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
         total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
         total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
         total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000) %>%
  as.data.frame()

# melt the dataframe
acs_cnt_2016_new_ext_housing_melt <- melt(acs_cnt_2016_new_ext_housing, id.var="county_name")

#https://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr/24821141

# create a column unit_type to assign new or exsting for housing units
#acs_cnt_2016_new_ext_housing_melt %>% filter(grepl('since_2000$', variable))

acs_cnt_2016_new_ext_housing_melt <- acs_cnt_2016_new_ext_housing_melt %>%
  mutate(Unit_Type = ifelse(grepl('since_2000$', variable),"New","Existing"))

# create a column unit_count to assign count for housing units
#acs_cnt_2016_new_ext_housing_melt %>% filter(grepl('*2_to_4*', variable))

acs_cnt_2016_new_ext_housing_melt <- acs_cnt_2016_new_ext_housing_melt %>%
  mutate("Units_Count"=ifelse(grepl('*single*', variable),"1 Unit",
                              ifelse(grepl('*2_to_4*', variable),"2 to 4 Units",ifelse(grepl('*5_to_19*', variable),"5 to 19 Units",
                              ifelse(grepl('*20_to_49*', variable),"20 to 49 Units","More than 50 Units")))))



# plot the graph
p <- ggplot(acs_cnt_2016_new_ext_housing_melt, aes(x = Unit_Type, y = value, fill = Units_Count)) +
  geom_bar(stat = "identity")+
  # scale_y_continuous(labels = comma,  limits = c(0, 12750000), breaks = c(seq(0,12750000,2000000)))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "unit type", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=F), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


# make the barplot horizontal
p1 <- p + coord_flip() #+ scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))

p1

# save the graph
ggsave(paste0(out_dir_ch02,"p2.g1_",dateo,"_acs_cnt_1950_2016_housing_units_by_unit_type_and_count.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


###################### P2.G2 Plot Bi directional Bar chart for new housing units and proportion of single family by county ###################################################

#https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr

# subset the data for New housing units
acs_cnt_2016_new_housing_melt <- acs_cnt_2016_new_ext_housing_melt %>% filter(Unit_Type=='New') %>% as.data.frame()

# fin the total new units for each county
acs_cnt_2016_new_housing_melt <- acs_cnt_2016_new_housing_melt %>%
  group_by(county_name) %>%
  mutate(total_new_units=sum(value))

# find the single family proportion
acs_cnt_2016_new_housing_melt <- acs_cnt_2016_new_housing_melt %>%
  filter(Units_Count=='1 Unit') %>%
  mutate(single_family_prop=round(value/total_new_units*100,2)) %>%
  as.data.frame()

# susbset for the relevant columns
d <- acs_cnt_2016_new_housing_melt %>%  select(county_name, total_new_units, single_family_prop)


# create a grid with labels for counties
g.mid<-ggplot(d,aes(x=1,y=county_name))+geom_text(aes(label=county_name))+
  geom_segment(aes(x=0.94,xend=0.96,yend=county_name))+
  geom_segment(aes(x=1.04,xend=1.065,yend=county_name))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g.mid

# plot the county vs single family proportion
g1 <- ggplot(data = d, aes(x = county_name, y = single_family_prop)) +
  geom_bar(stat = "identity") + ggtitle("Single Family Proportion") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

# plot the county vs total new units
g2 <- ggplot(data = d, aes(x = county_name, y = total_new_units)) +xlab(NULL)+
  geom_bar(stat = "identity") + ggtitle("Total New Housing Units") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()


g1
g2

# join the two graphs with the county label graph
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

gg.mid

p1 <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))

# save the graph
ggsave(paste0(out_dir_ch02,"p2.g2_",dateo,"_acs_cnt_1950_2016_single_famlily_prop_and_new_housing_units.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))



###################### P2.G3 Plot Owner Renter Occupancy Area Wise ###################################################

# get total housing units grouped by year and area
acs_cnt_1950_2016_area_wise_owner_renter_occupancy <- acs_cnt_1950_2016 %>% 
                                                            filter(year=="2016") %>% 
                                                                     group_by(area_type) %>%
                                                      summarise(total_owner_occupied = sum(total_owner_occupied, na.rm = T),
                                                              total_renter_occupied=sum(total_renter_occupied, na.rm = T)) %>%
                                                                                as.data.frame()


# melt the dataframe
acs_cnt_1950_2016_area_wise_owner_renter_occupancy_melt <- melt(acs_cnt_1950_2016_area_wise_owner_renter_occupancy, id.var="area_type")


# plot the graph
p <- ggplot(acs_cnt_1950_2016_area_wise_owner_renter_occupancy_melt, aes(x = area_type, y = value, fill = variable)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "area type", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.15),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=F), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


# make the barplot horizontal
p1 <- p + coord_flip() #+ scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))

p1

# save the plot
ggsave(paste0(out_dir_ch02,"p2.g3_",dateo,"_acs_cnt_1950_2016_owner_renter_occupancy_area_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))





##################################### CHAPTER 3 ##############################################################

###################### P3.G1 Timeline graph for median housing value and median housing income ###################################################

# get the median housing value and median housing income for all years
# acs_cnt_1950_2016_median_housing_value_income <- acs_cnt_1950_2016 %>%
#                                                        group_by(year) %>%
#                                               summarise(median_housing_value_yr = median(median_housing_value, na.rm = T),
#                                                         median_household_income_yr = median(median_household_income, na.rm = T))

# get the weighted mean (weighted by population) median housing value and median housing income for all years 
acs_cnt_1950_2016_median_housing_value_income <- acs_cnt_1950_2016 %>%
  group_by(year) %>%
  summarise(median_housing_value_yr = weighted.mean(median_housing_value, total_population, na.rm = T),
            median_household_income_yr = weighted.mean(median_household_income,total_population, na.rm = T))

# make the year column numeric
acs_cnt_1950_2016_median_housing_value_income$year <- as.numeric(acs_cnt_1950_2016_median_housing_value_income$year)

df <- na.omit(acs_cnt_1950_2016_median_housing_value_income)

# plot the graph
p1 <- ggplot(df, aes(x=year)) +
  geom_line(aes(y=median_housing_value_yr,size=0.1, color="median value")) +
  geom_line(aes(y=median_household_income_yr,size=0.1, color="median household income")) +
  #geom_point(aes(y=population,shape="np"), size=5) +
  #geom_point(aes(y=housing_units,shape="np"), size=5) +
  #geom_point(aes(shape="np"), size=5) +
  scale_y_continuous(labels = comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(breaks = trans_breaks(identity, identity, n = 7))+
  #scale_y_continuous(labels = comma,  limits = c(0, 450000), breaks = c(seq(0,450000,50000)))+
  #scale_x_continuous(limits= c(1980, 2016), breaks = c(seq(1980,2016,10))) +
  scale_x_continuous(breaks = c(1980,1990,2000,2011,2016), labels = paste0(c("1980", "1990", "2000", "2011", "2016")))+
  scale_colour_manual(values = c("orange","green"))+
  labs(y = "", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.15),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)

p1

# save the graph
ggsave(paste0(out_dir_ch03,"p3.g1_",dateo,"_acs_cnt_1950_2016_median_housing_value_household_income.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


###################### P3.G2 Bar graph for median housing value and median housing income 2016 at county level ###################################################

# create a function to plot the absolute values
plot_county_level_absolute_values <- function(df,colname,county_col){
  
  #print(colname)
  colname_str <- quo_name(colname)
  county_col_str <- quo_name(county_col)
  #print(colname_str)
  #print(county_col_str)
  
  #print(factor(df[,county_col_str]))
  #print(levels(df[,county_col_str]))
  
  # sort the data by colname and retain order by county name
  df <- df[order(df[colname_str]),] # sort
  
  df[[county_col_str]] <- factor(df[,county_col_str], levels = df[,county_col_str])  # to retain the order in plot.
  
  # create the scale vector to hold colors for all the 24 counties
  my_scale <- rep("NA",24)
  
  # provide desired colors to the counties on the basis of state
  my_scale[which(df$state_code=="DC")] <- rep("grey",1)
  my_scale[which(df$state_code=="VA")] <- rep("orange",17)
  my_scale[which(df$state_code=="MD")] <- rep("purple",5)
  my_scale[which(df$state_code=="WV")] <- rep("yellow",1)
  
  # create the plot for respective column
  p <- ggplot(df,aes_(x = county_col, y = colname,fill = county_col)) +
    geom_bar(stat = "identity")+ scale_fill_manual(values=my_scale)+
    geom_text(aes_(label=colname), hjust=-0.1, color="black", size=3.5)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          #panel.grid.major.y = element_line(color="gray"),
          legend.position = "none",
          title = element_text(size = 12.5),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12.5),
          panel.grid = element_blank(), panel.border = element_blank())
  
  # Here we define spaces as the big separator
  point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
  
  
  # make the barplot horizontal
  p1 <- p + coord_flip() + scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))
  
  print(p1)
  
  # save the graph
  ggsave(paste0(out_dir_ch03,"p3.g2_",dateo,"_acs_cnt_2016_",colname_str,".jpg"),
         plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))
  
}

acs_cnt_2016 <- acs_cnt_1950_2016 %>% filter(year=="2016") %>% 
                            mutate("median_housing_value_by_median_hh_income"=round(median_housing_value/median_household_income,2))


# provide column names for which we want absolute value plot on county level
col_vec <- c("median_housing_value_by_median_hh_income")

# call the funtion to create plot for each variable
for (col in col_vec){
  plot_county_level_absolute_values(acs_cnt_2016,quo(!!sym(col)),quo(!!sym("county_name")))
}


###################### P3.G2.2 Plot Housing Units Percent change from 2000 to 2016 vs Median Housing Value 2000 ###################################################

# plot the graph
p1 <- ggplot(data = subset(acs_cnt_1950_2016, year %in% c("2016")), aes(x = median_housing_value, y = median_household_income)) +
  #geom_point(aes(color = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none")),
  #                fill = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none"))),size=7) +
  #geom_point(aes(color = factor(STATE)),shape = 16, size = 3)+
  geom_point(shape = 16, size = 5)+geom_text(aes(label=county_name),hjust=-0.1, vjust=0.15)+
  scale_x_continuous(labels = comma,limits= c(0, 750000), breaks = c(seq(0,750000,100000))) +
  scale_y_continuous(limits= c(0, 130000), breaks = c(seq(0,130000,30000))) +
  labs(x = "median housing value 2016", y="median household income 2016")+
  #scale_color_manual(values = c("none" = damage_scale[4], "minimal" = damage_scale[3],"extensive" = damage_scale[2],"irreparable" = damage_scale[1]))+
  #geom_abline(intercept = 0, slope = 0.4, color = "grey")+  # 45 degree line
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #legend.position = "right",
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.2),
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10)))

p1

# save the graph
ggsave(paste0(out_dir_ch03,"p3.g2.2_",dateo,"_acs_cnt_2016_scatter_house_median_value_vs_median_household_income.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))