#####################################################################

# this program takes the summary data created in
# /Data_Collection/download_acs_county_data.py
# and performs analysis on it to produce required graphs
# for presentation purposes

# Januray 16, 2019

# acs_county_page_plots_2016_v03.R
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

# data and output directories
data_dir <- paste0("/Data_Collection/")
out_dir <- paste0("/Graphs/")


# load the data
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,"20190208_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)


################ write a function to reformat the data to bring it to county level and subset it for requried columns ################
reformat_subset_data <- function(df) {
  
  # transpose the data to show attributes as per county level
  data <- as.data.frame(t(df))
  
  # remove unnecessary columns
  data <- data[-c(1,2),]
  
  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index
  
  # print the df to check if the format is appropriate
  print(head(data))
  
  # rename the columns to avoid column duplicates error
  colnames(data) <- make.unique(names(data))
  
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
  
  
  return(data)
}

# get the data for year 2016 with relevant columns by calling the function
acs_cnt_2012_2016_subset <- reformat_subset_data(acs_cnt_2012_2016)

head(acs_cnt_2012_2016_subset)



################ create columns for structure type -- Graph 1 ################
acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>%
                      mutate('B25034_12_built_2010_to_2016'=select(.,matches('^B25034_2|B25034_3')) %>%
                            apply(1, sum, na.rm=TRUE))


################# create columns for structure type -- Graph 2 ################
acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>%
                      mutate('1_Units'=select(.,intersect(starts_with("B25127"),contains("1,_detached"))) %>%
                                   apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                            mutate('2_to_4_Units'=select(.,intersect(starts_with("B25127"),contains("2_to_4"))) %>%
                                     apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                                    mutate('5_to_19_Units'=select(.,intersect(starts_with("B25127"),contains("5_to_19"))) %>%
                                             apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                            mutate('20_to_49_Units'=select(.,intersect(starts_with("B25127"),contains("20_to_49"))) %>%
                                     apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                              mutate('50_or_more_Units'=select(.,intersect(starts_with("B25127"),contains("50_or_more"))) %>%
                                       apply(1, sum, na.rm=TRUE))

################# create columns for owner occupied homes -- Graph 3 ################
acs_cnt_2012_2016_subset %>% select(matches('^B25075')) %>% head(2)

acs_cnt_2012_2016_subset %>% select(matches('^B25075_([2-9]|[1][0-4])_')) %>% head(2)

acs_cnt_2012_2016_subset %>% select(matches('^B25075_([1][5-8])_')) %>% head(2)

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
  mutate('B25075_28_less_than_$100,000'=select(.,matches('^B25075_([2-9]|[1][0-4])_')) %>%
           apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
 mutate('B25075_29_$100,000_to_$199,999'=select(.,matches('^B25075_([1][5-8])_')) %>%
           apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset %>% select(matches('^B25075_([1][9]|[2][0-9])_')) %>% colnames()


################# create columns for structure type -- Graph 4 ################
acs_cnt_2012_2016_subset %>% select(matches('^B19001')) %>% colnames()

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
            mutate('B19001_18_less_than_$20,000'=select(.,matches('^B19001_2|B19001_3|B19001_4')) %>%
                apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
  mutate('B19001_19_$20,000_to_$39,999'=select(.,matches('^B19001_5|B19001_6|B19001_7|B19001_8')) %>%
           apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
  mutate('B19001_20_$40,000_to_$59,999'=select(.,matches('^B19001_9|B19001_10|B19001_11')) %>%
           apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset %>% select(matches('^B19001_([1][2-9]|[2][0])')) %>% colnames()

################ create columns for structure type -- Graph 5 ################
acs_cnt_2012_2016_subset %>% select(matches('^B03002_3_|B03002_4_|B03002_6_|B03002_12_'))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                                mutate('total_white_hispanic_black_asian'=select(.,matches('^B03002_3_|B03002_4_|B03002_6_|B03002_12_')) %>%
                                apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% mutate('All_Others'=B03002_1_total-total_white_hispanic_black_asian)


################# create columns for structure type -- Graph 6 ################
acs_cnt_2012_2016_subset %>% select(matches('^B25063')) %>% colnames()

acs_cnt_2012_2016_subset %>% select(matches('^B25063_([3-9]|[1][0-7])')) %>% colnames()

#acs_cnt_2012_2016_subset %>% select(matches('^B25063_([1][8-9])')) %>% colnames()

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
  mutate('B25063_28_less_than_$799'=select(.,matches('^B25063_([3-9]|[1][0-7])')) %>%
           apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>%
  mutate('B25063_29_$800_to_$999'=select(.,matches('^B25063_([1][8-9])')) %>%
           apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
  mutate('B25063_30_$3,000_or_more'=select(.,matches('^B25063_([2][5-6])')) %>%
           apply(1, sum, na.rm=TRUE))

#############################################################################################################

geom_text_col <- "#737373"
axis_labs_col <- "#737373"

############################################### 1 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_built_year <- acs_cnt_2012_2016_subset %>%
                                        select(STATE_FIPS,county_code,county_name,matches('^B25034_([4-9]|1[0-9])_')) %>%
                                        as.data.frame()

#built_cols <- colnames(acs_cnt_2012_2016_subset_built_year)[4:ncol(acs_cnt_2012_2016_subset_built_year)] 

built_cols <- c("2000 to 2009","1990 to 1999" ,"1980 to 1989", "1970 to 1979", "1960 to 1969" , "1950 to 1959" , "1940 to 1949",
                "before 1940","2010 to 2016")

colnames(acs_cnt_2012_2016_subset_built_year)[4:ncol(acs_cnt_2012_2016_subset_built_year)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_built_year_melt <- melt(acs_cnt_2012_2016_subset_built_year, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_built_year_melt_dc <- acs_cnt_2012_2016_subset_built_year_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

acs_cnt_2012_2016_subset_built_year_melt_dc$variable <- factor(acs_cnt_2012_2016_subset_built_year_melt_dc$variable, 
                                                           c("before 1940",  "1940 to 1949", "1950 to 1959", "1960 to 1969",
                                                             "1970 to 1979", "1980 to 1989", "1990 to 1999", "2000 to 2009","2010 to 2016"))
                                                               
levels(acs_cnt_2012_2016_subset_built_year_melt_dc$variable) <- gsub(" ", "\n", levels(acs_cnt_2012_2016_subset_built_year_melt_dc$variable))

acs_cnt_2012_2016_subset_built_year_melt_dc <- acs_cnt_2012_2016_subset_built_year_melt_dc %>% 
                                                      mutate(value_prop=round(value/sum(value, na.rm = T)*100,2))

# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_built_year_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill="#1f78b4")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value_prop), vjust=-0.5, color=geom_text_col, size=5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.title.x = element_text(colour = axis_labs_col),
        axis.title.y = element_text(colour = axis_labs_col),
        #axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
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



# save the graph
ggsave(paste0(out_dir,"c1.g1_",dateo,"_DC_acs_cnt_2016_housing_units_by_age_of_housing_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

acs_cnt_2012_2016_subset_built_year_melt_dc$variable <- gsub("\n"," ",acs_cnt_2012_2016_subset_built_year_melt_dc$variable)


############################################### 2 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_structure_type <- acs_cnt_2012_2016_subset %>%
                                select(STATE_FIPS,county_code,county_name,contains("_Units")) %>%
                                              as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_structure_type)[4:ncol(acs_cnt_2012_2016_subset_structure_type)] 

built_cols <- gsub("_Units","",built_cols)

built_cols <- gsub("_"," ",built_cols)

colnames(acs_cnt_2012_2016_subset_structure_type)[4:ncol(acs_cnt_2012_2016_subset_structure_type)] <- built_cols 

# melt the dataframe
acs_cnt_2012_2016_subset_structure_type_melt <- melt(acs_cnt_2012_2016_subset_structure_type, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_structure_type_melt_dc <- acs_cnt_2012_2016_subset_structure_type_melt %>% filter(STATE_FIPS=="11" & county_code=="001")


acs_cnt_2012_2016_subset_structure_type_melt_dc <- acs_cnt_2012_2016_subset_structure_type_melt_dc %>% 
                                                        mutate(value_prop=round(value/sum(value, na.rm = T)*100,2))

# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_structure_type_melt_dc, aes(x = variable, y = value )) +
  geom_bar(stat = "identity", fill="#1f78b4")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value_prop), vjust=-0.5, color=geom_text_col, size=5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.title.x = element_text(colour = axis_labs_col),
        axis.title.y = element_text(colour = axis_labs_col),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)


# save the graph
ggsave(paste0(out_dir,"c1.g2_",dateo,"_DC_acs_cnt_2016_housing_units_by_structure_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

############################################### 3 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_owner_occupied <- acs_cnt_2012_2016_subset %>%
                                     select(STATE_FIPS,county_code,county_name,matches("B25075_([1][9]|[2][0-9])_")) %>%
                                               as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_owner_occupied)[4:ncol(acs_cnt_2012_2016_subset_owner_occupied)] 

built_cols <- gsub("B25075_([0-9]|[1-2][0-9])_","",built_cols)

built_cols <- gsub("_"," ",built_cols)

colnames(acs_cnt_2012_2016_subset_owner_occupied)[4:ncol(acs_cnt_2012_2016_subset_owner_occupied)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_owner_occupied_melt <- melt(acs_cnt_2012_2016_subset_owner_occupied, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_owner_occupied_melt_dc <- acs_cnt_2012_2016_subset_owner_occupied_melt %>% filter(STATE_FIPS=="11" & county_code=="001")


new_levels <- c("less than $100,000","$100,000 to $199,999", "$200,000 to $249,999", "$250,000 to $299,999", "$300,000 to $399,999",  
                "$400,000 to $499,999",     "$500,000 to $749,999",     "$750,000 to $999,999" ,   
                "$1,000,000 to $1,499,999", "$1,500,000 to $1,999,999", "$2,000,000 or more")

acs_cnt_2012_2016_subset_owner_occupied_melt_dc$variable <- factor(acs_cnt_2012_2016_subset_owner_occupied_melt_dc$variable,new_levels)


acs_cnt_2012_2016_subset_owner_occupied_melt_dc <- acs_cnt_2012_2016_subset_owner_occupied_melt_dc %>% 
                                                              mutate(value_prop=round(value/sum(value, na.rm = T)*100,2))

# plot the graph
p <- ggplot(acs_cnt_2012_2016_subset_owner_occupied_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill="#1f78b4")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value_prop), hjust=-0.5, color=geom_text_col, size=5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.title.x = element_text(colour = axis_labs_col),
        axis.title.y = element_text(colour = axis_labs_col),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)

# make the barplot horizontal
p1 <-p+coord_flip()


# save the graph
ggsave(paste0(out_dir,"c1.g3_",dateo,"_DC_acs_cnt_2016_housing_units_by_owner_occupied.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


############################################### 4 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_household_income <- acs_cnt_2012_2016_subset %>%
                                            select(STATE_FIPS,county_code,county_name,matches('^B19001_([1][2-9]|[2][0])')) %>%
                                                    as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_household_income)[4:ncol(acs_cnt_2012_2016_subset_household_income)] 

built_cols <- gsub("B19001_([0-9]|[1-2][0-9])_","",built_cols)

built_cols <- gsub("_"," ",built_cols)

colnames(acs_cnt_2012_2016_subset_household_income)[4:ncol(acs_cnt_2012_2016_subset_household_income)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_household_income_melt <- melt(acs_cnt_2012_2016_subset_household_income, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_household_income_melt_dc <- acs_cnt_2012_2016_subset_household_income_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

new_levels <- c("less than $20,000",  "$20,000 to $39,999" ,
                "$40,000 to $59,999",
                "$60,000 to $74,999",  "$75,000 to $99,999", "$100,000 to $124,999",
                "$125,000 to $149,999", "$150,000 to $199,999", "$200,000 or more")

acs_cnt_2012_2016_subset_household_income_melt_dc$variable <- factor(acs_cnt_2012_2016_subset_household_income_melt_dc$variable,new_levels)

acs_cnt_2012_2016_subset_household_income_melt_dc <- acs_cnt_2012_2016_subset_household_income_melt_dc %>% 
                                                                mutate(value_prop=round(value/sum(value, na.rm = T)*100,2))


# plot the graph
p <- ggplot(acs_cnt_2012_2016_subset_household_income_melt_dc, aes(x = variable, y = value )) +
  geom_bar(stat = "identity", fill="#1f78b4")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value_prop), hjust=-0.5, color=geom_text_col, size=5)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.title.x = element_text(colour = axis_labs_col),
        axis.title.y = element_text(colour = axis_labs_col),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)

# make the barplot horizontal
p1 <-p+coord_flip()


# save the graph
ggsave(paste0(out_dir,"c1.g4_",dateo,"_DC_acs_cnt_2016_household_income_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

############################################### 5 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_race <- acs_cnt_2012_2016_subset %>%
                                               select("STATE_FIPS","county_code","county_name",matches('^B03002_3_|B03002_4_|B03002_6_|B03002_12_'),
                                                      "All_Others") %>%
                                                 as.data.frame()


built_cols <- c("White Alone" , "African American Alone" ,"Asian Alone"  , "Hispanic or Latino", "All Others")

colnames(acs_cnt_2012_2016_subset_race)[4:ncol(acs_cnt_2012_2016_subset_race)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_race_melt <- melt(acs_cnt_2012_2016_subset_race, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_race_melt_dc <- acs_cnt_2012_2016_subset_race_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

acs_cnt_2012_2016_subset_race_melt_dc$variable <- factor(acs_cnt_2012_2016_subset_race_melt_dc$variable, 
                                                               rev(levels(acs_cnt_2012_2016_subset_race_melt_dc$variable)))


acs_cnt_2012_2016_subset_race_melt_dc <- acs_cnt_2012_2016_subset_race_melt_dc %>% 
                                            mutate(value_prop=round(value/sum(value, na.rm = T)*100,2))

# plot the graph
p <- ggplot(acs_cnt_2012_2016_subset_race_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill="#1f78b4")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 3))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value_prop), hjust=-0.5, color=geom_text_col, size=5)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.title.x = element_text(colour = axis_labs_col),
        axis.title.y = element_text(colour = axis_labs_col),
        #axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)


p1 <-p+coord_flip()


# save the graph
ggsave(paste0(out_dir,"c1.g5_",dateo,"_DC_acs_cnt_2016_housing_units_by_race_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


############################################### 6 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_household_type <- acs_cnt_2012_2016_subset %>%
                                             select(STATE_FIPS,county_code,county_name,matches('^B25063_([2][0-4]|[2][7-9]|[3][0-9])_'))%>%
                                                                    as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_household_type)[4:ncol(acs_cnt_2012_2016_subset_household_type)] 

built_cols <- gsub("B25063_([0-9]|[1-3][0-9])_","",built_cols)

built_cols <- gsub("_"," ",built_cols)

colnames(acs_cnt_2012_2016_subset_household_type)[4:ncol(acs_cnt_2012_2016_subset_household_type)] <- built_cols

colnames(acs_cnt_2012_2016_subset_household_type)[9:10] <- c("No cash rent", "Less than $799")


# melt the dataframe
acs_cnt_2012_2016_subset_household_type_melt <- melt(acs_cnt_2012_2016_subset_household_type, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_household_type_melt_dc <- acs_cnt_2012_2016_subset_household_type_melt %>% filter(STATE_FIPS=="11" & county_code=="001")


new_levels <- c("No cash rent", "Less than $799" ,"$800 to $999", 
               "$1,000 to $1,249" ,"$1,250 to $1,499" ,"$1,500 to $1,999", "$2,000 to $2,499",
                "$2,500 to $2,999", "$3,000 or more")
   


acs_cnt_2012_2016_subset_household_type_melt_dc$variable <- factor(acs_cnt_2012_2016_subset_household_type_melt_dc$variable,new_levels)


acs_cnt_2012_2016_subset_household_type_melt_dc <- acs_cnt_2012_2016_subset_household_type_melt_dc %>% 
                                                      mutate(value_prop=round(value/sum(value, na.rm = T)*100,2))

# plot the graph
p <- ggplot(acs_cnt_2012_2016_subset_household_type_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill="#1f78b4")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 3))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value_prop, size=value), hjust=-0.5, color=geom_text_col, size=5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.title.x = element_text(colour = axis_labs_col),
        axis.title.y = element_text(colour = axis_labs_col),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)

# make the barplot horizontal
p1 <-p+coord_flip()


# save the graph
ggsave(paste0(out_dir,"c1.g6_",dateo,"_DC_acs_cnt_2016_housing_units_by_rent_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))