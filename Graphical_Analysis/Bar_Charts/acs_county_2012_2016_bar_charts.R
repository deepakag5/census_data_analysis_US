#####################################################################
# and tries to make plots from it
# for presentation purposes
# november 28, 2018
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
dateo <- paste(substr(Sys.Date(), 1, 4), substr(Sys.Date(), 6, 7), substr(Sys.Date(), 9, 10), sep = "")
dateo

# data and output directories
data_dir <- paste0('../data/summary_files_data/')
out_dir <- paste0('../data/r_output/')


# load the data
acs_cnt_2012_2016 <- read.csv(paste0(data_dir, "cnty_acs_2012_2016_absolute_values.csv"), stringsAsFactors = F)

# transpose the data to show attributes as per county level
data <- as.data.frame(t(acs_cnt_2012_2016))

# remove unnecessary columns
data <- data[- c(1, 2),]

# reassign the column names in transposed df with the indexes
colnames(data) <- acs_cnt_2012_2016$index

# print the df to check if the format is appropriate
print(head(data))

# rename the columns to avoid column duplicates error
colnames(data) <- make.unique(names(data))

#names of columns in data frame
cols <- colnames(data)

# character variables
cols.char <- c("FILEID", "FILETYPE", "STATE", "GEO_NAME", "GEO_ID")

# numeric variables
cols.num <- cols[! cols %in% cols.char]



# write a function to convert the required columns to numeric
make_num <- function(x)
{
    return(as.numeric(as.character(x)))
}

# make all the required columns numeric
data[cols.num] <- lapply(data[cols.num], make_num)

# print the dataframe to check the data types
print(str(data))

# create column state country code
data["state_county_code"] <- rownames(data)

# split the column GEO NAME to extract county and state name
data <- as.data.frame(cSplit(data, c("GEO_NAME"), ',', drop = F))

# split the column country code to get state and county codes
data <- as.data.frame(cSplit(data, c("state_county_code"), '_', drop = F, type.convert = FALSE))

# rename the splitted columns
names(data)[names(data) == "GEO_NAME_1"] <- "county_name"
names(data)[names(data) == "GEO_NAME_2"] <- "state_name"
names(data)[names(data) == "state_county_code_1"] <- "state_code"
names(data)[names(data) == "state_county_code_2"] <- "county_code"


############################ A. Plots for County Level ###################################################

######## Housing Units built since 2000 ################
data <- data %>% mutate("housing_units_built_since_2000" = select(., `Built 2014 or later` : `Built 2000 to 2009`) %>%
apply(1, sum, na.rm = TRUE))

######## Housing Units built before 2000 ################
data <- data %>% mutate("housing_units_built_before_2000" = select(., `Built 1990 to 1999` : `Built 1939 or earlier`) %>%
apply(1, sum, na.rm = TRUE))

######## Housing Units built before/since 2000 proportion ################
data <- data %>% mutate("housing_units_built_since_2000_prop" = round(housing_units_built_since_2000 /
total_housing_units, 3), "housing_units_built_before_2000_prop" = round(housing_units_built_before_2000 / total_housing_units, 3))


###################### Family Housholds #################################

######## Total Family Housing Units built since 2000 ################
data %>%
    select(intersect(starts_with('Built 20'), contains(':'))) %>%
    head(2)

data <- data %>% mutate('total_family_households_since_2000' = select(., intersect(starts_with('Built 20'), contains(':'))) %>% apply(1, sum, na.rm = TRUE))

data %>%
    select(intersect(starts_with('Built 19'), contains(':'))) %>%
    head(2)

data <- data %>% mutate('total_family_households_before_2000' = select(., intersect(starts_with('Built 19'), contains(':'))) %>% apply(1, sum, na.rm = TRUE))

######## Single Family Housing Units built since 2000 ################
data %>%
    select(matches('^1, detached.*attached$|attached.1$|attached.6$|attached.7$')) %>%
    head(2)

data <- data %>% mutate('total_single_family_households_since_2000' = select(., matches('^1, detached.*attached$|attached.1$|attached.6$|attached.7$')) %>%
apply(1, sum, na.rm = TRUE))

######## Single Family Housing Units built before 2000 ################
data %>%
    select(setdiff(starts_with('1, detached'),
    matches('attached$|attached.1$|attached.6$|attached.7$'))) %>%
    head(2)

data <- data %>% mutate('total_single_family_households_before_2000' = select(., setdiff(starts_with('1, detached'),
matches('attached$|attached.1$|attached.6$|attached.7$'))) %>% apply(1, sum, na.rm = TRUE))


######## Not Single Family Housing Units built since 2000 ################
data <- data %>% mutate('total_not_single_family_households_since_2000' = total_family_households_since_2000 - total_single_family_households_since_2000)

######## Not Single Family Housing Units built before 2000 ################
data <- data %>% mutate('total_not_single_family_households_before_2000' = total_family_households_before_2000 - total_single_family_households_before_2000)


################ Population and Housing Density ######################
# read the county area census 2010 data
county_area <- st_read("/maps/united_states/census2010/counties", layer = "cnty_2010_20140313", stringsAsFactors = F)

# make the geometry NULL
st_geometry(county_area) <- NULL

# convert the state_fips column to character from numeric for joining
data$STATE_FIPS <- as.character(data$STATE_FIPS)

# join the county area data to summary data for respective states and counties
data <- left_join(data, county_area, by = c("STATE_FIPS" = "STATE", "county_code" = "COUNTY"))

# data check - check whether the data for census area is same for each state,county combination in both summary df and county area df
########
state_county <- paste0(data$STATE_FIPS, data$county_code)
data %>% select(STATE_FIPS, county_code, CENSUSAREA)
county_area %>%
    filter(paste0(STATE, COUNTY) %in% state_county) %>%
    select(STATE, COUNTY, CENSUSAREA)
########

# create the poplation density and housing units density columns
data <- data %>% mutate('population_density' = round(Total_population / CENSUSAREA))
data <- data %>% mutate('housing_units_density' = round(total_housing_units / CENSUSAREA))


# create a function to plot the absolute values
plot_county_level_absolute_values <- function(df, colname, county_col){

    #print(colname)
    colname_str <- quo_name(colname)
    county_col_str <- quo_name(county_col)
    #print(colname_str)
    #print(county_col_str)

    #print(factor(df[,county_col_str]))
    #print(levels(df[,county_col_str]))

    # sort the data by colname and retain order by county name
    df <- df[order(df[colname_str]),] # sort

    df[[county_col_str]] <- factor(df[, county_col_str], levels = df[, county_col_str])  # to retain the order in plot.

    # create the scale vector to hold colors for all the 24 counties
    my_scale <- rep("NA", 24)

    # provide desired colors to the counties on the basis of state
    my_scale[which(df$state_code == "DC")] <- rep("grey", 1)
    my_scale[which(df$state_code == "VA")] <- rep("orange", 17)
    my_scale[which(df$state_code == "MD")] <- rep("purple", 5)
    my_scale[which(df$state_code == "WV")] <- rep("yellow", 1)

    # create the plot for respective column
    p <- ggplot(df, aes_(x = county_col, y = colname, fill = county_col)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = my_scale) +
        geom_text(aes_(label = colname), hjust = - 0.1, color = "black", size = 3.5) +
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
    p1 <- p +
        coord_flip() +
        scale_y_continuous(labels = point, expand = expand_scale(mult = c(0, .1)))

    ggsave(paste0(out_dir, dateo, "_acs_cnt_2012_2016_", colname_str, ".jpg"),
    plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))
}

# provide column names for which we want absolute value plot on county level
col_vec <- c("Total_population", "total_housing_units", "housing_units_built_since_2000",
"housing_units_built_before_2000", "housing_units_built_since_2000_prop", "housing_units_built_before_2000_prop",
"Median_household_income", "total_single_family_households_since_2000", "total_single_family_households_before_2000",
"total_not_single_family_households_since_2000", "total_not_single_family_households_before_2000", "population_density",
"housing_units_density")

# call the funtion to create plot for each variable
for (col in col_vec) {
    plot_county_level_absolute_values(data, quo(! ! sym(col)), quo(! ! sym("county_name")))
}





