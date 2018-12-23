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
############################ B. Plots for Area Types (Urban, SubUrban, ExUrban) ###################################################
# create area types
data <- data %>% mutate("area_type" = ifelse(county_name %in% c("District of Columbia", "Arlington County"), "Urban",
ifelse(county_name %in% c("Prince George's County",
"Montgomery County", "Fairfax County", "Fairfax city", "Falls Church city"), "SubUrban",
"ExUrban")))

# create a function to plot the absolute values for given cols in col_vec
plot_area_level_absolute_values <- function(df, colname, area_col){

    #print(colname)
    colname_str <- quo_name(colname)
    area_col_str <- quo_name(area_col)

    # create the plot for respective column
    p <- ggplot(df, aes_(x = area_col, y = colname, fill = area_col)) +
        geom_bar(stat = "identity") + #scale_fill_manual(values=my_scale)+
    #geom_text(aes_(label=colname), hjust=-0.1, color="black", size=3.5)+
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


    # save the plot in out_dir
    ggsave(paste0(out_dir, dateo, "_acs_cnt_2012_2016_", colname_str, "_by_area_type.jpg"),
    plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))
}

# provide column names for which we want absolute value plot on county level
col_vec <- c("Total_population", "total_housing_units", "housing_units_built_since_2000", "housing_units_built_before_2000")

# call the funtion to create plot for each variable
for (col in col_vec) {
    plot_area_level_absolute_values(data, quo(! ! sym(col)), quo(! ! sym("area_type")))
}