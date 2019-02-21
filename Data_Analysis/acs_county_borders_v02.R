#####################################################################

# this program makes the county borders for washington metro area
# january 28, 2019
# acs_county_borders_v02.R 

##############################################################################

##### A. start-up and set-up

library(RColorBrewer)
library(sp)
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(grid)
library(gtable)
library(data.table)
library(cowplot)

# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

out_dir <- paste0("/Graphs/")


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# read the county boundaries data
cborders1960 <- st_read(dsn = paste0("/maps/united_states/census2010/counties/"), layer = "cnty_2010_20140313")



cborders1960_urban <- cborders1960 %>% filter((COUNTY=="001" & STATE=="11")|(COUNTY %in% c("013","510") & STATE=="51"))

cborders1960_suburban <- cborders1960 %>% filter((COUNTY %in% c("031","033") & STATE=="24")|
                                                   (COUNTY %in% c("059","600","610") & STATE=="51"))

cborders1960_exurban <- cborders1960 %>% filter((COUNTY %in% c("009","017","021") & STATE=="24")|
                                                   (COUNTY %in% c("043","047","061","107","153","157","177","179",
                                                                      "187","630","683","685") & STATE=="51")|
                                                   (COUNTY=="037" & STATE=="54"))

get_city_in_name <- function(df){
  ind <- df$LSAD %in% "city"
  df1 <- df
  st_geometry(df1) <- NULL
  df1$NAME <- as.character(df1$NAME)
  df1[ind,"NAME"] <- paste0(df1[ind,"NAME"]," city")
  df1 <- df1 %>% dplyr::select(NAME)
  colnames(df1) <- "NEW_NAME"
  
  df$NAME <- as.character(df$NAME)
  
  df3 <- cbind(as.data.frame(df),df1)
  
  return(df3)
}


cborders1960_urban_new <- get_city_in_name(cborders1960_urban)
cborders1960_suburban_new <- get_city_in_name(cborders1960_suburban)
cborders1960_exurban_new <- get_city_in_name(cborders1960_exurban)





# draw the plot with county names in it without any demographic data
p1 <- ggplot() +
  geom_sf(data = cborders1960_urban_new, fill="#1f78b4", size=0.1)+
  geom_sf(data = cborders1960_suburban_new, fill="#a6cee3",size=0.1)+
  geom_sf_text(data = cborders1960_urban_new, aes(label = NEW_NAME))+
  geom_sf_text(data = cborders1960_suburban_new, aes(label = NEW_NAME))+
  coord_sf(crs = st_crs(4326))+
  theme_map() +
  theme(legend.position = "bottom",legend.background = element_rect(color = NA))


p1

ggsave(paste0(out_dir,"i1.m2_",dateo,"_county_borders_washington_urban_area.jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


# draw the plot with county names in it without any demographic data
p1 <- ggplot() +
  geom_sf(data = cborders1960_urban_new, fill="#1f78b4", size=0.1)+
  geom_sf(data = cborders1960_suburban_new, fill="#a6cee3",size=0.1)+
  geom_sf(data = cborders1960_exurban_new,fill="#b2df8a", size=0.1)+
  geom_sf_text(data = cborders1960_urban_new, aes(label = NEW_NAME))+
  geom_sf_text(data = cborders1960_suburban_new, aes(label = NEW_NAME))+
  geom_sf_text(data = cborders1960_exurban_new, aes(label = NEW_NAME))+
  coord_sf(crs = st_crs(4326))+
  theme_map() +
  theme(legend.position = "bottom",legend.background = element_rect(color = NA))


p1

ggsave(paste0(out_dir,"i1.m1_",dateo,"_county_borders_washington_metro_area.jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))