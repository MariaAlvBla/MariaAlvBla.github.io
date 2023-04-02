#Required libraries----
library(stringr)
library(ggplot2)
library(ggalluvial)
library(dplyr)
require(maps)
require(viridis)
library(tidyverse)

#Set working directory
setwd("name of working directory")

#Figure 1a----
#by Stephanie Jurburg

#Read the data file
Output<-read.table("Output.txt", sep="\t", header=TRUE, fill=TRUE)
Output$WB.Classification=as.factor(Output$WB.Classification)

#Set the WB$Classification levels
levels(Output$WB.Classification)=c("Low income", "Lower-middle income" ,"Upper-middle income", "High income")

#Create the plot
A=ggplot(Output, aes(x=WB.Classification, color=RD.investments, y=Percentage))+
  geom_point(size=4, alpha=0.7)+
  theme_classic()+
  scale_color_viridis()+
  ylab("% of ecological meta analysis (p/author, p/country)")+
  theme(axis.title.y =element_blank())+
  coord_flip()+
  guides(color=guide_legend(title="% GERD of GDP"))

#Figure 1b----
#by Stephanie Jurburg

theme_set(
  theme_void()
)

#Read the data file
EMP_coverage_by_WB_income<-read.table("EMP_coverage_by_WB_income.txt", sep="\t", header=TRUE, fill=TRUE)

#Set the world map
world=map_data("world")

#Create frequency table of the EMP_coverage_by_WB_income per region and income
Counts=EMP_coverage_by_WB_income %>%  
    group_by(region, income) %>%    
    tally()

#DON'T KNOW WHAT THIS DOES
countries=inner_join(world, Counts, by = "region")
countries$income=as.factor(countries$income)

#Create frequency table of countries per region and income
Counts=countries %>%  
    group_by(region, income) %>%        
    summarize(income=first(income))%>%
  tally()

#Set the level names of the income column
countries$income <- factor(countries$income, levels = c("High income", "Upper-middle income", "Lower-middle income"))

B=ggplot(data = world) +
  geom_polygon(data = world, fill = "gray", mapping = aes(x = long, y = lat, group=group))+
  geom_polygon(data = countries, aes(x = long, y = lat, group=group, fill=income), alpha=0.3)+
  geom_point(data = EMP_coverage_by_WB_income, aes(x = Longitude, y = Latitude))+
  theme_minimal()+
  scale_fill_viridis(discrete=TRUE)

#Present figures A and B alongside
cowplot::plot_grid(A, B, ncol=2)


#Figure 3a----
#by María José Álvarez Blanco

#read the data file
datathon_dataset <- readr::read_csv("Dataset_Datathon.csv")

#create frequency table of BioSamples per BioProject and availability status
allu_datathon_dataset <- datathon_dataset %>% 
  group_by(BioProject, sequence_public) %>%
  summarise(Freq = n())  

#change the name of the frequency column to avoid errors in the code for ggplot
colnames(allu_datathon_dataset)[3] <- "number_biosamples"

#set the order of the BioProjects to match how many samples they have (from biggest to smallest)
allu_datathon_dataset$BioProject <- factor(allu_datathon_dataset$BioProject ,
                                           levels=c("PRJNA644644","PRJNA234366","PRJNA784308",
                                                    "PRJNA730896","PRJNA730588","PRJNA899016",
                                                    "PRJNA861243","PRJNA892127","PRJNA889807",
                                                    "PRJNA646516","PRJNA944180","PRJNA646084","PRJNA889760",
                                                    "PRJNA889751","PRJNA637191","PRJNA889783","PRJNA891224",
                                                    "PRJNA889803","PRJNA672063","PRJNA889761","PRJNA834769"))

#set the order of the column of whether the data is public or not. In this case those that are public will appear first
allu_datathon_dataset$sequence_public <- factor(allu_datathon_dataset$sequence_public,
                                                levels=c("no","yes"))

#rename the levels of the sequence_public column
levels(allu_datathon_dataset$sequence_public) <- c("Not Public", "Public")

#create the alluvial plot
ggplot(data = allu_datathon_dataset, #frequency table that will be used
       aes(axis1 = BioProject, axis2 = sequence_public, #axes of the graph
           y = number_biosamples)) + #frequency values
  labs(y = "BioSamples within a BioProject")+ #label for the y axis
  scale_x_discrete(limits = c("BioProject", "BioProject Status"), expand=c(0.15,0.15))+ #names of the axes and size of the graph's area
  scale_y_continuous(breaks=c(0,100,200,300,400,500,600,700,800,900))+ #values for the breaks of the y axis
  geom_flow(stat = "alluvium", lode.guidance = "frontback",aes(fill = BioProject), color="black",decreasing = FALSE) + #type of alluvium
  geom_stratum(aes(fill = BioProject),decreasing = FALSE)+ # color of the alluvium, in this colored by BioProject, and the alluvium are presented in ascending order according to their frequency values
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),decreasing = FALSE,min.y = 150)+ #specifying the labeling of the stratum (BioProjects), which is needed for the legend, but setting min.y = 150, only BioProjects with frequency of minimum 150 BioSamples will have a label on the axis. Because none of the BioSamples have min 150 BioSamples, no layer appears
  guides(fill = guide_legend(title = "BioProject Code"))+ #title of the legend
  guides(fill=guide_legend(ncol =1))+ #present the figure with only on column
  theme( #styling of the graphic
    axis.text.y = element_text(size = 10,  colour = "black"),
    axis.text.x = element_text(size = 14,  colour = "black"),
    axis.title.y = element_text(size = 14,  colour = "black",vjust = +3),
    panel.grid.major.y = element_line(colour="grey84"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour="grey80"),
    legend.title = element_text(size = 14,  colour = "black"),
    legend.key.height = unit(0.7, "cm")
  )+scale_fill_viridis_d(limits=c("PRJNA644644","PRJNA234366","PRJNA784308", #this scale fill is necessary to match the order of the legend's elements to the graph's
                                  "PRJNA730896","PRJNA730588","PRJNA899016",
                                  "PRJNA861243","PRJNA892127","PRJNA889807",
                                  "PRJNA646516","PRJNA944180","PRJNA646084","PRJNA889760",
                                  "PRJNA889751","PRJNA637191","PRJNA889783","PRJNA891224",
                                  "PRJNA889803","PRJNA672063","PRJNA889761","PRJNA834769"))

#Figure 3b----
#by Stephanie Jurburg

#information
#https://www.geeksforgeeks.org/how-to-make-world-map-with-ggplot2-in-r/
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/

#Read data file
for_map=read.csv("Dataset_Datathon.csv")

#Create freq table of BioProject rows (BioSamples) per site
freq_coord_bioproj <- for_map %>% 
  group_by(BioProject,latitude,longitude, geo_loc_name_country, env_broad_scale) %>%   # grouping, drag country names into this, you will use inner join on country names later on
  tally()

#Set the column names
colnames(freq_coord_bioproj)= c("BioProject", "lat", "long", "region", "Realm", "Samples")

#Specify the columns to be used as latitude and longitud
freq_coord_bioproj$lat=as.numeric(freq_coord_bioproj$lat)
freq_coord_bioproj$long=as.numeric(freq_coord_bioproj$long)

#Set the world map
world_map <- map_data("world")

#Set map with only Argentina and Uruguay
datathon_countries <- c("Uruguay","Argentina","Antarctica")

#Set map with other South American countries
SouthAmerica=c("Brazil", "Chile", "Paraguay", "Bolivia")

# Retrieve the map with only Uruguay and Argentina
datathon_countries <- map_data("world", region = datathon_countries)

# Retrieve the map with the South American countries
SouthAmerica=map_data("world", region= SouthAmerica)

#Plot the BioSample's coordinates on the created maps
ggplot() +  
  geom_polygon(data = datathon_countries, fill ="gray", mapping = aes(x = long, y = lat, group=group))+
  geom_polygon(data = SouthAmerica, fill = "lightgray", mapping = aes(x = long, y = lat, group=group))+
  geom_point(data = freq_coord_bioproj, aes(x = long, y = lat,  size=Samples, color=Realm, alpha=0.7))+
  coord_map(projection = "mercator") +
  lims(x = c(-80, -40), y = c(-65, -20))+
  theme_minimal()+
  scale_color_viridis(discrete=TRUE)
