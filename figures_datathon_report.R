#Required libraries----

library(ggplot2)
library(ggalluvial)
library(tidyverse)

#Figure 3a---- by María José Álvarez Blanco

#read the csv file
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
