library(RMySQL)
library(ggplot2)
library(dplyr)

init_demog <- function(db, segments, current_year=2018) {
  #Retrieve 2018 segmentation
  segment = segments %>% filter(year == current_year)
  segment$Label=factor(segment$Label)
  segment <- subset(segment, select=c(contact_id, Label))
  
  #Get interesting contact information for all segments
  query = 'SELECT id, prefix_id, code_geo, active FROM contacts'
  contacts = fetch(dbSendQuery(db, query), n=-1)
  names(contacts)[1] = 'contact_id'
  
  contacts = merge(contacts, segment, all.x = T)
  
  contacts$code_geo <- substr(contacts$code_geo, 1, 2)
  region_info<- read.csv('region.csv')
  contacts$code_geo <- as.numeric(contacts$code_geo)
  region_info <- subset(region_info, select=c(region, code_region))
  names(region_info)[2]='code_geo'
  contacts <- merge(contacts, region_info, all.x = T)
  contacts = na.omit(contacts)
  
  return(contacts)
}





prefix_count_plot <- function(contacts) {
  # Frequency for different prefix in different segmentations
  prefix_count = contacts %>% 
    group_by(prefix_id, Label) %>%
    summarise(number = n()) 
  
  ggplot(prefix_count, aes(x=Label, y=number, fill=prefix_id))+ 
    geom_bar(stat="identity", width = 0.6) + 
    scale_fill_brewer(palette = 'Spectral')+theme_bw()+
    labs(y="Frequency")
}





region_count_plot <- function(contacts, label='Active top', n_regions=5) {
  region_count = contacts %>% 
    group_by(region, Label) %>%
    summarise(number = n())
  region_count$number = sort(region_count$number, decreasing=TRUE)
  
  #Plot the frequency for geographic situations in different segmentations, 
  #Here, I plot five pictures for different segmentations and select top 5 regions in each segmentation,
  #Because the number of regions is too big.
  
  data = region_count[region_count$Label==label,]
  ggplot(data[1:n_regions,], aes(x=region, y=number))+ 
    geom_bar(fill = "#2B83BA", stat="identity", width = 0.6) + 
    scale_fill_brewer(palette = 'Spectral')+theme_bw()+
    labs(x="Region", y="Number of donors in segment")
}