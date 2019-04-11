library(RMySQL)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(RColorBrewer)
library(shinydashboard)
library(shiny)
library(reshape2)


init_segments <- function(db, current_year=2018, range=4) {
  segments = seg(db, current_year)
  year = rep(current_year, nrow(segments))
  segments$year = year
  for(i in 1:range) {
	tmp = seg(db, current_year - i)
	year = rep(current_year, nrow(tmp))
	tmp$year = year
	segments = rbind(segments, tmp)
  }
  return(segments)
}

seg = function(db, year=2018){
  query = paste("SELECT contact_id,
                DATEDIFF(", year, "0625, MAX(act_date)) / 365 AS 'recency',
                COUNT(amount) AS 'frequency',
                AVG(amount) AS 'avgamount',
                SUM(amount) AS 'sumamount'
         FROM acts
         WHERE (act_type_id = 'DO') AND (act_date <= ", year, "0630)
         GROUP BY 1", sep="")
  
  data = fetch(dbSendQuery(db, query), n=-1)
  
  # divide by recency
  segment = data %>%
  mutate(Label = case_when(recency <= 1 ~ "Active",
                           recency > 1 & recency <= 2 ~ "Warm",
                           recency > 2 & recency <= 5 ~ "Cold",
                           recency > 5 ~ "Lost"))

  # redefine active according to average amount
  threshold = unname(quantile(segment[segment$Label=="Active",]$avgamount,0.8))
  segment$Label[(segment$Label=="Active") & (segment$avgamount<=threshold)] = "Active bottom"
  segment$Label[(segment$Label=="Active" & segment$avgamount>threshold)] = "Active top"

  return(segment)
}





# customer segmentation historical change by year
get_size = function(segments, year_filter=2018){
  size = segments %>%
	filter(year == year_filter) %>%
	group_by(Label) %>%
	summarise(Count = n())
  size$Count = 100*size$Count/sum(size$Count)
  return(size)
}

yearly_segment_size_plot <- function(segments, current_year=2018, range=4) {
  size = get_size(segments, current_year)
  for(i in 1:range) {
	size = cbind(size, get_size(segments, current_year - i)$Count)
  }

  colnames(size) = c("Label","Jun_18","Jun_17","Jun_16","Jun_15","Jun_14")
  
  size = melt(size,id=c('Label'),variable.name = "Year",value.name = "Proportion")
  
  size$Label=factor(size$Label, levels=c("Active top","Active bottom","Warm","Cold","Lost"))
  size$Year=factor(size$Year, levels=c("Jun_14","Jun_15","Jun_16","Jun_17","Jun_18"))
  
  ggplot(size, aes(x=Year, y=Proportion, fill=Label))+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(aes(label = round(Proportion)), size = 3, hjust = 0.5, vjust = 2, position = "stack")+
    labs(x="Year", y="Proportion %")+
    scale_fill_brewer(palette="Spectral")+
    theme_bw()
}





# frequency by segment
freq_plot = function(segments, year_filter=2018){
  data = segments %>%
  filter(year == year_filter) %>%
  group_by(Label) %>%
  summarise(avg_freq = mean(frequency))

  data$Label=factor(data$Label , levels=c("Active top","Active bottom","Warm","Cold","Lost"))

  ggplot(data,aes(x=Label, y=avg_freq))+
 	geom_bar(fill = "#2B83BA", stat='identity', width = 0.6)+
  	labs(x="Segment", y="Average frequency")+
  	geom_text(aes(label = round(avg_freq)), size = 4)+
  	scale_fill_brewer(palette="Spectral")+
  	theme_bw()
}