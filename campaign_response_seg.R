library(RMySQL)
library(dplyr)
library(ggplot2)

init_responses <- function(db, segments, current_year=2018, month_day='0625', range=4) {
  cols = c('contact_id', 'Label')  

  responses = response(db, current_year, month_day)
  year = rep(current_year, nrow(responses))
  responses$year = year

  seg = segments %>% filter(year == current_year)
  responses = merge(responses, seg[cols], all.x = T)

  for(i in 1:range) {
	tmp = response(db, current_year - i, month_day)

  	year = rep(current_year - i, nrow(tmp))
  	tmp$year = year

	seg = segments %>% filter(year == current_year - i)
	tmp = merge(tmp, seg[cols], by='contact_id')

	responses = rbind(responses, tmp)
  }
  return(responses)
}

# Show how many times each donor responded to a campaign
response <- function(db, year=2018, month_day='0625') {
  query = paste("SELECT  contact_id, 
         COUNT(amount)  AS 'response_compaign'
         from acts 
         WHERE (act_date  < ", year, month_day, ") AND (act_date > ", year-1, month_day, 
         ") AND (act_type_id like 'DO' ) AND (Not isnull(campaign_id)) 
         GROUP BY 1 ", sep="")
  data = fetch(dbSendQuery(db, query), n=-1)
  return(data)
}





year_response_plot <- function(responses, year_filter=2018) {
  data = responses %>% filter(year == year_filter)
  resp = tapply(data$response_compaign, data$Label, sum)
  data = data.frame(response = resp, segment = attributes(resp)$dimnames[[1]])
  
ggplot(data, aes(x=segment, y=response)) + 
    geom_bar(fill = "#2B83BA", stat="identity", width = 0.6) + 
    scale_fill_brewer(palette = 'Spectral')+
    theme_bw()+
    labs(y="Count")
}

segment_response_plot <- function(responses, label='Active top') {
  data = responses %>% filter(Label == label)
  resp = tapply(data$response_compaign, data$year, sum)
  data = data.frame(response = resp, year = attributes(resp)$dimnames[[1]])
  
  ggplot(data, aes(x = year, y = response)) + 
    geom_bar(fill = "#2B83BA", stat= 'identity', width = 0.6)+ 
    scale_fill_brewer(palette = "Spectral")+
    theme_bw()+
    labs(y="Count")
}