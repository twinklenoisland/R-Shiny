library(RMySQL)
library(dplyr)

new_donors <- function(db, year=2018, month_day='0625') {
  query = paste("SELECT new.contact_id
            FROM 
                (SELECT contact_id,
                DATEDIFF(", year, "0101, MIN(act_date)) AS 'firstdonation' 
                FROM acts 
                WHERE act_date <= ", year, month_day,
                " GROUP BY 1) AS new 
            WHERE new.firstdonation <= 0", sep="")

  data = fetch(dbSendQuery(db, query), n=-1)
  write.csv(data, paste('donors_data_', year, '.csv', sep=""), row.names=FALSE)
  return(nrow(data))
}

donors_info <- function(db, current_year=2018, month_day='0625') {
  new = new_donors(db, current_year, month_day)
  progress = new/new_donors(db, current_year - 1, month_day)
  return(list(new, progress))
}

new_donations <- function(db, year=2018, month_day='0625') {
  query = paste("SELECT SUM(amount) FROM acts 
            WHERE (act_date >=", year, "0101) 
                AND (act_date <= ", year, month_day, ")",sep="")
  
  data = fetch(dbSendQuery(db, query), n=-1)
  write.csv(data, paste('donations_data_', year, '.csv', sep=""), row.names=FALSE)
  return(data[1,1])
}

donations_info <- function(db, current_year=2018, month_day='0625'){
  new = new_donations(db, current_year, month_day)
  progress = new/new_donations(db, current_year - 1, month_day)
  return(list(new, progress))
}