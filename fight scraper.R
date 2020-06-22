# This is a web scraper that gathered MMA fighter stats from sherdog.com
# It was a piece of a bigger project, it took a fighter_list.txt of current ufc fighters,
# searched the name on sherdog fight finder, and parsed the html code to pull relevant info.
# 
# The main trouble was formatting everything and getting a consistent output
# to put into a csv file. 
# 
# I looked at the text strings associated with each field of the fighter page
# and made the code check for them. As a result the code here kind of inherits 
# the quirks of the webpage itself.

library(rvest)
library(dplyr)
library(stringr)
fighter_table <- matrix("",ncol = 6)
history_table <- matrix("",ncol = 6)
bufferRow <- matrix(" ", ncol = 6)
stat_table <- matrix("", ncol = 8)

bigGuy <- function(fighter_page){
  tableToAppend <- matrix("",ncol=6)
  
  #start get name and make matrix
  name <- fighter_page %>% 
    html_nodes(".fn") %>%
    html_text

  name <- as.data.frame(name,stringsAsFactors = F)
  name <- name %>%
    matrix(ncol = 6, byrow = T)
  
  #start get record and clean
  fighter_record <- fighter_page %>%
    html_nodes(".card") %>%
    html_text()
  
  fighter_record <- trimws(fighter_record)
  fighter_record <- toString(fighter_record)
  fighter_record <- gsub("\\s+", " ", str_trim(fighter_record))
  #end get record and clean

  name[2] <- " "
  name[3] <- " "
  name[4] <- " "
  name[5] <- " "
  name[6] <- " "
  #end get name and make matrix



  #start get fight history
  #need check loop for node
  
  
  
  node_checker <- fighter_page %>%
    html_nodes("td")%>%
    html_text
  
  if (node_checker[2] == "VS. Fighter"){
    fighter_table <- fighter_page %>%
      html_nodes("section:nth-child(4) td") %>%
      html_text() %>%
      matrix(ncol = 6, byrow = T)
  } else if (node_checker[2] == "Fighter"){
    fighter_table <- fighter_page %>%
      html_nodes("td") %>%
      html_text() %>%
      matrix(ncol = 6, byrow = T)
  }
  
  
  
  #end get fight history



  # Add column names from first entries of table and make consistent
  colnames(fighter_table) <- fighter_table[1,]
  colnames(tableToAppend) <- colnames(fighter_table)
  colnames(name) <- colnames(fighter_table)
  colnames(bufferRow) <- colnames(fighter_table)
  # end colnames and make consistent

  fighter_table <- fighter_table[-1,, drop = F]

  fighter_table <- fighter_table %>%
    as.data.frame(stringsAsFactors = F) %>% tbl_df() %>%
    # reorder
    select(Result, Fighter, `Method/Referee`, R, Time, Event)


  #start concatenate all tables to main
  tableToAppend <- rbind(tableToAppend, fighter_table)
  tableToAppend <- rbind(name,tableToAppend)
  tableToAppend <- rbind(tableToAppend, bufferRow)
  tableToAppend <- tableToAppend %>%
    as.data.frame(stringsAsFactors = F)
  #end concatenate all tables to main
  tableToAppend <- apply(tableToAppend,2,as.character)
  return(tableToAppend)
}





fighter_stats <- function(fighter_page){
  tableToAppend <- matrix("",ncol = 8)
  
  #get name UFC
  name <- fighter_page %>%
    html_nodes("h1")%>%
    html_text()
  tableToAppend[1,1] <- name[1]
  
  #get age UFC
  age <- fighter_page %>%
    html_nodes("#fighter-age")%>%
    html_text()
  if (length(age) != 0){
    tableToAppend[1,2] <- age
  }
  else if (length(age) == 0){
    tableToAppend[1,2] <- "NA"
  }
  

  #get record UFC
  record <- fighter_page %>%
    html_nodes("#fighter-skill-record")%>%
    html_text()
  if (length(record) != 0){
    tableToAppend[1,3] <- record
  }
  else if (length(record) == 0){
    tableToAppend[1,3] <- "NA"
  }
  
  #get weight UFC
  weight <- fighter_page %>%
    html_nodes("#fighter-weight")%>%
    html_text()
  tableToAppend[1,4] <- weight
  
  #get height UFC
  height <- fighter_page %>%
    html_nodes("#fighter-height")%>%
    html_text() 
  if (length(height) != 0){
    tableToAppend[1,5] <- height
  }
  else if (length(height) == 0){
    tableToAppend[1,5] <- "NA"
  }

  #get reach UFC
  reach <- fighter_page %>%
    html_nodes("#fighter-reach")%>%
    html_text() 
  if (length(reach) != 0){
    tableToAppend[1,6] <- reach
  }
  else if (length(reach) == 0){
    tableToAppend[1,6] <- "NA"
  }
  
  
  #get leg reach UFC
  legreach <- fighter_page %>%
    html_nodes("#fighter-leg-reach")%>%
    html_text() 
  if (length(legreach) != 0){
    tableToAppend[1,7] <- legreach
  }
  else if (length(legreach) == 0){
    tableToAppend[1,7] <- "NA"
  }
  
  #total strikes UFC
  strikes <- read_html(link_list[i])%>%
    html_nodes(".max-number") %>%
    html_text()
  totalstrikes <- strikes[1]
  tableToAppend[1,8] <- totalstrikes



return(tableToAppend)
}

for (i in seq(1,length(link_list))){
  mid_list <- fighter_stats(read_html(link_list[i]))
  stat_table <- rbind(stat_table, mid_list)
  cat("iteration",i,"complete\n")
}
print("stats gathered")



setwd('C:/Users/hey/Desktop/R projects')
write.csv(stat_table, file = "fighterFeatures.csv")
print("stats saved")



sherdoglinks <- list()
searchform <- "http://www.sherdog.com/stats/fightfinder?SearchTxt="
sherdogbase <- "http://www.sherdog.com"
for (i in seq(1,length(link_list))){
     fname <- read_html(link_list[i])%>%
       html_nodes("h1")
       fname <- fname[1] %>%
       html_text()
     sname <- gsub(" ", "+", fname)
     page <- paste0(searchform,sname)
     sherpage <- read_html(page)%>%
       html_nodes("td:nth-child(2) a")%>%
       html_attr("href")
       sherpage <- paste0(sherdogbase, sherpage[1])
       print(sherpage)
       sherdoglinks <- c(sherdoglinks,sherpage)
#       Append to sherdog links
}
sherdoglinks <- sherdoglinks[sherdoglinks != "http://www.sherdog.comNA"]
sherdoglinks <- sherdoglinks[sherdoglinks != "http://www.sherdog.com/events/ROTR-BJ-Penn-Presents-Just-Scrap-14354"]
sherdoglinks <- sherdoglinks[sherdoglinks != "http://www.sherdog.com/fighter/Tae-Hyun-Bang-137929"]

for (i in seq(1,length(sherdoglinks))){
  mid_list <- bigGuy(read_html(as.character(sherdoglinks[i])))
  history_table <- rbind(history_table, mid_list)
}
print("records gathered")

setwd('C:/Users/hey/Desktop/R projects')

write.csv(history_table, file = "recordTable.csv")
print("records saved")
setwd('C:/Users/hey/Desktop/')


link_list <- readLines("fighter_list.txt")

