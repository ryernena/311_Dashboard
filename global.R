library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shiny)
library(shinydashboard)
library(lubridate)
library(googleVis)
library(leaflet)
library(maps)
library(plotly)

srdata=read.csv('311_SR_MiamiDade_2019.csv', stringsAsFactors = F)

srdata=mutate(srdata,date_created=as.Date(srdata$ticket_created_date_time ))
srdata=mutate(srdata,date_updated=as.Date(srdata$ticket__last_update_date_time ))
srdata=mutate(srdata,date_closed=as.Date(srdata$ticket_closed_date_time ))
srdata$city =gsub("_"," ",srdata$city)
#only selected columns
table_disp=srdata%>%select(ticket_id,
                          issue_type,
                          issue_description,
                          case_owner,
                          street_address,
                          city,
                          state,
                          zip_code,
                          ticket_status,
                          location_city,
                          latitude,
                          longitude,
                          goal_days,
                          actual_completed_days,
                          method_received,
                          date_created,
                          date_closed)

#top 10 service requests by count
top10=group_by(srdata,issue_type)%>%summarise(n())%>%top_n(10)
vec10=as.vector(unlist(top10$issue_type))
srdata_top10=table_disp%>%filter(issue_type %in% vec10)
srdata_top10=mutate(srdata_top10,month=month((srdata_top10$date_closed),label=TRUE))

# marker icons
srdata_top10 <- mutate(srdata_top10,info=paste0(srdata_top10$issue_type,
                                                     "\n",
                                                     ", ", srdata_top10$street_address,
                                                     ", ", srdata_top10$city,
                                                     ", ", srdata_top10$state,
                                                     ", ", srdata_top10$zip_code))

# number of issues by each date
srs_bydate=srdata_top10%>%group_by(.,by=date_created)%>%summarise(n())
srs_bydate=rename(srs_bydate,date_created="by",total="n()")

#Top10 cities of requests
city_top10=group_by(srdata_top10,by=city)%>%summarise(n())%>%top_n(10)
city_top10=rename(city_top10,city='by',Total='n()')

#total number of each issues by city
issues_by_city=group_by(srdata_top10,by=city,issue_type)%>%summarise(city_total=n())
issues_by_city=rename(issues_by_city,city='by')

#for gauge chart
x=filter(srdata_top10,!is.na(actual_completed_days & !is.na(month)))%>%select(case_owner,month,goal_days,actual_completed_days)
x=mutate(x,ontime=ifelse(actual_completed_days<=0,'pending',ifelse(actual_completed_days<=goal_days,'pass','fail')))

#for Data table display
data_table=select(srdata_top10,ticket_id,issue_type,ticket_status,case_owner,info,method_received,month)

