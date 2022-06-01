library(dplyr)

nDigits <- function(x) nchar( trunc( abs(x) ) )

flight=read.csv('Flight_on_time_HIX.csv')

weather=read.csv('weather.csv')

Convert_String_To_Date <- function(date) {
  
  if(grepl('/',date)){
    #return(as.Date(date,"%m/%d/%Y"))
    return(format(as.Date(date,"%m/%d/%Y"),format="%Y-%m-%d"))
  }
  else{
    return(format(as.Date(date,"%Y-%m-%d"),format="%Y-%m-%d"))
  }
  
}

Flight_HR <- function(number) {
  
  if(nDigits(number)==1) {
    return(substr(paste0("000",number),0,2))
  }
  if(nDigits(number)==2) {
    return(substr(paste0("00",number),0,2))
  }
  if(nDigits(number)==3) {
    return(substr(paste0("0",number),0,2))
  }
  if(number==2400) {
    return(substr("0000",0,2))
  }
  if(nDigits(number)==4) {
    return(substr(as.character(number),0,2))
  }
}

Flight_Min <- function(number) {
  
  if(nDigits(number)==1) {
    return(substr(paste0("000",number),3,4))
  }
  if(nDigits(number)==2) {
    return(substr(paste0("00",number),3,4))
  }
  if(nDigits(number)==3) {
    return(substr(paste0("0",number),3,4))
  }
  if(number==2400) {
    return(substr("0000",3,4))
  }
  if(nDigits(number)==4) {
    return(substr(as.character(number),3,4))
  }
}


flight$Scheduled_Departure_Time_Hr <- sapply(flight$Scheduled_Departure_Time, Flight_HR)

flight$Scheduled_Departure_Time_Min <- sapply(flight$Scheduled_Departure_Time, Flight_Min)


flight$Actual_Departure_Time_Hr <- sapply(flight$Actual_Departure_Time, Flight_HR)

flight$Actual_Departure_Time_Min <- sapply(flight$Actual_Departure_Time, Flight_Min)


flight$Scheduled_Arrival_Time_Hr <- sapply(flight$Scheduled_Arrival_Time, Flight_HR)

flight$Scheduled_Arrival_Time_Min <- sapply(flight$Scheduled_Arrival_Time, Flight_Min)


flight$Actual_Arrival_Time_Hr <- sapply(flight$Actual_Arrival_Time, Flight_HR)

flight$Actual_Arrival_Time_Min <- sapply(flight$Actual_Arrival_Time, Flight_Min)

flight$FlightDate2 <- sapply(flight$FlightDate, Convert_String_To_Date)

#flight_filtered_origin --> Departure Delay

#flight_filtered_destination --> Arrival Delay

weather = weather %>% filter(airport=="Highland")

flight$FlightDepTs=paste0(flight$FlightDate2," ",flight$Scheduled_Departure_Time_Hr,":00:00")

flight$FlightArrTs=paste0(flight$FlightDate2," ",flight$Scheduled_Arrival_Time_Hr,":00:00")

flight_filtered_origin = flight %>% filter(Origin_Airport=="HIX")

flight_filtered_destination = flight %>% filter(Destination_Airport=="HIX")

flight_filtered_origin <- inner_join(flight_filtered_origin, weather, by = c("FlightDepTs" = "time")) 

flight_filtered_destination <- inner_join(flight_filtered_destination, weather, by = c("FlightArrTs" = "time")) 


#Only Departure Data
flight_filtered_origin_subset =select(flight_filtered_origin, -c(Flight_Number,
                                                                 Plane_ID,
                                                                 FlightDate,
                                                                 Scheduled_Departure_Time,
                                                                 Actual_Departure_Time,
                                                                 Scheduled_Arrival_Time,
                                                                 Actual_Arrival_Time,
                                                                 Arrival_Delay_Minutes,
                                                                 Arrival_Taxi,
                                                                 Arrival_WheelsOn,
                                                                 Scheduled_Departure_Time_Min,
                                                                 Actual_Departure_Time_Min,
                                                                 Scheduled_Arrival_Time_Hr,
                                                                 Scheduled_Arrival_Time_Min,
                                                                 Actual_Arrival_Time_Hr,
                                                                 Actual_Arrival_Time_Min,
                                                                 FlightDate2,
                                                                 FlightDepTs,
                                                                 FlightArrTs,
                                                                 X,
                                                                 airport,
                                                                 time2,
                                                                 precipAccumulation,
                                                                 precipType,
                                                                 Delay_Reason,
                                                                 Origin_Airport))

#Only Arrival Data
flight_filtered_destination_subset =select(flight_filtered_destination, -c(Flight_Number,
                                                                           Plane_ID,
                                                                           FlightDate,
                                                                           Scheduled_Departure_Time,
                                                                           Actual_Departure_Time,
                                                                           Scheduled_Arrival_Time,
                                                                           Actual_Arrival_Time,
                                                                           Departure_Delay_Minutes,
                                                                           Departure_Taxi,
                                                                           Departure_WheelsOff,
                                                                           Scheduled_Departure_Time_Min,
                                                                           Actual_Departure_Time_Min,
                                                                           Scheduled_Departure_Time_Hr,
                                                                           Scheduled_Arrival_Time_Min,
                                                                           Actual_Departure_Time_Hr,
                                                                           Actual_Arrival_Time_Min,
                                                                           FlightDate2,
                                                                           FlightDepTs,
                                                                           FlightArrTs,
                                                                           X,
                                                                           airport,
                                                                           time2,
                                                                           precipAccumulation,
                                                                           precipType,
                                                                           Delay_Reason,
                                                                           Destination_Airport))
flight_filtered_destination_subset=flight_filtered_destination_subset%>%
  mutate(weather_harsh=
           case_when(as.character(summary) == "Drizzle" ~ "Rainy",
                     as.character(summary) == "Light Rain"~ "Rainy",
                     as.character(summary) == "Light Snow"~ "Snow",
                     as.character(summary) == "Partly Cloudy" ~ "Cloudy", 
                     as.character(summary) == "Possible Drizzle" ~ "Rainy",
                     as.character(summary) == "Possible Flurries" ~ "Snow",
                     as.character(summary) == "Possible Light Rain" ~ "Rainy",
                     as.character(summary) == "Possible Light Snow" ~ "Snow",
                     as.character(summary) == "Rain" ~ "Rainy",
                     as.character(summary) == "Overcast"~ "Cloudy",
                     as.character(summary) == "Foggy"~ "Foggy",
                     as.character(summary) == "Mostly Cloudy" ~ "Cloudy",
                     TRUE ~ as.character(summary)
           ))

flight_filtered_origin_subset=flight_filtered_origin_subset%>%
  mutate(weather_harsh=
           case_when(as.character(summary) == "Drizzle" ~ "Rainy",
                     as.character(summary) == "Light Rain"~ "Rainy",
                     as.character(summary) == "Light Snow"~ "Snow",
                     as.character(summary) == "Partly Cloudy" ~ "Cloudy", 
                     as.character(summary) == "Possible Drizzle" ~ "Rainy",
                     as.character(summary) == "Possible Flurries" ~ "Snow",
                     as.character(summary) == "Possible Light Rain" ~ "Rainy",
                     as.character(summary) == "Possible Light Snow" ~ "Snow",
                     as.character(summary) == "Rain" ~ "Rainy",
                     as.character(summary) == "Overcast"~ "Cloudy",
                     as.character(summary) == "Foggy"~ "Foggy",
                     as.character(summary) == "Mostly Cloudy" ~ "Cloudy",
                     TRUE ~ as.character(summary)
           ))


flight_filtered_origin_subset=flight_filtered_origin_subset%>%mutate(delay=ifelse(flight_filtered_origin_subset$Departure_Delay_Minutes>0,1,0))

flight_filtered_destination_subset=flight_filtered_destination_subset%>%mutate(delay=ifelse(flight_filtered_destination_subset$Arrival_Delay_Minutes>0,1,0))

# attributes which are categorical - Airline, summary, uvIndex, Origin_airport, Destination_airport

#col_names_origin <- c("Airline", "summary", "uvIndex", "Destination_Airport","delay","Scheduled_Departure_Time_Hr")

flight_filtered_destination_subset=flight_filtered_destination_subset%>%
  mutate(time_band=
           case_when(as.numeric(Scheduled_Arrival_Time_Hr)>=0 & as.numeric(Scheduled_Arrival_Time_Hr)<6 ~ "H1", 
                     as.numeric(Scheduled_Arrival_Time_Hr)>=6 & as.numeric(Scheduled_Arrival_Time_Hr)<12 ~ "H2",
                     as.numeric(Scheduled_Arrival_Time_Hr)>=12 & as.numeric(Scheduled_Arrival_Time_Hr)<18 ~ "H3",
                     TRUE ~ "H4",
           ))

flight_filtered_origin_subset=flight_filtered_origin_subset%>%
  mutate(time_band=
           case_when(as.numeric(Scheduled_Departure_Time_Hr)>=0 & as.numeric(Scheduled_Departure_Time_Hr)<6 ~ "H1", 
                     as.numeric(Scheduled_Departure_Time_Hr)>=6 & as.numeric(Scheduled_Departure_Time_Hr)<12 ~ "H2",
                     as.numeric(Scheduled_Departure_Time_Hr)>=12 & as.numeric(Scheduled_Departure_Time_Hr)<18 ~ "H3",
                     TRUE ~ "H4",
           ))

col_names_origin <- c("Airline", "summary", "uvIndex", "Destination_Airport","delay","time_band","weather_harsh")

col_names_destination <- c("Airline", "summary", "uvIndex", "Origin_Airport","delay","time_band","weather_harsh")

sapply(flight_filtered_destination_subset,class)

flight_filtered_destination_subset[col_names_destination] <- lapply(flight_filtered_destination_subset[col_names_destination] , factor)


flight_filtered_origin_subset[col_names_origin] <- lapply(flight_filtered_origin_subset[col_names_origin] , factor)

sapply(flight_filtered_destination_subset,class)

flight_filtered_destination_subset$Scheduled_Arrival_Time_Hr=as.numeric(flight_filtered_destination_subset$Scheduled_Arrival_Time_Hr)

flight_filtered_destination_subset$Actual_Arrival_Time_Hr =as.numeric(flight_filtered_destination_subset$Actual_Arrival_Time_Hr)


flight_filtered_origin_subset$Scheduled_Departure_Time_Hr=as.numeric(flight_filtered_origin_subset$Scheduled_Departure_Time_Hr)

flight_filtered_origin_subset$Actual_Departure_Time_Hr =as.numeric(flight_filtered_origin_subset$Actual_Departure_Time_Hr)

library(devtools)

install_github("dgrtwo/broom")

library(broom)


library(caTools)
library(ROCR) 
library(tidyverse)
library(caret)

set.seed(123)
training.samples <- flight_filtered_origin_subset$delay %>% createDataPartition(p = 0.8, list = FALSE)
origin_train.data  <- flight_filtered_origin_subset[training.samples, ]
origin_test.data <- flight_filtered_origin_subset[-training.samples, ]

#model <- glm( delay ~., data = train.data, family = binomial)
origin_model <- glm( delay ~ Flight_Distance+Departure_Taxi+Departure_WheelsOff+
                +windSpeed+windBearing+visibility+time_band+precipIntensity+
                  +dewPoint+humidity+windGust+uvIndex+visibility
              , data = origin_train.data, family = binomial)
                     
              

summary(origin_model)
probabilities <- origin_model %>% predict(origin_test.data, type = "response")

max_accuracy=0
threshold_value=0
for (i in seq(0.01, 1, by=0.01)) 
{
#  print(i)
  predicted.classes = ifelse(probabilities > i, 1, 0)
  accuracy=mean(predicted.classes == origin_test.data$delay)
  if(accuracy>max_accuracy)
  {
    threshold_value=i;
    max_accuracy=accuracy
    print(threshold_value)
  }
}

predicted.classes = ifelse(probabilities > threshold_value, 1, 0)
accuracy=mean(predicted.classes == origin_test.data$delay)

print(paste0("Max accuracy at threshold value=",threshold_value," for which the accuracy is ",accuracy))

set.seed(123)

training.samples <- flight_filtered_destination_subset$delay %>% createDataPartition(p = 0.8, list = FALSE)
destination_train.data  <- flight_filtered_destination_subset[training.samples, ]
destination_test.data <- flight_filtered_destination_subset[-training.samples, ]

model <- glm( delay ~ Flight_Distance+Arrival_Taxi+Arrival_WheelsOn+
              +windSpeed+windBearing+cloudCover+visibility+time_band+summary
              , data = destination_train.data, family = binomial)


probabilities <- model %>% predict(destination_test.data, type = "response")


max_accuracy=0
threshold_value=0

for (i in seq(0.01, 1, by=0.01)) 
{
  #  print(i)
  predicted.classes = ifelse(probabilities > i, 1, 0)
  accuracy=mean(predicted.classes == destination_test.data$delay)
  if(accuracy>max_accuracy)
  {
    threshold_value=i;
    max_accuracy=accuracy
    #print(max_accuracy)
  }
}



predicted.classes = ifelse(probabilities > threshold_value, 1, 0)
accuracy=mean(predicted.classes == destination_test.data$delay)

print(paste0("Max accuracy at threshold value=",threshold_value," for which the accuracy is ",accuracy))


#######################################
