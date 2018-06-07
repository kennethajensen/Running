#### Loading libraries and setting constants ####

library(googlesheets)  # Allows direct access to Google Sheets
library(ggplot2)       # Library to create plots and graphs
library(extrafont)     # Provides access to fonts
library(caret)         # Used to access the BRNN modeling algorithm
library(rwunderground) # Provides access to weather information from Weather Underground

# Weather Underground API key
set_api_key("9b542ab4bf664712")

# To access the fonts, they must first be imported into the extrafont database
# This is only required the first time and takes several minutes to complete
# font_import()
loadfonts(device = "win")

#### Constants and Functions ####

# Constants
origin_for_dates         <- "1899-12-30 00:00:00 America/Chicago"
secs_per_day             <- 24* 60 * 60
weather_location_default <- "60605"
weather_sheet_name       <- "Weather Data"      # Name of the Google Sheet
weather_actuals_ws_name  <- "Weather Actuals"   # Name of the worksheet with weather actuals
weather_normals_ws_name  <- "Weather Normals"   # Name of the worksheet with weather normals

# Get the current time to determine if a race start is in the past or in the future
current_time <- Sys.time()

# Date and time conversion from the Google Sheets format to the R POSIX format
gs_date_to_posix <- function(date_in_days)
{
  # First convert from days to seconds
  date_in_secs <- round(date_in_days * secs_per_day)
  # Then convert to POSIX using the origin and time zone that was found through trial-and-error
  as.POSIXct(date_in_secs, origin = origin_for_dates, tz = "UTC")
}



#### Retrieving and preparing the Google Sheet with the race times ####

# Register sheet
sheet_competitive_running <- gs_title("Competitive Running")

# Read the "Events" worksheet as a dataframe
running_events <- gs_read(sheet_competitive_running,
                          ws = "Events",
                          range = cell_cols(2:7),
                          literal = FALSE)

# Rename the data frame columns to follow the naming standards
names(running_events)[names(running_events)=="Event"]      <- "race_name"
names(running_events)[names(running_events)=="Distance"]   <- "race_distance_miles"
names(running_events)[names(running_events)=="Date"]       <- "race_date"
names(running_events)[names(running_events)=="Start Time"] <- "race_start_time"
names(running_events)[names(running_events)=="Time"]       <- "race_chip_time"
names(running_events)[names(running_events)=="Pace"]       <- "race_pace"

# Discard any records that does not have a distance
running_events <- subset(running_events, !(is.na(race_distance_miles)))

# Add start times to any races where the start time has not been recorded
# Most races are in the early morning, so a default start time is set to 8:00 AM
running_events <- within(running_events,
                         {
                           race_start_time[is.na(race_start_time)] <- 1/3
                         })

# Date and time conversion from the Google Sheets format to the R format
running_events <- within(running_events,
                         {
                           # Get the race pace in seconds for use in the models
                           race_pace_secs    <- round(race_pace * secs_per_day)
                           race_start_time   <- race_date + race_start_time
                           race_finish_time  <- race_start_time + race_chip_time
                           race_mid_time     <- race_start_time + (race_chip_time / 2)
                           # Convert dates and times from days R timestamps
                           race_date         <- gs_date_to_posix(race_date)
                           race_start_time   <- gs_date_to_posix(race_start_time)
                           race_finish_time  <- gs_date_to_posix(race_finish_time)
                           race_mid_time     <- gs_date_to_posix(race_mid_time)
                           race_chip_time    <- gs_date_to_posix(race_chip_time)
                           race_pace         <- gs_date_to_posix(race_pace)
                           # Get the year for each event
                           race_year         <- as.integer(strftime(race_date, format = "%Y"))
                           # Get the day of the year for each event
                           race_ordinal_date <- as.integer(strftime(race_date, format = "%j"))
                         })

# Recode the distance in Miles to the distance name
running_events <- within(running_events,
                         {
                           # Generic race distance name using the distance in miles
                           race_distance_name <- paste(race_distance_miles, "Miles", sep=" ")
                           # Special race distances that require special treatment
                           race_distance_name[race_distance_miles == 3.10686]  <- "5K"
                           race_distance_name[race_distance_miles == 6.21371]  <- "10K"
                           race_distance_name[race_distance_miles == 13.10940] <- "Half Marathon"
                           race_distance_name[race_distance_miles == 26.21900] <- "Marathon"
                         })


# Add the shoes used in the race
running_events <- within(running_events,
                         {
                           race_shoes                                 <- "Other"
                           race_shoes[race_date <  "2017-07-28"]      <- "Adidas Glide Boost"
                           race_shoes[race_date >= "2017-07-28"]      <- "Nike Air Zoom Vomero 12"
                           race_shoes[race_date >  "2017-08-08" & 
                                      race_distance_miles <= 6.21371] <- "Nike Flyknit Racer"
                           
                         })

#### Loading Chicago weather data for all races ####

# First determine if the Google Sheet with weather data already exists
weather_sheet_exists <- tryCatch(
  {
    # Attempt to register the Google Sheet with the weather data
    # If the sheet does not exist, then an error will be thrown
    weather_sheet <- gs_title(weather_sheet_name)
    TRUE
  },
  error = function(e)
  {
    return(FALSE)
  }
)

# If the Google Sheet exists, then read it and get each unique race date
if(weather_sheet_exists)
{
  message("The Google Sheet '", weather_sheet_name, "' was found")
  # Then determine if the two required worksheets are present
  weather_actuals_ws_exists <- is.element(weather_actuals_ws_name , gs_ws_ls(weather_sheet))
  weather_normals_ws_exists <- is.element(weather_normals_ws_name , gs_ws_ls(weather_sheet))

  if(weather_actuals_ws_exists)
  {
    message("The workSheet '", weather_actuals_ws_name, "' was found")
    # Read the Google Sheet with the actual weather data that was already retrieved
    # Read the "Weather Actuals" worksheet as a dataframe
    weather_actuals_ws <- gs_read(weather_sheet,
                                  ws = weather_actuals_ws_name,
                                  literal = FALSE)
    
    # Convert dates and times from days to the R POSIX format
    weather_actuals_ws <- within(weather_actuals_ws,
                                 {
                                   race_date       <- gs_date_to_posix(race_date)
                                   date            <- gs_date_to_posix(date)
                                 })
    
    # Get a list of the race dates for which the weather has already been retrieved
    race_dates_w_weather_actuals <- weather_actuals_ws[!duplicated(weather_actuals_ws$race_date), ]
    race_dates_w_weather_actuals <- race_dates_w_weather_actuals[,"race_date"]
  } else
  {
    # If the workheet does not exist already
    message("The workheet '", weather_actuals_ws_name, "' was NOT found")
    # Create a data frame with a single 'dummy' date
    race_dates_w_weather_actuals <- data.frame(race_date = as.POSIXct("1900-01-01"))
    # Indicate that the worksheet is not present
    weather_actuals_ws_exists <- FALSE
  }
  
  if(weather_normals_ws_exists)
  {
    message("The workSheet '", weather_normals_ws_name, "' was found")
    # Read the worksheet with the normal weather data that was already retrieved
    # Read the "Weather Normals" worksheet as a dataframe
    weather_normals_ws <- gs_read(weather_sheet,
                                  ws = weather_normals_ws_name,
                                  literal = FALSE)
    
    # Convert dates and times from days to the R POSIX format
    message(weather_actuals_ws_name, ": Converting dates")
    weather_normals_ws <- within(weather_normals_ws,
                                 {
                                   race_date <- gs_date_to_posix(race_date)
                                 })
    
    # Get a list of the race dates for which the weather has already been retrieved
    message(weather_actuals_ws_name, ": Creating list of unique dates")
    race_dates_w_weather_normals <- weather_normals_ws[!duplicated(weather_normals_ws$race_date), ]
    race_dates_w_weather_normals <- race_dates_w_weather_normals[,"race_date"]
  } else
  {
    # If the workheet does not exist already
    message("The workheet '", weather_normals_ws_name, "' was NOT found")
    # Create a data frame with a single 'dummy' date
    race_dates_w_weather_normals <- data.frame(race_date = as.POSIXct("1900-01-01"))
    # Indicate that the worksheet is not present
    weather_normals_ws_exists <- FALSE
  }
  
} else
{
  # If the Google Sheet does not exist already
  message("The Google Sheet '", weather_sheet_name, "' was NOT found")
  # Create a data frame with a single 'dummy' date for both worksheets
  race_dates_w_weather_actuals <- data.frame(race_date = as.POSIXct("1900-01-01"))
  race_dates_w_weather_normals <- data.frame(race_date = as.POSIXct("1900-01-01"))
  # Indicate that the two required worksheets are not present
  weather_actuals_ws_exists <- FALSE
  weather_normals_ws_exists <- FALSE
}

# Convert the race date to a string in the formats needed by the rwunderground API "YYYYMMDD" and "MMDD"
running_events$race_YYYYMMDD <- gsub("-", "", as.character(running_events$race_date))
running_events$race_MMDD     <- substr(running_events$race_YYYYMMDD, 5, 8)

# Initialize the booleans that show if it's the first past or future date being processed
first_weather_actual <- TRUE
first_weather_normal <- TRUE

# Loop through each of the race dates and retrieve the weather data
# for any races where we do not already have it
for (ii in 1:nrow(running_events))
# for (ii in 18:22)
{
  race_date       <- running_events[ii, "race_date"]
  message("ii = ", ii, " - Race Date = ", c(running_events[ii, "race_YYYYMMDD"]))
  # Determine if the current race is in the past
  if(race_date[1,1] < current_time)
  {
    # Determine if the weather for current race date has been retrieved previously
    if(!(is.element(race_date, race_dates_w_weather_actuals$race_date)))
    {
      # If this is not the first weather data retrieved, add a 6 second delay
      # The API will not service more than 10 dates per minute
      if(!first_weather_actual || !first_weather_normal)
      {
        message("Wait for 6 seconds before retrieving the next weather record")
        Sys.sleep(6)
      }
      # Actual hourly weather for past races
      # The API returns the weather for each hour of the selected date
      message("Retrieving the weather for ", c(running_events[ii, "race_YYYYMMDD"]))
      weather_location <- set_location(zip_code = weather_location_default)
      weather_this_race_date <- history(weather_location,
                                        date = running_events[ii, "race_YYYYMMDD"])
      # Add the race date to the dataframe
      weather_this_race_date <- merge(race_date,
                                      weather_this_race_date,
                                      all.x = TRUE)
      # Add the date and time of when the weather record was retrieved
      weather_this_race_date$retrieval_date <- current_time
      
      # Combine the weather from each race into a single dataframe
      if(first_weather_actual)
      {
        weather_actuals <- weather_this_race_date
        first_weather_actual <- FALSE
      } else
      {
        weather_actuals <- rbind(weather_actuals, weather_this_race_date)
      }
    } else
    {
      # The weather for this race date has been retrieved earlier
      message("The weather for ", c(running_events[ii, "race_YYYYMMDD"]), " was retrieved earlier")
    }
  } else if(race_date[1,1] >= current_time)
  {
    # Daily weather normals for future race dates
    # Code to handle these will be developed later
    message("The race on ", c(running_events[ii, "race_YYYYMMDD"]), " is in the future")
    
    # Determine if the weather for current race date has been retrieved previously
    if(!(is.element(race_date, race_dates_w_weather_normals$race_date)))
    {
      # If this is not the first weather data retrieved, add a 6 second delay
      # The API will not service more than 10 dates per minute
      if(!first_weather_actual || !first_weather_normal)
      {
        message("Wait for 6 seconds before retrieving the next weather record")
        Sys.sleep(6)
      }
      # Actual hourly weather for past races
      # The API returns the weather for each hour of the selected date
      message("Retrieving the weather for ", c(running_events[ii, "race_YYYYMMDD"]))
      weather_location <- set_location(zip_code = weather_location_default)
      weather_this_race_date <- planner(weather_location,
                                        start_date = running_events[ii, "race_MMDD"],
                                        end_date = running_events[ii, "race_MMDD"])
      # Add the race date to the dataframe
      weather_this_race_date <- merge(race_date,
                                      weather_this_race_date,
                                      all.x = TRUE)
      # Add the date and time of when the weather record was retrieved
      weather_this_race_date$retrieval_date <- current_time
      
      # Combine the weather from each race into a single dataframe
      if(first_weather_normal)
      {
        weather_normals <- weather_this_race_date
        first_weather_normal <- FALSE
      } else
      {
        weather_normals <- rbind(weather_normals, weather_this_race_date)
      }
    } else
    {
      # The weather for this race date has been retrieved earlier
      message("The weather normals for ", c(running_events[ii, "race_MMDD"]), " was retrieved earlier")
    }
  }
}
running_events$race_YYYYMMDD <- NULL
running_events$race_MMDD     <- NULL

# If any new weather actuals were retrieved, then create the Google Sheet, add the worksheets
# and add the new records to the worksheets as needed
if(exists("weather_actuals"))
{
  if(weather_sheet_exists && weather_actuals_ws_exists)
  {
    # If the Google Sheet and the Weather Actuals Worksheet already exists, then append the new records
    # Add the weather actuals data retrieved from wunderground to the Google Sheet
    message("Appending the date to the worksheet '", weather_actuals_ws_name, "'")
    weather_sheet <- gs_add_row(weather_sheet, 
                                ws = weather_actuals_ws_name, 
                                input = weather_actuals)
    # Combine the weather data from the worksheet with the newly retrieved weather data
    weather_actuals <- rbind(weather_actuals_ws, weather_actuals)
  } else if(weather_sheet_exists && !weather_actuals_ws_exists)
  {
    # If the Google Sheet exists, but does not have the Weather Actuals Worksheet
    # Add the weather normals data retrieved from wunderground to a new worksheet
    message("Creating the worksheet '", weather_actuals_ws_name, "'")
    weather_sheet <- gs_ws_new(weather_sheet, 
                               ws_title = weather_actuals_ws_name, 
                               input = weather_actuals, 
                               trim = TRUE, 
                               verbose = FALSE)
    weather_actuals_ws_exists <- TRUE
  } else
  {
    # If the Google Sheet does not exist, then create the Sheet and the workbook
    message("Creating new Google Sheet and the worksheet '", weather_actuals_ws_name, "'")
    weather_sheet <- gs_new(weather_sheet_name, 
                            ws_title = weather_actuals_ws_name, 
                            input = weather_actuals, 
                            trim = TRUE, 
                            verbose = FALSE)
    weather_sheet_exists <- TRUE
    weather_actuals_ws_exists <- TRUE
  }
} else
{
  # No new weather data was retrieved
  message("No new weather actuals were retrieved")
  weather_actuals <- weather_actuals_ws
}

# If any new weather normals were retrieved, then add the new records to the worksheet
# or create the worksheet if required
if(exists("weather_normals"))
{
  if(weather_sheet_exists && weather_normals_ws_exists)
  {
    # If the Google Sheet and the Weather Normals Worksheet already exists, then append the new records
    # Add the data retrieved from wunderground to the Google Sheet
    message("Appending the date to the worksheet '", weather_normals_ws_name, "'")
    weather_sheet <- gs_add_row(weather_sheet, 
                                ws = weather_normals_ws_name, 
                                input = weather_normals)
    # Combine the weather data from the worksheet with the newly retrieved weather data
    weather_normals <- rbind(weather_normals_ws, weather_normals)
  } else if(weather_sheet_exists && !weather_normals_ws_exists)
  {
    # If the Google Sheet exists, but does not have the Weather Normals Worksheet
    # Add the data retrieved from wunderground to a new worksheet
    message("Creating the worksheet '", weather_normals_ws_name, "'")
    weather_sheet <- gs_ws_new(weather_sheet, 
                               ws_title = weather_normals_ws_name, 
                               input = weather_normals, 
                               trim = TRUE, 
                               verbose = FALSE)
    weather_normals_ws_exists <- TRUE
  } else
  {
    # If the Google Sheet does not exist, then create the Sheet and the workbook
    message("Creating new Google Sheet and the worksheet '", weather_normals_ws_name, "'")
    weather_sheet <- gs_new(weather_sheet_name, 
                            ws_title = weather_normals_ws_name, 
                            input = weather_normals, 
                            trim = TRUE, 
                            verbose = FALSE)
    weather_sheet_exists <- TRUE
    weather_normals_ws_exists <- TRUE
  }
} else
{
  # No new weather data was retrieved
  message("No new weather normals were retrieved")
  weather_normals <- weather_normals_ws
}







# Only keep the actual weather records that correspond with the time of day for a race on that date
# Compute the time difference between the weather observation and the race start time
race_dates_and_times = subset(running_events, select = c(race_date, race_start_time, race_mid_time))
race_dates_and_times <- within(race_dates_and_times,
                               {
                                 race_mid_time[is.na(race_mid_time)] <- race_start_time[is.na(race_mid_time)]
                               })
weather_actuals <- merge(weather_actuals, 
                         race_dates_and_times, 
                         by = "race_date")
weather_actuals$minutes_race_to_weather <- abs(difftime(weather_actuals$date, 
                                                        weather_actuals$race_mid_time, 
                                                        units = "mins"))
# Aggregate the dataframe by the race date and computing the minimum time difference between
# the race start time and the weather observation
closest_weather_actual <- aggregate(x   = weather_actuals["minutes_race_to_weather"],
                                    by  = list(race_date = weather_actuals$race_date),
                                    FUN = min)
weather_actuals <- merge(weather_actuals,
                         closest_weather_actual)
rm(closest_weather_actual)
weather_actuals$race_start_time <- NULL
weather_actuals$race_mid_time <- NULL
weather_actuals$minutes_race_to_weather <- NULL
running_events$race_mid_time <- NULL


# Clean up the weather data to include only the relevant features
# Weather actuals
weather_actuals = subset(weather_actuals, select = c(race_date, cond, precip, temp))
names(weather_actuals)[names(weather_actuals)=="cond"]   <- "cond_actual"
names(weather_actuals)[names(weather_actuals)=="precip"] <- "precip_actual"
names(weather_actuals)[names(weather_actuals)=="temp"]   <- "temp_actual"
weather_actuals <- within(weather_actuals,
                          {
                            cond_normal   <- NA
                            precip_normal <- NA
                            temp_normal   <- NA
                          })

# If a precipitation measurement is missing, set it to zero
weather_actuals$precip_actual[is.na(weather_actuals$precip_actual)] <- 0

# Weather normals
weather_normals <- within(weather_normals,
                          {
                            temp <- (temp_low_avg + temp_high_avg) / 2
                            precip <- precip_avg
                          })
weather_normals = subset(weather_normals, select = c(race_date, cond, precip, temp))
names(weather_normals)[names(weather_normals)=="cond"]   <- "cond_normal"
names(weather_normals)[names(weather_normals)=="precip"] <- "precip_normal"
names(weather_normals)[names(weather_normals)=="temp"]   <- "temp_normal"
weather_normals <- within(weather_normals,
                          {
                            cond_actual   <- NA
                            precip_actual <- NA
                            temp_actual   <- NA
                          })

# Combine the actual and normal weather
weather_data <- rbind(weather_actuals, weather_normals)
weather_data <- within(weather_data,
                       {
                         condition <- cond_actual
                         condition[is.na(condition)] <- cond_normal[is.na(condition)]
                         condition <- tolower(condition)
                         cond_actual <- NULL
                         cond_normal <- NULL
                         temperature <- temp_actual
                         temperature[is.na(temperature)] <- temp_normal[is.na(temperature)]
                         temp_actual <- NULL
                         temp_normal <- NULL
                         precipitation <- precip_actual
                         precipitation[is.na(precipitation)] <- precip_normal[is.na(precipitation)]
                         precip_actual <- NULL
                         precip_normal <- NULL
                       })

# Merge the race data and the weather data
running_events <- merge(running_events,
                        weather_data,
                        by = "race_date",
                        all.x = TRUE)

# Clean up the environment by removing objects that are no longer needed
if(exists("first_weather_actual"))         rm(first_weather_actual)
if(exists("first_weather_normal"))         rm(first_weather_normal)
if(exists("ii"))                           rm(ii)
if(exists("race_date"))                    rm(race_date)
if(exists("race_start_time"))              rm(race_start_time)
if(exists("race_dates_and_times"))         rm(race_dates_and_times)
if(exists("weather_location_default"))     rm(weather_location_default)
if(exists("weather_location"))             rm(weather_location)
if(exists("weather_actuals"))              rm(weather_actuals)
if(exists("weather_normals"))              rm(weather_normals)
if(exists("race_dates_w_weather_actuals")) rm(race_dates_w_weather_actuals)
if(exists("race_dates_w_weather_normals")) rm(race_dates_w_weather_normals)
if(exists("weather_sheet_name"))           rm(weather_sheet_name)
if(exists("weather_sheet_exists"))         rm(weather_sheet_exists)
if(exists("weather_actuals_ws_name"))      rm(weather_actuals_ws_name)
if(exists("weather_actuals_ws_exists"))    rm(weather_actuals_ws_exists)
if(exists("weather_actuals_ws"))           rm(weather_actuals_ws)
if(exists("weather_normals_ws_name"))      rm(weather_normals_ws_name)
if(exists("weather_normals_ws_exists"))    rm(weather_normals_ws_exists)
if(exists("weather_normals_ws"))           rm(weather_normals_ws)
if(exists("weather_sheet"))                rm(weather_sheet)
if(exists("weather_this_race_date"))       rm(weather_this_race_date)
if(exists("weather_data"))                 rm(weather_data)


#### Split the data into races with (past) and without (future) a finishing time ####
running_events_past   <- subset(running_events, !(is.na(race_chip_time)))
running_events_future <- subset(running_events, is.na(race_chip_time))

# Get the date of the most recently completed race
latest_race_date     <- max(running_events_past$race_date)
latest_race_date_txt <- strftime(latest_race_date, "Based on data through %B %d, %Y" )

#### Creating plots and models ####

# Race Pace by Event Date
ggplot(running_events_past, aes(race_date, race_pace, colour = race_distance_name, shape = race_shoes)) + 
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour  = "Race Distance", 
       shape   = "Shoes",
       x       = "Date of Event", 
       y       = "Race Pace (secs/mi)",
       title   = "Competitive Running",
       caption = latest_race_date_txt) +
  theme(text=element_text(family = "Tahoma"))

# Race Pace by Weather Conditions
ggplot(running_events_past, aes(condition, race_pace, colour = race_distance_name, shape = race_shoes)) + 
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour  = "Race Distance", 
       shape   = "Shoes",
       x       = "Weather Conditions", 
       y       = "Race Pace (secs/mi)",
       title   = "Competitive Running",
       caption = latest_race_date_txt) +
  theme(text=element_text(family = "Tahoma"))

# Race Pace by Temperature
ggplot(running_events_past, aes(temperature, race_pace, colour = race_distance_name, shape = race_shoes)) + 
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour  = "Race Distance", 
       shape   = "Shoes",
       x       = "Temperature", 
       y       = "Race Pace (secs/mi)",
       title   = "Competitive Running",
       caption = latest_race_date_txt) +
  theme(text=element_text(family = "Tahoma"))


# Linear Regression Model
model_race_pace_lm <- lm(race_pace_secs ~ race_year + race_ordinal_date + sqrt(race_distance_miles) + temperature + precipitation, running_events_past)
running_events_past$race_pace_secs_lm <- round(fitted(model_race_pace_lm), digits = 0)
running_events_past$race_pace_secs_lm_residuals <- round(residuals(model_race_pace_lm), digits = 0)
running_events_past$race_pace_lm <- as.POSIXct(running_events_past$race_pace_secs_lm,
                                               origin = origin_for_dates,
                                               tz = "UTC")

# Estimated by actual race pace
ggplot(running_events_past, aes(x = race_pace, y = race_pace_lm)) +
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
    labs(colour = "Race Distance", 
       shape = "Shoes",
       x = "Race Pace (secs/mi)", 
       y = "Fitted Race Pace (secs/mi)",
       title = "Linear Regression Model Fit",
       caption = latest_race_date_txt) +
  geom_smooth(method = "loess") +
  theme(text=element_text(family = "Tahoma"))

# Estimated race pace by race distance
ggplot(running_events_past, aes(x = race_distance_miles, y = race_pace_lm)) +
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour = "Race Distance", 
       shape = "Shoes",
       x = "Race Distance (miles)", 
       y = "Fitted Race Pace (secs/mi)",
       title = "Linear Regression Model Fit",
       caption = latest_race_date_txt) +
  theme(text=element_text(family = "Tahoma"))

# Residuals by race date
ggplot(running_events_past, aes(race_date, race_pace_secs_lm_residuals)) +
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour = "Race Distance", 
       shape = "Shoes",
       x = "Race Date", 
       y = "Residuals (secs/mi)",
       title = "Linear Regression Model Fit",
       caption = latest_race_date_txt) +
  geom_smooth(method = "lm") +
  theme(text=element_text(family = "Tahoma"))

# Residuals by race distance
ggplot(running_events_past, aes(x = race_distance_miles, y = race_pace_secs_lm_residuals)) +
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour = "Race Distance", 
       shape = "Shoes",
       x = "Race Distance (miles)", 
       y = "Residuals (secs/mi)",
       title = "Linear Regression Model Fit",
       caption = latest_race_date_txt) +
  theme(text=element_text(family = "Tahoma"))


# Predictions based on the linear regression
running_events_future$race_pace_secs_lm <- round(predict(model_race_pace_lm, newdata = running_events_future), digits = 1)
running_events_future$race_chip_time_secs_lm <-round(running_events_future$race_distance_miles * running_events_future$race_pace_secs_lm, digits = 0)


# Bayesian Regularization for Feed-Forward Neural Networks
model_race_pace_brnn <- train(race_pace_secs ~ race_year + race_ordinal_date + race_distance_miles + race_shoes + temperature + precipitation, 
                              data = running_events_past, 
                              method = "brnn")
running_events_past$race_pace_secs_brnn <- round(fitted(model_race_pace_brnn), digits = 0)
running_events_past$race_pace_secs_brnn_residuals <- round(residuals(model_race_pace_brnn), digits = 0)

running_events_past$race_pace_brnn <- as.POSIXct(running_events_past$race_pace_secs_brnn,
                                                 origin = origin_for_dates,
                                                 tz = "UTC")

ggplot(running_events_past, aes(x = race_pace, y = race_pace_brnn)) +
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour = "Race Distance", 
       shape = "Shoes",
       x = "Race Pace (secs/mi)", 
       y = "Fitted Race Pace (secs/mi)",
       title = "BRNN Model Fit",
       caption = latest_race_date_txt) +
  geom_smooth(method = "loess") +
  theme(text=element_text(family = "Tahoma"))

ggplot(running_events_past, aes(race_date, race_pace_secs_brnn_residuals)) +
  geom_point(aes(colour=factor(race_distance_name), shape=factor(race_shoes)), size = 3) +
  labs(colour = "Race Distance", 
       shape = "Shoes",
       x = "Race Date", 
       y = "Residuals (secs/mi)",
       title = "BRNN Model Residuals",
       caption = latest_race_date_txt) +
  geom_smooth(method = "lm") +
  theme(text=element_text(family = "Tahoma"))

# Predictions based on the linear regression
running_events_future$race_pace_secs_brnn <- round(predict(model_race_pace_brnn, newdata = running_events_future), digits = 1)
running_events_future$race_chip_time_secs_brnn <-round(running_events_future$race_distance_miles * running_events_future$race_pace_secs_brnn, digits = 0)
