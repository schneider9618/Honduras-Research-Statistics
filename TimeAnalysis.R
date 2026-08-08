#### Load packages
library(tidyverse)
library(gt)

#### Drop scientific notation
options(scipen = 999)

#### Import the REDCap CSV file
data <- read_csv(
  "/Users/anthonyschneider/Desktop/HospitalWaitTimes/Actual Research Project/WholeDataSet.csv"
)



#### CALCULATE TIME INTERVALS
### Calculate total emergency department length of stay for each patient
data <- data |>
  mutate(
    total_length_of_stay_minutes = as.numeric(
      difftime(
        time_admission_2,
        time_arrival,
        units = "mins"
      )
    )
  )
### Calculate door to physician time 
data <- data |>
  mutate(
    door_to_physician_time = as.numeric(
      difftime(
        time_seen,
        time_arrival,
        units = "mins"
      )
    )
  )
### Calculate time to disposition
# Looking at interval from seen by physician to disposition
data <- data |>
  mutate(
    time_to_dispo = as.numeric(
      difftime(
        time_admission_2,
        time_seen,
        units = "mins"
      )
    )
  )

### Analyze intervals
## Calculate average LOS, standard deviations, and 95% CI
average_los_by_month <- data |>
  group_by(time_period) |>
  summarise(
    n = sum(!is.na(total_length_of_stay_minutes)),
    average_los = mean(total_length_of_stay_minutes, na.rm = TRUE),
    sd_los = sd(total_length_of_stay_minutes,na.rm = TRUE),
    standard_error = sd_los / sqrt(n),
    critical_value = qt(0.975, df = n - 1),
    lower_95_ci = average_los - critical_value * standard_error,
    upper_95_ci = average_los + critical_value * standard_error,
    minimum_los = min(total_length_of_stay_minutes, na.rm = TRUE),
    maximum_los = max(total_length_of_stay_minutes, na.rm = TRUE),
    range_los = maximum_los - minimum_los)

## Calculate average door-to-physician time, standard deviations, and 95% CI
average_door_to_physician_by_month <- data |>
  group_by(time_period) |>
  summarise(
    n = sum(!is.na(door_to_physician_time)),
    average_dtp = mean(door_to_physician_time, na.rm = TRUE),
    sd_dtp = sd(door_to_physician_time,na.rm = TRUE),
    standard_error = sd_dtp / sqrt(n),
    critical_value = qt(0.975, df = n - 1),
    lower_95_ci = average_dtp - critical_value * standard_error,
    upper_95_ci = average_dtp + critical_value * standard_error,
    minimum_dtp = min(door_to_physician_time, na.rm = TRUE),
    maximum_dtp = max(door_to_physician_time, na.rm = TRUE),
    range_dtp = maximum_dtp - minimum_dtp)

## Calculate average time to dispo, standard deviations, and 95% CI
average_time_to_dispo <- data |>
  group_by(time_period) |>
  summarise(
    n = sum(!is.na(time_to_dispo)),
    average_ttd = mean(time_to_dispo, na.rm = TRUE),
    sd_ttd = sd(time_to_dispo, na.rm = TRUE),
    standard_error = sd_ttd / sqrt(n),
    critical_value = qt(0.975, df = n - 1),
    lower_95_ci = average_ttd - critical_value * standard_error,
    upper_95_ci = average_ttd + critical_value * standard_error,
    minimum_ttd = min(time_to_dispo, na.rm = TRUE),
    maximum_ttd = max(time_to_dispo, na.rm = TRUE),
    range_ttd = maximum_ttd - minimum_ttd)

View(average_los_by_month)
View(average_door_to_physician_by_month)
View(average_time_to_dispo)

## Create month by month table
average_times_table <- data |>
  group_by(time_period) |>
  summarise(
    average_los = mean(
      total_length_of_stay_minutes,
      na.rm = TRUE
    ),
    average_dtp = mean(
      door_to_physician_time,
      na.rm = TRUE
    ),
    average_ttd = mean(
      time_to_dispo,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(time_period)

View(average_times_table)


### Random
dec22 <-data |>
  filter(time_period == 2) |>
  select(time_arrival:time_admission_2,total_length_of_stay_minutes)
# apr24 <-data |> 
#   filter(time_period == 10)
 view(dec22)
# view(apr24)