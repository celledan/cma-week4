library("readr")
library("dplyr")
library("sf")

wildschwein <- read_delim("wildschwein_BE_2056.csv", ",")

# Careful! What Timezone is assumed?
sabi <- wildschwein |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE) |>
  filter(TierName == "Sabi", DatetimeUTC >= "2015-07-01", DatetimeUTC < "2015-07-03")

library("ggplot2")
ggplot(sabi, aes(E, N, colour = DatetimeUTC))+
  geom_point() +
  geom_path() +
  labs(colour = "DatetimeUTC") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",   
        legend.key.width = unit(4, "lines"))

#Step a)
#Specify a temporal window = 60 min, interval 15 min

#Step b) measure the distance from every point to every other point within this temporal window
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

sabi <- sabi |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2)))  # distance to pos +30 minutes

#Calculate the mean distance
sabi <- sabi |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

sabi

#Step c) remove static points
sabi <- sabi |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

sabi_filter <- sabi |>
  filter(!static)

sabi_filter |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

################################################################################
#Segmentation

# Read the GPX file using its path
file_path <- "Data/20180810.gpx"
st_layers(file_path)
gpx_data <- st_read(file_path, layer = "track_points")

# Transform the data to the Swiss LV95 coordinate system (EPSG:2056)
gpx_data <- st_transform(gpx_data, 2056)

# Ensure the correct time format and sort the data by time
gpx_data$time <- as.POSIXct(gpx_data$time, format = "%Y-%m-%dT%H:%M:%SZ")
gpx_data <- gpx_data %>% arrange(time)

# Extract coordinates after transformation
gpx_data <- gpx_data %>% mutate(
  easting = st_coordinates(geometry)[, 1],
  northing = st_coordinates(geometry)[, 2]
)

#Task 1
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

#The time window of my data is irregular. How do I define how many points need 
#to be calculated if the number of points varies?
gpx_data <- gpx_data |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos ?minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos ? minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos ? mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2)))  # distance to pos ? minutes


#Calculate the mean distance
gpx_data <- gpy_data |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

gpx_data

#Step c) remove static points
gpx_data <- gpx_data |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

data_filter <- gpx_data |>
  filter(!static)

data_filter |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")
#no plot