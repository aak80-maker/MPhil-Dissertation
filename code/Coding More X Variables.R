#The data sets loaded here can be found in data/intermediary
#Relevant data sets are: 'latslons.csv', 'updated_data_distance_from_water_and_roads.csv','data_with_urban.csv'

library(tidyverse)
#install.packages(c("tidyverse", "ggplot2", "rnaturalearth", "rnaturalearthdata", "sf"))
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

#Load Data
data<-read_csv("updated_data_distance_from_water_and_roads.csv")
data

#plot map of the world!
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  geom_point(data = data, aes(x = Longitude, y = Latitude), size = 0.02) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", title='Location of REDD+ Projects')


library(sf)
points<-st_as_sf(data, coords = c("Longitude", "Latitude"), crs=4326) #makes lats and lons spatial
library(osmdata)
install.packages(c("osmdata"))
get_roads_one_point <- function(pt, dist_m = 10000) { #creates function to extract data to closest roads within 10,000m of each point
  bb <- pt |>
    st_transform(3857) |>
    st_buffer(dist_m) |>
    st_transform(4326) |>
    st_bbox()
  
  opq(bb, timeout = 180) |>
    add_osm_feature(
      key = "highway",
      value = c("primary", "secondary", "tertiary", "unclassified")
    ) |>
    osmdata_sf()
}
test <- get_roads_one_point(points[1, ], dist_m = 10000) #extracts roads within 10,000m of one project

roads <- test$osm_lines
nrow(roads)
point_m <- st_transform(points[1, ], 3857)
roads_m <- st_transform(roads, 3857)

dist_to_road <- min(st_distance(point_m, roads_m)) #takes minimum distance

dist_to_road

library(sf)
library(osmdata)
library(dplyr)
library(purrr)

get_road_distance_safe <- function(pt, dist_m = 10000) {
  tryCatch({
    roads <- get_roads_one_point(pt, dist_m = dist_m)$osm_lines
    
    if (is.null(roads) || nrow(roads) == 0) {
      return(NA_real_)
    }
    
    pt_m <- st_transform(pt, 3857)
    roads_m <- st_transform(roads, 3857)
    
    as.numeric(min(st_distance(pt_m, roads_m)))
  }, error = function(e) {
    return(NA_real_)
  })
}

data$dist_road_m <- purrr::map_dbl(
  seq_len(nrow(points)),
  ~ get_road_distance_safe(points[.x, ], dist_m = 10000)
)
which(is.na(data$dist_road_m))

data$dist_road_m <- NA_real_ #this filters through all points to find distance to roads within 5000m

for (i in seq_len(nrow(points))) {
  message("Running point ", i, " of ", nrow(points))
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 5000)
  Sys.sleep(2)
}
# for rows that do not have roads within 5000m, try 10,000m
na_rows <- which(is.na(data$dist_road_m))
for (i in na_rows) {
  message("Re-running point ", i)
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 10000)
  Sys.sleep(2)
}
na_rows
na_rows <- which(is.na(data$dist_road_m))
na_rows
#for rows that do not have roads within 5000m, try 20,000m
na_rows <- which(is.na(data$dist_road_m))

for (i in na_rows) {
  message("Re-running point ", i)
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 20000)
  Sys.sleep(2)
  }
#for rows that do not have roads within 20,000, try 30,000m
na_rows <- which(is.na(data$dist_road_m))
for (i in na_rows) {
  message("Re-running point ", i)
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 30000)
  Sys.sleep(2)
}
#for rows that do not have roads within 20,000, try 40,000m
na_rows <- which(is.na(data$dist_road_m))
for (i in na_rows) {
  message("Re-running point ", i)
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 40000)
  Sys.sleep(2)
}
#for rows that do not have roads within 40,000, try 50,000m
na_rows <- which(is.na(data$dist_road_m))
for (i in na_rows) {
  message("Re-running point ", i)
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 50000)
  Sys.sleep(2)
}
#for rows that do not have roads within 50,000, try 60,000m
na_rows <- which(is.na(data$dist_road_m))
for (i in na_rows) {
  message("Re-running point ", i)
  data$dist_road_m[i] <- get_road_distance_safe(points[i, ], dist_m = 60000)
  Sys.sleep(2)
}

# include tracks and unclassified roads too
get_roads_with_track <- function(pt, dist_m = 10000) {
  tryCatch({
    roads <- opq(st_bbox(
      st_transform(
        st_buffer(st_transform(pt, 3857), dist_m),
        4326
      )
    ), timeout = 180) %>%
      add_osm_feature(
        key = "highway",
        value = c("primary","secondary","tertiary","unclassified","track")
      ) %>%
      osmdata_sf()
    
    roads_sf <- roads$osm_lines
    
    if (is.null(roads_sf) || nrow(roads_sf) == 0) {
      return(NA_real_)
    }
    
    pt_m <- st_transform(pt, 3857)
    roads_m <- st_transform(roads_sf, 3857)
    
    as.numeric(min(st_distance(pt_m, roads_m)))
    
  }, error = function(e) {
    return(NA_real_)
  })
}

data$dist_road_track_m <- NA_real_

for (i in seq_len(nrow(points))) {
  message("Running point ", i, " of ", nrow(points))
  
  data$dist_road_track_m[i] <- get_roads_with_track(
    points[i, ],
    dist_m = 10000
  )
  
  message("Distance = ", data$dist_road_track_m[i], " m")
  
  Sys.sleep(2)
}
# find rows with distances still missing
na_rows <- which(is.na(data$dist_road_track_m))

# rerun only those rows with expanded buffer area
for (i in na_rows) {
  message("Re-running point ", i, " of ", nrow(points), " with 25 km buffer")
  
  data$dist_road_track_m[i] <- get_roads_with_track(
    points[i, ],
    dist_m = 25000
  )
  
  message("New distance = ", data$dist_road_track_m[i], " m")
  
  Sys.sleep(2)
}
# find rows still missing
na_rows <- which(is.na(data$dist_road_track_m))

# rerun only those rows with expanded buffer- even bigger
for (i in na_rows) {
  message("Re-running point ", i, " of ", nrow(points), " with 40 km buffer")
  
  data$dist_road_track_m[i] <- get_roads_with_track(
    points[i, ],
    dist_m = 40000
  )
  
  message("New distance = ", data$dist_road_track_m[i], " m")
  
  Sys.sleep(2)
}
# find rows still missing
na_rows <- which(is.na(data$dist_road_track_m))

# rerun only those rows with expanded buffer- even bigger
for (i in na_rows) {
  message("Re-running point ", i, " of ", nrow(points), " with 60 km buffer")
  
  data$dist_road_track_m[i] <- get_roads_with_track(
    points[i, ],
    dist_m = 60000
  )
  
  message("New distance = ", data$dist_road_track_m[i], " m")
  
  Sys.sleep(2)
}

#now try with water! same data set, just adapting function
get_water_distance <- function(pt, dist_m = 10000) {
  tryCatch({
    
    water <- opq(st_bbox(
      st_transform(
        st_buffer(st_transform(pt, 3857), dist_m),
        4326
      )
    ), timeout = 180) %>%
      add_osm_feature(
        key = "waterway",
        value = c("river","stream","canal")
      ) %>%
      osmdata_sf()
    
    water_sf <- water$osm_lines
    
    if (is.null(water_sf) || nrow(water_sf) == 0) {
      return(NA_real_)
    }
    
    pt_m <- st_transform(pt, 3857)
    water_m <- st_transform(water_sf, 3857)
    
    as.numeric(min(st_distance(pt_m, water_m)))
    
  }, error = function(e) {
    return(NA_real_)
  })
}

data$dist_water_m <- NA_real_

for (i in seq_len(nrow(points))) {
  message("Running point ", i, " of ", nrow(points))
  
  data$dist_water_m[i] <- get_water_distance(
    points[i, ],
    dist_m = 10000
  )
  
  message("Distance = ", data$dist_water_m[i], " m")
  
  Sys.sleep(2)
}

na_rows <- which(is.na(data$dist_water_m))

for (i in na_rows) {
  message("Re-running point ", i, " with 25 km buffer")
  
  data$dist_water_m[i] <- get_water_distance(
    points[i, ],
    dist_m = 25000
  )
  
  Sys.sleep(2)
}

na_rows <- which(is.na(data$dist_water_m))

for (i in na_rows) {
  message("Re-running point ", i, " with 25 km buffer")
  
  data$dist_water_m[i] <- get_water_distance(
    points[i, ],
    dist_m = 35000
  )
  
  Sys.sleep(2)
}
data$dist_road_m_test <- NULL
write.csv(data, "updated_data.csv", row.names=FALSE) #save data as a new csv

# Now lets try with urban areas! loading new data set - Global Human Settlement Layer
library(sf)
urban_sf <- st_read("GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_UC_V2_0.shp")
urban_duc<- st_read("GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_DUC_V2_0.shp")
# reproject both layers to meters
urban_m  <- st_transform(urban_sf, 3857)
urban_new <-st_transform(urban_duc, 3857)
coords <- read.csv("latslons.csv")
names(coords) <- c("ID", "longitude", "latitude")
data <- merge(data, coords, by = "ID", all.x = TRUE)
points<-st_as_sf(data, coords = c("longitude", "latitude"), crs=4326) #makes lats and lons spatial
points_m <- st_transform(points, 3857)
points_m
names(data)

# distance to nearest urban centre, in meters
data$dist_urban_m <- apply(st_distance(points_m, urban_m), 1, min)
data$dist_urban_new <- apply(st_distance(points_m, urban_new), 1, min)

# convert to numeric
data$dist_urban_m <- as.numeric(data$dist_urban_m)
data$dist_urban_new <- as.numeric(data$dist_urban_new)


#visualising the distance between a project and closest urban area
# choose one project row
i <- 1

d <- st_distance(points_m[i, ], urban_m)
nearest_city_index <- which.min(d)

project_pt <- points_m[i, ]
nearest_city <- urban_m[nearest_city_index, ]

project_coords <- st_coordinates(project_pt)[1, 1:2]
city_coords <- st_coordinates(nearest_city)[1, 1:2]

distance_line <- st_sfc(
  st_linestring(rbind(project_coords, city_coords)),
  crs = st_crs(points_m)
)

plot(st_geometry(urban_m), col = "grey80", pch = 16, cex = 0.3) #plots all city centers
points(project_coords[1], project_coords[2], col = "red", pch = 20, cex = 0.5)
points(city_coords[1], city_coords[2], col = "blue", pch = 16, cex = 0.5)
plot(distance_line, add = TRUE, col = "black", lwd = 2)
write.csv(data, "data_with_urban.csv", row.names=FALSE) #save data as a new csv

