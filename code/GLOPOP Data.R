#This code loads and processes GLOPOP and GDL
#See README.md for more information on where to access GDL. GLOPOP data set can be found in data/original "GLOPOP_regional_statistics.csv"

library(sf)
library(tidyverse)

# Load GDL shapefile (download from https://globaldatalab.org/shdi/shapefiles/)
gdl_regions <- st_read("/Users/alexandrakassinis/Desktop/Dissertation/GDL_Shapefiles_V6.6_large.shp")
gdl_regions

# Convert your dataframe to a spatial object - added a column to merged which is a spatial object with lat and lon per project
projects_sf <- st_as_sf(merged, 
                        coords = c("Longitude", "Latitude"), 
                        crs = 4326)
sf::sf_use_s2(FALSE) #switch to flat/planar geometry, no longer uses Earth as a sphere 

# Spatial join to match each project to its GDL region
projects_with_region <- st_join(projects_sf, gdl_regions)

glopop_stats <- read_csv("GLOPOP_regional_statistics.csv")
head(glopop_stats)
colnames(glopop_stats)

# Filter to your 12 countries
my_iso <- c("BRA", "PER", "COL", "COD", "ZMB", "SLE", 
            "MDG", "MOZ", "TZA", "KHM", "LAO", "CAF")

#here i am only extracting wealth columns - will only get you countries from DHS dataset
glopop_filtered <- glopop_stats %>%
  filter(iso_code %in% my_iso) %>%
  select(iso_code, GDLcode, WEALTH_AVG, WEALTH_1:WEALTH_5)

# Join to your projects (which already have GDL region from the spatial join)
projects_final <- projects_with_region %>%
  left_join(glopop_filtered, by = c("gdlcode" = "GDLcode"))

#here i add another column form income which exist for LIS countries. And i add a merged column that combines average household wealth and income in that GDL block
glopop_filtered <- glopop_stats %>%
  filter(iso_code %in% my_iso) %>%
  mutate(
    # Use WEALTH_AVG for DHS countries, INCOME_AVG for LIS countries
    wealth_income_avg = ifelse(WEALTH_AVG > 0, WEALTH_AVG, INCOME_AVG)
  ) %>%
  select(iso_code, GDLcode, wealth_income_avg, WEALTH_AVG, INCOME_AVG)

#here you add it to the bigger dataset
projects_final <- projects_with_region %>%
  left_join(glopop_filtered, by = c("gdlcode" = "GDLcode"))

#here you create merged column
projects_final <- projects_final %>%
  mutate(wealth_income_avg = ifelse(WEALTH_AVG > 0, WEALTH_AVG, INCOME_AVG))

projects_final_clean <- projects_final %>%
  # Drop the sf class FIRST so it becomes a regular dataframe
  st_drop_geometry() %>%
  # Add geometry back as a plain text column
  mutate(geometry = st_as_text(st_geometry(projects_final))) %>%
  # Fix WEALTH_AVG and INCOME_AVG list columns
  mutate(
    WEALTH_AVG = sapply(WEALTH_AVG, function(x) x[1]),
    INCOME_AVG = sapply(INCOME_AVG, function(x) x[1])
  )

write.csv(projects_final_clean, "all_data.csv", row.names = FALSE)

# Check all column names in glopop_stats
colnames(glopop_stats)


#this verifies that wealth/income avg takes average across all individuals
glopop_stats %>%
  filter(GDLcode == "TZAr205") %>%
  mutate(
    total = WEALTH_1 + WEALTH_2 + WEALTH_3 + WEALTH_4 + WEALTH_5,
    manual_avg = (WEALTH_1*1 + WEALTH_2*2 + WEALTH_3*3 + WEALTH_4*4 + WEALTH_5*5) / total
  ) %>%
  select(GDLcode, WEALTH_AVG, manual_avg)

#cleaning it up
# Drop columns you don't need
data_final <- projects_final %>%
  st_drop_geometry() %>%  # removes the spatial geometry column
  select(-X1, -X2, -iso_code.x)  # remove redundant/raw columns
head(data_final)
dim(data_final)  # should be 52 rows
