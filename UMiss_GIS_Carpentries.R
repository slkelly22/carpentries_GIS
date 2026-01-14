# U of Mississippi GIS Workshop 
# January 14, 2026 - Day 1

### Harley ### 
# know where your data file is located (the one you downloaded)
# anyone have OneDrive...that can create challenges
# 8:45 Creating a New Project

# dir.create("data_output")
# dir.create("documents")
dir.create("fig_output")
dir.create("scripts")

# now we need to put that data folder into our directory
# now creating a script

# installing packages

# data types
1+100
1*200
x <- 1/40
x
sqrt(x)
x <- 100

# numeric
numeric_vector <- c(2, 6, 3)
str(numeric_vector)

# character
character_vector <- c("Amsterdam", "'s Gravenhage", "Delft")
str(character_vector)

# logical
logical_vector <- c(TRUE, FALSE, TRUE)

# combining vectors
ab_vector <- c("a", "b")
abcd_vector <- c(ab_vector, "c", "d")
abcd_vector

combined_vector <- c(abcd_vector, numeric_vector)
combined_vector
str(combined_vector)

with_na <- c(1, 2, 1, 1, NA, 3, NA)
mean(with_na)
mean(with_na, na.rm = TRUE)

is.na(with_na)

sum(is.na(with_na))

sum(!is.na(with_na))

without_na <- with_na[!is.na(with_na)]
without_na

# factors
nordic_str <- c("Norway", "Sweden", "Denmark", "Sweden")
nordic_str

nordic_cat <- factor(nordic_str)
nordic_cat
str(nordic_cat)
levels(nordic_cat)
nlevels(nordic_cat)

# reordering factors
nordic_cat <- factor(nordic_cat, 
                     levels = c("Norway", "Denmark", "Sweden"))
levels(nordic_cat)

nordic_cat2 <- factor(nordic_str, 
                      levels = c("Norway", "Denmark"))
nordic_cat2

# exploring dfs and dplyr

gapminder <- read_csv("data/gapminder-data.csv")
getwd()

View(gapminder)
str(gapminder)
head(gapminder)
summary(gapminder)
nrow(gapminder)
ncol(gapminder)

country_vec <- gapminder$country
head(country_vec)
head(unique(country_vec), 10)

year_country_gdp <- select(gapminder, year, country, gdpPercap)
head(year_country_gdp)

# the pipe
year_country_gdp <- gapminder |> 
  select(year, country, gdpPercap)

year_country_gdp_euro <- gapminder |> 
  filter(continent != "Europe" & year >= 2000) |> 
  select(year, country, gdpPercap)

head(year_country_gdp_euro)

# only North American countries
year_gdp_namerica <- year_country_gdp_euro |> 
  filter(country == "Canada" | country == "Mexico" | country == "United States")

# Challenge: Filtering a dataframe
EurAsia <- gapminder |>
  filter(continent == "Europe" | continent == "Asia") |>
  select(lifeExp, country, year)

nrow(EurAsia)

# group and summarize
gapminder |> 
  group_by(continent) |> 
  summarize(ave_gdpPercap = mean(gdpPercap))

gapminder |> 
  group_by(country) |> 
  summarize(avg_lifeExp = mean(lifeExp)) |> 
  filter(avg_lifeExp == min(avg_lifeExp) | avg_lifeExp == max(avg_lifeExp)) 
# someone had quesiton about this one

gapminder |> 
  group_by(continent, year) |> 
  summarize(avg_gdpPercap = mean(gdpPercap))

# Time: 10:35

gdp_pop_bycontinents_byyear <- gapminder |> 
  group_by(continent, year) |> 
  summarize(
    avg_gdpPercap = mean(gdpPercap), 
    sd_gdpPercap = sd(gdpPercap), 
    avg_pop = mean(pop), 
    sd_pop = sd(pop), 
    n_obs = n()
  ) # that makes a useful df

head(gdp_pop_bycontinents_byyear)


gapminder |> 
  count(continent)

gapminder_gdp <- gapminder |> 
  mutate(gdpBillion = gdpPercap * pop / 10^9)

head(gapminder_gdp)

# attendee question
gapminder |> 
  count(continent, country)

## I taught ## 

### Abbie ### 
# using Canva slides
# GIS - Geographic Information System(s)
# ArcGIS, QGIS (open source), OSM Open Street Maps (open source)
# geospatial data is data that includes information related to locations on the Earth's surface

# an ellipsoid (slightly wider than tall); the geoid
# latitude (north | south); equator 0 degrees; -90 to + 90
# longitude (east | west); -180 to + 180; prime meridian in Greenwich Enggland

# Map projections: Cylindrical, Conical, Azimuthal (more distorted)

# Map projection properties: conformal (preserves the correct shape of small areas; Africa and greenland look like the same size in this one)
# and equal-area (best for preserving distances; Africa is much bigger than greenland)

# CRS - Coordinate Reference Systems - coordinate based (Lat. and Long. systems) for locating geographic entities, which uses a specific map projection
# Most popular CRS is WGS - World Geodetic System 84 (EPSG:4326)

# CRS used in today's workshop dataset is Amersfoort / RD New (EPSG:28992)

# Public Register - European Petroleum Survey Group (EPSG) has 7,000 + CRS systems (epsg.org)
# https://epsg.org/home.html
# each CRS has a unique EPSG code

# Map Scale - measures the ratio between distance on a map and the corresponding distance on the ground

# Vector Data, Raster Data
# Vector Data represents discrete features using geometry
# Vectors - points, lines, polygons to represent features on the Earth
# Vectors - shapefiles, geojson, geopackage, kml (google earth)

# Raster Data - continuous or grid data using pixals
# temperature, elevation, gradient

# Attributes are non-spatial data about geographic features
# geometry without context provides little valuable information

# spatial feature is anything on a map that has a place or shape such as a lake, road, grocery store
# map layer is a raster or vector dataset added to a map document
# symbology is the style (color, size, symbols, etc) assigned to vector features based on attribute values

# Shapefiles - the most common geospatial file type, is an open source format for vector data that can be opened in any GIS software
# Shapefiles are in zip files because they include several different file formats; some of these files are required for the shapefile to work properly
# common file types include
# - .shp - feature geometry
# - .shx - shape index position
# - .dbf - attribute data
# - .prj - coordinate system and map project

# Tabular Data - GIS software can read common tabular data formats
# If there is geographic information included in the data table, GIS tools can be used to transform the table into a shapefile

# Metadata - descriptive information about an object - data about data

# Where to find data
# Mississippi Geospatial Data Catalog, UnData, etc. 
# Restricted Access - esri, social explorer, etc. 

# Episode 7 - Open and Plot Vector Layers

library(sf)
# sf stands for simple features

# import our opening shapefile
boundary_Delft <- st_read("data/delft-boundary.shp", quiet = TRUE)
# if you remove quiet = TRUE, then you get the metadata
boundary_Delft <- st_read("data/delft-boundary.shp")

st_geometry_type(boundary_Delft) # the city of Delft is a polygon

# to see the CRS
st_crs(boundary_Delft)

st_crs(boundary_Delft)$Name # WGS 84
st_crs(boundary_Delft)$epsg # 4326

# boundary box
st_bbox(boundary_Delft) # latitude and longitudinal degrees

# change our CRS for something specifically built for Netherlands and measured in meters
boundary_Delft <- st_transform(boundary_Delft, crs = 28992)
st_crs(boundary_Delft)$Name # Amersfoort / RD New
st_crs(boundary_Delft)$epsg # 28992

# measured in meters? 
st_bbox(boundary_Delft)
st_crs(boundary_Delft)$units_gdal # metre; GDAL is a translator R library (out of MIT) for raster and vector files

boundary_Delft # to see everything we just changed

# question from audience: Can you change several shapefiles' CRS at the same time? 

# plot a vector layer
ggplot(data = boundary_Delft) + 
  geom_sf(size = 3, color = "black", fill = "cyan1") + 
  labs(title = "Delft Administrative Boundary") + 
  coord_sf(datum = st_crs(28992))

# Time is 2:30

# Challenge Activity
# import shapefiles
lines_Delft <- st_read("data/delft-streets.shp")
points_Delft <- st_read("data/delft-leisure.shp")

# what is the crs
st_crs(lines_Delft) # Amersfoot, 28992
lines_Delft$geometry # lines
st_geometry_type(lines_Delft)

st_crs(points_Delft) # Amersfoot, 28992
points_Delft$geometry # points
st_geometry_type(points_Delft)

st_bbox(lines_Delft)
st_bbox(points_Delft)

# Ending at 2:45