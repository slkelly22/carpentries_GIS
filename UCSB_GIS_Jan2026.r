# UCSB Geospatial Carpentry Online
# January 6, 8, 9 (skipped Jan 5)
# Geospatial Data Carpentry for Urbanism using R

# Day 2, January 6, 11:30 - 2:00
# Intro to Geospatial concepts and vector data

# Episode 4 - they are halfway through from yesterday
library(tidyverse)
library(here)
install.packages("here")

# Note: Make sure we install and teach the here package
gapminder <- read.csv(here("data", "gapminder-data.csv"))

tail(gapminder$year)

# GDP vs. Life Expectancy
ggplot(data = gapminder,
       aes(x = lifeExp, y = gdpPercap)) + 
  geom_point()

# with coord flip
ggplot(data = gapminder,
       aes(x = lifeExp, y = gdpPercap)) + 
  geom_point() + coord_flip()

# unique
unique(gapminder$continent)

# filter and select
americas <- gapminder |> 
  filter(continent == "Americas" & year == 2007) |> 
  select(year, country, gdpPercap, lifeExp)

# doesn't save the new var
gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(country = fct_reorder(country, gdpPercap))

gapminder_filtered <- gapminder |> 
  filter(continent == "Americas") |> # if you pull out the year you can see
  mutate(country = fct_reorder(country, gdpPercap))

# he's been talking for 30 minutes at this point

gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(country = fct_reorder(country, gdpPercap)) |> 
  ggplot(aes(
    x = country, 
    y = gdpPercap, 
    fill = lifeExp)) +
  geom_col() + coord_flip()
# next you add the viridis scale

# then the mean life expectancy plot
# then ggsave
# instructor seems a little disorganized and unprepared

# Moving to the GIS CONTENT
# Is Reno, Nevada, west of San Diego? 

# Episode 6, Plot Shapefiles
library(sf)
# giving people time to download the files
# doesn't use "here" anymore; it uses relative paths

boundary_Delft <- st_read("data/delft-boundary.shp")
# boring shape file, only has 1 polygon

str(boundary_Delft)

# all these will begin with st
st_geometry(boundary_Delft) # 1 polygon
st_crs(boundary_Delft) # crs stands for coordinate reference system
st_crs(boundary_Delft)$Name # WGS 84
st_bbox(boundary_Delft) # boundary box, 4.32 degrees over from Greenwich

# plot - just plotting the shape
ggplot(data = boundary_Delft) +  # this doesn't have an aes
  geom_sf(size = 3, color = "black", fill = "cyan1") + 
  labs(title = "Delft Boundary") + # SK: I like with without changing the coordinates to meters but whatev 
  coord_sf(datum = st_crs(28992)) # now it's in meters

# Challenge - import two more shape files
# interestingly, he's not telling you what to name the objects and we'll need to know that for the next episode so I'm grabbing the correct object names before importing

lines_Delft <- st_read("data/delft-streets.shp") 
points_Delft <- st_read("data/delft-leisure.shp")
lines_Delft # geometry - these are lines
points_Delft # geometry - these are points

# Episode 7 
# remember you can check the crs of something like this
st_crs(lines_Delft)
# to check if they are equal to each other
st_crs(lines_Delft) == st_crs(points_Delft) # TRUE
st_crs(lines_Delft) == st_crs(boundary_Delft) # FALSE, even through it's false, you can still plot these on top of each other

head(points_Delft)
unique(points_Delft$leisure)

factor(lines_Delft$highway) |> levels() # this will give same answer as unique(lines_Delft$highway), just in different order
unique(lines_Delft$highway)

# they got a little stuck on this
lines_Delft_fct <- lines_Delft |> 
  mutate(highway_fct = factor(highway))
str(lines_Delft_fct)

cycleways_Delft <- lines_Delft |> 
  filter(highway == "cycleway")
nrow(cycleways_Delft)
nrow(lines_Delft)

# doing math; what's buried in the geometry is the length of each line
cycleways_length <- cycleways_Delft |> 
  mutate(length = st_length(geometry))

cycleways_length # you can see the length now

cycleways_length |> 
  summarize(total_length = sum(length))

# now he's jumping to copying and pasting to save time...
# I'm copying and pasting from when I did this earlier on my own (they didn't provide the code in the chat or etherpad or anything...)
# watch how they use road_types
road_types <- c("motorway", "primary", "secondary", "cycleway")

lines_Delft_selection <- lines_Delft |> # note this is new object
  filter(highway %in% road_types) |> # see how they used the object; they could also just list the 4 types
  mutate(highway = factor(highway, levels = road_types)) # also used road_types as the fct levels

# they are adding colors like this?
road_colors <- c("blue", "green", "navy", "purple")

# now creating the plot with the different colors
ggplot(lines_Delft_selection) + 
  geom_sf(aes(color = highway)) + # technically this is all you need for colors
  scale_color_manual(values = road_colors) + # the blue and navy are hard to differentiate
  labs(
    color = "Road Type", 
    title = "Mobility Network of Delft", 
    subtitle = "Main Roads & Cycleways"
  ) +
  coord_sf(datum = st_crs(28992))

# they are discussing whether to do the challenge or skip to stay on schedule
# skipping and moving to next episode

# Episode 8
# she's making sure everyone has the previous code run so we can move forward

# polygons + lines + points; now plotting all together
ggplot() + 
  # the shapefile you want first is the bottom base of the plot; outline of the city
  geom_sf(data = boundary_Delft, fill = "lightgrey", color = "lightgrey") + 
  geom_sf(data = lines_Delft_selection, aes(color = highway), size = 1) + 
  geom_sf(data = points_Delft)

# turn the points type into a factor
points_Delft$leisure <- factor(points_Delft$leisure)
str(points_Delft$leisure)
levels(points_Delft$leisure) # 15 levels
# we want to make these different colors
leisure_colors <- rainbow(15)
leisure_colors # you can see the HEXcodes

# now copy and paste that base map and then specify the legends and colors
ggplot() + 
  geom_sf(data = boundary_Delft, fill = "lightgrey", color = "lightgrey") + 
  geom_sf(data = lines_Delft_selection, aes(color = highway), size = 1) + 
  geom_sf(data = points_Delft, aes(fill = leisure), shape = 21) + 
  scale_color_manual(values = road_colors, name = "Road Type") + 
  scale_fill_manual(values = leisure_colors, name = "Leisure Location")

# Challenge: change to shapes to squares
ggplot() + 
  geom_sf(data = boundary_Delft, fill = "lightgrey", color = "lightgrey") + 
  geom_sf(data = lines_Delft_selection, aes(color = highway), size = 1) + 
  geom_sf(data = points_Delft, aes(fill = leisure), shape = 22) + 
  scale_color_manual(values = road_colors, name = "Road Type") + 
  scale_fill_manual(values = leisure_colors, name = "Leisure Location")

# Challenge: create a map of playgrounds and picnic tables with different shapes
# giving us five minutes to do this
# I went back and looked at the code b/c I remember they did something different than I expected with filter
leisure_locations_selection <- st_read("data/delft-leisure.shp") |> 
  filter(leisure %in% c("playground", "picnic_table")) 

ggplot() +
  geom_sf(data = boundary_Delft) + 
  geom_sf(data = leisure_locations_selection, aes(fill = leisure), shape = 22) + 
  scale_fill_manual(values = c("red", "blue")) # okay, that's different colors but not different shapes....oh yeah....scale_shape_manual function

ggplot() + 
  geom_sf(data = boundary_Delft) + 
  geom_sf(
  data = leisure_locations_selection, 
  aes(fill = leisure, shape = leisure)) + 
  scale_shape_manual(values = c(21, 22))

# Exporting a shape file (I think they jumped ahead...this is from Episode 9)
dir.create("data_output")
st_write(leisure_locations_selection, "data_output/leisure_location_selection.shp", 
         driver = "ESRI Shapefile")
# whoa! That created four different files
# That's where we're ending today
# note: you can only do one scale_color_manual per ggplot function

#######################################################################################
# Day 3, January 8, 11:30 - 2:00

# he's quickly going through lesson 9 content
ggplot() + 
  geom_sf(data = municipal_boundary_NL) + 
  labs(title = "Map of Continguous NL Municipal Boundaries")

# coloring Delft in purple
ggplot() +
  geom_sf(
    data = country_boundary_NL,
    linewidth = 2,
    color = "gray18"
  ) +
  geom_sf(
    data = municipal_boundary_NL,
    color = "gray40"
  ) +
  geom_sf(
    data = boundary_Delft,
    color = "purple",
    fill = "purple"
  ) +
  labs(title = "Map of Contiguous NL Municipal Boundaries") +
  coord_sf(datum = st_crs(28992))
# if you don't transform to align the CRS it can look strange, projections is off, etc. 


###############################
# Episode 10: Introduction to Raster Data
# Note: This is all new for me (haven't worked through before)

library(tidyverse)
library(terra) # sf package for vector data, terra package for raster data

# two datasets, one is elevation and other is aerial photos (?)

# describe function from terra
describe("data/tud-dtm-5m.tif")

# now loading it
DSM_TUD <- rast("data/tud-dtm-5m.tif")
DSM_TUD

summary(DSM_TUD) # goes up to 16 meters above sea level; mean is a little below sea level
# DSM is digital surface model (vs. DTM, digital terrain model) and captures how high things are...

# turning this into a dataframe
DSM_TUD_df <- as.data.frame(DSM_TUD, xy = TRUE) 
# cool, now you can open the object in your environment

str(DSM_TUD_df)

# plot raster data using ggplot
ggplot() + 
  geom_raster(data = DSM_TUD_df, aes(x = x, y = y, fill = `tud-dtm-5m`)) + # geom_raster
  scale_fill_viridis_c(option = "turbo") + 
  coord_equal() # you lose coordinate ref system when you turned into a df so you add 'coord_equal' here
# this is cool

# crs() to look at metadata
crs(DSM_TUD, proj = TRUE)

# Challenge Exercise
describe('data/tud-dsm-hill.tif') # that's an error in the instructors, should be below file
describe("data/tud-dsm-5m-hill.tif") # CRS is EPSG,28992 (see under "USAGE")

#######################
# Episode 11: Plot Raster Data # 

# Modify our raster to create a new map/plot
# Break our values into three groups

DSM_TUD_df <- DSM_TUD_df |> 
  mutate(fct_elevation = cut(`tud-dtm-5m`, breaks = 3)) # error in the instructions (had dtm)

unique(DSM_TUD_df$fct_elevation)

ggplot() + 
  geom_bar(data = DSM_TUD_df, aes(x = fct_elevation)) # barplot to see how many pixals are in each group

ggplot() + 
  geom_raster(data = DSM_TUD_df, aes(x = x, y = y, fill = fct_elevation)) + 
  coord_equal() # okay, but not great so let's do different breaks in the raster

# Make a similar raster plot but breaking the continuous elevation values into six groups
DSM_TUD_df <- DSM_TUD_df |> 
  mutate(fct_elevation_6 = cut(`tud-dtm-5m`, breaks = 6))

ggplot() + 
  geom_raster(data = DSM_TUD_df, aes(x = x, y = y, fill = fct_elevation_6)) + 
  coord_equal()

# Note: you can also create custom breaks
custom_breaks <- c(-10, 0, 5, 20)
DSM_TUD_df <- DSM_TUD_df |> 
  mutate(fct_elevation_cb = cut(`tud-dtm-5m`, breaks = custom_breaks))

# now plotting with those custom breaks
ggplot() + 
  geom_raster(data = DSM_TUD_df, aes(x = x, y = y, fill = fct_elevation_cb)) + 
  coord_equal()

#########################
# Episode 12: Reproject Raster Data
# You want things to have the same CRS
# How to work with raster data in different projections
# Digital Surface Model (captures elevation) and Digital Terrain Model (surface level)

# We have DSM loaded now need to load DTM
DTM_TUD <- rast("data/tud-dtm-5m.tif")
DTM_hill_TUD <- rast("data/tud-dtm-5m-hill-WGS84.tif")

# Find out CRS of raster datasets
crs(DTM_TUD, proj = TRUE)
crs(DTM_hill_TUD, proj = TRUE)
# they are different

crs(DTM_hill_TUD) == crs(DTM_TUD) # they didn't show but I like this approach; FALSE

# transform to df
DTM_TUD_df <- as.data.frame(DTM_TUD)
DTM_hill_TUD_df <- as.data.frame(DTM_hill_TUD)

## Reproject hill shade to the DTM CRS
# project function from terra
DTM_hill_EPSG29882_TUD <- project(DTM_hill_TUD, crs(DTM_TUD))

DTM_hill_EPSG29882_TUD_df <- as.data.frame(DTM_hill_EPSG29882_TUD, xy = TRUE)

# he was cutting and pasting and I didn't get that below
# switching instructors

##################################
# Episode 14
library(raster)
install.packages("raster") # we need this package but it's not in the Carp install instructions
describe("data/tudlib-rgb.tif") # pay attention to the colors

RGB_stack_TUD <- rast("data/tudlib-rgb.tif")

plotRGB(RGB_stack_TUD, r = 1, g = 2, b = 3) # WOW, that looks like a photograph!


#######################################################################################
# Day 4, January 9, 11:30 - 2:00

# Episode 13
# Canopy Height Models
# Digital Terrain Model (DTM) = Base Earth = Final Return
# Digital Surface Model (DSM) = Covered Earth = First Return = Tree/Building Tops
# Canopy Height Model (CHM) = DSM - DTM (tops of tress - surface of earth)

library(terra)
library(tidyverse)

describe("data/tud-dtm-5m.tif") #raster data
describe("data/tud-dsm-5m.tif")

# importing with rast function
DTM_TUD <- rast("data/tud-dtm-5m.tif")
DSM_TUD <- rast("data/tud-dsm-5m.tif")

# test the projections
# important to check this before you turn them into dataframe b/c then that info is lost
crs(DTM_TUD) == crs(DSM_TUD) # TRUE; is the crs of the terrain model equal to the crs of the surface model? Yes

# convert to a df
DTM_TUD_df <- as.data.frame(DTM_TUD, xy = TRUE) # DON'T FORGET THAT ARGUMENT!
DSM_TUD_df <- as.data.frame(DSM_TUD, xy = TRUE)

# plot - remember geom_raster() not geom_sf()
ggplot() + 
  geom_raster(
    data = DTM_TUD_df, 
    aes(x = x, y = y, fill = `tud-dtm-5m`) # note: it wants the back tics
  ) + 
  scale_fill_gradientn(colors = terrain.colors(10))

ggplot() + 
  geom_raster(
    data = DSM_TUD_df, 
    aes(x = x, y = y, fill = `tud-dsm-5m`) # note: it wants the back tics
  ) + 
  scale_fill_gradientn(name = "DSM: tree tops", colors = terrain.colors(10))


# the math
CHM_TUD_df <- DSM_TUD_df - DTM_TUD_df # creating the canopy height value, NOPE; you can't use the dfs, you have the use the original rasters
# REDO, THEY DID THIS WRONG ABOVE
# This is correct below
CHM_TUD <- DSM_TUD - DTM_TUD
CHM_TUD_df <- as.data.frame(CHM_TUD, xy = TRUE)


# renaming that tricky column
CHM_TUD_df$canopy <- CHM_TUD_df$`tud-dsm-5m`
# or this
CHM_TUD_df <- CHM_TUD_df |> 
  rename(canopy = `tud-dsm-5m`)

# Plotting the Canopy Model
ggplot() + 
  geom_raster(
    data = CHM_TUD_df, 
    aes(x = x, y = y, fill = canopy) # note: it wants the back tics
  ) + 
  scale_fill_gradientn(name = "DSM: tree tops", colors = terrain.colors(10))

summary(CHM_TUD_df)

# saving geotiffs
writeRaster(CHM_TUD, "data_output/CHM_TUD.tiff", 
            filetype = "GTiff", 
            overwrite = TRUE)


##################################
# Episode 15: Import and Visualize OSM (Open Street Map) Data

install.packages(c("osmdata", "leaflet", "lwgeom", "units"))
library(tidyverse)
library(sf)
library(osmdata)

# magic line from lesson
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# let's get a bounding box
bb <- osmdata::getbb("Brielle")
bb

# get data inside our bounding box
# opq function; query function

x <- opq(bbox = bb) |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()

str(x)
buildings <- x$osm_polygons
start_date <- as.numeric(buildings$start_date)
summary(start_date)
unique(start_date)

# highlight anything older than 1900
start_date <- as.numeric(buildings$start_date)
buildings$build_date <- if_else(start_date < 1900, 1900, start_date)

# ggplot
ggplot(data = buildings) + 
  geom_sf() # that's cool

ggplot(data = buildings) + 
  geom_sf(aes(fill = build_date, color = build_date)) + 
  scale_fill_viridis_c() + 
  scale_color_viridis_c() #this gets rid of that second legend


#########
# oxford ms playing
bb_wh <- osmdata::getbb("38655")
bb_wh

oxford <- opq(bbox = bb_wh) |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()


buildings_oxford <- oxford$osm_polygons
start_date <- as.numeric(buildings_oxford$start_date)
buildings_oxford$build_date <- if_else(start_date < 1900, 1900, start_date)

# plot oxford! YAY!!!!
ggplot(data = buildings_oxford) + 
  geom_sf()

ggplot(data = buildings_oxford) + 
  geom_sf(aes(fill = building)) + 
  scale_fill_discrete() 
# scale_fill_viridis_d() # I believe the d is for discrete so this works too (the viridis_c is for continuous)

### Back to Instructor
old <- 1800
old_buildings <- buildings |> 
  filter(start_date <= old)

ggplot(data = old_buildings) + 
  geom_sf(colour = "red") + 
  coord_sf(datum = st_crs(28992))

distance <- 100
buffer_old_buildings <- st_buffer(x = old_buildings, dist = distance)

ggplot(data = buffer_old_buildings) +
  geom_sf()

# I'm copying and pasting directly now from the lesson...
single_old_buffer <- st_union(buffer_old_buildings) |>
  st_cast(to = "POLYGON") |>
  st_as_sf()

single_old_buffer <- single_old_buffer |>
  mutate("ID" = as.factor(seq_len(nrow(single_old_buffer)))) |>
  st_transform(crs = 28992)

sf::sf_use_s2(FALSE) # switching off spherical geometry

centroids_old <- st_centroid(old_buildings) |> 
  st_transform(crs = 28992)

ggplot() +
  geom_sf(data = single_old_buffer, aes(fill = ID)) +
  geom_sf(data = centroids_old) +
  coord_sf(datum = st_crs(28992))

