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
