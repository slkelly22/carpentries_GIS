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


