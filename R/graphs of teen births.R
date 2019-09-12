library(broom)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sp)


# Mapping Europe (code from https://bhaskarvk.github.io/) ----------------------------------------------------

world <- rnaturalearthdata::countries50
europe <- world[world$region_un=="Europe"&world$name!='Russia',]

# Let's add a unique ID column to our data.
{{europe@data$id <- row.names(europe@data)}}

# A bounding box for continental Europe.
europe.bbox <- SpatialPolygons(list(Polygons(list(Polygon(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)
)), ID = 1)), proj4string = CRS(proj4string(europe)))

# Get polygons that are only in continental Europe.
europe.clipped <-
{{  rgeos::gIntersection(europe, europe.bbox, byid = TRUE, id=europe$id)}}

# tidy up the data for ggplot2
europe.tidy <- tidy(europe.clipped)
europe.tidy <- left_join(europe.tidy, europe@data %>% select(name, id), by='id')


# Merging map with births data -------------------------------------------------------------------------------

EU_nat <- europe.tidy %>%
  mutate(name=ifelse(name=="Czech Republic", "Czechia", name)) %>%
  rename(Country=name) %>%
  select(long, lat, order, group, Country)

birthsandborders <- merge(birthRates[c(2,3,4,7)], EU_nat, by="Country")

# Animated plots ----------------------------------------------------------------------------------------------

birthsandborders %>%  # Attempted animation 1 - facets
  arrange(Country, order) %>%
  ggplot(aes(x=long, y=lat,group=group, fill=rate)) +
  geom_polygon(data=EU_nat, aes(x=long, y=lat, group=group), fill="grey") +
  geom_polygon() + 
  theme(line=element_blank(),
        rect=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.title=element_text(size=20, hjust=0.5),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))    +
  coord_map(projection = "ortho", orientation=c(35, 10, 0), xlim = c(-30, 35), ylim=c(32, 72)) +
  facet_wrap(~agegrp) +
  #    ylim(32, NA) +
  #    xlim(-30, 35)+
  scale_fill_continuous(name="Rates of pregnancy\n
                        in under-16s",
                        type = "viridis") +
  transition_states(Year, transition_length = 1, state_length = 2) +
  labs(title='{previous_state}')


u18anim <- birthsandborders %>%  # Animation for under-18 rate
  arrange(Country, order) %>%
  filter(agegrp=="Under 18", Year<2014, Country!="Bulgaria") %>%  # Bulgaria's abnormally high rate
  ggplot(aes(x=long, y=lat,group=group, fill=rate)) +
  geom_polygon(data=EU_nat, aes(x=long, y=lat, group=group), fill="grey") +  # background countries with no data
  geom_polygon() + 
  theme(line=element_blank(),
        rect=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.title=element_text(size=20, face="bold", family="arial", hjust=0, margin = margin(b=10, l=150)),
        plot.subtitle=element_text(size=15, family="arial", margin = margin(b=-60, l=150)),
        plot.margin=margin(1, 0,-3,0, "cm"))    +
  coord_map(projection = "ortho", orientation=c(35, 10, 0), xlim = c(-10, 30), ylim=c(29, 75)) +
  scale_fill_continuous(name="Rates of births\nto under-18s\n(per 1000 women)", type="viridis", option="A") +
  transition_states(Year, transition_length = 1, state_length = 1, wrap=FALSE) +
  labs(title = "Human Fertility Database - teenage birth rates", subtitle="Year: {closest_state}")

animate(u18anim, duration=15, fps=20, height = 600, width=700, start_pause = 10, end_pause = 40) %>% 
  anim_save("teenagebirthrates.gif", animation = (.), path = getwd())# Unit comp - no ffmpeg
# animate(u18anim, duration=20, fps=20, start_pause = 20, end_pause = 40,renderer=ffmpeg_renderer())
