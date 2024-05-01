library(tidyverse)
# install.packages("here")
library(here)
# install.packages("ggbeeswarm")
library(ggbeeswarm)

# Read in data
plotbeaches <- read_csv(here("data/cleanbeaches_new.csv"))

# ggplot anatomy
plotbeaches |> 
  ggplot(aes(x = year, y = beachbugs)) +
  geom_point()

# table of counts of beachbugs
plotbeaches |> 
  group_by(year) |> 
  summarise(obs = n())

# introduce geom_jitter!
plotbeaches |> 
  ggplot(aes(x = year, y = beachbugs)) +
  geom_jitter()

# histogram
hist(plotbeaches$beachbugs)

plotbeaches |> 
  ggplot(aes(x = beachbugs)) +
  geom_histogram()

# Use a transformation so data looks nicer
glimpse(plotbeaches)

plotbeaches |> 
  mutate(logbeachbugs = log10(beachbugs))

hist(plotbeaches$logbeachbugs)

plotbeaches |> 
  ggplot(aes(x = year, y = logbeachbugs)) +
  geom_jitter()

#intro violin plots!

# treat year as a factor
plotbeaches |> 
  ggplot(aes(x = factor(year), y = logbeachbugs)) +
  geom_violin()

plotbeaches_year_as_factor <- plotbeaches |> 
  mutate(year = factor(year))

# quasirandom
plotbeaches |> 
  ggplot(aes(y = factor(year), x = logbeachbugs)) +
  geom_quasirandom() 
  # coord_flip()

# beeswarm
plotbeaches |> 
  ggplot(aes(y = factor(year), x = beachbugs)) +
  geom_beeswarm()

install.packages("palmerpenguins")
library(palmerpenguins)

# Intro to shapes
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) + 
  geom_beeswarm(shape = 1)

# Controling transperancy
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) + 
  geom_beeswarm(alpha = 0.2)

# Saving plots
ggsave(here("output/penguins.png"), width = 16, height = 6)

glimpse(plotbeaches)

# Introduce facet_wraps
plotbeaches |> 
  ggplot(aes(y = factor(year), x = logbeachbugs)) +
  geom_quasirandom(shape = 1) + 
  facet_wrap(~site)

# Combine some Cleanitup with ggplot
plotbeaches |> 
  filter(year == 2015) |> 
  ggplot(aes(y = factor(year), x = logbeachbugs)) +
  geom_quasirandom(shape = 1) + 
  facet_wrap(~site)

plotbeaches |> 
  filter(site == "Maroubra Beach") |> 
  ggplot(aes(y = factor(year), x = logbeachbugs)) +
  geom_quasirandom(shape = 1) + 
  facet_wrap(~site)

# Axes labels 
plotbeaches |> 
  filter(site == "Maroubra Beach") |> 
  ggplot(aes(y = factor(year), x = logbeachbugs)) +
  geom_quasirandom(shape = 1) + 
  labs(x = "Enterococci bacteria on log10 scale",
       y = "Years from 2013 - 2015", 
       title = "Maroubra Beach") 

# Themes
plotbeaches |> 
  filter(site == "Maroubra Beach") |> 
  ggplot(aes(y = factor(year), x = logbeachbugs)) +
  geom_quasirandom(shape = 1) + 
  labs(x = "Enterococci bacteria on log10 scale",
       y = "Years from 2013 - 2015", 
       title = "Maroubra Beach") + 
  theme_classic()

