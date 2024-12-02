# Load these packages
# If you don't have them, use `Tools > Install packages...` to get them
library(tidyverse)
library(lubridate)
library(stringr)
library(viridis)

# Make object for seasons
season.meta = data.frame(month = lubridate::month(1:12)) |> # Make a list of all months
  # Assign seasons to each month
  # You can add additional month numbers in each season
  mutate(season = case_when(
    month %in% c(12, 1) ~ "Winter",
    month %in% c(3, 4) ~ "Spring",
    month %in% c(5,6) ~ "Summer",
    month %in% c(10) ~ "Autumn"
  ))

# Read in your data
# These data should be structured as each row is a single truffle observation
# Each truffle should have these columns filled out: burn_history, site, genus, date
truffle.data = read.csv("raw_data/", na.strings = c("", "NA")) |> # Fill in the file path
  # If you have column names different than I've assigned them to, unhash the next line and fill it out
  # rename(burn_history = oldname, site = oldname, genus = oldname, date = oldname) |>
  # Clean up potential white space issues
  mutate(genus = trimws(genus)) |>
  # Make the date R-readable
  mutate(date.long = paste0(20, date, sep = ""),
         date = lubridate::ymd(date.long),
         # set month in a way that R can read them
         month = lubridate::month(date)
  ) |>
  # Add in seasons
  left_join(season.meta)

# You can check out your data with these objects -----
# But feel free to skip if they aren't working or aren't useful

## Count how many genera are in each season ----
genus.season.count = truffle.data |>
  select(genus, season) |>
  distinct() |>
  arrange(genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = season, values_from = n, values_fill = 0) |>
  mutate(season.count = Autumn + Winter + Spring + Summer)

table(genus.season.count$season.count)

## Count season richness ----
season.richness = truffle.data |>
  # Remove instances where the truffle is only ID'd to genus
  filter(species != genus) |>
  select(species, season) |>
  distinct() |>
  mutate(n = 1) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0)

sum(season.richness$Autumn)
sum(season.richness$Winter)
sum(season.richness$Spring)
sum(season.richness$Summer)

# Calculate abundance ----
# This is the next important bit

trufflecounts = truffle.data |>
  # Tell R to group all the truffles by genus
  group_by(genus) |>
  # and to count how many instances are of each genus
  summarize(ct = length(genus))

ggplot(trufflecounts, # This is the dataframe with your data
       #aes stands for aesthetics. This sets the x axis as count of truffles descending
       # y axis is count of truffles
       # fill colours each bar by burn_history
       aes(x = reorder(genus, -ct), y = ct, fill = burn_history)) +
  # This tells ggplot to make a barchart
  geom_bar(stat = "identity") +
  # Add in labels counting all the truffles with some extra code to say how big the labels are
  geom_label(aes(label = ct, fill = burn_history, alpha = 0.8),
             size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
  # Colour the bars by burn history
  # If you want to change this, change the fill variable back up in the aes line
  scale_fill_viridis_d(option = "D") + # To check out other options, type ?viridis into the console
  # Or if you want to manually set colours, hash out the preceding line and unhash the next line
  # Make sure you have the same number of colours as you have levels in your variable
  # scale_fill_manual(values = c("#006d2c", "#4292c6", "#a63603")) +
  theme_bw() +
  # Everything else is just messing with the text, etc.
  # https://stackoverflow.com/questions/61956199/ggplot-color-axis-labels-based-on-variable
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

# Save the graph as a png
ggsave("TruffleFreq_BurnHistory.png", width = 10, height = 8, units = "in")
