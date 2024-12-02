# Presence abscense to show seasonality
## Time graphs ----
round(sum(genea$presence)/length(genea$presence), 3)

genea |>
  select(transect.or.site, Forest) |>
  arrange(presence) |>
  relocate(elev.category, month, .after = transect.or.site)

# genea.cts = genea |>
#   group_by(month.label, elev.category) |>
#   summarize(n = length(obs_id))

pres.cts = trufflepoints |>
  filter(Habitat %in% c("Oak", "Conifer")) |>
  group_by(Genus, Season) |>
  summarize(n = length(Point))

pres.sums = trufflepoints |>
  filter(Habitat %in% c("Oak", "Conifer")) |>
  mutate(season_habitat = paste0(Season, "_", Habitat)) |>
  group_by(season_habitat, Genus) |>
  summarize(n = length(Point)) |>
  pivot_wider(names_from = season_habitat, values_from = n, values_fill = 0) |>
  pivot_longer(cols = -Genus, names_to = "season_habitat", values_to = "value") |>
  separate(season_habitat, into = c("Season", "Habitat")) |>
  left_join(pres.cts) |>
  # Calculate percentage in each season
  mutate(percent = value/n*100,
         Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) |>
  filter(Genus != "Unknown")

ggplot(pres.sums,
       aes(x = Habitat, y = percent, fill = Season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("gold", "orange", "purple3", "lightblue")) +
  # ylim(0, 100) +
  # scale_x_continuous(breaks=genea$month, label=firsts$month.label) +
  facet_wrap(~Genus) +
  # geom_text(aes(label = presence.n.fire, color = presence.word), size = 3, position = position_stack(vjust = 0.5), fontface = "bold") +
  # scale_color_manual(values = c("black", "white"), name = "", labels = c("Absence", "Presence")) +
  # labs(y = "Percent of site visits", x = "Month") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))

ggsave("output/2024.11.30_SeasonalAbundance.png", width = 12, height = 10, units = "in")

ggplot(pres.sums,
       aes(x = Season, y = percent, fill = Habitat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("green4", "orange")) +
  # ylim(0, 100) +
  # scale_x_continuous(breaks=genea$month, label=firsts$month.label) +
  facet_wrap(~Genus) +
  # geom_text(aes(label = presence.n.fire, color = presence.word), size = 3, position = position_stack(vjust = 0.5), fontface = "bold") +
  # scale_color_manual(values = c("black", "white"), name = "", labels = c("Absence", "Presence")) +
  # labs(y = "Percent of site visits", x = "Month") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))

ggsave("output/2024.11.30_HabitatAbundance.png", width = 12, height = 10, units = "in")

# Try to really detect absence
pres.ct.genus = trufflepoints |>
  # filter(Habitat %in% c("Oak", "Conifer")) |>
  select(Season, Site, Date) |>
  distinct() |>
  group_by(Season) |>
  summarize(n = length(Site))

pres.abs = trufflepoints |>
  # filter(Habitat %in% c("Oak", "Conifer")) |>
  select(Site, Date, Season, Genus) |>
  distinct() |>
  mutate(n = 1,
         site_date = paste0(Site, "_", Date)) |>
  select(-c(Site, Date)) |>
  pivot_wider(names_from = Genus, values_from = n, values_fill = 0) |>
  # mutate(n = length(site_date))
  left_join(pres.ct.genus) |>
  pivot_longer(cols = -c(Season, site_date, n), names_to = "Genus", values_to = "presence") |>
  group_by(Season, Genus, n, presence) |>
  summarize(presence.n = length(presence)) |>
  ungroup() |>
  mutate(percent = presence.n/n*100,
         presence.word = case_when(
           presence == 1 ~ "Presence",
           presence == 0 ~ "Absence",
           TRUE ~ NA
         ),
         Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) |>
  filter(Genus != "Unknown")

ggplot(pres.abs,
       aes(x = Season, y = percent, fill = presence.word)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey90", "dodgerblue4"), name = "", labels = c("Absence", "Presence")) +
  # ylim(0, 100) +
  # scale_x_continuous(breaks=genea$month, label=firsts$month.label) +
  facet_wrap(~Genus) +
  geom_text(aes(label = presence.n, color = presence.word), size = 3, position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_color_manual(values = c("black", "white"), name = "", labels = c("Absence", "Presence")) +
  # labs(y = "Percent of site visits", x = "Month") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))

ggsave("output/2024.11.30_PresenceAbsence.png", width = 12, height = 10, units = "in")

# Original Genea script ----
genea.sums = genea |>
  group_by(month, month.label, elev.category, presence) |>
  summarize(presence.n = length(presence)) |>
  left_join(genea.cts) |>
  ungroup() |>
  as.data.frame() |>
  mutate(percent = presence.n/n*100,
         presence.word = case_when(
           presence == 1 ~ "Presence",
           presence == 0 ~ "Absence",
           TRUE ~ NA
         ),
         elev.category = factor(elev.category, levels = c("High", "Mid", "Low")),
         presence.n.fire = case_when(
           month.label == "Jan" & elev.category == "Low" & presence == 0 ~ "2 (2)",
           month.label == "Mar" & elev.category == "Low" & presence == 1 ~ "5 (1)",
           month.label == "Apr" & elev.category == "Low" & presence == 0 ~ "1 (1)",
           month.label == "May" & elev.category == "Low" & presence == 1 ~ "3 (1)",
           month.label == "May" & elev.category == "Low" & presence == 0 ~ "2 (1)",
           month.label == "July" & elev.category == "High" & presence == 0 ~ "7 (1)",
           month.label == "Sep" & elev.category == "Low" & presence == 0 ~ "3 (1)",
           month.label == "Sep" & elev.category == "High" & presence == 0 ~ "6 (1)",
           month.label == "Oct" & elev.category == "Low" & presence == 0 ~ "6 (2)",
           month.label == "Nov" & elev.category == "Low" & presence == 0 ~ "4 (1)",
           month.label == "Nov" & elev.category == "Mid" & presence == 0 ~ "2 (1)",
           TRUE ~ as.character(presence.n)
         ))

# write.csv(genea.sums, "outputs/2024.07.24_genea.sums.csv")

ggplot(genea.sums,
       aes(x = month.label, y = percent, fill = presence.word)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey90", "dodgerblue4"), name = "", labels = c("Absence", "Presence")) +
  ylim(0, 100) +
  # scale_x_continuous(breaks=genea$month, label=firsts$month.label) +
  facet_grid(elev.category~.) +
  geom_text(aes(label = presence.n.fire, color = presence.word), size = 3, position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_color_manual(values = c("black", "white"), name = "", labels = c("Absence", "Presence")) +
  labs(y = "Percent of site visits", x = "Month") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))

ggsave("genea_presence.png", width = 8, height = 6, units = "in")

# How many sites?

genea.sites = genea |>
  as.data.frame() |>
  dplyr::group_by(transect.or.site, Forest, elev.category) |>
  # slice_head()
  summarize(n = length(obs_id), presence = sum(presence))

genea.siteList = genea |>
  as.data.frame() |>
  dplyr::group_by(transect.or.site, Forest, elev, latitude, longitude) |>
  summarize(n = length(obs_id), presence = sum(presence))

table(genea.sites$n)

write.csv(genea.siteList, "outputs/2024.07.24_Genea_siteList.csv")

genea.sites.burned = genea |>
  filter(str_detect(Forest, "urn")) |>
  group_by(transect.or.site, Forest) |>
  # slice_head()
  summarize(n = length(obs_id))

genea.sums.burned = genea |>
  filter(str_detect(Forest, "urn")) |>
  group_by(month, month.label, elev.category, presence) |>
  summarize(presence.n = length(presence))

# Map of sites
library(mapview)

sp::coordinates(genea) = ~longitude+latitude

mapview(genea, zcol = "elev.category")

genea.site.list = genea |>
  arrange(desc(presence)) |>
  group_by(transect.or.site) |>
  slice_head()

write.csv(genea.site.list, "outputs/genea.site.list.csv")
