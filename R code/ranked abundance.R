library(tidyverse)
library(lubridate)
library(stringr)
library(tidylog)

trufflepoints_2324 = read.csv("clean_data/CSNM_GPSpoints_2023-24.csv") |>
  mutate(Date = lubridate::ymd(Date)) |>
  select(Species, Genus, Point, Date, Site, Season, Habitat, Specimen)

trufflepoints_2223 = read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points Oct '22.csv") |>
  mutate(Season = "Autumn") |>
  bind_rows(read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points Feb '23.csv") |>
              mutate(Season = "Winter"),
            read.csv("raw_data/2023-24_GPSdata/CSNM Species list - GPS points May '23.csv") |>
              mutate(Season = "Spring")) |>
  mutate(Specimen = readr::parse_number(original.ID)) |>
  # Add species
  rename(field.guess = species) |>
  mutate(species = case_when(
    original.ID %in% c("#2", "#33") ~ "Elaphomyces sp. CS2",
    original.ID %in% c("#3 Genea", "#4 Genea") ~ "Genea sp. CS3",
    original.ID == "Genea 45" ~ "Genea sp. CS45",
    original.ID == "Genea 46" ~ "Genea sp. CS46",
    original.ID %in% c("#5", "#11", "#15") ~ "Choiromyces sp. CS5",
    original.ID %in% c("#6", "#7", "#8") ~ "Russula cf. similaris",
    original.ID == "#9" ~ "Leucogaster odoratus",
    original.ID == "#10" ~ "Rhizopogon sp. CS10",
    original.ID %in% c("#12", "#13", "#14", "#20", "#24") ~ "Tuber aff. anniae",
    original.ID == "#16" ~ "Gautieria sp. CS16",
    original.ID == "#17" ~ "Gautieria sp. CS17",
    original.ID == "#18" ~ "Leucogaster sp. CS18",
    original.ID %in% c("#19", "#23") ~ "Tuber whetstonense",
    original.ID == "#21" ~ "Hysterangium cf. setchellii",
    original.ID == "#22" ~ "Tuber sp. CS22",
    original.ID %in% c("#25", "#27") ~ "Tuber beyerlei",
    original.ID == "#26" ~ "Choiromyces",
    original.ID %in% c("#30 Balsamia", "#37 Balsamia") ~ "Balsamia aff. filamentosa",
    original.ID == "#32 Balsamia" ~ "Balsamia cf. setchellii",
    original.ID == "#33 Elapho" ~ "Elaphomyces sp. CS2",
    original.ID == "#35 Tuber" ~ "Tuber candidum",
    original.ID == "#38 Balsamia" ~ "Hysterangium sp. CS38",
    original.ID == "#39 Genea" ~ "Genea arenaria",
    original.ID == "#40 Zellero" ~ "Lactarius sp. CS40",
    original.ID %in% c("Balsamia 42", "Balsamia 1", "Balsamia 43") ~ "Balsamia aff. latispora",
    original.ID == "Leucangium 44" ~ "Leucangium carthusianum",
    original.ID == "Elaphomyces 50" ~ "Elaphomyces sp. CS50",
    original.ID == "Hysterangium 49" ~ "Hysterangium sp. CS38",
    original.ID == "Hymenogaster 57" ~ "Hymenogaster raphanodorus",
    TRUE ~ field.guess
  )) |>
  relocate(species, .after = field.guess) |>
  select(waypoint, species, Season, Site, Date, Habitat, Specimen) |>
  separate(species, into = "Genus", remove = FALSE) |>
  mutate(across(c(species, Genus), ~na_if(., ""))) |>
  drop_na(Genus) |>
  rename(Species = species, Point = waypoint) |>
  mutate(Point = as.character(Point)) |>
  mutate(Date = lubridate::ymd(Date))

# Pull in GenBank numbers
genbank_its_pacbio = readxl::read_excel("raw_data/CSNMGenBankUploadsWithAccession#s.xlsx",
                                        sheet = "PacBio") |>
  select('GenBank Accession #', 'specimen-voucher', Organism) |>
  separate(Organism, into = "Genus_GB", remove = FALSE) |>
  separate('specimen-voucher', into = c("X1", "X2", "X3"), sep = " ") |>
  mutate(Specimen = readr::parse_number(X1),
         iNaturalist_Nr_GB = readr::parse_number(X3)) |>
  rename(GenBank_Nr_ITS = 'GenBank Accession #') |>
  select(-c(X1, X2, X3)) |>
  mutate(DNA_method = "PacBio")

genbank_its_sanger = readxl::read_excel("raw_data/CSNMGenBankUploadsWithAccession#s.xlsx",
                                        sheet = "Sanger") |>
  select('Genbank Accession #', 'specimen-voucher', Organism) |>
  separate(Organism, into = "Genus_GB", remove = FALSE) |>
  separate('specimen-voucher', into = c("X1", "X2", "X3"), sep = " ") |>
  mutate(Specimen = readr::parse_number(X1),
         iNaturalist_Nr_GB = readr::parse_number(X3)) |>
  rename(GenBank_Nr_ITS = 'Genbank Accession #') |>
  select(-c(X1, X2, X3)) |>
  mutate(DNA_method = "Sanger")

genbank_its = bind_rows(genbank_its_sanger, genbank_its_pacbio) |>
  # Remove GenBank parasites
  filter(!GenBank_Nr_ITS %in% c("PQ409100", "PQ409066", "PQ409103"))

# Pull in iNat numbers
inat = read.csv("raw_data/observations-518161.csv", na.strings = c("", " ", "NA")) |>
  select(id, url, field.id.., field.voucher.number.s.) |>
  mutate(field.id.. = as.character(field.id..),
    X1 = coalesce(field.id.., field.voucher.number.s.),
    Specimen = readr::parse_number(X1)) |>
  rename(iNaturalist_Nr = id, iNaturalist_URL = url) |>
  select(Specimen, iNaturalist_Nr, iNaturalist_URL) |>
  # Remove obs not linked to specific specimens
  drop_na(Specimen)

trufflepoints = read.csv("raw_data/CSNM_Truffles_Missing_GPS_Data.csv") |>
  mutate(Date = lubridate::ymd(Date)) |>
  bind_rows(trufflepoints_2324) |>
  bind_rows(trufflepoints_2223) |>
  filter(Genus != "Tomst") |>
  filter(Genus != "") |>
  mutate(
    # Update site names
    Site = case_when(
      Site == "Mixed oak-conifer" ~ "Site 1",
      Site == "PCT Pilot" ~ "PCT Pilot Rock",
      Site == "Tyler Rd" ~ "Taylor Rd",
      Site == "Emigrant Trail" ~ "Buck Rock",
      Site == "Keno Access Rd" ~ "Surveyor Lower",
      Site == "Surveyor Campground" & Date == "2024-05-27" ~ "Surveyor Peak",
      Site == "Surveyor Peak" & Date == "2024-05-27" ~ "Surveyor",
      TRUE ~ Site
    ),
    # Remove collection numbers that were never actually collected
    Specimen = case_when(
      Specimen %in% c(48, 85, 177) ~ NA,
      Genus == "Balsamia" & Specimen == 1 ~ 41,
      Point == "06511" ~ 167,
      Species == "Geopora sp. CS218" ~ 218,
      Point == "0560" ~ 65,
      TRUE ~ Specimen
    ),
    # Update taxonomy
    Genus = case_when(
    Genus == "Endogone" ~ "Endogonaceae",
    Genus == "Zygomyces" ~ "Glomeraceae",
    Genus == "Glomerales" ~ "Glomeraceae",
    Genus == "Gymnomyces" ~ "Russula",
    Genus == "Genear" ~ "Genea",
    Genus == "Glomus" ~ "Glomeraceae",
    Genus == "Hydnotryopsis" ~ "Sarcosphaera",
    Genus == "Zelleromyces" ~ "Lactarius",
    Genus == "Phallales" ~ "Trappea",
    Genus == "Albatrellaceae" ~ "Leucophleps",
    Genus == "Geastrales" ~ "Geastrum",
    Genus == "Blue" ~ "Lepiota",
    Genus == "Hymenogaster" & Date == "3/10/2024" ~ "Xerocomellus",
    TRUE ~ Genus
  ),
  Species_original = Species,
  Species = coalesce(Species_original, Genus),
  Species_updated = case_when(
    Genus == "Agaricus" ~ "Agaricus cf. inapertus",
    Genus == "Alpova" ~ "Alpova trappei",
    Species == "Balsamia cf. latispora w/Microascus sp." ~ "Balsamia aff. latispora",
    Species %in% c("Balsamia sp. CS105", "Balsamia sp. CS106", "Balsamia sp. CS117") ~ "Balsamia sp. CS101",
    Species == "Balsamia cf. quercicola" ~ "Balsamia quercicola",
    Species %in% c("Balsamia latispora", "Balsamia cf. latispora", "Balsamia aff. latispora") ~ "Balsamia aff. latispora",
    Species == "Balsamia cf. setchellii" ~ "Balsamia setchellii",
    Genus == "Cazia" ~ "Cazia flexiascus",
    Species == "Choiromyces alveolatus" ~ "Choiromyces", # Only CS61 can be confidently called cf alveolatus
    Species == "Choiromyces sp. CS52" ~ "Choiromyces sp. CS5",
    Species == "Choiromyces cf. alveolatus" ~ "Choiromyces sp. CS61",
    Genus == "Coniophora" ~ "Coniphora sp. CS143",
    Genus == "Cortinarius" ~ "Cortinarius pinguis",
    Species == "Elaphomyces sp" ~ "Elaphomyces",
    Species == "Endogonaceae sp. CS155" ~ "Jimgerdemannia sp. CS155",
    Species == "Gautieria sp" ~ "Gautieria",
    Species == "Gautieria cf. pterosperma" ~ "Gautieria sp. CS53",
    Species == "Geastrales sp. CS131" ~ "Geastrum sp. CS131",
    Species == "Geastrales sp. CS140" ~ "Geastrum sp. CS140",
    Species %in% c("Genea sp", "Genea harknessii", "Genea gardneri") ~ "Genea",
    Species %in% c("Genea sp. CS201") ~ "Genea sp. CS79",
    Species == "Genear arenaria" ~ "Genea arenaria",
    Species %in% c("Glomerales", "Zygomyces") ~ "Glomeraceae",
    Species %in% c("Hydnotryopsis", "Hydnotryopsis cf. setchellii", "Hydnotryopsis setchellii") ~ "Sarcosphaera setchellii",
    Species == "Hydnotryopsis sp. CS252" ~ "Sarcosphaera sp. CS252",
    Species == "Hysterangium sp" ~ "Hysterangium",
    Species %in% c("Hysterangium sp. CS75", "Hysterangium sp. CS134") ~ "Hysterangium sp. CS38",
    Species == "Zelleromyces" ~ "Lactarius",
    Genus == "Zelleromyces" ~ "Lactarius",
    Species == "Lactarius sp. CS78" ~ "Lactarius sp. CS40",
    Genus == "Leucangium" ~ "Leucangium carthusianum",
    Species == "Leucogaster odoratus" ~ "Leucogaster cf. odoratus",
    Species %in% c("Leucogaster sp. CS107", "Leucogaster citrinus") ~ "Leucogaster",
    Species %in% c("Leucogaster cf. rubescens", "Leucogaster sp. CS89") ~ "Leucogaster rubescens",
    Species == "Albatrellaceae sp. CS113" ~ "Leucophleps sp. CS113",
    Genus == "Lepiota" ~ "Lepiota viridigleba",
    Species %in% c("Melanogaster cf. ambiguus", "Melanogaster euryspermus", "Melanogaster cf. euryspermus") ~ "Melanogaster euryspermus",
    Genus == "Paraglactinia" | Species == "Paragalactinia" ~ "Paragalactinia infossa",
    Genus == "Phallogaster" ~ "Phallogaster phillipsii",
    Genus == "Phlegmacium" ~ "Phlegmacium sublilacinum",
    Species == "Rhizopogon sp" ~ "Rhizopogon",
    Species %in% c("Gymnomyces sp", "Gymnomyces") ~ "Russula",
    Genus == "Schenella" | Species == "Schenella" ~ "Schenella pityophila",
    Genus == "Sedecula" ~ "Seducula pulvinata",
    Genus == "Scleroderma" ~ "Scleroderma sp. CS210",
    Genus == "Trappea" ~ "Trappea sp. CS127",
    Species == "Tuber sp" ~ "Tuber",
    Species == "Tuber anniae" ~ "Tuber aff. anniae",
    Species == "Tuber cf. candidum" ~ "Tuber candidum",
    Species == "Tuber cf. whetstonense" ~ "Tuber whetstonense",
    Species == "Phylum unknown" ~ "Unknown",
    Genus == "Xerocomellus" ~ "Xerocomellus cf. macmurphyi",
    TRUE ~ Species
  ),
  Species_updated = trimws(Species_updated)
  ) |>
  # Add in taxonomy
  left_join(read.csv("raw_data/2024.11.08_Genera.csv") |> rename(Genus = Taxon)) |>
  # Add iNat IDs
  full_join(inat) |>
  # Add in GenBank info
  full_join(genbank_its) |>
  # Remove point that isn't a truffle
  filter(Species != "Lactarius crassus")

write.csv(trufflepoints |> select(-c(lat, lon, Point, Species_original, Notes)) |> arrange(Date),
          paste0("output/", Sys.Date(), "_AllTrufflePoints.csv"), row.names = FALSE)

# write.csv(trufflepoints |> drop_na(iNaturalist_Nr) |>
#             select(Specimen, Species_updated, iNaturalist_Nr, iNaturalist_URL, GenBank_Nr_ITS, DNA_method) |>
#             arrange(Specimen),
#           "output/2024.02.09_TrufflePoints_iNat.csv", row.names = FALSE)

# Missing CS numbers
cs.missing = trufflepoints |>
  drop_na(GenBank_Nr_ITS) |>
  filter(is.na(Genus))

# Missing iNat numbers
inat.missing = trufflepoints |>
  drop_na(Specimen) |>
  filter(is.na(iNaturalist_Nr)) |>
  arrange(Specimen)

#iNat numbers without points
inat.only = trufflepoints |>
  drop_na(Specimen) |>
  filter(is.na(Family)) |>
  arrange(Specimen)

#iNat cross check
inat.verify = trufflepoints |>
  filter(iNaturalist_Nr != iNaturalist_Nr_GB) |>
  relocate(GenBank_Nr_ITS, iNaturalist_Nr, iNaturalist_Nr_GB)

# write.csv(inat.verify, "output/2025.01.11_Genbank_Wrong_iNat.csv")

# Check that Genbank IDs match
id.verify = trufflepoints |>
  filter(Genus != Genus_GB) |>
  relocate(Species, Organism)

# Make lists of genera and species -----
## Habitat ----
genera.list.habitat = trufflepoints |>
  select(Phylum, Order, Family, Genus, Habitat) |>
  distinct() |>
  mutate(n = "X") |>
  pivot_wider(names_from = Habitat, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Genus) |>
  relocate(Mixed, .after = Oak) |>
  rename(Taxon = Genus)
  # write.csv("output/2024.11.08_Genera.csv", row.names = FALSE)

species.list.habitat = trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Phylum, Order, Family, Species_updated, Habitat) |>
  mutate(n = "X") |>
  distinct() |>
  pivot_wider(names_from = Habitat, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Species_updated) |>
  relocate(Mixed, .after = Oak) |>
  rename(Taxon = Species_updated)

table.lists.habitat = genera.list.habitat |>
  bind_rows(species.list.habitat)

### Species specific habitats ----
trufflepoints |>
  filter(Genus == "Genea") |>
  group_by(Habitat) |>
  summarize(n = length(Species_updated))

trufflepoints |>
  filter(Genus == "Balsamia") |>
  group_by(Habitat) |>
  summarize(n = length(Species_updated))

trufflepoints |>
  filter(Genus == "Tuber") |>
  group_by(Species_updated) |>
  summarize(n = length(Species_updated))

## Season ----
genera.list.season = trufflepoints |>
  select(Phylum, Order, Family, Genus, Season) |>
  distinct() |>
  mutate(n = "X") |>
  pivot_wider(names_from = Season, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Genus) |>
  # relocate(Mixed, .after = Oak) |>
  rename(Taxon = Genus)

species.list.season = trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Phylum, Order, Family, Species_updated, Season) |>
  mutate(n = "X") |>
  distinct() |>
  pivot_wider(names_from = Season, values_from = n, values_fill = "--") |>
  arrange(Phylum, Order, Family, Species_updated) |>
  # relocate(Mixed, .after = Oak) |>
  rename(Taxon = Species_updated)

table.lists.season = genera.list.season |>
  bind_rows(species.list.season)

table.lists = table.lists.habitat |>
  left_join(table.lists.season) |>
  relocate(Phylum, Order, Family, .after = last_col()) |>
  arrange(Phylum, Order, Family, Taxon) |>
  filter(Taxon != "Unknown")
  write.csv("output/2024.02.02_Table1.csv", row.names = FALSE)

spp.list = trufflepoints |>
  group_by(Genus, Species_updated, Season) |>
  summarize(n = length(Species_updated)) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0) |>
  arrange(Species_updated)
  # write.csv("output/2024.10.25_SpeciesCounts.csv", row.names = FALSE)

trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Species_updated) |>
  distinct() |>
  arrange(Species_updated)

genus.habitat.list = trufflepoints |>
  select(Genus, Habitat) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Habitat, values_from = n) |>
  mutate(Habitat = case_when(
    is.na(Oak) & Conifer == 1 ~ "Conifer",
    is.na(Conifer) & Oak == 1 ~ "Oak",
    Oak == 1 & Conifer == 1 ~ "Both",
    TRUE ~ NA
  )) |>
  select(-c(Oak, Conifer, Mixed))

genus.habitat.list.seasonal = trufflepoints |>
  select(Genus, Habitat, Season) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Habitat, values_from = n) |>
  mutate(Habitat = case_when(
    is.na(Oak) & Conifer == 1 ~ "Conifer",
    is.na(Conifer) & Oak == 1 ~ "Oak",
    Oak == 1 & Conifer == 1 ~ "Both",
    is.na(Oak) & is.na(Conifer) & Mixed == 1 ~ "Both",
    TRUE ~ NA
  )) |>
  select(-c(Oak, Conifer, Mixed))

genus.season.list = trufflepoints |>
  select(Genus, Season) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = Season) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = " ") |>
  mutate(season.colour = str_c(Autumn, Winter, Spring, Summer, sep = " "),
         season.colour = str_trim(season.colour))

genus.season.count = trufflepoints |>
  select(Genus, Season) |>
  distinct() |>
  arrange(Genus) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0) |>
  mutate(season.count = Autumn + Winter + Spring + Summer)

table(genus.season.count$season.count)

# Count season richness ----
season.richness.species = trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Genus, Species_updated, Season) |>
  distinct()

season.richness.genus.only = trufflepoints |>
  filter(Species_updated == Genus) |>
  select(Genus, Season) |>
  distinct() |>
  anti_join(season.richness.species)

season.richness = bind_rows(season.richness.species, season.richness.genus.only) |>
  mutate(Species_updated = coalesce(Species_updated, Genus)) |>
  mutate(n = 1) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = 0) |>
  filter(Species_updated != "Unknown")

sum(season.richness$Autumn)
sum(season.richness$Winter)
sum(season.richness$Spring)
sum(season.richness$Summer)

# Species abundance ----
trufflepoints |>
  group_by(Season) |>
  summarize(n = length(Species_updated))

# Count foray productivity ----
foray.productivity = trufflepoints |>
  group_by(Site, Date) |>
  summarize(n = length(Point))

foray.richness = trufflepoints |>
  select(Species_updated, Site, Date) |>
  distinct() |>
  group_by(Site, Date) |>
  summarize(n = length(Species_updated))

# Count site visits ----
sites = trufflepoints |>
  select(Site, Date) |>
  distinct() |>
  group_by(Site) |>
  summarize(n = length(Date)) |>
  arrange(desc(n))

sites.season = trufflepoints |>
  select(Site, Season, Date) |>
  distinct() |>
  group_by(Site, Season) |>
  summarize(n = length(Date)) |>
  mutate(n = as.character(n)) |>
  pivot_wider(names_from = Season, values_from = n, values_fill = '--')

site.list = trufflepoints |>
  select(Site, Habitat) |>
  distinct() |>
  mutate(X1 = "X") |>
  pivot_wider(names_from = Habitat, values_from = X1, values_fill = '--') |>
  left_join(sites.season) |>
  arrange(Site)

# write.csv(site.list, "output/2025.02.09_SiteList.csv", row.names = FALSE)

trufflepoints |>
  select(Date) |>
  distinct()

trufflepoints |>
  select(Date, Season) |>
  distinct() |>
  group_by(Season) |>
  summarize(n = length(Date))

# Check how many CS species there are ----
trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Species_updated) |>
  distinct() |>
  filter(str_detect(Species_updated, "CS"))

# Count species ----
trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Species_updated) |>
  distinct()

trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Genus) |>
  distinct()

trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Family) |>
  distinct()

trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Order) |>
  distinct()

trufflepoints |>
  filter(Species_updated != Genus) |>
  select(Phylum) |>
  distinct()

# Count sporocarps collected ----
sporocarps = trufflepoints |>
  drop_na(Specimen)

# Count overlap between oak and conifer ----
habitat.overlap = trufflepoints |>
  # Remove truffles only ID'd to genus
  filter(Species_updated != Genus) |>
  # Extract only relevant info
  select(Species_updated, Habitat) |>
  # Make a list of distinct entries
  distinct() |>
  # Assign marker
  mutate(n = 1) |>
  # Pivot out
  pivot_wider(names_from = Habitat, values_from = n, values_fill = 0) |>
  # Remove mixed
  # select(-Mixed) |>
  # Find any that are in both
  mutate(Both = case_when(
    Oak == 1 & Conifer == 1 ~ "Both",
    Oak == 0 & Conifer == 0 & Mixed == 1 ~ "Mixed only",
    Oak == 1 & Conifer == 0 & Mixed == 1 ~ "Oak + Mixed",
    Oak == 0 & Conifer == 1 & Mixed == 1 ~ "Conifer + Mixed",
    Oak == 0 & Conifer == 1 & Mixed == 0 ~ "Conifer only",
    Oak == 1 & Conifer == 0 & Mixed == 0 ~ "Oak only")
  )

table(habitat.overlap$Both)

# Calculate abundance ----

trufflecounts = trufflepoints |>
  group_by(Phylum, Genus, Habitat) |>
  summarize(n = length(Point))

truffletotals = trufflepoints |>
  group_by(Genus) |>
  summarize(n.total = length(Point))

abundance = trufflecounts |>
  rename(ct = n) |>
  arrange(ct) |>
  # mutate(category = case_when(
  #   ct >= 100 ~ "Abundant",
  #   ct < 100 & ct > 30 ~ "Common",
  #   ct <= 30 & ct >= 10 ~ "Uncommon",
  #   ct < 10 ~ "Rare"
  # )) |>
  # mutate(category = factor(category, levels = c("Abundant", "Common", "Uncommon", "Rare"))) |>
  filter(!Genus %in% c("Unknown")) |>
  # left_join(genus.habitat.list) |>
  # left_join(genus.season.count) |>
  mutate(Habitat = factor(Habitat, levels = c("Conifer", "Oak", "Mixed"))) |>
  # Add total counts
  left_join(truffletotals)
  # # Add colour
  # mutate(label.colour = case_when(
  #   Habitat == "Oak" ~ "#a63603",
  #   Habitat == "Conifer" ~ "#006d2c",
  #   Habitat == "Mixed" ~ "#4292c6"
  # ))

# write.csv(abundance |> select(Genus) |> distinct(), "output/2024.10.05_Genera.csv")

viz.abundance =
ggplot(abundance, aes(x = reorder(Genus, -n.total), y = ct, fill = Habitat)) +
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  geom_label(aes(label = ct, fill = Habitat, alpha = 0.8),
             size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
  # scale_fill_manual(values = c("#006d2c", "#a63603", "#4292c6")) + # Manuscript
  scale_fill_manual(values = c("#006d2c", "#a63603", "#4292c6")) + # MSA2025
  # scale_fill_manual(values = c("#006d2c","#f4e53f", "#a63603")) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  # labs(x = "", y = "Number of truffles found") +
  ylim(0,260) +
  # scale_y_log10() +
  labs(x = "", y = "Total truffles collected") +
  theme_bw() +
  # https://stackoverflow.com/questions/61956199/ggplot-color-axis-labels-based-on-variable
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        # legend.position = "none",
        )

# ggsave("presentation/2024.06.05_TruffleFreq.png", width = 19, height = 10, units = "in")
# ggsave("output/2024.09.28_TruffleFreq.png", width = 19, height = 10, units = "in")
ggsave(paste0("output/", Sys.Date(), "_TruffleFreq.svg"), width = 12, height = 8, units = "in")

viz.abundance.asco =
  ggplot(abundance |> filter(Phylum %in% c("Ascomycota")),
         aes(x = reorder(Genus, -n.total), y = ct, fill = Habitat)) +
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  geom_label(aes(label = ct, fill = Habitat, alpha = 0.8),
             size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
  # scale_fill_manual(values = c("#006d2c", "#a63603", "#4292c6")) + # Manuscript
  scale_fill_manual(values = c("#65881E","#9C413A", "#D7BE98")) + # MSA2025
  # scale_fill_manual(values = c("#006d2c","#f4e53f", "#a63603")) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of truffles found") +
  ylim(0,260) +
  facet_grid(~Phylum, scale = "free_x") +
  # scale_y_log10() +
  # labs(x = "", y = "Total truffles collected") +
  theme_bw() +
  # https://stackoverflow.com/questions/61956199/ggplot-color-axis-labels-based-on-variable
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        # legend.position = "none",
  )

viz.abundance.basidio =
  ggplot(abundance |> filter(Phylum %in% c("Basidiomycota")),
         aes(x = reorder(Genus, -n.total), y = ct, fill = Habitat)) +
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  geom_label(aes(label = ct, fill = Habitat, alpha = 0.8),
             size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
  # scale_fill_manual(values = c("#006d2c", "#a63603", "#4292c6")) + # Manuscript
  scale_fill_manual(values = c("#65881E","#9C413A", "#D7BE98")) + # MSA2025
  # scale_fill_manual(values = c("#006d2c","#f4e53f", "#a63603")) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of truffles found") +
  ylim(0,260) +
  facet_grid(~Phylum, scale = "free_x") +
  # scale_y_log10() +
  # labs(x = "", y = "Total truffles collected") +
  theme_bw() +
  # https://stackoverflow.com/questions/61956199/ggplot-color-axis-labels-based-on-variable
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        # legend.position = "none",
  )

library(patchwork)

viz.abundance.asco + viz.abundance.basidio +
  plot_layout(widths = c(1, 2)) +
  plot_layout(axes = "collect") +
  plot_layout(guides = "collect")

ggsave(paste0("output/", Sys.Date(), "_TruffleFreq_Phylum.svg"), width = 12, height = 8, units = "in")

# Seasonal abundance ----
trufflecounts.seasonal = trufflepoints |>
  filter(Genus != "Zygomyces") |>
  group_by(Season, Genus, Habitat) |>
  summarize(n = length(Point))

abundance.seasonal = trufflecounts.seasonal |>
  rename(ct = n) |>
  arrange(ct) |>
  left_join(abundance |> select(Genus, category, ct) |> rename(total = ct) |> distinct()) |>
  # mutate(category = case_when(
  #   ct >= 100 ~ "Abundant",
  #   ct < 100 & ct > 30 ~ "Common",
  #   ct <= 30 & ct >= 10 ~ "Uncommon",
  #   ct < 10 ~ "Rare"
  # )) |>
  mutate(category = factor(category, levels = c("Abundant", "Common", "Uncommon", "Rare")),
         Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) |>
  filter(!Genus %in% c("Unknown"))
  # left_join(genus.habitat.list.seasonal) |>
  # mutate(Habitat = factor(Habitat, levels = c("Conifer", "Oak", "Both")))

## Visualise with all spp present ----
ggplot(abundance.seasonal, aes(x = Genus, y = ct, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of truffles found") +
  ggh4x::facet_grid2(Season~., scale = "free_y", independent = "y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

# ggsave("output/2024.09.28_TruffleFreq_seasonal_alphabetical.png", width = 19, height = 10, units = "in")

## Visualise with independent X ----
plot.abundance = function(season, background.color){
  ggplot(abundance.seasonal |> filter(Season == season),
         aes(x = reorder(Genus, -ct), y = ct, fill = Habitat)) +
    geom_bar(position="stack", stat="identity") +
    # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
    geom_label(aes(label = ct, fill = Habitat, alpha = 0.8), size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
    scale_fill_manual(values = c("#006d2c", "#a63603", "#4292c6")) +
    # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
    ggh4x::facet_grid2(~Season, scale = "free_x", independent = "x") +
    ylim(0,175) +
    labs(x = "", y = "Number of truffles found") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
          text=element_text(size=20),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill=background.color))
}

library(patchwork)

spring = plot.abundance("Spring", "#fefadc")
summer = plot.abundance("Summer", "#fee1dc")
autumn = plot.abundance("Autumn", "#f3e8fc")
winter = plot.abundance("Winter", "#e8f9fc")

(viz.abundance |
(autumn + winter + spring + summer + plot_layout(ncol = 2, axes = "collect"))) +
plot_layout(nrow = 2, heights = c(1, 3), axes = "collect")

# ggsave("output/2024.10.04_TruffleFreq_seasonal_freeX.png", width = 19, height = 10, units = "in")
# ggsave("output/2024.10.05_TruffleFreq_seasonal_freeX.png", width = 19, height = 10, units = "in")
# ggsave("output/2024.02.01_TruffleFreq_all.svg", width = 15, height = 15, units = "in")

## Stacked ----
ggplot(abundance.seasonal, aes(x = Genus, y = ct, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  ggh4x::facet_grid2(Season~., scale = "free", independent = "all") +
  labs(x = "", y = "Number of truffles found") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

ggsave("output/2024.09.28_TruffleFreq_seasonal_freeX.png", width = 10, height = 19, units = "in")

ggplot(abundance.seasonal, aes(x = reorder(Genus, -total), y = ct, fill = Season)) +
  geom_bar(stat = "identity", position = "stack") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("#0099D1FF", "#FEC289FF", "#F09000FF", "#9DC7E4FF"))+
  labs(x = "", y = "Number of truffles found") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.8))

ggsave("output/2024.09.28_TruffleFreq_seasonal_stacked.png", width = 19, height = 10, units = "in")

## Calculate stats seasonally ----
### How many forays? ----
length(table(trufflepoints$Date))

forays = trufflepoints |>
  select(Season, Date) |>
  distinct()

table(forays$Season)

truffle.seasons = trufflepoints |>
  group_by(Season, Genus) |>
  summarize(n = length(Genus)) |>
  pivot_wider(names_from = Genus, values_from = n)

season.counts = trufflepoints |>
  select(Season, Species) |>
  distinct()

table(season.counts$Season)

# Ranked abundance for NATS ----
nats = read.csv("raw_data/observations-445963_NATS.csv") |>
  separate(scientific_name, into = c("Genus", "Species"), remove = FALSE) |>
  drop_na(Species) |>
  group_by(scientific_name) |>
  summarize(ct = length(id)) |>
  filter(ct >= 30)

ggplot(nats, aes(x = reorder(scientific_name, -ct), y = ct)) +
  geom_bar(stat = "identity", fill = "#73ab00") +
  geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  labs(x = "", y = "Number of iNat observations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=65,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank())

ggsave("visualizations/2024.06.04_NATSTruffleFreq.png", width = 10, height = 10, units = "in")

# Habitat abundance ----
trufflecounts.habitat = trufflepoints |>
  filter(Genus != "Zygomyces") |>
  group_by(Habitat, Genus) |>
  summarize(n = length(Point))

abundance.habitat = trufflecounts.habitat |>
  rename(ct = n) |>
  arrange(ct) |>
  filter(!Genus %in% c("Unknown"))

ggplot(abundance.habitat |> filter(Habitat == "Oak") |>
         filter(!Genus %in% c("Glomeraceae", "Endogone", "Rhizopogon","Scleroderma")),
       aes(x = reorder(Genus, -ct), y = ct)) +
  geom_bar(stat = "identity", fill = "#FEC289FF", colour = "#FEC289FF") +
  # geom_text(aes(label = ct), size = 5, vjust = -0.5) +
  geom_label(aes(label = ct, alpha = 0.8), size = 5, vjust = -0.5, label.padding = unit(0.05, "lines")) +
  # scale_fill_manual(values = c("#006d2c", "#a63603", "#4292c6")) +
  # scale_fill_manual(values = c("#014636", "#02818a", "#67a9cf", "#d0d1e6"))+
  # ggh4x::facet_grid2(~Season, scale = "free_x", independent = "x") +
  ylim(0,175) +
  labs(x = "", y = "Number of truffles found") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust = 1, face="bold"),
        text=element_text(size=20),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        # strip.background = element_rect(fill=background.color)
        )

ggsave("output/2025.02.16_CSNM_Oaks.png")
