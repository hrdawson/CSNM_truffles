library(janitor)
library(vegan)
library(Hotelling)
library(iNEXT)
# # Rarefaction curve
# iNat = read.csv("raw_data/observations-481166.csv") |>
# # iNat = read.csv("raw_data/observations-431064.csv") |>
#   select(id, observed_on, field.id.., field.voucher.number, field.voucher.number.s.) |>
#   mutate(field.id = case_when(
#     !is.na(field.id..) ~ paste("CS", field.id.., sep = ""),
#     TRUE ~ NA
#   ),
#   sample_id = coalesce(field.id, field.voucher.number, field.voucher.number.s.)) |>
#   filter(sample_id != "") |>
#   # mutate(sample_id = str_extract(tag_list, "CS\\d+")) |>
#   select(id, observed_on, sample_id)
#
# truffle.coll = read.csv("raw_data/2024.09.27_CSNM Species List All.csv") |>
#   left_join(iNat) |>
#   # This gives really odd numbers but they seem to work anyway
#   # https://stackoverflow.com/questions/56365579/is-there-an-r-function-that-will-rank-dates-times-by-other-column-criteria
#   mutate(tripDay = rank(observed_on),
#          species = paste(Genus, epithet)) |>
#   filter(species != " ") |>
#   select(tripDay, species) |>
#   add_column(count = 1) |>
#   distinct() |>
#   pivot_wider(names_from = tripDay, values_from = count) |>
#   tibble::column_to_rownames(var = "species") %>%
#   # Turn binary
#   replace(is.na(.), 0)

truffle.coll = read.csv("output/2024.02.02_AllTrufflePoints.csv") |>
  filter(Species_updated != Genus) |>
  mutate(tripDay = rank(as_date(Date)),
         species = Species_updated) |>
  select(tripDay, species) |>
  add_column(count = 1) |>
  distinct() |>
  pivot_wider(names_from = tripDay, values_from = count) |>
  tibble::column_to_rownames(var = "species") %>%
  # Turn binary
  replace(is.na(.), 0)

incid = list()
#Low dieback severity
incid[[1]] = as.data.frame(truffle.coll) |> mutate_if(is.character,as.numeric)

rare.inext = iNEXT(incid, datatype = "incidence_raw", endpoint = 80)

# Species diversity
ggiNEXT(rare.inext, type = 1)+
  # scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  labs(x = "Number of trip-days") +
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=20),
        panel.border = element_rect(colour = "black", fill=NA))

# ggsave("output/2024.06.05_CSNMtruffleDiversity.png", width = 8, height = 6, units = "in")
ggsave("output/2024.02.01_CSNMtruffleDiversity.png", width = 8, height = 6, units = "in")

#Sample coverage
ggiNEXT(rare.inext, type = 2)+
  # scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  labs(x = "Number of trip-days") +
  ylim(0,1) +
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=20),
        panel.border = element_rect(colour = "black", fill=NA))

# ggsave("output/2024.06.05_CSNMsampleCoverage.png", width = 8, height = 6, units = "in")
ggsave("output/2024.02.01_CSNMsampleCoverage.png", width = 8, height = 6, units = "in")
