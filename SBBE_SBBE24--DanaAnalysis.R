### The BEGINNING ~~~~~
##
# Plots SBBE -- Map | Written by George Pacheco ~


# Cleans environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads packages ~
pacman::p_load(tidyverse, ggstar, ggrepel, shadowtext, readxl, writexl, cowplot, ggpubr, lemon, reshape2, writexl, stringr, lubridate,
               geobr, ggspatial, showtext, png, extrafont, sf, ggiraphExtra, fontawesome, shiny, DT,
               rvest, stringr, purrr, tibble, dplyr, extrafont, emojifont)


# Loads extra fonts ~
loadfonts()
font_add_google("Barlow", "Barlow")
font_add_google("Cormorant Garamond", "Cormorant")
showtext_auto()


# Loads data ~
fulldf <- read.csv("SBBEmembers_28mar25.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")


# Corrects Institution for better visualisation ~
levels(fulldf$Institution <- gsub("Universidade|Universidad|University", "Uni.", fulldf$Institution))


# Gets Stage ~
fulldf$Stage <- ifelse(grepl("Profissional", fulldf$Labels), "Profissional",
                ifelse(grepl("Pos-Graduacao", fulldf$Labels), "Pós-graduação",
                ifelse(grepl("Graduacao", fulldf$Labels), "Graduação", NA)))


# Expands fulldf by creating Region ~
fulldf$Region <- ifelse(fulldf$State %in% c("AC", "AM", "AP", "PA", "RR", "RO", "TO"), "Norte",
                 ifelse(fulldf$State %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"), "Nordeste",
                 ifelse(fulldf$State %in% c("DF", "GO", "MT", "MS"), "Centro-Oeste",
                 ifelse(fulldf$State %in% c("MG", "ES", "RJ", "SP"), "Sudeste",
                 ifelse(fulldf$State %in% c("PR", "RS", "SC"), "Sul",
                 ifelse(fulldf$State %in% c("Exterior"), "Exterior", "Error"))))))


# Defines the custom capitalization function ~
capitalize_words <- function(text) {
  words <- str_split(text, " ")[[1]]
  exclude_patterns <- c("of", "de", "da", "do", "ABC", "Não-binário", "(EUA)")
  patterns_map <- setNames(exclude_patterns, tolower(exclude_patterns))
  words <- sapply(words, function(word) {
    word_lower <- tolower(word)
    if (word_lower %in% names(patterns_map)) {
      patterns_map[[word_lower]]}
    else {str_to_title(word)}})
  str_c(words, collapse = " ")}


# Apply the function to the pattern column
fulldf$Name <- sapply(fulldf$Name , capitalize_words)
fulldf$Institution <- sapply(fulldf$Institution , capitalize_words)
fulldf$Gender <- sapply(fulldf$Gender, capitalize_words)


# Sets all Brazilian states ~ 
AllBRLStates <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                  "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                  "GO", "MT", "MS", "DF",
                  "ES", "MG", "RJ", "SP",
                  "PR", "RS", "SC", "Exterior")


# Sets all Brazilian regions ~ 
AllBRLRegions <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul", "Exterior")


# Defines a common set of levels and ordering for Variable ~
variable_levels <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                     "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                     "GO", "MT", "MS", "DF",
                     "ES", "MG", "RJ", "SP",
                     "PR", "RS", "SC",
                     "Profissional",
                     "Pós-graduação",
                     "Graduação",
                     "Dias até o SBBE24",
                     "Seguidores no Bluesky",
                     "Seguidores no X",
                     "Seguidores no Instagram",
                     "Instituições representadas na SBBE",
                     "Afiliados à SBBE",
                     "Uni. of Ottawa (Canadá)",
                     "Pennsylvania State Uni. (EUA)",
                     "Uni. of Texas (EUA)",
                     "Texas A&M Uni. (EUA)",
                     "Uni. of California — Los Angeles (EUA)",
                     "Rice Uni. (EUA)",
                     "Uni. of West Florida (EUA)",
                     "Uni. of Aberdeen (Escócia)",
                     "Uni. of Oslo (Noruega)",
                     "Uni. of Copenhagen (Dinamarca)",
                     "Uni. Nacional de Misiones (Argentina)",
                     "Uni. Nacional de Trujillo (Argentina)",
                     "Instituto de Biología Subtropical (Argentina)",
                     "Instituto Multidisciplinario de Biología Vegetal (Argentina)",
                     "Uni. Nacional de Córdoba (Argentina)",
                     "Uni. de Buenos Aires (Argentina)",
                     "Uni. Federal de Santa Maria",
                     "Uni. Federal do Rio Grande",
                     "Uni. Federal de Pelotas",
                     "Uni. Federal de Ciências da Saúde de Porto Alegre",
                     "Uni. Federal do Rio Grande do Sul",
                     "Pontifícia Uni. Católica do Rio Grande do Sul",
                     "Uni. Federal da Integração Latino-Americana",
                     "Uni. Estadual de Maringá",
                     "Centro Universitário Claretiano",
                     "Uningá",
                     "Museu de História Natural Capão da Imbuia",
                     "Instituto Carlos Chagas — Fiocruz Paraná",
                     "Uni. Estadual de Ponta Grossa",
                     "Uni. Estadual do Centro-Oeste do Paraná",
                     "Secretaria de Educação do Estado do Paraná",
                     "Instituto Federal — Paraná",
                     "Uni. Tecnológica Federal do Paraná",
                     "Pontifícia Uni. Católica do Paraná",
                     "Uni. Federal do Paraná",
                     "Hospital Regional Hans Dieter Schmidt",
                     "Centro Universitário Leonardo da Vinci",
                     "Uni. da Região de Joinville",
                     "Uni. Federal de Santa Catarina",
                     "Uni. do Vale do Paraíba",
                     "Instituto Butantan",
                     "Uni. do Vale do Itajaí",
                     "Uni. Santo Amaro",
                     "Uni. do Oeste Paulista",
                     "Uni. de Mogi Das Cruzes",
                     "Uni. Federal de São Carlos",
                     "Uni. Federal do ABC",
                     "Uni. Federal de São Paulo",
                     "Uni. de São Paulo",
                     "Uni. Estadual de Campinas",
                     "Uni. Estadual Paulista",
                     "Unisãojosé",
                     "Uni. Estadual do Norte Fluminense Darcy Ribeiro",
                     "Instituto Oswaldo Cruz",
                     "Uni. Federal do Rio de Janeiro",
                     "Uni. do Estado do Rio de Janeiro",
                     "Uni. Federal do Espírito Santo",
                     "Instituto Nacional da Mata Atlântica",
                     "Pontifícia Uni. Católica de Minas Gerais",
                     "Uni. Federal de Uberlândia", 
                     "Uni. Federal do Triângulo Mineiro",
                     "Uni. Federal de Juiz de Fora",
                     "Uni. do Estado de Minas Gerais",
                     "Uni. Federal de Minas Gerais",
                     "Uni. Federal de Lavras",
                     "Uni. Federal de Viçosa", 
                     "Uni. Federal de São João Del-Rei",
                     "Uni. Federal de Jataí",
                     "Empresa Brasileira de Pesquisa Agropecuária",
                     "Uni. de Brasília",
                     "Uni. Federal de Goiás",
                     "Uni. Estadual de Goiás",
                     "Uni. do Estado de Mato Grosso",
                     "Uni. Federal de Mato Grosso",
                     "Uni. Federal de Mato Grosso do Sul",
                     "Uni. Federal da Grande Dourados",
                     "Uni. Estadual de Santa Cruz",
                     "Uni. Federal da Bahia",
                     "Uni. Estadual de Feira de Santana",
                     "Uni. Federal do Recôncavo da Bahia",
                     "Uni. Estadual do Sudoeste da Bahia",
                     "Uni. Federal de Sergipe",
                     "Uni. Federal de Alagoas",
                     "Uni. Federal do Vale do São Francisco",
                     "Uni. de Pernambuco",
                     "Instituto Aggeu Magalhães — Fiocruz Pernambuco",
                     "Instituto Federal — Pernambuco",
                     "Uni. Federal de Pernambuco",
                     "Uni. Estadual da Paraíba",
                     "Uni. Federal da Paraíba",
                     "Uni. Federal do Rio Grande do Norte",
                     "Museu Paraense Emílio Goeldi",
                     "Uni. Federal do Pará",
                     "Instituto Nacional de Pesquisas da Amazônia",
                     "Uni. Federal do Amazonas",
                     "Feminino",
                     "Masculino",
                     "Outro",
                     "Exterior",
                     "Sul", 
                     "Sudeste",
                     "Centro-Oeste",
                     "Nordeste",
                     "Norte")


# Gets current date ~
current_date <- Sys.Date()


# Checks missing Institution ~
setdiff(fulldf$Institution, variable_levels)


# Gets general numbers ~
fulldf_Descriptive <- fulldf %>%
                      summarise("Afiliados à SBBE" = n_distinct(Name),
                                "Instituições representadas na SBBE" = n_distinct(Institution),
                                "Seguidores no Instagram" = 1922, 
                                "Seguidores no Bluesky" = 29) %>%
  mutate(Stats = "General") %>%
  pivot_longer(cols = -Stats, names_to = "Variable", values_to = "Percentage") %>%
  mutate(n = 0) %>%
  relocate(n, .before = Percentage) %>%
  relocate(Stats, .after = Percentage) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets percentage for Institution ~
fulldf_StageMembersPerc <- fulldf %>%
  count(Stage) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Stage) %>%
  mutate(Stats = "StageMembers") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Create a data frame with counts and proportions per institution ~
fulldf_StateMembersPerc <- fulldf %>%
  count(State) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = State) %>%
  complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "StateMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionMembersPerc <- fulldf %>%
  filter(Region != "Error") %>%
  count(Region) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Region) %>%
  complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "RegionMembers")


# Gets percentage for Gender ~
fulldf_GenderMembersPerc <- fulldf %>%
  filter(Region != "Error") %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Gender) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "GenderMembers")


fulldf_InstitutionMembersPerc <- fulldf %>%
  filter(Institution != "") %>%
  count(Institution, Region) %>%  # Count by both Institution and Region
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Institution) %>%
  mutate(Stats = "InstitutionMembers") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Combine the data frames ~  
fulldfUp <- bind_rows(fulldf_StageMembersPerc,
                      fulldf_StateMembersPerc,
                      fulldf_RegionMembersPerc,
                      fulldf_GenderMembersPerc,
                      fulldf_InstitutionMembersPerc)


# Expands fulldfUp by creating BarFill ~
fulldfUp$BarFill <- ifelse(str_detect(fulldfUp$Stats, "Members"), "#006837",
                    ifelse(str_detect(fulldfUp$Stats, "Attendees"), "#41ab5d", "#fbb4ae"))


# Reorders Population ~
fulldfUp$Stats <- factor(fulldfUp$Stats, ordered = T,
                         levels = c("StageMembers",
                                    "StateMembers",
                                    "RegionMembers",
                                    "GenderMembers",
                                    "InstitutionMembers"))


# Sets custom x-axis labels ~
ylabel <- c("StageMembers" = "Afiliados à SBBE por Estágio Acadêmico",
            "GenderMembers" = "Membros da SBBE por por Gênero")


# Load geom data ~
BRL_Regions <- read_region(simplified = TRUE, year = 2020)
BRL_States <- read_state(code_state = "all", simplified = TRUE, year = 2020)


# Corrects entries ~
levels(BRL_Regions$name_region <- gsub("Centro Oeste", "Centro-Oeste", BRL_Regions$name_region))
levels(BRL_States$name_region <- gsub("Centro Oeste", "Centro-Oeste", BRL_States$name_region))


# Expands BRL_Regions by creating Region ~
BRL_Regions$Region <- ifelse(BRL_Regions$name_region %in% c("Norte"), "North",
                      ifelse(BRL_Regions$name_region %in% c("Nordeste"), "Northeast",
                      ifelse(BRL_Regions$name_region %in% c("Centro-Oeste"), "Central-West",
                      ifelse(BRL_Regions$name_region %in% c("Sudeste"), "Southeast",
                      ifelse(BRL_Regions$name_region %in% c("Sul"), "South", "Error")))))


# Expands BRL_Regions by adding the SBBE24 & Abroad rows ~
BRL_Regions <- add_row(BRL_Regions, name_region = "SBBE24", Region = "SBBE24")
BRL_Regions <- add_row(BRL_Regions, name_region = "Abroad", Region = "Abroad")
BRL_States <- add_row(BRL_States, abbrev_state = "Abroad", name_region = "Abroad")


# Creates a data frame with the centroids of the Brazilian regions ~
BRL_Regions_Centroids_df <- data.frame(Region = c("North", "Northeast", "Central-West", "Southeast", "South", "SBBE24", "Abroad", "SP"),
                                       Longitude = c(-58, -41.25, -53.15, -44.85, -51.4, -49.271111, -65, -48.62),
                                       Latitude = c(-3.5, -8, -15.5, -20, -27.5, -25.429722, -25, -21.9))


# Merges data frame to perform the change ~
BRL_Regions <- left_join(BRL_Regions, BRL_Regions_Centroids_df, by = "Region")


# Reduces data ~ 
BRL_Regions <- BRL_Regions %>%
  dplyr::select(-code_region)
BRL_States <- BRL_States %>%
  dplyr::select(-c(code_state, code_region, name_state))


# Renames columns ~
BRL_States <- BRL_States %>%
  rename(Variable = abbrev_state)


# Joins data frames ~
fulldf_RegionMembersPerc <- fulldf_RegionMembersPerc %>%
                            rename(name_region = Variable)


# Corrects the Estrangeiro -> Abroad ~
levels(fulldf_StateMembersPerc$Variable <- gsub("Exterior", "Abroad", fulldf_StateMembersPerc$Variable))
levels(fulldf_RegionMembersPerc$name_region <- gsub("Exterior", "Abroad", fulldf_RegionMembersPerc$name_region))


# Merges data frames individually ~
RegionMembers_df <- BRL_States %>%
  inner_join(fulldf_RegionMembersPerc, by = "name_region") %>%
  mutate(Stats = "Members")  %>%
  mutate(Division = "Per Region")
StateMembers_df <- BRL_States %>%
  inner_join(fulldf_StateMembersPerc, by = "Variable") %>%
  mutate(Stats = "Members") %>%
  mutate(Division = "Per State")


# Combines data frames ~
combined_sfs <- bind_rows(StateMembers_df, RegionMembers_df)


# Expands combined_sfs by adding the SBBE24 row ~
combined_sfs <- add_row(combined_sfs, name_region = "SBBE24", Division = "Per Region", Stats = "Members")


# Converts to data frames ~
combined_dfs <- as.data.frame(combined_sfs)
BRL_Regions_df <- as.data.frame(BRL_Regions)


# Merges data frame to perform the change ~
merged_dfs <- left_join(combined_dfs, BRL_Regions_df, by = "name_region", suffix = c("", ".BRL"))


# Performs the change ~
resulting_dfs <- merged_dfs %>%
                 mutate(geom = ifelse(Division == "Per Region", geom.BRL, geom))


# Eliminates unnecessary column ~
resulting_dfs <- resulting_dfs %>% 
                 dplyr::select(-c(n, geom.BRL))


# Converts data frame back to sf ~
fulldf_map <- st_as_sf(resulting_dfs, crs = st_crs(combined_sfs))


# Renames columns ~
BRL_Regions_Centroids_df <- BRL_Regions_Centroids_df %>%
                            rename(Variable = Region)


# Converts to data frames ~
combined_dfs <- as.data.frame(combined_sfs)
BRL_Regions_df <- as.data.frame(BRL_Regions)


# Merges data frame to perform the change ~
fulldf_map <- left_join(fulldf_map, BRL_Regions_Centroids_df, by = "Variable", suffix = c("", ".SP"))


# Reorders Population ~
fulldf_map$Stats <- factor(fulldf_map$Stats, ordered = T,
                           levels = c("Members"))


# Reorders Division ~
fulldf_map$Division <- factor(fulldf_map$Division, ordered = T,
                              levels = c("Per Region",
                                         "Per State"))


# Expands BRL_Regions by creating Region ~
fulldf_map$Region_PT <- ifelse(fulldf_map$Region %in% c("Northeast"), "Nordeste",
                       ifelse(fulldf_map$Region %in% c("North"), "Norte",
                       ifelse(fulldf_map$Region %in% c("Central-West"), "Centro-Oeste",
                       ifelse(fulldf_map$Region %in% c("Southeast"), "Sudeste",
                       ifelse(fulldf_map$Region %in% c("South"), "Sul",
                       ifelse(fulldf_map$Region %in% c("SBBE24"), "SBBE24",
                       ifelse(fulldf_map$Region %in% c("Abroad"), "Exterior", "Error")))))))


# Sets custom x-axis labels ~
xlabel_PT <- c("Per Region" = "Por Região",
               "Per State" = "Por Estado")
ylabel_PT <- c("Members" = "% de Membros Fundadores da SBBE",
               "Attendees" = "% de Participantes no SBBE24")
xlabel_EN <- c("Members" = "% of SBBE Members",
               "Attendees" = "% of SBBE24 Attendees")
ylabel_EN <- c("Members" = "% of SBBE Members",
               "Attendees" = "% of SBBE24 Attendees")


# Creates the Circular data frame ~
Circular <- subset(fulldfUp, Stats == "InstitutionMembers") %>% arrange(desc(Percentage))


# Reorders Population ~
Circular$Region <- factor(Circular$Region, ordered = TRUE,
                          levels = c("Norte",
                                     "Nordeste",
                                     "Centro-Oeste",
                                     "Sudeste",
                                     "Sul",
                                     "Exterior"))


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar * nlevels(Circular$Region), ncol(Circular)))
colnames(to_add) <- colnames(Circular)
to_add$Region <- rep(levels(Circular$Region), each = empty_bar)
Circular <- rbind(Circular, to_add)
Circular <- Circular %>% arrange(Region)
Circular$ID <- seq(1, nrow(Circular))


# Get the name and the y position of each label
label_data_Circular <- Circular
number_of_bar <- nrow(label_data_Circular)
angle <- 90 - 360 * (label_data_Circular$ID - .5) / number_of_bar
label_data_Circular$hjust <- ifelse(angle < -90, 1, 0)
label_data_Circular$angle <- ifelse(angle < -90, angle + 180, angle)


# Prepares a data frame for base lines ~
base_data_Circular <- Circular %>% 
  group_by(Region) %>% 
  summarize(start = min(ID), 
            end = max(ID) - empty_bar, 
            N = n(), .groups = "drop") %>%
  mutate(end = ifelse(N == 1, start + 1, end)) %>%
  mutate(title = (start + end) / 2)


# Prepares a data frame for grid ~
grid_data_Circular <- base_data_Circular
grid_data_Circular$end <- grid_data_Circular$end[ c( nrow(grid_data_Circular), 1:nrow(grid_data_Circular) -1)] + 1
grid_data_Circular$start <- grid_data_Circular$start - 1
grid_data_Circular <- grid_data_Circular[-1, ]


# Reorders Population ~
fulldf_map$name_region <- factor(fulldf_map$name_region, ordered = TRUE,
                          levels = c("Norte",
                                     "Nordeste",
                                     "Centro-Oeste",
                                     "Sudeste",
                                     "Sul",
                                     "Abroad"))


# Creates panel ~
MiniMap <-
  ggplot() +
  geom_sf(data = subset(fulldf_map, name_region != "SBBE24"), aes(fill = name_region), colour = "#f7fbff") +
  geom_star(data = subset(fulldf_map, Division == "Per Region" & Region == "Abroad"),
            aes(x = Longitude, y = Latitude, fill = name_region), size = 26, starshape = 8, starstroke = .3, colour = "#f7fbff") +
  scale_fill_manual(values = c("#1b9e77", "#fdb462", "#fb8072", "#bebada", "#80b1d3", "#c994c7")) +
  geom_text(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region != "SBBE24"),
                  aes(x = Longitude, y = Latitude, label = Region_PT),
                  family = "Cormorant", size = 5.25, colour = "#ffffff") +
  coord_sf(xlim = c(-75.75, -33),
           ylim = c(-35, 6.5),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.margin =  margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


# Convert map plot into a grob
MiniMap_Grob <- ggplotGrob(MiniMap)


max_height <- max(Circular$Percentage, na.rm = TRUE) * 100 + 3


# Generates plot ~
Institution_Plot <-
 ggplot(Circular, aes(x = as.factor(ID), y = Percentage * 100, fill = Region)) +
 geom_bar(aes(x = as.factor(ID), y = Percentage * 100, fill = Region), stat = "identity", alpha = 1) +
  scale_fill_manual(values = c("#1b9e77", "#fdb462", "#fb8072", "#bebada", "#80b1d3", "#c994c7"), na.translate = FALSE) +
  geom_segment(data = grid_data_Circular, aes(x = end, y = 5, xend = start, yend = 5), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Circular, aes(x = end, y = 10, xend = start, yend = 10), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Circular, aes(x = end, y = 15, xend = start, yend = 15), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Circular, aes(x = end, y = 20, xend = start, yend = 20), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  annotate("text", x = rep(max(Circular$ID), 4), y = c(5, 10, 15, 20), label = c("5%", "10%", "15%", "20%"), family = "Cormorant", size = 4, fontface = "bold", color = "#737373", angle = 0, hjust = 1) +
  geom_bar(aes(x = as.factor(ID), y = Percentage * 100, fill = Region), stat = "identity", alpha = .5) +
  ylim(-100, 80) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(-5.8, -4, -4, -3), "cm"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank()) +
  coord_polar() + 
  #geom_text(data = label_data, aes(x = ID, y = max_height, label = Variable, hjust = hjust), family = "Barlow", size = 3.5, color = "#000000", fontface = "bold", alpha = .6, angle = label_data$angle, inherit.aes = FALSE ) +
  geom_text(data = label_data_Circular, aes(x = ID, y = Percentage * 100 + 6, label = Variable, hjust = hjust), family = "Cormorant", size = 5, color = "#737373", fontface = "bold", angle = label_data_Circular$angle, inherit.aes = FALSE ) +
  geom_segment(data = base_data_Circular, aes(x = start, y = -5, xend = end, yend = -5), colour = "#000000", alpha = 1, size = .6, inherit.aes = FALSE)
 

# Overlays plots ~
Institution_Plot <- ggdraw() +
                    draw_plot(Institution_Plot) + 
                    draw_plot(MiniMap, x = .31, y = .33, width = .4, height = .4)
      
      
# Saves Institution plot ~
ggsave(Institution_Plot, file = "Institution_Plot_B.pdf", limitsize = FALSE,
       device = cairo_pdf, scale = 1, width = 15, height = 14, dpi = 600)


Gender <- fulldfUp %>% 
  filter(Stats == "GenderMembers" | Stats == "StageMembers") %>%
  droplevels() %>%
  arrange(desc(Percentage))

empty_bar <- 6
to_add <- data.frame(matrix(NA, empty_bar * nlevels(Gender$Stats), ncol(Gender)))
colnames(to_add) <- colnames(Gender)
to_add$Stats <- rep(levels(Gender$Stats), each = empty_bar)
Gender <- rbind(Gender, to_add)
Gender <- Gender %>% arrange(Stats)
Gender$ID <- seq(1, nrow(Gender))

label_data_Gender <- Gender
number_of_bar <- nrow(label_data_Gender)
angle <- 90 - 360 * (label_data_Gender$ID - .5) / number_of_bar
label_data_Gender$hjust <- ifelse(angle < -90, 1, 0)
label_data_Gender$angle <- ifelse(angle < -90, angle + 180, angle)

base_data_Gender <- Gender %>% 
  group_by(Stats) %>% 
  summarize(start = min(ID), 
            end = max(ID) - empty_bar, 
            N = n(), .groups = "drop") %>%
  mutate(end = ifelse(N == 1, start + 1, end)) %>%
  mutate(title = (start + end) / 2)

grid_data_Gender <- base_data_Gender
grid_data_Gender$end <- grid_data_Gender$end[ c( nrow(grid_data_Gender), 1:nrow(grid_data_Gender) -1)] + 1
grid_data_Gender$start <- grid_data_Gender$start - 1
grid_data_Gender <- grid_data_Gender[-1, ]


# Create a data frame with swapped x, y positions for icons and labels
data <- data.frame(x = c(.2, .6), y = c(.36, .725), label = fontawesome(c("fa-group", "fa-graduation-cap")))
text_data <- data.frame(x = c(.2, .6), y = c(.17, .55), label = c("Gênero", "Estágio Acadêmico"))


Icons <-
  ggplot(data, aes(x, y, color = label, label = label)) +
  geom_text(family = "fontawesome-webfont", size = 30) +
  geom_text(data = text_data, aes(x, y, label = label), family = "Cormorant", size = 6, fontface = "bold", color = "#000000") +
  scale_colour_manual(values = c("#fdbf6f", "#e5d8bd"), na.translate = FALSE) +
  scale_x_continuous("",
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous("",
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


# Generates plot ~
Gender_Plot <-
  ggplot(Gender, aes(x = as.factor(ID), y = Percentage * 100, fill = Stats)) +
  geom_bar(aes(x = as.factor(ID), y = Percentage * 100, fill = Stats), stat = "identity", alpha = 1) +
  geom_segment(data = grid_data_Gender, aes(x = end, y = 10, xend = start, yend = 10), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Gender, aes(x = end, y = 20, xend = start, yend = 20), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Gender, aes(x = end, y = 30, xend = start, yend = 30), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Gender, aes(x = end, y = 40, xend = start, yend = 40), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  geom_segment(data = grid_data_Gender, aes(x = end, y = 50, xend = start, yend = 50), colour = "#737373", alpha = 1, linewidth = .4, linetype = 4, inherit.aes = FALSE ) +
  annotate("text", x = rep(max(Gender$ID), 5), y = c(10, 20, 30, 40, 50), label = c("10%", "20%", "30%", "40%", "50%"), family = "Cormorant", size = 4, fontface = "bold", color = "#737373", angle = 0, hjust = 1) +
  geom_bar(aes(x = as.factor(ID), y = Percentage * 100, fill = Region), stat = "identity", alpha = .5) +
  scale_fill_manual(values = c("#e5d8bd", "#fdbf6f"), na.translate = FALSE) +
  ylim(-90, 70) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(-2, -2, -2, -7), "cm"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank()) +
  coord_polar() +
  geom_text(data = label_data_Gender, aes(x = ID, y = Percentage * 100 + 6, label = Variable, hjust = hjust), family = "Cormorant", size = 5, color = "#737373", fontface = "bold", angle = label_data_Gender$angle, inherit.aes = FALSE ) +
  geom_segment(data = base_data_Gender, aes(x = start, y = -5, xend = end, yend = -5), colour = "#000000", alpha = 1, size = .6, inherit.aes = FALSE)


# Overlays plots ~
Gender_PlotUp <- ggdraw() +
                 draw_plot(Gender_Plot) + 
                 draw_plot(Icons, x = .27, y = .31, width = .4, height = .4)


Final_Plot <- plot_grid(Institution_Plot, Gender_PlotUp, ncol = 2, rel_widths = c(.5, .5))


# Saves Institution plot ~
ggsave(Final_Plot, file = "Layka.pdf", limitsize = FALSE,
       device = cairo_pdf, scale = 1, width = 30, height = 14, dpi = 600)


# Creates panel ~
Map <-
  ggplot() +
  geom_sf(data = fulldf_map, aes(fill = Percentage * 100), colour = "#f7fbff") +
  coord_sf(xlim = c(-75.75, -33),
           ylim = c(-35, 6.5),
           expand = FALSE) +
  scale_y_continuous(breaks = c(0, -10, -20, -30)) + 
  geom_star(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region == "SBBE24"),
            aes(x = Longitude, y = Latitude), size = 4, starshape = 15, starstroke = .3, fill = "#FF7B00", colour = "#000000") +
  geom_star(data = subset(fulldf_map, Division == "Per Region" & Region == "Abroad"),
            aes(x = Longitude, y = Latitude, fill = Percentage), size = 30, starshape = 8, starstroke = .3, colour = "#f7fbff") +
  geom_label(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region != "SBBE24"),
             aes(x = Longitude, y = Latitude, label = Region),
             size = 4.5, label.size = .1, family = "Barlow", fill = "#f7fbff", colour = "#000000") +
  geom_label(data = subset(fulldf_map,  Division == "Per Region" & Stats == "Members" & Region == "Abroad"),
             aes(x = Longitude, y = Latitude, label = Region),
             size = 4.5, label.size = .1, family = "Barlow", fill = "#f7fbff", colour = "#000000") +
  geom_label_repel(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region == "SBBE24"),
                   aes(x = Longitude, y = Latitude, label = Region), point.padding = 0,
                   nudge_x = 3.6, nudge_y = -1, segment.size = 0, segment.color = NA, 
                   size = 4.5, label.size = .1, family = "Barlow", fill = "#FF7B00", colour = "#000000") +
  scale_fill_continuous(low = "#d6d6d6", high = "#004529",
                        breaks = c(10, 20, 30, 40, 50),
                        labels = c("10%", "20%", "30%", "40%", "50%"),
                        limits = c(0, 60)) +
  facet_wrap(Division ~ ., labeller = labeller(Division = xlabel_PT)) +
  annotation_scale(data = subset(fulldf_map, Division == "Por Região" & Stats == "Members"),
                   text_family = "cormorant", location = "bl", line_width = 1.25, text_cex = 1, style = "ticks",
                   pad_x = unit(.2, "in"), pad_y = unit(.2, "in")) +
  annotation_north_arrow(data = subset(fulldf_map, Division == "Por Região" & Stats == "Members"), 
                         location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                         pad_x = unit(.2, "in"), pad_y = unit(.3, "in")) +
  theme(legend.position = "right",
        legend.margin = margin(t = 0, b = 0, r = 0, l = 30),
        legend.box.margin = margin(t = 0, b = 20, r = 0, l = 0),
        panel.background = element_rect(fill = "#f7fbff"),
        panel.border = element_rect(colour = "#000000", linewidth = .25, fill = NA),
        panel.grid.major = element_line(color = "#d9d9d9", linetype = "dashed", linewidth = .00005),
        plot.margin =  margin(t = 0, b = 0, r = .5, l = .5, unit = "cm"),
        axis.text = element_text(family = "Barlow", color = "#000000", size = 13, face = "bold"),
        axis.title = element_blank(),
        axis.ticks = element_line(color = "#000000", linewidth = .25),
        strip.text = element_text(family = "Barlow", colour = "#000000", size = 22, face = "bold"),
        strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = .25)) +
  guides(fill = guide_colourbar(title = "", label.theme = element_text(family = "Barlow", size = 14, face = "bold"),
                                barwidth = 1.5, barheight = 14, order = 1, frame.linetype = 1, frame.colour = "#000000",
                                ticks.colour = "#f7fbff", direction = "vertical", reverse = FALSE, even.steps = TRUE,
                                draw.ulim = TRUE, draw.llim = TRUE))



ggsave(Panel, file = "SBBE_SBBE24--DescriptiveMaps_EN.jpeg", limitsize = FALSE,
       scale = 1, width = 15, height = 12, dpi = 600)

#
##
### The END ~~~~~