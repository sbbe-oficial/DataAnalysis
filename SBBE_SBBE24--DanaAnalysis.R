### The BEGINNING ~~~~~
##
# ~ SBBE--DataAnalysis by George Pacheco.


# Cleans environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads packages ~
pacman::p_load(tidyverse, ggstar, ggrepel, readxl, writexl, cowplot, ggpubr, lemon, reshape2, writexl, stringr, lubridate, geobr, ggspatial, showtext, png, extrafont, sf)


# Loads extra fonts ~
loadfonts()
font_add_google("Cormorant Garamond", "cormorant")
showtext_auto()


# Loads data ~
fulldf <- read_excel("./Lists/ListaParticipante_22-08-2024_09-31-38.xlsx") %>%
          rename(Sub_Area = `Sub Área`) %>%
          filter(Inscrição == "Aprovado")
fulldf_descontos <- read_excel("./Lists/planilha-descontos_sbbe_2024-2version.xlsx")


# Loads SBBE logo ~
SBBElogo <- readPNG("./Logos/SBBElogo.png")
SBBE24logo <- readPNG("./Logos/SBBE24logo.png")


# Renames column ~
fulldf_descontos <- fulldf_descontos %>%
          rename(Etnicidade = `Você se identifica como:`)


# Corrects Institution ~
levels(fulldf$Instituição <- sub("IB USP - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("INMA - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- sub(" - FFCLRP", "", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UFPR - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UFPR- ", "", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UNIVERSIDADE FEDERAL DO PARANA - Curitiba", "UNIVERSIDADE FEDERAL DO PARANÁ", fulldf$Instituição))
levels(fulldf$Instituição <- sub("Parana", "Paraná", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UFPR", "Universidade Federal do Paraná", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UFSC", "Universidade Federal de Santa Catarina", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UFMS -", "", fulldf$Instituição))
levels(fulldf$Instituição <- sub("USP", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UCLA", "University of California — Los Angeles \\(EUA\\)", fulldf$Instituição))
levels(fulldf$Instituição <- sub("MZUSP - Museu de Zoologia da Universidade de São Paulo", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- sub("Universidade de São Paulo - UNIVERSIDADE DE SÃO PAULO - FFCLRP", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- sub("Universidade de São Paulo - UNIVERSIDADE DE SAO PAULO - ICB", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- sub("MZUniversidade de São Paulo - Museu de Zoologia da Universidade de São Paulo", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- sub("Unesp - Rio Claro", "Universidade Estadual Paulista", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Universidade de São Paulo \\(Universidade de São Paulo\\) - FFCLRP", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" Universidade", "Universidade", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Inst. Biologia - Unicamp", "Universidade Estadual de Campinas", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Universidade de São Paulo - Universidade de São Paulo - Ffclrp", "Universidade de São Paulo", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("HRHDS-", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFSJ -", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFJ -", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UNESP\\/IBILCE", "Universidade Estadual Paulista", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("219UFRJ - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UEM", "Universidade Estadual de Maringá", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("ICB - Instituto de Ciências Biológicas da UFMG", "Universidade Federal de Minas Gerais", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFBA", "Universidade Federal da Bahia", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("DEL REI", "DEL-REI", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" -Universidade Federal do Paraná", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" e UFUUniversidade Federal de Uberlândia", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFTM - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("University of Oslo", "University of Oslo (Noruega)", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Instituto de Biologia Subtropical", "Instituto de Biología Subtropical (Argentina)", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("MPEG Museu Paraense Emilio Goeldi", "Museu Paraense Emílio Goeldi", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Universidade de São Paulo - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" \\(Universidade de São Paulo\\)", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFRGS", "Universidade Federal do Rio Grande do Sul", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Universidade Federal de Santa Catarina-", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("-Universidade Estadual de Maringá", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("- Universidade Estadual de Maringá", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("-Universidade Estadual de  Maringá", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Universidade Estadual de Maringá ", "Universidade Estadual de Maringá", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" -Universidade de Sao Paulo - IB", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFRN", "Universidade Federal do Rio Grande do Norte", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" - UNIVERSIDADE FEDERAL DE SANTA CATARINA,", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFPB - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("  ", " ", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" - Universidade Federal do Rio Grande do Sul", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFPB -Universidade Federal da Paraíba\\/UFPB", "Universidade Federal da Paraíba", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("\\.", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("PUCRS - PONTIFÍCIA UNIVERSIDADE CATÓLICA DO RIO GRANDE DO SUL/QUEEN MARY UNIVERSITY OF LONDON", "PONTIFÍCIA UNIVERSIDADE CATÓLICA DO RIO GRANDE DO SUL", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("FUNDAÇÃO UNIVERSIDADE FEDERAL DO ABC", "UNIVERSIDADE FEDERAL DO ABC", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("FURG - UNIVERSIDADE FEDERAL RIO GRANDE", "UNIVERSIDADE FEDERAL DO RIO GRANDE", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("PUCRS", "Pontifícia Universidade Católica do Rio Grande do Sul", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("MPEG - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFABC -", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" - Pontifícia Unioversidade Católica do Rio Grande do Sul", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("- Departamento de Bioquímica", "", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UNIVERSIDADE FEDERAL DO PARAN", "UNIVERSIDADE FEDERAL DO PARANÁ", fulldf$Instituição))
levels(fulldf$Instituição <- sub("UNIVERSIDADE FEDERAL DO PARANÁÁ", "UNIVERSIDADE FEDERAL DO PARANÁ", fulldf$Instituição))
levels(fulldf$Instituição <- sub(" - Escócia", " \\(Escócia\\)", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UNIVASF - FUNDAÇÃO UNIVERSIDADE FEDERAL DO VALE DO SÃO FRANCISCO \\(campus ciências agrarias\\)", "UNIVERSIDADE FEDERAL DO VALE DO SÃO FRANCISCO", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("INPA - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("IAM/FIOCRUZ-PE", "Instituto Aggeu Magalhães — Fiocruz Pernambuco", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("IMBIV-CONICET-UNC", "Instituto Multidisciplinario de Biología Vegetal \\(Argentina\\)", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("UFRJ - ", "", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("IBB - UNESP Botucatu", "Universidade Estadual Paulista", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("University of West Florida", "University of West Florida \\(EUA\\)", fulldf$Instituição))
levels(fulldf$Instituição <- gsub("Rice University", "Rice University \\(EUA\\)", fulldf$Instituição))
levels(fulldf$Instituição <- gsub(" - UdelaR", "", fulldf$Instituição))


# Corrects UF ~
levels(fulldf$UF <- gsub("Rio Grande do Sul", "RS", fulldf$UF))



# Corrects UF ~
fulldf$UF <- ifelse(fulldf$UF %in% c(NA, "Aberdeen City", "california", "Florida", "Misiones", "Texas"), "Estrangeiro", fulldf$UF)


# Corrects Gender ~
fulldf$Gênero <- ifelse(fulldf$Gênero %in% c("M"), "Masculino",
                 ifelse(fulldf$Gênero %in% c("F"), "Feminino",
                 ifelse(fulldf$Gênero %in% c("O"), "Outro", "Error")))


# Corrects Category ~
levels(fulldf$Categoria <- sub("CBBE", "SBBE24", fulldf$Categoria))


MembersSBBE <- c("Estudante de Graduação + Membro fundador da SBBE",
                 "Membro fundador da SBBE - Categoria Graduação - Sem inscrição no SBBE24",
                 "Membro fundador da SBBE - Categoria Pós-Graduação e Pós-doutorado - Sem inscrição no SBBE24",
                 "Membro fundador da SBBE - Categoria Profissional - Sem inscrição no SBBE24",
                 "Mestrando, Doutorando e Pós doutorando + Membro fundador da SBBE",
                 "Profissional + Membro fundador da SBBE")


AttendeesSBBE24 <- c("Membro fundador da SBBE - Categoria Graduação - Sem inscrição no SBBE24",
                     "Membro fundador da SBBE - Categoria Pós-Graduação e Pós-doutorado - Sem inscrição no SBBE24",
                     "Membro fundador da SBBE - Categoria Profissional - Sem inscrição no SBBE24")


# Gets SBBE Founders ~
SBBEFounders <- fulldf %>%
                filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
                select(Nome, "E-mail")
write_xlsx(SBBEFounders, "./Lists/SBBEMembers.xlsx")


# Gets SBBE24 Attendees ~
SBBE24Attendees <- fulldf %>%
                   filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
                   select(Nome, "E-mail")
write_xlsx(SBBE24Attendees, "./Lists/SBBE24Attendees.xlsx")


# Corrects certain patterns ~
fulldf_descontos$Gênero <- sub("Não binário", "Não-binário", fulldf_descontos$Gênero)
fulldf_descontos$Gênero <- sub("Agênero/Não-binario", "Não-binário", fulldf_descontos$Gênero)
levels(fulldf_descontos$`Quanto à sua nacionalidade, você é:` <- gsub("nato", "Nato", fulldf_descontos$`Quanto à sua nacionalidade, você é:`))
levels(fulldf_descontos$`Quanto à sua atual localização geográfica, marque a opção que melhor a define:` <- gsub("Região ", "", fulldf_descontos$`Quanto à sua atual localização geográfica, marque a opção que melhor a define:`))


# Expands fulldf by creating Region ~
fulldf$Region <- ifelse(fulldf$UF %in% c("AC", "AM", "AP", "PA", "RR", "RO", "TO"), "Norte",
                 ifelse(fulldf$UF %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"), "Nordeste",
                 ifelse(fulldf$UF %in% c("DF", "GO", "MT", "MS"), "Centro-Oeste",
                 ifelse(fulldf$UF %in% c("MG", "ES", "RJ", "SP"), "Sudeste",
                 ifelse(fulldf$UF %in% c("PR", "RS", "SC"), "Sul",
                 ifelse(fulldf$UF %in% c("Estrangeiro"), "Estrangeiro", "Error"))))))


# Expands fulldf by creating Stage ~
fulldf$Stage <- ifelse(str_detect(fulldf$Categoria, "Profissional"), "Profissional",
                ifelse(str_detect(fulldf$Categoria, "Mestrando"), "Mestrado, Doutorado & Pós-doutorado",
                ifelse(str_detect(fulldf$Categoria, "Graduação"), "Graduação", "Error")))


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
fulldf$Instituição <- sapply(fulldf$Instituição, capitalize_words)
fulldf_descontos$Gênero <- sapply(fulldf_descontos$Gênero, capitalize_words)
fulldf_descontos$Etnicidade <- sapply(fulldf_descontos$Etnicidade, capitalize_words)


# Defines a common set of levels and ordering for Variable ~
variable_levels <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                     "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                     "GO", "MT", "MS", "DF",
                     "ES", "MG", "RJ", "SP",
                     "PR", "RS", "SC",
                     "Graduação",
                     "Mestrado, Doutorado & Pós-doutorado",
                     "Profissional",
                     "Dias até o SBBE24",
                     "Seguidores no X",
                     "Seguidores no Instagram",
                     "Instituições representadas no SBBE24",
                     "Instituições representadas na SBBE",
                     "Inscritos no SBBE24",
                     "Afiliados à SBBE",
                     "Prefiro Não Declarar",
                     "Não-binário",
                     "Homem Trans",
                     "Homem Cis",
                     "Mulher Cis",
                     "Amarelo",
                     "Pardo",
                     "Branco",
                     "Preto",
                     "University of California — Los Angeles (EUA)",
                     "Rice University (EUA)",
                     "University of West Florida (EUA)",
                     "Universidade de Aberdeen (Escócia)",
                     "University of Oslo (Noruega)",
                     "Instituto de Biología Subtropical (Argentina)",
                     "Instituto Multidisciplinario de Biología Vegetal (Argentina)",
                     "Universidade Federal do Rio Grande",
                     "Universidade Federal de Pelotas",
                     "Universidade Federal do Rio Grande do Sul",
                     "Pontifícia Universidade Católica do Rio Grande do Sul",
                     "Universidade Federal da Integração Latino-Americana",
                     "Universidade Estadual de Maringá",
                     "Centro Universitário Claretiano",
                     "Universidade Tecnológica Federal do Paraná",
                     "Pontifícia Universidade Católica do Paraná",
                     "Universidade Federal do Paraná",
                     "Hospital Regional Hans Dieter Schmidt",
                     "Universidade Federal de Santa Catarina",
                     "Universidade do Vale do Itajaí",
                     "Universidade do Oeste Paulista",
                     "Universidade Federal de São Carlos",
                     "Universidade Federal do ABC",
                     "Universidade Federal de São Paulo",
                     "Universidade de São Paulo",
                     "Universidade Estadual Paulista Júlio de Mesquita Filho",
                     "Universidade Estadual de Campinas",
                     "Universidade Estadual Paulista",
                     "Universidade Estadual do Norte Fluminense Darcy Ribeiro",
                     "Universidade Federal do Rio de Janeiro",
                     "Universidade do Estado do Rio de Janeiro",
                     "Universidade Federal do Espírito Santo",
                     "Instituto Nacional da Mata Atlântica",
                     "Universidade Federal de Uberlândia", 
                     "Universidade Federal do Triângulo Mineiro",
                     "Universidade Federal de Juiz de Fora",
                     "Universidade do Estado de Minas Gerais",
                     "Universidade Federal de Minas Gerais",
                     "Universidade Federal de Lavras",
                     "Universidade Federal de Viçosa", 
                     "Universidade Federal de São João Del-Rei",
                     "Universidade Federal de Jataí",
                     "Universidade de Brasília",
                     "Universidade Federal de Goiás",
                     "Universidade Federal de Mato Grosso do Sul",
                     "Universidade Federal da Bahia",
                     "Universidade Federal do Recôncavo da Bahia",
                     "Universidade Estadual do Sudoeste da Bahia",
                     "Universidade Federal de Sergipe",
                     "Universidade Federal de Alagoas",
                     "Universidade Federal do Vale do São Francisco",
                     "Instituto Aggeu Magalhães — Fiocruz Pernambuco",
                     "Universidade Federal da Paraíba",
                     "Universidade Federal do Rio Grande do Norte",
                     "Museu Paraense Emílio Goeldi",
                     "Instituto Nacional de Pesquisas da Amazônia",
                     "Outro",
                     "Masculino",
                     "Feminino",
                     "Estrangeiro",
                     "Brasileiro Nato",
                     "Sul", 
                     "Sudeste",
                     "Centro-Oeste",
                     "Nordeste",
                     "Norte",
                     #"Membro fundador da SBBE - Categoria Graduação - Sem inscrição no SBBE24",
                     #"Estudante de Graduação",
                     #"Estudante de Graduação + Membro fundador da SBBE",
                     #"Membro fundador da SBBE - Categoria Pós-Graduação e Pós-doutorado - Sem inscrição no SBBE24",
                     #"Mestrando, Doutorando e Pós doutorando",
                     #"Mestrando, Doutorando e Pós doutorando + Membro fundador da SBBE",
                     #"Membro fundador da SBBE - Categoria Profissional - Sem inscrição no SBBE24",
                     #"Profissional",
                     #"Profissional + Membro fundador da SBBE",
                     "Ciências Sociais Aplicadas",
                     "Ciências Exatas e da Terra",
                     "Ciências Humanas",
                     "Ciências da Saúde",
                     "Ciências Agrárias",
                     "Ciências Biológicas",
                     "Ciência da Computação",
                     "Geociências",
                     "Museologia",
                     "Psicologia",
                     "Odontologia",
                     "Agronomia",
                     "Morfologia",
                     "Fisiologia",
                     "Bioinformática",
                     "Genética",
                     "Bioquímica",
                     "Microbiologia",
                     "Ecologia",
                     "Botânica",
                     "Zoologia",
                     "Biologia Geral",
                     "Prefiro não declarar", 
                     "Não", 
                     "Sim")


# Gets current date ~
current_date <- Sys.Date()


# Checks missing Institution ~
left_out_patterns <- setdiff(fulldf$Instituição, variable_levels)


# Defines the event date ~# Defines Instituiçãothe event date ~
target_date <- as.Date("2024-11-20")


# Gets general numbers ~
fulldf_Descriptive <- fulldf %>%
  mutate(MembersSBBE_flag = ifelse(Categoria %in% MembersSBBE, TRUE, FALSE),
         AttendeesSBBE24_flag = ifelse(!Categoria %in% AttendeesSBBE24, TRUE, FALSE)) %>%
  summarise("Afiliados à SBBE" = n_distinct(ID[MembersSBBE_flag]),
            "Inscritos no SBBE24" = n_distinct(ID[AttendeesSBBE24_flag]),
            "Instituições representadas na SBBE" = n_distinct(Instituição[MembersSBBE_flag]), 
            "Instituições representadas no SBBE24" = n_distinct(Instituição[AttendeesSBBE24_flag]), 
            "Seguidores no Instagram" = 876, 
            "Seguidores no X" = 176,
            "Dias até o SBBE24" = as.numeric(difftime(target_date, current_date, units = "days")),
            Stats = "General") %>%
  gather(key = "Variable", value = "Percentage", -Stats) %>%
  mutate(n = 0) %>%
  relocate(n, .before = Percentage) %>%
  relocate(Stats, .after = Percentage) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets SocialMedia ~
fulldf_SocialMedia <- fulldf_Descriptive %>% 
                      filter(Variable %in% c("Seguidores no Instagram", "Seguidores no X")) %>%
                      mutate(Stats = "SocialMedia") %>%
                      mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets GeneralNumber ~
fulldf_GeneralNumbers <- fulldf_Descriptive %>% 
                         filter(!Variable %in% c("Seguidores no Instagram", "Seguidores no X")) %>%
                         mutate(Stats = "GeneralNumbers") %>%
                         mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets percentage for Institution ~
fulldf_StageAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Stage) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Stage) %>%
  mutate(Stats = "StageAttendees") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets percentage for Institution ~
fulldf_StageMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(Stage) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Stage) %>%
  mutate(Stats = "StageMembers") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Sets all Brazilian states ~ 
AllBRLStates <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                  "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                  "GO", "MT", "MS", "DF",
                  "ES", "MG", "RJ", "SP",
                  "PR", "RS", "SC", "Estrangeiro")


# Sets all Brazilian states ~ 
AllBRLRegions <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul", "Estrangeiro")


# Create a data frame with counts and proportions per institution ~
fulldf_StateAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(UF) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = UF) %>%
  complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "StateAttendees")

# Create a data frame with counts and proportions per institution ~
fulldf_StateMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(UF) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = UF) %>%
  complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "StateMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Region) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Region) %>%
  complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "RegionAttendees")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(Region) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Region) %>%
  complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "RegionMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_StateAttendeesPerc_Map <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  filter(!is.na(UF)) %>%
  count(UF) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = UF) %>%
  complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "StateAttendees")


# Create a data frame with counts and proportions per institution ~
fulldf_StateMembersPerc_Map <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  filter(!is.na(UF)) %>%
  count(UF) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = UF) %>%
  complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "StateMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionAttendeesPerc_Map <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  filter(!is.na(UF)) %>%
  count(Region) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Region) %>%
  complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "RegionAttendees")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionMembersPerc_Map <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  filter(!is.na(UF)) %>%
  count(Region) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Region) %>%
  complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "RegionMembers")


# Gets percentage for Gender ~
fulldf_GenderAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Gênero) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Gênero) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "GenderAttendees")


# Gets percentage for Gender ~
fulldf_GenderMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(Gênero) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Gênero) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "GenderMembers")


# Gets percentage for Institution ~
fulldf_InstitutionMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(Instituição) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Instituição) %>%
  mutate(Stats = "InstitutionMembers") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets percentage for Institution ~
fulldf_InstitutionAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Instituição) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Instituição) %>%
  mutate(Stats = "InstitutionAttendees") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Create a data frame with counts and proportions per institution ~
fulldf_AreaMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(Área) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Área) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "AreaMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_AreaAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Área) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Área) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "AreaAttendees")


# Create a data frame with counts and proportions per institution ~
fulldf_SubAreaMembersPerc <- fulldf %>%
  filter(str_detect(Categoria, "Membro fundador da SBBE")) %>%
  count(Sub_Area) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Sub_Area) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "SubAreaMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_SubAreaAttendeesPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Sub_Area) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Sub_Area) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "SubAreaAttendees")


# Create a data frame with counts and proportions per institution ~
fulldf_CategoryPerc <- fulldf %>%
  filter(!str_detect(Categoria, "Sem inscrição no SBBE24")) %>%
  count(Categoria) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Categoria) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "Category")


# Gets percentage for Gender Support ~
fulldf_descontos_GenderPerc <- fulldf_descontos %>%
  count(Gênero) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Gênero) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "GenderSupport")


# Gets percentage for Ethnicity
fulldf_descontos_EthnicityPerc <- fulldf_descontos %>%
  count(Etnicidade) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = Etnicidade) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "Ethnicity")


# Gets percentage for Ethnicity
fulldf_descontos_DiversityPerc <- fulldf_descontos %>%
  count(.[[5]]) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = ".[[5]]") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "Diversity")


# Gets percentage for Ethnicity
fulldf_descontos_PCDPerc <- fulldf_descontos %>%
  count(PCD) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = PCD) %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "PCD")


# Gets percentage for Ethnicity
fulldf_descontos_NationalityPerc <- fulldf_descontos %>%
  count(.[[9]]) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = ".[[9]]") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "Nationality")


# Gets percentage for Ethnicity
fulldf_descontos_RegionSupportPerc <- fulldf_descontos %>%
  count(.[[10]]) %>%
  mutate(Percentage = n / sum(n)) %>%
  rename(Variable = ".[[10]]") %>%
  mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
  mutate(Stats = "RegionSupport")


# Combine the data frames ~  
fulldfUp <- bind_rows(fulldf_GeneralNumbers,
                      fulldf_SocialMedia,
                      fulldf_StageMembersPerc,
                      fulldf_StageAttendeesPerc,
                      #fulldf_RegionMembersPerc,
                      #fulldf_RegionAttendeesPerc,
                      fulldf_GenderMembersPerc,
                      fulldf_GenderAttendeesPerc,
                      fulldf_InstitutionMembersPerc,
                      fulldf_InstitutionAttendeesPerc,
                      fulldf_AreaMembersPerc,
                      fulldf_AreaAttendeesPerc,
                      fulldf_SubAreaMembersPerc,
                      fulldf_SubAreaAttendeesPerc)
                      #fulldf_CategoryPerc,
                      #fulldf_descontos_EthnicityPerc,
                      #fulldf_descontos_GenderPerc, 
                      #fulldf_descontos_DiversityPerc,
                      #fulldf_descontos_PCDPerc,
                      #fulldf_descontos_NationalityPerc,
                      #fulldf_descontos_RegionSupportPerc)


# Expands fulldfUp by creating BarFill ~
fulldfUp$BarFill <- ifelse(str_detect(fulldfUp$Stats, "Members"), "#006837",
                    ifelse(str_detect(fulldfUp$Stats, "Attendees"), "#41ab5d", "#fbb4ae"))


# Reorders Population ~
fulldfUp$Stats <- factor(fulldfUp$Stats, ordered = T,
                            levels = c("GeneralNumbers",
                                       "SocialMedia",
                                       "StageMembers",
                                       "StageAttendees",
                                       "RegionMembers",
                                       "RegionAttendees",
                                       "GenderMembers",
                                       "GenderAttendees",
                                       "InstitutionMembers",
                                       "InstitutionAttendees",
                                       "AreaMembers",
                                       "AreaAttendees",
                                       "SubAreaMembers",
                                       "SubAreaAttendees",
                                       "GenderSupport",
                                       "Ethnicity",
                                       "Diversity",
                                       "PCD",
                                       "Nationality",
                                       "RegionSupport"))

# Sets custom x-axis labels ~
ylabel <- c("GeneralNumbers" = "Números Gerais",
            "SocialMedia" = "Mídias Sociais",
            "StageMembers" = "Percentagem de Afiliados por Estágio Acadêmico",
            "StageAttendees" = "Percentagem de Inscritos por Estágio Acadêmico",
            "RegionMembers" = "Percentagem de Afiliados por Região de Atuação",
            "RegionAttendees" = "Percentagem de Inscritos por Região de Atuação",
            "GenderMembers" = "Percentagem de Afiliados por Gênero",
            "GenderAttendees" = "Percentagem de Inscritos por Gênero",
            "InstitutionMembers" = "Percentagem de Afiliados por Instituição",
            "InstitutionAttendees" = "Percentagem de Inscritos por Instituição",
            "AreaMembers" = "Percentagem de Afiliados por Área",
            "AreaAttendees" = "Percentagem de Inscritos por Área",
            "SubAreaMembers" = "Percentagem de Afiliados por Sub-Área",
            "SubAreaAttendees" = "Percentagem de Inscritos por Sub-Área",
            #"Category" = "Percentagem de Inscritos e/ou Membros por Categoria",
            "GenderSupport" = "Percentagem de Solicitantes de Apoio por Gênero",
            "Ethnicity"= "Percentagem de Solicitantes de Apoio por Etnicidade", 
            "Diversity" = "Percentagem de Solicitantes da Comunidade LGBTQIAP+",
            "PCD" = "Percentagem de Solicitantes com Deficiência",
            "Nationality" = "Percentagem de Solicitantes Brasileiros",
            "RegionSupport" = "Percentagem de Solicitantes por Região de Atuação")


# Custom y-axis breaks ~
breaks_fun <- function(y){
  caseVal <- max(y)
  if (caseVal < 1){
    seq(.1, .9, by = .1)}
  else if (caseVal > 800){
    seq(50, 1000, by = 100)}
  else { 
    seq(50, 250, by = 50)}}


# Custom y-axis labels ~
plot_index_labels <- 0
labels_fun <- function(z) {
  plot_index_labels <<- plot_index_labels + 1L
  switch(plot_index_labels,
         scales::label_number(accuracy = 1)(z),
         scales::label_number(accuracy = 1)(z),
         #scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         #scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z),
         scales::label_percent(accuracy = 1, scale = 1 * 100, big.mark = "")(z))}


# Custom y-axis limits ~
limits_fun <- function(x){
  limitVal <- max(x)
  if (limitVal > 500){
    c(0, 1000)}
  else if (limitVal > 200){
    c(0, 240)}
  else if (limitVal < .2){
    c(0, .22)}
  else if (limitVal < .3){
    c(0, .32)}
  else if (limitVal < .4){
    c(0, .42)}
  else if (limitVal < .5){
    c(0, .52)}
  else if (limitVal < .6){
    c(0, .62)}
  else { 
    c(0, .9975)}}


# Gets current date ~
current_date <- format(Sys.Date())


# Formats date ~
current_date_PT <- paste0(day(current_date), " de ", 
                          month(current_date, label = TRUE, abbr = FALSE, locale = "pt_BR"), " de ", 
                          year(current_date))


# Gets panel ~
Panel <-
ggplot() +
  geom_bar(data = fulldfUp, aes(x = as.factor(Variable), y = Percentage, fill = BarFill),
           colour = "#000000", linewidth = .3, stat = "identity", position = "dodge") +
  geom_text(data = subset(fulldfUp, Stats == "GeneralNumbers" | Stats == "SocialMedia"), aes(x = as.factor(Variable), y = Percentage, label = Percentage),
            position = position_dodge(width = .9), family = "Optima", size = 4, fontface = "bold", hjust = -.5) +
  labs(title = "Sociedade Brasileira de Biologia Evolutiva (SBBE)",
       subtitle = "I Congresso Brasileiro de Biologia Evolutiva (SBBE24)",
       caption = paste0("Data: ", current_date_PT, ".")) +
  scale_fill_identity() +
  facet_wrap(Stats ~ ., scales = "free", ncol = 2, labeller = labeller(Stats = ylabel)) +
  scale_y_continuous("Percentagem",
                     breaks = breaks_fun,
                     labels = labels_fun,
                     limits = limits_fun,
                     expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major.x = element_line(color = "#d9d9d9", linetype = "dashed", linewidth = .05),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing.y = unit(1, "cm"),
        plot.title = element_text(family = "cormorant", size = 42, colour = "#006837", face = "bold", hjust = .5, margin = margin(t = 50, r = 0, b = 15, l = 0)),
        plot.subtitle = element_text(family = "cormorant", size = 42, colour = "#41ab5d", face = "bold", hjust = .5, margin = margin(t = 0, r = 0, b = 75, l = 0)),
        plot.caption = element_text(family = "cormorant", size = 20, face = "bold", hjust = -.175, margin = margin(t = 20, r = 0, b = 10, l = 0)),
        axis.title = element_blank(),
        axis.text.x = element_text(family = "cormorant", color = "#000000", size = 13, face = "bold"),
        axis.text.y = element_text(family = "cormorant", color = "#000000", size = 13, face = "bold"),
        axis.ticks = element_line(colour = "#000000", linewidth = .5),
        strip.text = element_text(colour = "#000000", size = 20, face = "bold", family = "cormorant"),
        strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = .3),
        axis.line = element_line(colour = "#000000", linewidth = .3),
        legend.position = "none") +
  coord_flip()


# Adds SBBElogo ~
PanelUp <- ggdraw(Panel) +
           draw_image(SBBElogo, scale = .06, x = .3005, y = .978, hjust = .5, vjust = .5) +
           draw_image(SBBE24logo, scale = .08, x = .84, y = .978, hjust = .5, vjust = .5)


# Saves panel ~
ggsave(PanelUp, file = "SBBE_SBBE24--DescriptivePanel.pdf", limitsize = FALSE,
       device = cairo_pdf, scale = 1, width = 32, height = 60, dpi = 600)


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
                                       Longitude = c(-58, -41.25, -53.15, -44.85, -51.2, -49.271111, -65, -48.62),
                                       Latitude = c(-3.5, -8, -15.5, -20, -27.5, -25.429722, -25, -21.9))


# Merges data frame to perform the change ~
BRL_Regions <- left_join(BRL_Regions, BRL_Regions_Centroids_df, by = "Region")


# Reduces data ~ 
BRL_Regions <- BRL_Regions %>%
  select(-code_region)
BRL_States <- BRL_States %>%
  select(-c(code_state, code_region, name_state))


# Renames columns ~
BRL_States <- BRL_States %>%
  rename(Variable = abbrev_state)


# Joins data frames ~
fulldf_RegionAttendeesPerc <- fulldf_RegionAttendeesPerc %>%
  rename(name_region = Variable)
fulldf_RegionMembersPerc <- fulldf_RegionMembersPerc %>%
  rename(name_region = Variable)


# Corrects the Estrangeiro -> Abroad ~
levels(fulldf_StateAttendeesPerc$Variable <- gsub("Estrangeiro", "Abroad", fulldf_StateAttendeesPerc$Variable))
levels(fulldf_StateMembersPerc$Variable <- gsub("Estrangeiro", "Abroad", fulldf_StateMembersPerc$Variable))
levels(fulldf_RegionAttendeesPerc$name_region <- gsub("Estrangeiro", "Abroad", fulldf_RegionAttendeesPerc$name_region))
levels(fulldf_RegionMembersPerc$name_region <- gsub("Estrangeiro", "Abroad", fulldf_RegionMembersPerc$name_region))


# Merges data frames individually ~
RegionMembers_df <- BRL_States %>%
  inner_join(fulldf_RegionMembersPerc, by = "name_region") %>%
  mutate(Stats = "Members")  %>%
  mutate(Division = "Per Region")
RegionAttendees_df <- BRL_States %>%
  inner_join(fulldf_RegionAttendeesPerc, by = "name_region") %>%
  mutate(Stats = "Attendees") %>%
  mutate(Division = "Per Region")
StateMembers_df <- BRL_States %>%
  inner_join(fulldf_StateMembersPerc, by = "Variable") %>%
  mutate(Stats = "Members") %>%
  mutate(Division = "Per State")
StateAttendees_df <- BRL_States %>%
  inner_join(fulldf_StateAttendeesPerc, by = "Variable") %>%
  mutate(Stats = "Attendees")  %>%
  mutate(Division = "Per State")


# Combines data frames ~
combined_sfs <- bind_rows(StateAttendees_df, StateMembers_df, RegionAttendees_df, RegionMembers_df)


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
  select(-c(n, geom.BRL))


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
                           levels = c("Members",
                                      "Attendees"))


# Reorders Division ~
fulldf_map$Division <- factor(fulldf_map$Division, ordered = T,
                              levels = c("Per Region",
                                         "Per State"))


# Sets custom x-axis labels ~
ylabel <- c("Members" = "% of SBBE Members",
            "Attendees" = "% of SBBE24 Attendees")


# Creates panel ~
Panel <-
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
             size = 4.5, label.size = .1, family = "cormorant", fill = "#f7fbff", colour = "#000000") +
  geom_label(data = subset(fulldf_map,  Division == "Per Region" & Stats == "Members" & Region == "Abroad"),
             aes(x = Longitude, y = Latitude, label = Region),
             size = 4.5, label.size = .1, family = "cormorant", fill = "#f7fbff", colour = "#000000") +
  #geom_label(data = subset(fulldf_map,  Division == "Per State" & Stats == "Members" & Variable == "SP"),
  #           aes(x = Longitude.SP, y = Latitude.SP, label = Variable),
  #           size = 4.5, label.size = .1, family = "cormorant", fill = "#f7fbff", colour = "#000000") +
  geom_label_repel(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region == "SBBE24"),
                   aes(x = Longitude, y = Latitude, label = Region), point.padding = 0,
                   nudge_x = 3.6, nudge_y = -1, segment.size = 0, segment.color = NA, 
                   size = 4.5, label.size = .1, family = "cormorant", fill = "#FF7B00", colour = "#000000") +
  scale_fill_continuous(low = "#d6d6d6", high = "#004529",
                        breaks = c(10, 20, 30, 40),
                        labels = c("10%", "20%", "30%", "40%"),
                        limits = c(0, 50)) +
  facet_grid(Division ~ Stats, labeller = labeller(Stats = ylabel)) +
  annotation_scale(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members"),
                   text_family = "cormorant", location = "bl", line_width = 1.25, text_cex = 1, style = "ticks",
                   pad_x = unit(.2, "in"), pad_y = unit(.2, "in")) +
  annotation_north_arrow(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members"), 
                         location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                         pad_x = unit(.2, "in"), pad_y = unit(.3, "in")) +
  theme(legend.position = "right",
        legend.margin = margin(t = 0, b = 0, r = 0, l = 30),
        legend.box.margin = margin(t = 0, b = 20, r = 0, l = 0),
        panel.background = element_rect(fill = "#f7fbff"),
        panel.border = element_rect(colour = "#000000", linewidth = .25, fill = NA),
        panel.grid.major = element_line(color = "#d9d9d9", linetype = "dashed", linewidth = .00005),
        plot.margin = margin(0, 0, 0, 0),
        axis.text = element_text(family = "cormorant", color = "black", size = 13, face = "bold"),
        axis.title = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = .25),
        strip.text = element_text(family = "cormorant", colour = "#000000", size = 22, face = "bold"),
        strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = .25)) +
  guides(fill = guide_colourbar(title = "", label.theme = element_text(family = "cormorant", size = 14, face = "bold"),
                                barwidth = 1.5, barheight = 14, order = 1, frame.linetype = 1, frame.colour = "#000000",
                                ticks.colour = "#f7fbff", direction = "vertical", reverse = FALSE, even.steps = TRUE,
                                draw.ulim = TRUE, draw.llim = TRUE))


# Saves panel ~
ggsave(Panel, file = "SBBE_SBBE24--DescriptiveMaps.pdf", limitsize = FALSE,
       device = cairo_pdf, scale = 1, width = 15, height = 12, dpi = 600)


#
##
### The END ~~~~~