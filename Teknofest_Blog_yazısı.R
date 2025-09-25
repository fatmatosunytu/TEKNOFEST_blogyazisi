install.packages(c("tidyverse", "readxl", "ggwordcloud", "igraph", "ggraph"))
library(tidyverse)
library(readxl)
library(ggwordcloud)
library(igraph)
library(ggraph)


# Çalışma klasörünü ayarla
setwd("C:/Users/User/Desktop")   # kendi yolunu yaz 

# Dergipark
df_dergi <- read_excel("dergi.xls")

# Scopus
df_scopus <- read_csv("export_ce51d640-0ea5-4a6c-a9b0-2d0c176ce149_2025-09-25T084325.98743652.csv")

# Tezler
df_tez <- read_excel("tezler.xlsx")



install.packages("janitor")
library(janitor)
df_dergi <- df_dergi %>% clean_names()
df_scopus <- df_scopus %>% clean_names()
df_tez <- df_tez %>% clean_names()



# Dergipark
df_dergi <- df_dergi %>%
  select(Title = baslik, 
         Year = yil, 
         Keywords = anahtar_kelime, 
         Abstract = ozet) %>%
  mutate(Source = "Dergipark")

# Scopus
df_scopus <- df_scopus %>%
  select(Title = title, 
         Year = year, 
         Keywords = author_keywords, 
         Abstract = abstract) %>%
  mutate(Source = "Scopus")

# Tezler
df_tez <- df_tez %>%
  select(Title = tez_adi, 
         Year = yil, 
         Keywords = anahtar_kelime, 
         Abstract = ozet) %>%
  mutate(Source = "Tez")

df_all <- bind_rows(df_dergi, df_scopus, df_tez)

################################################################################
df_all %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Yıllara Gore Yayın / Tez Sayısı", 
       x = "Yıl", y = "Sayı")
###############################################################################
keywords_df <- df_all %>%
  filter(!is.na(Keywords)) %>%
  separate_rows(Keywords, sep = ";|,") %>%
  mutate(Keywords = str_trim(Keywords)) %>%
  group_by(Keywords) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

keywords_df %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(Keywords, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "En sık Kullanılan Anahtar Kelimeler", 
       x = "Anahtar Kelime", y = "Sıklık")
################################################################################
set.seed(123)
keywords_df %>%
  ggplot(aes(label = Keywords, size = n, color = n)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  labs(title = "Anahtar Kelime Bulutu")
################################################################################
kw_pairs <- df_all %>%
  filter(!is.na(Keywords)) %>%
  separate_rows(Keywords, sep = ";|,") %>%
  mutate(Keywords = str_trim(Keywords)) %>%
  group_by(Title) %>%
  summarise(keywords = list(unique(Keywords))) %>%
  pull(keywords) %>%
  map(~ combn(.x, 2, simplify = FALSE)) %>%
  unlist(recursive = FALSE)

edges <- map_dfr(kw_pairs, ~ tibble(from = .x[1], to = .x[2]))
g <- graph_from_data_frame(edges)

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 5, color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Anahtar Kelime İlişki Ağı")


