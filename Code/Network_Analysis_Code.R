# This code produces plots and results relevant to the paper
# "Temporal Network Analysis as a Method to Explore Australian World War I Diaries"
# 
# Last updated: 31/05/2024


## Load Libraries ----

pacman::p_load(tidyverse, jsonlite, tidyjson, data.table, ggrepel, ragg, ggraph, readtext, stringi, ggExtra, igraph, ggpubr)


## Set Graph Theme ----

theme_set(theme_bw() + theme(axis.title.y = element_text(vjust = +3), axis.title.x = element_text(vjust = -0.75)))  # Set the theme for all graphs


## Set Working Directory ----

directory <- "Temporal Network Analysis/"  # Directory we are working in

setwd(directory)  # Set working directory


## Load JSON Files ----

network_data <- fromJSON(paste0(directory, "Data/network_df.json"))  # Load network data

entry_data <- fromJSON(paste0(directory, "Data/entry_data_sentiments.json"))  # Load entry data


## Convert JSON Files to Tibbles ----

network_tibble <- network_data %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble() %>%
  unnest(c(From, To, Date, Sim))

entry_tibble <- entry_data %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble() %>%
  unnest(c(`Document Name`, Node, Author, Date, Entry, Sentiment))


## Load Author Data ----

author_data <- read_csv(paste0(directory, "Data/Diarist_Metadata.csv"), show_col_types = FALSE)


## Clean Author Data ----

# Data Cleaning: Remove authors who only have out-of-order diaries

author_data_clean <- author_data %>%
  filter(`Last Name` != "alcock") %>%
  filter(`Last Name` != "job") %>%
  filter(`Last Name` != "lowe") %>%
  filter(`Last Name` != "holmes")

# Data Cleaning: Change Enlistment Date Format

author_data_clean <- author_data_clean %>%
  mutate(`Enlistment Date` = ymd(enlistyymmdd))

# Data Cleaning: Simplify Enlistment Rank

author_data_clean <- author_data_clean %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Captain Chaplain', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Captain', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Captain (Honorary Major)', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'COL', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Major', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == '2nd Lieutenant', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == '2nd Lieutenant (Honorary Captain)', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'CAPT (attached)', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Lieutenant', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Lieutenant-Colonel', 'Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'SIG SGT', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Sergeant', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Farrier Sergeant', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Acting Sergeant', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Acting Corporal', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Corporal', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Lance Corporal', 'Non-Commissioned Officer')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'PTE', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'PTE (AAMC)', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Sapper', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'SPR', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Driver', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Able Bodied Driver', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Motor Transport Driver', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'Gunner', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace(enlistmentRank, enlistmentRank == 'SIG', 'Other Ranks')) %>%
  mutate(enlistmentRank = replace_na(enlistmentRank, 'Unknown'))

# Data Cleaning: Simplify Fate

author_data_clean <- author_data_clean %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'RTA', 'Returned to Australia')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'KIA', 'Died in WWI')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'DOW', 'Died in WWI')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'EA', 'Effective Abroad')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'DIS', 'Discharged')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'Killed', 'Died in WWI')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'DOD', 'Died of Disease')) %>%
  mutate(fdwFate = replace(fdwFate, fdwFate == 'ND', 'Unknown')) %>%
  mutate(fdwFate = replace_na(fdwFate, 'Unknown'))

# Data Cleaning: Marital Status

author_data_clean <- author_data_clean %>%
  mutate(maritalStatus = replace_na(maritalStatus, 'Unknown')) 


## Create Histogram of Number of Entries over Time ----

plt1 <- ggplot(entry_tibble, aes(x = Date)) +
  geom_histogram(fill = "deepskyblue", color = "black", binwidth = 31) +
  scale_x_continuous(breaks=c(186, 372, 558, 744, 930, 1116, 1302, 1488, 1674, 1860, 2046, 2232),
                     labels = c("Jul 1914", "Jan 1915", "Jul 1915", "Jan 1916", "Jul 1916", "Jan 1917", "Jul 1917", "Jan 1918", "Jul 1918", "Jan 1919", "Jul 1919", "Jan 1920")) + 
  theme(axis.text.x=element_text(angle = -90, vjust = 0.25, hjust=1)) +
  labs(x = "Month", y = "Number of Entries per Month")

ragg::agg_png(paste0(directory, "Figures/Histogram_Entries_Over_Time.png"), height = 4, width = 6, units = "in", res = 300)

plt1

dev.off()


## Create Plot of Enlistment Age, Date, and Rank ----

plt2 <- author_data_clean %>%
  drop_na(ageEnlist, `Enlistment Date`) %>%
  ggplot(aes(x = `Enlistment Date`, y = ageEnlist, color = enlistmentRank)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_segment(aes(x = as.Date("1914-07-31"), xend = as.Date("1915-06-01"), y = 38, yend = 38), linetype = "dashed", color = "black", size = 0.8) +
  geom_segment(aes(x = as.Date("1915-06-01"), xend = as.Date("1918-05-01"), y = 45, yend = 45), linetype = "dashed", color = "black", size = 0.8) +
  labs(x = "Enlistment Date", y = "Age at Enlistment", color = "Enlistment Rank") +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(
    breaks = seq.Date(as.Date("1914-07-31"), as.Date("1918-05-01"), by = "6 month"),
    labels = ~ format(.x, "%b %Y")) +
  theme(axis.text.x=element_text(angle = -90, vjust = 0.25, hjust=1))

plt3 <- ggMarginal(plt2, 
                   type = "histogram", size = 3, fill = "deepskyblue", color = "black", 
                   xparams = list(bins = 45), yparams = list(binwidth = 1))

ragg::agg_png(paste0(directory, "Figures/Author_Enlistment_Plot.png"), height = 6, width = 8, units = "in", res = 300)

plt3

dev.off()


## Table Giving Breakdown of Ranks ----

table(author_data_clean$enlistmentRank)  # Table of counts

format(round(prop.table(table(author_data_clean$enlistmentRank))*100, 1), nsmall = 1)  # Table of percentages


## Table Giving Breakdown of Marital Status ----

table(author_data_clean$maritalStatus)  # Table of counts

format(round(prop.table(table(author_data_clean$maritalStatus))*100, 1), nsmall = 1)  # Table of percentages


## Table Giving Breakdown of Fate ----

table(author_data_clean$fdwFate)  # Table of counts

format(round(prop.table(table(author_data_clean$fdwFate))*100, 1), nsmall = 1)  # Table of percentages


## Graph Diagram Explaining Density and Transitivity (FIX) ----

set.seed(1234)

g1 <- graph(edges = c(1, 2, 2, 3, 3, 1, 4, 5, 5, 6, 6, 4), n = 6, directed = FALSE) 

plt4 <- ggraph(g1, layout = "fr") +
  geom_edge_arc(strength = 0) +
  geom_node_point(size = 5, fill = "cadetblue1", color = "black", shape = 21) +
  theme_graph(background = "white") +
  scale_edge_alpha(range = c(0, 0.5)) 

g2 <- graph(edges = c(1, 2, 2, 3, 3, 1, 3, 4, 4, 5, 5, 6, 6, 7, 7, 5, 5, 1, 2, 6, 6, 1, 4, 1, 4, 6, 3, 5, 7, 1, 2, 4, 2, 7), n = 7, directed = FALSE) 

plt5 <- ggraph(g2, layout = "kk") +
  geom_edge_arc(strength = 0) +
  geom_node_point(size = 5, fill = "cadetblue1", color = "black", shape = 21) +
  theme_graph(background = "white") +
  scale_edge_alpha(range = c(0, 0.5)) 

plt6 <- ggarrange(plt4, plt5,
                 labels = c("A", "B"),
                 ncol = 2, nrow = 1)

ragg::agg_png(paste0(directory, "Figures/Density_Transitivity_Diagram.png"), height = 4, width = 6, units = "in", res = 300)

plt6

dev.off()

transitivity(g1)

edge_density(g1)

transitivity(g2)

edge_density(g2)

## Histogram and Density Curve of Cosine Similarity Scores ----

plt7 <- ggplot(network_tibble, aes(x = Sim)) +
  geom_histogram(aes(y=..density..), fill = 'deepskyblue', color = 'black', bins = 60) +
  geom_density(color="red", size = 0.8) +  
  labs(x = 'Cosine Similarity Score', y = '')

ragg::agg_png(paste0(directory, "Figures/Histogram_Density_Curve_Similarity_Scores.png"), height = 4, width = 6, units = "in", res = 300)

plt7

dev.off()


## Similarity Thresholds for Various Percentages ----

network_tibble %>%
  summarise(P50 = quantile(Sim, p = 0.5),
            P55 = quantile(Sim, p = 0.55),
            P60 = quantile(Sim, p = 0.6),
            P65 = quantile(Sim, p = 0.65),
            P70 = quantile(Sim, p = 0.7),
            P75 = quantile(Sim, p = 0.75),
            P80 = quantile(Sim, p = 0.8),
            P85 = quantile(Sim, p = 0.85),
            P90 = quantile(Sim, p = 0.9),
            P95 = quantile(Sim, p = 0.95))


## Load Data for Method 1 and Combine into Single Tibble ----

method_1_dir <- paste0(directory, "Data/Method 1 Metrics/Method_1_Metrics_Summary_Threshold_")

method_1_sim_0.487 <- read_csv(paste0(method_1_dir, "0.487.csv")) %>% mutate(Threshold = "0.487")
method_1_sim_0.514 <- read_csv(paste0(method_1_dir, "0.514.csv")) %>% mutate(Threshold = "0.514")
method_1_sim_0.539 <- read_csv(paste0(method_1_dir, "0.539.csv")) %>% mutate(Threshold = "0.539")
method_1_sim_0.564 <- read_csv(paste0(method_1_dir, "0.564.csv")) %>% mutate(Threshold = "0.564")
method_1_sim_0.588 <- read_csv(paste0(method_1_dir, "0.588.csv")) %>% mutate(Threshold = "0.588")
method_1_sim_0.613 <- read_csv(paste0(method_1_dir, "0.613.csv")) %>% mutate(Threshold = "0.613")
method_1_sim_0.639 <- read_csv(paste0(method_1_dir, "0.639.csv")) %>% mutate(Threshold = "0.639")
method_1_sim_0.666 <- read_csv(paste0(method_1_dir, "0.666.csv")) %>% mutate(Threshold = "0.666")
method_1_sim_0.696 <- read_csv(paste0(method_1_dir, "0.696.csv")) %>% mutate(Threshold = "0.696")
method_1_sim_0.736 <- read_csv(paste0(method_1_dir, "0.736.csv")) %>% mutate(Threshold = "0.736")

method_1_threshold_data <- method_1_sim_0.487 %>%
  add_row(method_1_sim_0.514) %>%
  add_row(method_1_sim_0.539) %>%
  add_row(method_1_sim_0.564) %>%
  add_row(method_1_sim_0.588) %>%
  add_row(method_1_sim_0.613) %>%
  add_row(method_1_sim_0.639) %>%
  add_row(method_1_sim_0.666) %>%
  add_row(method_1_sim_0.696) %>%
  add_row(method_1_sim_0.736)


## Load Data for Method 2 and Combine into Single Tibble ----

method_2_dir <- paste0(directory, "Data/Method 2 Metrics/Method_2_Metrics_Summary_Threshold_")

method_2_sim_0.487 <- read_csv(paste0(method_2_dir, "0.487.csv")) %>% mutate(Threshold = "0.487")
method_2_sim_0.514 <- read_csv(paste0(method_2_dir, "0.514.csv")) %>% mutate(Threshold = "0.514")
method_2_sim_0.539 <- read_csv(paste0(method_2_dir, "0.539.csv")) %>% mutate(Threshold = "0.539")
method_2_sim_0.564 <- read_csv(paste0(method_2_dir, "0.564.csv")) %>% mutate(Threshold = "0.564")
method_2_sim_0.588 <- read_csv(paste0(method_2_dir, "0.588.csv")) %>% mutate(Threshold = "0.588")
method_2_sim_0.613 <- read_csv(paste0(method_2_dir, "0.613.csv")) %>% mutate(Threshold = "0.613")
method_2_sim_0.639 <- read_csv(paste0(method_2_dir, "0.639.csv")) %>% mutate(Threshold = "0.639")
method_2_sim_0.666 <- read_csv(paste0(method_2_dir, "0.666.csv")) %>% mutate(Threshold = "0.666")
method_2_sim_0.696 <- read_csv(paste0(method_2_dir, "0.696.csv")) %>% mutate(Threshold = "0.696")
method_2_sim_0.736 <- read_csv(paste0(method_2_dir, "0.736.csv")) %>% mutate(Threshold = "0.736")

method_2_threshold_data <- method_2_sim_0.487 %>%
  add_row(method_2_sim_0.514) %>%
  add_row(method_2_sim_0.539) %>%
  add_row(method_2_sim_0.564) %>%
  add_row(method_2_sim_0.588) %>%
  add_row(method_2_sim_0.613) %>%
  add_row(method_2_sim_0.639) %>%
  add_row(method_2_sim_0.666) %>%
  add_row(method_2_sim_0.696) %>%
  add_row(method_2_sim_0.736)


## Plot of Density and Transitivity as Threshold Changes ----

span <- 0.15

size <- 0.25

plt8 <- ggplot(method_1_threshold_data, aes(x = Date, y = Density, color = Threshold)) +
  geom_smooth(method = "loess", span = span, size = size) +
  scale_x_continuous(breaks=c(186, 372, 558, 744, 930, 1116, 1302, 1488, 1674, 1860, 2046, 2232),
                     labels = c("Jul 1914", "Jan 1915", "Jul 1915", "Jan 1916", "Jul 1916", "Jan 1917", "Jul 1917", "Jan 1918", "Jul 1918", "Jan 1919", "Jul 1919", "Jan 1920")) + 
  theme(axis.text.x=element_text(angle = -90, vjust = 0.25, hjust=1)) +
  labs(x = "Date", y = "Density", color = "Threshold")

plt9 <- ggplot(method_1_threshold_data, aes(x = Date, y = Transitivity, color = Threshold)) +
  geom_smooth(method = "loess", span = span, size = size) +
  scale_x_continuous(breaks=c(186, 372, 558, 744, 930, 1116, 1302, 1488, 1674, 1860, 2046, 2232),
                     labels = c("Jul 1914", "Jan 1915", "Jul 1915", "Jan 1916", "Jul 1916", "Jan 1917", "Jul 1917", "Jan 1918", "Jul 1918", "Jan 1919", "Jul 1919", "Jan 1920")) + 
  theme(axis.text.x=element_text(angle = -90, vjust = 0.25, hjust=1)) +
  labs(x = "Date", y = "Transitivity", color = "Threshold")

plt10 <- ggarrange(plt8, plt9,
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "bottom")

ragg::agg_png(paste0(directory, "Figures/Method_1_Metrics_Thresholds.png"), height = 4, width = 6, units = "in", res = 300)

plt10

dev.off()


## Top Two Diaries for Method 2 as Threshold Changes ----

diary_names <- entry_tibble %>%
  select(Node, `Document Name`) %>%
  unique()

method_2_threshold_data %>%
  left_join(diary_names) %>%
  select(Threshold, `Document Name`, `Weighted Degree`, `Weighted EC`) %>%
  arrange(desc(`Weighted EC`), desc(`Weighted Degree`)) %>%
  group_by(Threshold) %>%
  slice(1:2)

method_2_threshold_data %>%
  left_join(diary_names) %>%
  select(Threshold, `Document Name`, `Weighted Degree`, `Weighted EC`) %>%
  arrange(desc(`Weighted Degree`), desc(`Weighted EC`)) %>%
  group_by(Threshold) %>%
  slice(1:2)

## Method 1: Find Dates of Interest ----

# Function to convert epoch date (date as number of days since 1 Jan 1914) to form DD MM YYYY:

convert_to_expanded <- function(x) {
  
  y1 <- x %/% 372
  year <- y1 + 1914
  
  r1 <- x %% 372
  m1 <- r1 %/% 31
  m2 <- m1 + 1
  
  if (m2 == 1) { 
    month = "Jan" 
  }else if(m2 == 2){
    month = "Feb"
  }else if(m2 == 3){
    month = "Mar"
  }else if(m2 == 4){
    month = "Apr"
  }else if(m2 == 5){
    month = "May"
  }else if(m2 == 6){
    month = "Jun"
  }else if(m2 == 7){
    month = "Jul"
  }else if(m2 == 8){
    month = "Aug"
  }else if(m2 == 9){
    month = "Sep"
  }else if(m2 == 10){
    month = "Oct"
  }else if(m2 == 11){
    month = "Nov"
  }else if(m2 == 12){
    month = "Dec"
  }
  
  r2 <- r1 %% 31
  date <- r2 + 1
  
  date <- paste(as.character(date), month, as.character(year))
  
  return(date)
  
}

method_1_data <- method_1_sim_0.613 %>%
  rowwise() %>%
  mutate(`Expanded Date` = convert_to_expanded(Date)) %>%  # Convert dates to expanded form
  ungroup()

# Plot to find dates of interest:

plt11 <- method_1_data %>% 
  ggplot() + 
  geom_point(aes(x = `Num Nodes`, y = Density, color = cut(Transitivity, 5)), size = 1) +
  labs(x = "Number of Nodes", y = "Density", color = "Transitivity") +
  theme(legend.position = "bottom") +
  geom_label_repel(data = subset(method_1_data, `Num Nodes` > 37 & Density > 0.45),
                   aes(x = `Num Nodes`, y = Density, label = `Expanded Date`),
                   nudge_y       = 0.7 - subset(method_1_data, `Num Nodes` > 37 & Density > 0.45)$Density,
                   size          = 2.5,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x") +
  geom_label_repel(data = subset(method_1_data, `Num Nodes` > 15 & Density > 0.7),
                   aes(x = `Num Nodes`, y = Density, label = `Expanded Date`),
                   nudge_y       = 1 - subset(method_1_data, `Num Nodes` > 15 & Density > 0.7)$Density,
                   size          = 2.5,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x")

ragg::agg_png(paste0(directory, "Figures/Method_1_Dates_of_Interest_Plot.png"), height = 4, width = 6, units = "in", res = 300)

plt11

dev.off()

# Find dates with top 10% of density and transitivity scores and top 50% of number of nodes

method_1_data %>% 
  filter(Density >= quantile(method_1_data$Density, p = 0.9)  & Transitivity >= quantile(method_1_data$Transitivity, p = 0.9) & `Num Nodes` >= quantile(method_1_data$`Num Nodes`, p = 0.5)) %>%
  select(`Expanded Date`, `Num Nodes`, Density, Transitivity) %>%
  print(n = 15)


## Method 1: Network Plots for Dates of Interest ----

# Network for 9 Nov 1914:

doi <- network_tibble %>%
  filter(Sim >= 0.613) %>%
  filter(Date == 318)

doi_nodes <- entry_tibble %>%
  filter(Date == 318) %>%
  filter(Node %in% doi$From | Node %in% doi$To) %>%
  select(Node, Sentiment)

ig <- igraph::graph_from_data_frame(d = doi, vertices = doi_nodes, directed = FALSE)

tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes)

set.seed(534)

plt12 <- tg %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_arc(strength = 0, aes(alpha = doi$Sim)) +
  geom_node_point(size = 4, aes(fill = Sentiment), color = "black", shape = 21) +
  scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1,1)) +
  #geom_node_text(size = 2.5, aes(label = doi_nodes$Node)) +
  guides(edge_width = "none",
         edge_alpha = "none") +
  labs(fill = "Sentiment") +
  theme_graph(background = "white") 

ragg::agg_png(paste0(directory, "Figures/Network_for_09Nov1914.png"), height = 4, width = 6, units = "in", res = 300)

plt12

dev.off()


## Network for 3 May 1915:

doi <- network_tibble %>%
  filter(Sim >= 0.613) %>%
  filter(Date == 498)

doi_nodes <- entry_tibble %>%
  filter(Date == 498) %>%
  filter(Node %in% doi$From | Node %in% doi$To) %>%
  select(Node, Sentiment)

ig <- igraph::graph_from_data_frame(d = doi, vertices = doi_nodes, directed = FALSE)

tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes)

set.seed(534)

plt13 <- tg %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_arc(strength = 0, aes(alpha = doi$Sim)) +
  geom_node_point(size = 4, aes(fill = Sentiment), color = "black", shape = 21) +
  scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1,1)) +
  #geom_node_text(size = 2.5, aes(label = doi_nodes$Node)) +
  guides(edge_width = "none",
         edge_alpha = "none") +
  labs(color = "Sentiment") +
  theme_graph(background = "white")

ragg::agg_png(paste0(directory, "Figures/Network_for_03May1915.png"), height = 4, width = 6, units = "in", res = 300)

plt13

dev.off()

# Network for 25 Dec 1915:

size2 <- 1.5

doi16 <- network_tibble %>%
  filter(Sim >= 0.613) %>%
  filter(Date == 737)

doi_nodes16 <- entry_tibble %>%
  filter(Date == 737) %>%
  filter(Node %in% doi16$From | Node %in% doi16$To) %>%
  select(Node, Sentiment)

ig16 <- igraph::graph_from_data_frame(d = doi16, vertices = doi_nodes16, directed = FALSE)

tg16 <- tidygraph::as_tbl_graph(ig16) %>% 
  tidygraph::activate(nodes)

set.seed(554)

plt16 <- tg16 %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_arc(strength = 0, aes(alpha = doi16$Sim)) +
  geom_node_point(size = size2, aes(fill = Sentiment), color = "black", shape = 21) +
  scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1,1)) +
  #geom_node_text(size = 2.5, aes(label = doi_nodes$Node)) +
  guides(edge_width = "none",
         edge_alpha = "none") +
  labs(color = "Sentiment") +
  theme_graph(background = "white") +
  scale_edge_alpha(range = c(0, 0.5))


# Network for 25 Dec 1916:

doi17 <- network_tibble %>%
  filter(Sim >= 0.613) %>%
  filter(Date == 1109)

doi_nodes17 <- entry_tibble %>%
  filter(Date == 1109) %>%
  filter(Node %in% doi17$From | Node %in% doi17$To) %>%
  select(Node, Sentiment)

ig17 <- igraph::graph_from_data_frame(d = doi17, vertices = doi_nodes17, directed = FALSE)

tg17 <- tidygraph::as_tbl_graph(ig17) %>% 
  tidygraph::activate(nodes)

set.seed(53543)

plt17 <- tg17 %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_arc(strength = 0, aes(alpha = doi17$Sim)) +
  geom_node_point(size = size2, aes(fill = Sentiment), color = "black", shape = 21) +
  scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1,1)) +
  #geom_node_text(size = 2.5, aes(label = doi_nodes$Node)) +
  guides(edge_width = "none",
         edge_alpha = "none") +
  labs(color = "Sentiment") +
  theme_graph(background = "white") +
  scale_edge_alpha(range = c(0, 0.5))


# Network for 11 Nov 1918:

doi18 <- network_tibble %>%
  filter(Sim >= 0.613) %>%
  filter(Date == 1808)

doi_nodes18 <- entry_tibble %>%
  filter(Date == 1808) %>%
  filter(Node %in% doi18$From | Node %in% doi18$To) %>%
  select(Node, Sentiment)

ig18 <- igraph::graph_from_data_frame(d = doi18, vertices = doi_nodes18, directed = FALSE)

tg18 <- tidygraph::as_tbl_graph(ig18) %>% 
  tidygraph::activate(nodes)

set.seed(5434)

plt18 <- tg18 %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_arc(strength = 0, aes(alpha = doi18$Sim)) +
  geom_node_point(size = size2, aes(fill = Sentiment), color = "black", shape = 21) +
  scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1,1)) +
  #geom_node_text(size = 2.5, aes(label = doi_nodes$Node)) +
  guides(edge_width = "none",
         edge_alpha = "none") +
  labs(color = "Sentiment") +
  theme_graph(background = "white") +
  scale_edge_alpha(range = c(0, 0.5))

# Network for 25 Dec 1918:

doi19 <- network_tibble %>%
  filter(Sim >= 0.613) %>%
  filter(Date == 1853)

doi_nodes19 <- entry_tibble %>%
  filter(Date == 1853) %>%
  filter(Node %in% doi19$From | Node %in% doi19$To) %>%
  select(Node, Sentiment)

ig19 <- igraph::graph_from_data_frame(d = doi19, vertices = doi_nodes19, directed = FALSE)

tg19 <- tidygraph::as_tbl_graph(ig19) %>% 
  tidygraph::activate(nodes)

set.seed(2344)

plt19 <- tg19 %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_arc(strength = 0, aes(alpha = doi19$Sim)) +
  geom_node_point(size = size2, aes(fill = Sentiment), color = "black", shape = 21) +
  scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1,1)) +
  #geom_node_text(size = 2.5, aes(label = doi_nodes$Node)) +
  guides(edge_width = "none",
         edge_alpha = "none") +
  labs(color = "Sentiment") +
  theme_graph(background = "white") +
  scale_edge_alpha(range = c(0, 0.5))


# Combined plot for plt16, plt17, plt18, plt19:

plt20 <- ggarrange(plt16, plt17, plt18, plt19,
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2,
                   common.legend = TRUE, legend = "right")

ragg::agg_png(paste0(directory, "Figures/4_Networks_Combined.png"), height = 4, width = 6, units = "in", res = 300)

plt20

dev.off()


## Method 2: Find Diaries of Interest ----

diary_names <- entry_tibble %>%
  select(Node, `Document Name`) %>%
  unique()

method_2_data <- method_2_sim_0.613 %>%
  left_join(diary_names)

plt21 <- method_2_data %>%
  ggplot(aes(x = `Weighted Degree`, y = `Weighted EC`)) +
  geom_point() +
  geom_label_repel(data = subset(method_2_data, `Weighted Degree` > 15000),
                   aes(x = `Weighted Degree`, y = `Weighted EC`, label = `Document Name`),
                   nudge_y       = 0.15 - subset(method_2_data, `Weighted Degree` > 15000)$`Weighted EC`,
                   nudge_x = 12000  - subset(method_2_data, `Weighted Degree` > 15000)$`Weighted Degree`,
                   size          = 3,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x")

ragg::agg_png(paste0(directory, "Figures/Method_2_Diaries_of_Interest_Plot.png"), height = 4, width = 6, units = "in", res = 300)

plt21

dev.off()
