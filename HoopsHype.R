library("rvest")
library("dplyr")
library(tidyverse)
hoops_hype_vec <- c("https://hoopshype.com/nba2k/2013-2014/", "https://hoopshype.com/nba2k/2014-2015/", "https://hoopshype.com/nba2k/2015-2016/", "https://hoopshype.com/nba2k/2016-2017/", "https://hoopshype.com/nba2k/2017-2018/", "https://hoopshype.com/nba2k/2018-2019/", "https://hoopshype.com/nba2k/2019-2020/", "https://hoopshype.com/nba2k/")
bball_ref_vec <- c("https://www.basketball-reference.com/leagues/NBA_2014_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2015_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2016_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2017_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2018_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2020_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2021_advanced.html")
hoops_df <- data.frame()
bball_df <- data.frame()
count <- 1
merged_total_df <- data.frame()
for (year in 13:22){
  hoops_html <- read_html(hoops_hype_vec[count])
  hoops_table_box <- hoops_html %>%
    rvest::html_nodes(".hh-ranking-ranking")
  hoops_yearly_table <- hoops_table_box %>%
    rvest::html_table(header = TRUE) %>%
    as.data.frame()
  names(hoops_yearly_table)[1] <- "Rank"
  names(hoops_yearly_table)[3] <- "Rating"
  # hoops_df <- dplyr::bind_rows(hoops_df, hoops_yearly_table)
  bball_html <- read_html(bball_ref_vec[count])
  ref_table_class <- bball_html %>%
    rvest::html_nodes("#div_advanced_stats")
  ref_table <- ref_table_class %>%
    rvest::html_table(header = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(Player, G, MP, PER, BPM) %>%
    dplyr::mutate(
      G = as.numeric(G, na.rm = TRUE),
      MP = as.numeric(MP, na.rm = TRUE),
      PER = as.numeric(PER, na.rm = TRUE),
      BPM = as.numeric(BPM, na.rm = TRUE)
    )
  bball_df <- ref_table[!duplicated(ref_table$Player), ]
  count <- count + 1
  restricted_mins_df <- filter(bball_df, MP > 750)
  merged_yearly_df <- dplyr::inner_join(hoops_yearly_table, restricted_mins_df, by = 'Player') %>%
    dplyr::mutate(Year = year)
  merged_total_df <- dplyr::bind_rows(merged_total_df, merged_yearly_df)
  
}

ggplot(data = merged_total_df, mapping = aes(x = Rating, y = BPM, label = Player)) +
  geom_point() +
  theme_bw() +
  geom_smooth() + 
  geom_label() +
  facet_wrap(~ Year, nrow = 2)
