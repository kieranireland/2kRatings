library("rvest")
library("dplyr")
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
  hoops_df <- dplyr::bind_rows(hoops_df, hoops_yearly_table)
  bball_html <- read_html(bball_ref_vec[count])
  ref_table_class <- bball_html %>%
    rvest::html_nodes("#div_advanced_stats")
  ref_table <- ref_table_class %>%
    rvest::html_table(header = TRUE) %>%
    as.data.frame()
  final_bball_table <- subset(ref_table, select = c(Player, G, PER, BPM))
  bball_ref_df <- dplyr::bind_rows(bball_ref_df, final_table)
  bball_df <- bball_ref_df[!duplicated(bball_ref_df$Player), ]
  count <- count + 1
  merged_yearly_df <- dplyr::inner_join(hoops_df, bball_df, by = 'Player') %>%
    dplyr::mutate(Year = year)
  merged_total_df <- dplyr::bind_rows(merged_total_df, merged_yearly_df)
}

plot(merged_total_df$Rating ~ merged_total_df$BPM)
abline(lm(merged_total_df$Rating ~ merged_total_df$BPM))

# How aligned are 2k ratings to real plus-minus/PER
# Who's overrated/underrated?
# Where does 2k hit or miss in terms of ratings?