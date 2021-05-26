library("rvest")
library("dplyr")
hoops_hype_vec <- c("https://hoopshype.com/nba2k/2013-2014/", "https://hoopshype.com/nba2k/2014-2015/", "https://hoopshype.com/nba2k/2015-2016/", "https://hoopshype.com/nba2k/2016-2017/", "https://hoopshype.com/nba2k/2017-2018/", "https://hoopshype.com/nba2k/2018-2019/", "https://hoopshype.com/nba2k/2019-2020/", "https://hoopshype.com/nba2k/")
html <- read_html("https://hoopshype.com/nba2k/2014-2015/")
final_df <- data.frame()
count <- 13
for (link in hoops_hype_vec) {
  html <- read_html(link)
  table_box <- html %>%
    rvest::html_nodes(".hh-ranking-ranking")
  yearly_table <- table_box %>%
    rvest::html_table(header = TRUE) %>%
    as.data.frame() %>%
    dplyr::mutate(Year = count)
  names(yearly_table)[1] <- "Rank"
  names(yearly_table)[3] <- "Rating"
  final_df <- dplyr::bind_rows(final_df, yearly_table)
  count <- count + 1
}

bball_ref_vec <- c("https://www.basketball-reference.com/leagues/NBA_2014_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2015_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2016_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2017_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2018_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html", "https://www.basketball-reference.com/leagues/NBA_2020_advanced.html")
year_count <- 13
bball_ref_df <- data.frame()
for (link in bball_ref_vec){
  html <- read_html(link)
  table_class <- html %>%
    rvest::html_nodes("#div_advanced_stats")
  ref_table <- table_class %>%
    rvest::html_table(header = TRUE) %>%
    as.data.frame()
  final_table <- subset(ref_table, select = c(Player, G, PER, BPM)) %>%
    dplyr::mutate(Year = year_count)
  bball_ref_df <- dplyr::bind_rows(bball_ref_df, final_table)
  year_count <- year_count + 1
  
  
}


# How aligned are 2k ratings to real plus-minus/PER
# Who's overrated/underrated?
# Where does 2k hit or miss in terms of ratings?
# Make a github