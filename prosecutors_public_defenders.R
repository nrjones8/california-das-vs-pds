library(dplyr)
library(ggplot2)
library(jsonlite)
# See Law_Enforcement_Personnel_and_Criminal_Justice_Survey_Context_062017.pdf
# Prosecution_TOTAL - The total number of criminal justice personnel reported by Prosecution entities
# DA_Attorneys - Prosecution: Attorney personnel
# DA_Investigators - Prosecution: Investigator personnel
# DA_Clerical - Prosecution: Clerical personnel
# DA_Other - Prosecution: Other personnel

# PublicDefense_TOTAL - The total number of criminal justice personnel reported by Public Defense entities
# PD_Attorneys - Public Defense: Attorney personnel
# PD_Investigators - Public Defense: Investigator personnel
# PD_Clerical - Public Defense: Clerical personnel
# PD_Other - Public Defense: Other personnel

# There are a _lot_ of 0-reported public defenders:
# Zero's may indicate that a department or office does not have personnel in that classification, that a
# department may have closed or merged with another, that a county may not have a police department or a
# public defender’s office, or that data were not reported.

summarize_missing_data <- function() {
  df <- read.csv('LE_and_CJ_Personnel_2004-2016.csv')
  
  # 754 total entries
  print("Total entries")
  print(nrow(df))
  
  # There are 339 instances of 0-reported Public defenders offices, or NAs - note that that's across 12 years
  print("Number of rows with 0 or missing Public Defense numbers")
  print(nrow(df %>% filter(PublicDefense_TOTAL == 0 | is.na(PublicDefense_TOTAL))))
  
  # 0 instances of 0-reported prosectuion offices
  print("Number of rows with 0 or missing prosecution numbers")
  print(nrow(df %>% filter(Prosecution_TOTAL == 0 | is.na(Prosecution_TOTAL))))
  
  print("Number of counties that provided public defense totals at least once")
  print(length(unique(df %>% filter(PublicDefense_TOTAL > 0) %>% pull(COUNTY))))
  
  print("Counties with no PD data in 2016, and their relative sizes")
  print(df %>%
          filter(YEAR == 2016) %>%
          filter(PublicDefense_TOTAL == 0 | is.na(PublicDefense_TOTAL)) %>%
          select(COUNTY, ST_TOTAL, Prosecution_TOTAL) %>%
          arrange(desc(ST_TOTAL))
  )
}

prep_data <- function() {
  df <- read.csv('LE_and_CJ_Personnel_2004-2016.csv')
  
  # Removes both 0s and NAs
  prepped <- df %>%
    filter(PublicDefense_TOTAL > 0) %>%
    mutate(
      num_more_prosec = Prosecution_TOTAL - PublicDefense_TOTAL,
      pct_more_prosec = Prosecution_TOTAL / PublicDefense_TOTAL,
      pct_pd_of_pros = PublicDefense_TOTAL / Prosecution_TOTAL
    )
  
  return(prepped)
}

write_to_json <- function(df, include_LA=TRUE) {
  fields_to_write <- c('YEAR', 'COUNTY', 'Prosecution_TOTAL', 'PublicDefense_TOTAL')
  only_fields_we_want <- df %>%
    select(fields_to_write) %>%
    mutate(
      x = Prosecution_TOTAL,
      y = PublicDefense_TOTAL
    )
  
  if (!include_LA) {
    only_fields_we_want <- only_fields_we_want %>% filter(COUNTY != 'Los Angeles County')
  }
  as_json <- toJSON(only_fields_we_want, pretty=TRUE)
  
  output_file_name <- ifelse(include_LA, 'das_pds_with_la.json', 'das_pds_excluding_la.json')
  conn <- file(output_file_name)
  writeLines(as_json, conn)
  close(conn)
}

calc_yearly_summaries <- function(df) {
  yearly <- df %>%
    group_by(YEAR) %>%
    summarise(
      min_pd_pct = min(pct_pd_of_pros),
      median_pd_pct = median(pct_pd_of_pros),
      max_pd_pct = max(pct_pd_of_pros)
    )
  
  return(yearly)
}

boxplot_of_ratios_over_time <- function(df) {
  boxplot <- ggplot(df, aes(x=YEAR, y=pct_pd_of_pros, group=YEAR)) + 
    geom_boxplot() +
    scale_x_continuous('Year') +
    scale_y_continuous('Distribution of size of Public Defender Offices, as % of DA Office') +
    ggtitle('Public Defender Office Size, as % of DA Office - Distribution over time') +
    theme(plot.title = element_text(hjust = 0.5))
  print(boxplot)
}

pd_vs_prosecut_scatter <- function(df) {
  # Find the max from both offices, use that as the limit - it should be "square" plot, i.e. NxN
  max_axis <- max(df$Prosecution_TOTAL, max(df$PublicDefense_TOTAL))
  scatter <- ggplot(df, aes(x=Prosecution_TOTAL, y=PublicDefense_TOTAL)) +
    geom_point() +
    #geom_text(aes(label=COUNTY), hjust=0, vjust=0) +
    scale_x_continuous("Number of DA Employees", limits=c(0, max_axis)) +
    scale_y_continuous("Number of Public Defender Employees", limits=c(0, max_axis)) +
    ggtitle("District Attorney staffing vs. Public Defender staffing") +
    # Put an "80%" line - that is, if there are 1000 prosecutors, and 80% of the cases are indigent, then there
    # should be ~800 public defenders.
    geom_abline(intercept = 0, slope = .8) +
    geom_abline(intercept = 0, slope = 1)
  print(scatter)
}

hist_of_pd_ratio <- function(df) {
  hist <- ggplot(df, aes(pct_pd_of_pros)) +
    geom_histogram()
  print(hist)
}

with_defined_pd <- prep_data()
# bleh, can't show all of these in one plot, too many counties. But can show the changes over time for a few of them.
# Could use a damn heatmap again, but that I feel like that's getting old...
ordered_by_total_peeps <- with_defined_pd %>%
  group_by(COUNTY) %>%
  summarise(total_people= sum(ST_TOTAL)) %>%
  arrange(desc(total_people))

most_populous <- head(ordered_by_total_peeps$COUNTY, 15)

g <- ggplot(with_defined_pd %>% filter(COUNTY %in% most_populous), aes(x=YEAR, y=pct_more_prosec, color=COUNTY)) +
  geom_line()
print(g)

pd_vs_prosecut_scatter(with_defined_pd %>% filter(YEAR == 2016))
pd_vs_prosecut_scatter(with_defined_pd %>% filter(Prosecution_TOTAL < 1750))
hist_of_pd_ratio(with_defined_pd %>% filter(YEAR == 2016))

boxplot_of_ratios_over_time(with_defined_pd)
yearly_summaries <- calc_yearly_summaries(with_defined_pd)


