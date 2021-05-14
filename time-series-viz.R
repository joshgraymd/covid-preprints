library(ggplot2)
library(dplyr)
library(lubridate)
library(here)

metric = 'abstract_sim_cosine_tf'

if (!dir.exists(here('figs', metric))) {
    dir.create(here('figs', metric))
}

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

setwd(here())

df <- read.csv(here('data', 'python_comparison_output_newdata.csv'), header = TRUE, stringsAsFactors = FALSE)


df$peerreviewedPubDate = ymd(df$peerreviewedPubDate)

# Filter to see particular year range
#df <- df %>% filter(peerreviewedPubDate >= "2019-01-01")

covid_cohort <- df %>% filter(cohort == 'intra-pandemic COVID-19 articles')
non_covid_cohort <- df %>% filter(!(cohort %in% c('intra-pandemic COVID-19 articles')))

p <- ggplot(df, aes(x=peerreviewedPubDate, y=.data[[metric]], group=cohort)) +
    geom_point(aes(color = cohort)) +
    xlab("")
p_covid <- ggplot(covid_cohort, aes(x=peerreviewedPubDate, y=.data[[metric]])) +
    geom_point() +
    xlab("")
p_noncovid <- ggplot(non_covid_cohort, aes(x=peerreviewedPubDate, y=.data[[metric]])) +
    geom_point() +
    xlab("")

q <- ggplot(df, aes(x=peerreviewedPubDate, y=.data[[metric]], group=cohort)) +
    geom_smooth(aes(color = cohort)) +
    #xlab("Year") +
    ylab("% cosine similarity by term frequency vectors") +
    ggtitle('Abstract similary of 1st version preprint and resulting publication over time') +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = "3 months",
                 date_minor_breaks = "1 month",
                 limits = as.Date(c("2013-08-01","2020-06-30"))) +
    theme(legend.position = "top") +
    geom_vline(xintercept = c(as.Date("2013-11-07"),as.Date("2019-06-01"), as.Date("2020-01-22"))) +
    annotate("text",
             x=c(as.Date("2013-11-01"),as.Date("2019-06-01"), as.Date("2020-01-19")),
             y=0.6,
             label=c("bioRxiv launch",
                     "medRxiv launch", 
                     "first preprint in COVID-19 collection"),
             hjust=-0.1)
# Save
ggsave(plot = q,
       here('figs', metric, 'abstract_change_by_cohort.pdf'), 
       width = 96, 
       height = 10, 
       units = "in",
       limitsize = FALSE)

q_rolled_up <- ggplot(df, aes(x=peerreviewedPubDate, y=.data[[metric]])) +
    geom_smooth() +
    xlab("") 

r <- ggplot(df, aes(x=cohort, y=.data[[metric]])) + 
    geom_boxplot(outlier.colour = "black", outlier.shape = 16,
             outlier.size = 2, notch=FALSE)

r <- r + stat_summary(fun=mean, geom="point", shape=23, size=4)

l <- ggplot(df %>% filter(peerreviewedPubDate >= "2019-01-01"), aes(x=cohort, y=.data[[metric]])) + 
    geom_violin() +
    geom_boxplot(width=0.1)

l_2 <- ggplot(df %>% filter(!(cohort %in% c('pre-pandemic'))), aes(x=cohort, y=.data[[metric]])) + 
    geom_violin() +
    geom_boxplot(width=0.02)
n_fun <- function(x){
    return(data.frame(y = max(x) + 2, label = paste0("n = ",length(x))))
}
l_2 <- l_2 + 
    scale_fill_manual(values=c(gg_color_hue(1), gg_color_hue(2))) + 
    stat_summary(fun.data = n_fun, geom = "text")

ggsave(plot = l_2,
       here('figs', metric, 'intra-pandemic_covid_vs_noncovid.pdf'), 
       width = 5.69, 
       height = 6.19, 
       units = "in",
       limitsize = FALSE)
ggsave(plot = l_2,
       here('figs', metric, 'intra-pandemic_covid_vs_noncovid.jpg'), 
       width = 5.69, 
       height = 6.19, 
       units = "in",
       limitsize = FALSE)

df_byweek_covid_cohort <- df %>% 
    filter((cohort %in% "intra-pandemic COVID-19 articles")) %>% 
    mutate(peerreviewedPubDateBOW = floor_date(peerreviewedPubDate, 
                                               unit="week", 
                                               week_start = getOption("lubridate.week.start", 1)
    )) %>% 
    group_by(peerreviewedPubDateBOW) %>% 
    summarize(count=n())
df_byweek_noncovid_cohort <- df %>% 
    filter((cohort %in% "intra-pandemic non-COVID-19 articles")) %>% 
    mutate(peerreviewedPubDateBOW = floor_date(peerreviewedPubDate, 
                                               unit="week", 
                                               week_start = getOption("lubridate.week.start", 1)
    )) %>% 
    group_by(peerreviewedPubDateBOW) %>% 
    summarize(count=n())
df_byweek_prepandemic <- df %>% 
    filter((cohort %in% "pre-pandemic")) %>% 
    mutate(peerreviewedPubDateBOW = floor_date(peerreviewedPubDate, 
                                               unit="week", 
                                               week_start = getOption("lubridate.week.start", 1)
    )) %>% 
    group_by(peerreviewedPubDateBOW) %>% 
    summarize(count=n())

dates <- seq(as.Date("2019/12/30"), as.Date("2020/6/8"), "weeks")

# Bar chart of COVID articles published by week
o <- ggplot(df_byweek_covid_cohort, aes(y=count, x=peerreviewedPubDateBOW)) + 
    geom_bar(stat="identity", fill = gg_color_hue(1)) +
    ylim(-0.85, 17.85) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90)) +
    scale_x_date(breaks = dates,
                 minor_breaks = NULL,
                 labels = dates,
                 date_labels = "%b %d",
                 limits = c(
                     as.Date("2019/12/30"), as.Date("2020/6/8")
                 )) +
    labs(x = "Week published (i.e. made available online)",
         y = "Number of articles",
         title = "2020 week of resulting publication for former preprints*",
         subtitle = "*former {bio,med}Rxiv preprints APPEARING in shared SARS-CoV-2/COVID-19 collection")

# Bar chart of non-COVID articles published by week
k <- ggplot(df_byweek_noncovid_cohort, aes(y=count, x=peerreviewedPubDateBOW)) + 
    geom_bar(stat="identity", fill="#32CD32") +
    ylim(-0.85, 17.85) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90)) +
    # theme(legend.position = "top", 
    #       axis.text.x = element_text(angle = 90)) +
    scale_x_date(breaks = dates,
                 minor_breaks = NULL,
                 labels = dates,
                 date_labels = "%b %d",
                 limits = c(
                     as.Date("2019/12/30"), as.Date("2020/6/8")
                 )) +
    labs(x = "Week published (i.e. made available online)",
         y = "Number of articles",
         title = "2020 week of resulting publication for former preprints*",
         subtitle = "*former {bio,med}Rxiv preprints NOT APPEARING in shared SARS-CoV-2/COVID-19 collection")

ggsave(plot = k,
       here('figs', 'bar_NONcovid_by_week.pdf'), 
       width = 7.23, 
       height = 6.23, 
       units = "in",
       limitsize = FALSE)
ggsave(plot = k,
       here('figs', 'bar_NONcovid_by_week.jpg'), 
       width = 7.23, 
       height = 6.23, 
       units = "in",
       limitsize = FALSE)

ggsave(plot = o,
       here('figs', 'bar_covid_by_week.pdf'), 
       width = 7.23, 
       height = 6.23, 
       units = "in",
       limitsize = FALSE)
ggsave(plot = o,
       here('figs', 'bar_covid_by_week.jpg'), 
       width = 7.23, 
       height = 6.23, 
       units = "in",
       limitsize = FALSE)

j <- ggplot(df_byweek_prepandemic, aes(y=count, x=peerreviewedPubDateBOW)) + 
    geom_bar(stat="identity") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90)) +
    # theme(legend.position = "top", 
    #       axis.text.x = element_text(angle = 90)) +
    scale_x_date(breaks = dates,
                 minor_breaks = NULL,
                 labels = dates,
                 date_labels = "%b %d",
                 limits = c(
                     as.Date("2019/12/30"), as.Date("2020/6/8")
                 )) +
    labs(x = "Week published (i.e. made available online)",
         y = "Number of articles",
         title = "2020 week of resulting publication for preprints posted pre-pandemic*",
         subtitle = "*preprints posted prior to 2020")


df_test <- df %>%  
    filter(!(cohort %in% "pre-pandemic")) %>% 
    mutate(peerreviewedPubDateBOW = floor_date(peerreviewedPubDate, 
                                               unit="week", 
                                               week_start = getOption("lubridate.week.start", 1)
    )) %>% 
    group_by(peerreviewedPubDateBOW, cohort) %>%
    summarise(n = n()) %>% 
    ungroup(peerreviewedPubDateBOW) %>% 
    group_by(cohort) %>%
    mutate(n = cumsum(n))

# Cumulative counts of non-COVID-19 and COVID-19
s <- ggplot(df_test, 
       aes(x = peerreviewedPubDateBOW, y = n)) + 
    ylab("Cumulative number published") +
    xlab("Week") +
    geom_line(aes(color=cohort, linetype=cohort)) +
    scale_x_date(breaks = dates,
                 minor_breaks = NULL,
                 labels = dates,
                 date_labels = "%b %d",
                 limits = c(
                     as.Date("2019/12/30"), as.Date("2020/6/8")
                 )
                 ) +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "top"
    )
    

ggsave(plot = s,
       here('figs', 'intra-pandemic_cumulative_counts.pdf'), 
       width = 6.23, 
       height = 6.23, 
       units = "in",
       limitsize = FALSE)
ggsave(plot = s,
       here('figs', 'intra-pandemic_cumulative_counts.jpg'), 
       width = 6.23, 
       height = 6.23, 
       units = "in",
       limitsize = FALSE)

