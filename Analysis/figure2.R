#Load data
datos <- load('data/exp1_data.RData')

#Compute influence score based on Zanete and factor column for good/bad news 
dexp1 <- dexp1 %>%
  mutate(
    influencia = (p2-p1)/(pT-p1),
    news = factor(ifelse(I==0, "bad news", "good news"))
  )

paired_update <- dexp1 %>% group_by(subj, news) %>% summarise(media = mean(update))

ggplot(
  paired_update,
  aes(x = news, y = media)
) +
  geom_line(aes(group = subj)) +
  geom_point(aes(color = news)) +
  ylab("Update Mean") +
  scale_x_discrete("News Valence", labels = c("Bad News", "Good News")) +
  theme_bw() +
  theme(legend.position = 'none') +
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        legend.title=element_blank()) 


ggplot(
  dexp1,
  aes(x = update, fill = news)
) +
  geom_histogram(alpha = 0.7, bins = 15) +
  xlab("Update") + ylab("Frequency") +
  scale_fill_discrete('News Valence', labels = c('Bad News', 'Good News')) +
  theme_bw() +
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        legend.title=element_blank()) 
