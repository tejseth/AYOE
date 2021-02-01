library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)
library(dplyr)
library(bayesboot)
library(ggplot2)

pbp <- pbp_2020 %>% 
  filter(!is.na(epa),
         !is.na(ayoe)) %>%
  group_by(passer) %>%
  mutate(
    num_plays = n()
  ) %>% 
  ungroup() %>%
  filter(
    num_plays >= 230
  )

lst = list()
qbs = unique(pbp$passer)
for (qb in qbs){
  pbp_qb = pbp %>% filter(passer == qb)
  b <- bayesboot(as.vector(pbp_qb$ayoe), mean)
  s = summary(b)
  mean_ayoe = s$value[1]
  lci = s$value[3]
  uci = s$value[4]
  df = data.frame('mean'=mean_ayoe,'LCI'=lci,'UCI'=uci)
  lst[[qb]] = df
}
df = dplyr::bind_rows(lst)
df$passer = qbs
df = df %>% arrange(mean)

df <- df %>%
  left_join(table_20)

df %>%
  ggplot(aes(x=factor(passer, level = passer),y=mean)) + 
  geom_pointrange(aes(ymin=(LCI),
                      ymax=(UCI)), fatten = 0.01)+
  geom_image(aes(y = mean, x = passer, image = headshot_url), size = 0.03, asp = 16/9) +
  coord_flip()+
  theme_bw() +
  labs(y = "Average Air Yards Over Expected (AYOE)",
       x = "",
       title = "Bayesian Bootstrapping for Average AYOE | min. 200 pass attempts in 2020",
       caption = "By Tej Seth | @mfbanalytics | @adrian_stats") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('bayes_ayoe.png',height = 8, width = 10)

lst = list()
qbs = c('T.Brady','P.Rivers', 'B.Roethlisberger', 'D.Brees', 'A.Smith')
for (qb in qbs){
  pbp_qb = pbp %>% filter(passer == qb)
  b <- bayesboot(as.vector(pbp_qb$ayoe), mean)
  df = data.frame('estimate'=b$V1,'QB'=qb,team_abbr  = unique(pbp_qb$posteam))
  lst[[qb]] = df
}

df = dplyr::bind_rows(lst)
colors = nflfastR::teams_colors_logos %>% filter(team_abbr %in% c('TB','IND', 'PIT', 'NO', 'WAS'))

df %>% ggplot(aes(x=estimate)) +
  geom_density(aes(fill=QB),alpha=.6)+
  scale_fill_manual(
    values = c(colors$team_color[3],colors$team_color[1],colors$team_color[2], 
               colors$team_color[4], colors$team_color[5])  
  ) + 
  theme_bw() + 
  labs(x = "Air Yards Over Expected (AYOE)",
       y = "Density",
       title = "AYOE Range of Outcomes for the Old Quarterbacks",
       caption = "By Tej Seth | @mfbanalytics | @adrian_stats") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )
ggsave('bayes_ayoe4.png',height = 8, width = 10)



