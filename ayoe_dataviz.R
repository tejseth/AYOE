scatter <- passers_2020 %>%
  ungroup() %>%
  arrange(desc(avg_ayoe)) %>%
  mutate(rank = row_number()) %>%
  select(rank, passer, passes, mean_epa, avg_ayoe, posteam)

scatter <- scatter %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

scatter_plot <- scatter %>% 
  ggplot() +
  geom_smooth(aes(x = avg_ayoe, y = mean_epa), method = "lm", color = "grey") +
  ggrepel::geom_text_repel(
    aes(x = avg_ayoe, y = mean_epa, label = passer),
    box.padding = 0.5, size = 4
  ) + 
  geom_point(
    aes(x = avg_ayoe, y = mean_epa, size = passes, fill = team_color, color = team_color2), 
    shape = 21
  ) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  scale_size(name = "Passes") +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Average Air Yards Over Expected (AYOE)",
       y = "QB's EPA/Pass",
       title = "AYOE and EPA/Play Are Correlated",
       subtitle = "The correlation coefficient is 0.38",
       caption = "By Tej Seth | @mfbanalytics using code from @thomas_mock") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) 
scatter_plot
ggsave('ayoe-2.png', dpi=300, height=9*.8, width=16*.8)

logos_headshots <- scatter %>%
  left_join(unique_join, by = c("passer"))

team_eay <- pbp_2020 %>%
  group_by(posteam) %>%
  summarize(avg_eay = mean(x_air_yards, na.rm = T))

logos_headshots <- logos_headshots %>%
  left_join(team_eay, by.x = "posteam.x", by.y = "posteam")

#Download the CSV's to clean up. I'll have it on my github
write.csv(logos_headshots, "logos_headshots.csv")
write.csv(team_eay, "team_eay.csv")
write.csv(teams_colors_logos, "teams_colors_logos.csv")

logos_headshots <- read.csv("~/Expected Air Yards/logos_headshots.csv")
logos_headshots <- logos_headshots %>%
  arrange(rank) %>%
  mutate(ayoe = avg_ayoe + avg_eay)

logos_headshots %>%
  ggplot() +
  geom_link(
    mapping = aes(x = avg_eay, y = rank, xend = ayoe, yend = rank, size = 2, color = team_color)
  ) +
  theme_bw() +
  scale_colour_identity() +
  geom_image(aes(x = avg_eay, y = rank, image = team_logo_espn), size = 0.04, asp = 16/9) +
  geom_image(aes(x = ayoe, y = rank, image = headshot_url), size = 0.04, asp = 16/9) +
  labs(
    x = "Air Yards Per Pass",
    y = "",
    title = "Each Quarterback's Air Yards Over Expected",
    subtitle = "The team logo is their avg. expected air yards and the face of each quarterback is how many air yards over expected",
    caption = "By Tej Seth | @mfbanalytics using code from @benbbaldwin"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10))
ggsave('ayoe-3.png', dpi=300, height=9*.8, width=16*.8)

logos_headshots <- logos_headshots %>%
  mutate(sum_ayoe = passes * avg_ayoe)

logos_headshots <-logos_headshots %>%
  arrange(desc(sum_ayoe)) %>%
  mutate(rank2 = row_number())

bar_plot <- logos_headshots %>% 
  mutate(label = headshot_url,
         rank = as.integer(rank2)) %>% 
  ggplot() +
  geom_col(
    aes(
      x = rank, y = sum_ayoe,
      fill = team_color, color = team_color2
    ),
    width = 0.4
  ) + 
  scale_color_identity(aesthetics =  c("fill", "color")) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1, seq(1, 32, by = 1)), limits = c(0.5, 32.5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = NULL,
       y = "Total AYOE\n",
       title = "Total Air Yards Over Expected for 2020 Season",
       subtitle = "",
       caption = "By Tej Seth | @mfbanalytics") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

qb_col_img <- bar_plot +
  geom_image(
    aes(
      x = rank, y = sum_ayoe,
      image = headshot_url,
    ),
    size = 0.06
  )
qb_col_img

ggsave(
  "ayoe-4.png", qb_col_img, 
  height = 10, width = 16, dpi = "retina"
)










