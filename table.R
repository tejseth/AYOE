table_20 <- table_20 %>%
  ungroup() %>%
  arrange(desc(avg_ayoe)) %>%
  mutate(rank = row_number()) %>%
  select(rank, passer, headshot_url, passes, mean_epa, avg_ayoe)

table_20 <- table_20 %>% mutate_if(is.numeric, ~round(., 2))

tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>% 
    cols_label(
      rank = "RK",
      passer = "Passer",
      passes = "Passes",
      headshot_url = "",
      mean_epa = "EPA/Pass",
      avg_ayoe = "AYOE") %>%
    data_color(
      columns = vars(avg_ayoe),
      colors = scales::col_numeric(
        palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"),
        domain = c(-3, 2)
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(rank, passer)
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    ) %>%
    opt_table_font(
      font = list(
        default_fonts()
      )
    ) 
}

gt_tab1 <- table_20 %>%
  filter(rank < 19) %>%
  tab_function()
gt_tab1
gtsave(gt_tab1, "gt-tab1.png")

gt_tab2 <- table_20 %>% 
  filter(rank >= 19) %>% 
  tab_function() %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "black",
      weight = px(3)
    ),
    locations = 
      list(
        cells_body(
          columns = 1
        ),
        cells_column_labels(1)
      )
  )
gt_tab2
gtsave(gt_tab2, "gt-tab2.png")

img1 <- magick::image_read("gt-tab1.png")
img2 <- magick::image_read("gt-tab2.png")

img3 <- magick::image_append(c(img1, img2))
img3

ggsave(img3, "ayoe-1.png")

