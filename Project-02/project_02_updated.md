---
title: "Threes, Ratings, and Shorelines: A Three-Dataset Tour of the NBA, FIFA 18, and Florida's Lakes"
author: "Owen Telis - Otelis0019@floridapoly.edu"
output:
  html_document:
    theme: flatly
    highlight: tango
    keep_md: true
    toc: true
    toc_float: true
    toc_depth: 3
    code_folding: show
    df_print: paged
    
---



---

## Introduction

Does a strong data visualization project have to come from one dataset, or can three completely unrelated ones each carry their own story? This project takes the second approach. Instead of squeezing every chart type out of a single source, it moves through three independent datasets that have nothing to do with each other, chosen specifically to get hands-on practice with different kinds of charts:

- **NBA Champions data**: game-by-game box scores for every team that won the NBA Finals from 1980 to 2018.
- **FIFA 18 player data**: attributes for over 17,000 soccer players, used here to build a simple linear model.
- **Florida Lakes shapefile**: polygon shapes for over 4,000 lakes across the state of Florida, used for the spatial visualization.

To keep the report feeling like one cohesive piece of work instead of four random charts stapled together, every static plot below shares the same color palette and theme.


```r
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(sf)
library(broom)
library(scales)
library(viridis)
library(maps)
```


```r
# colors reused across every static plot in this report
accent_color <- "#1b4965"
highlight_color <- "#ef5675"

# a consistent ggplot theme so all the static charts look like they belong together
theme_owen <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 10)),
      plot.caption = element_text(color = "grey50", size = rel(0.75), hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

theme_owen_map <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 10)),
      plot.caption = element_text(color = "grey50", size = rel(0.75), hjust = 0),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "right"
    )
}
```

---

## Dataset 1: NBA Finals Champions (1980-2018)

The raw data has one row per game played by the team that won the NBA Finals that year, with box score stats for that specific game. **538 games, one champion per season, 1980 through 2018.**

### Cleaning the Data

Two text problems showed up in the `Team` column before anything else could be done: one team was listed as `'Heat'` with stray quote marks baked into the name, and another was listed as `Warriorrs` with an extra "r". Both were fixed with `case_when()`. A second issue was less of a typo and more of a quirk of the data: six games have a missing value for three-point percentage (`TPP`). That happens because in the early 1980s some teams attempted zero three-pointers in a given game, which makes the percentage mathematically undefined, since you can't divide zero by zero. Those rows were kept rather than dropped, with `na.rm = TRUE` used whenever this column gets averaged.


```r
nba_raw <- read.csv("../data/NBAchampionsdata.csv")

nba <- nba_raw %>%
  mutate(Team = case_when(
    Team == "'Heat'" ~ "Heat",
    Team == "Warriorrs" ~ "Warriors",
    TRUE ~ Team
  )) %>%
  mutate(Decade = paste0(floor(Year / 10) * 10, "s"))

knitr::kable(nba %>% distinct(Team) %>% arrange(Team), caption = "Teams after name cleaning")
```



Table: Teams after name cleaning

|Team      |
|:---------|
|Bulls     |
|Cavaliers |
|Celtics   |
|Heat      |
|Lakers    |
|Mavericks |
|Pistons   |
|Rockets   |
|Sixers    |
|Spurs     |
|Warriors  |

### Variable Notes

| Variable | Description |
|---|---|
| `Team` | NBA Finals champion that season |
| `Year` | Season year |
| `PTS` | Points scored in that game |
| `TPA` | Three-point attempts in that game |
| `TPP` | Three-point percentage in that game |
| `FGP` | Field goal percentage in that game |
| `Decade` | Derived grouping variable (1980s, 1990s, etc.) |

### Summarizing the Data

The data was grouped by year to get the average stats per game for each championship team, and separately counted up to see how many titles each team has won within this window.


```r
nba_yearly <- nba %>%
  group_by(Year) %>%
  summarize(
    champion = first(Team),
    avg_pts = mean(PTS),
    avg_tpa = mean(TPA),
    avg_tpp = mean(TPP, na.rm = TRUE),
    avg_fgp = mean(FGP),
    .groups = "drop"
  )

nba_titles <- nba %>%
  group_by(Team) %>%
  summarize(titles = n_distinct(Year), .groups = "drop") %>%
  arrange(desc(titles))

# nba_titles is used downstream; printed inline in the text below
knitr::kable(nba_titles, caption = "Championship wins per team, 1980-2018")
```



Table: Championship wins per team, 1980-2018

|Team      | titles|
|:---------|------:|
|Lakers    |     10|
|Bulls     |      6|
|Spurs     |      5|
|Celtics   |      4|
|Heat      |      3|
|Pistons   |      3|
|Warriors  |      3|
|Rockets   |      2|
|Cavaliers |      1|
|Mavericks |      1|
|Sixers    |      1|

The Lakers lead the pack with 10 titles in this window, followed by the Bulls with 6 and the Spurs with 5. None of that is especially surprising. What is surprising is the trend buried in the three-point numbers below.

### Visualizations

#### Plot 1: The Three-Point Revolution: Average 3PT Attempts by NBA Finals Champion, 1980-2018

This first chart tracks how many three-pointers the average championship team attempted per game, season by season, with the 2017 Warriors called out as the clear endpoint of the trend.


```r
p1 <- ggplot(nba_yearly, aes(x = Year, y = avg_tpa)) +
  geom_line(color = accent_color, linewidth = 1) +
  geom_point(color = accent_color, size = 1.8) +
  geom_point(
    data = nba_yearly %>% filter(Year == 2017),
    aes(x = Year, y = avg_tpa),
    color = highlight_color, size = 3.2
  ) +
  annotate(
    "text", x = 2013.3, y = 41,
    label = "2017 Warriors:\n37.2 three-point attempts\nper game",
    color = highlight_color, size = 3.3, fontface = "bold", hjust = 0.5
  ) +
  scale_x_continuous(breaks = seq(1980, 2018, 4)) +
  scale_y_continuous(limits = c(0, 46), expand = expansion(mult = c(0.02, 0))) +
  labs(
    title = "The Three-Point Revolution in the NBA Finals",
    subtitle = "Average three-point attempts per game for the championship team, 1980 to 2018",
    x = NULL, y = "Average 3PT Attempts per Game",
    caption = "Source: NBA Champions box score data, 1980-2018"
  ) +
  theme_owen()

p1
```

<img src="https://github.com/OwenTelis/dataviz_final_project/blob/main/figures/nba-static-plot-1.png" style="display: block; margin: auto;" />

**Interpretation:** The shift in shooting philosophy over four decades is hard to overstate once it's laid out as a line chart.

**The growth from 1980 to 2017 is enormous.** Championship teams in 1980 barely shot threes at all, averaging less than one attempt per game. By 2017, the Warriors were jacking up an average of 37.2 attempts per game, as the annotation on the chart calls out directly. That is roughly a 40-fold increase in less than 40 years, for a shot that barely existed as part of the offensive playbook at the start of the dataset.

**The line isn't perfectly smooth on the way there.** A couple of points break from the overall upward direction, most notably the early-1990s Pistons and the 2016 Cavaliers, both of which lean more on a slower, paint-focused style than the era around them would suggest. Those dips are small compared to the overall trend, but they are a useful reminder that even a dominant league-wide shift doesn't move in a perfectly straight line every single year.

**None of this directly contradicts what NBA fans already know about the league's shift toward three-point shooting,** but narrowing the data down to only the championship teams makes the shift feel even bigger. These are the best teams in the league in a given year, and even they went from almost never shooting threes to building entire offenses around them.

#### Plot 2: Field Goal Percentage vs. Three-Point Percentage: An Interactive Look Across Four Decades

For the interactive version, the data drops down from yearly averages to the individual game level, so there's more to explore by hovering. Field goal percentage is plotted against three-point percentage for every game in the dataset, colored by decade, with a tooltip showing the team, year, game number, and whether that specific game was a win or a loss.


```r
nba_games <- nba %>%
  filter(!is.na(TPP)) %>%
  mutate(hover_label = paste0(
    Team, " - ", Year, " (Game ", Game, ")",
    "<br>FG%: ", percent(FGP, accuracy = 0.1),
    "<br>3P%: ", percent(TPP, accuracy = 0.1),
    "<br>Result: ", ifelse(Win == 1, "Win", "Loss")
  ))

decade_colors <- c(
  "1980s" = "#1b4965", "1990s" = "#5fa8d3", "2000s" = "#bee9e8",
  "2010s" = "#ef5675"
)

p2_base <- ggplot(nba_games, aes(x = TPP, y = FGP, color = Decade, text = hover_label)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = decade_colors) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Field Goal % vs Three-Point % by Game, NBA Champions 1980-2018",
    x = "Three-Point Percentage", y = "Field Goal Percentage", color = "Decade"
  ) +
  theme_minimal(base_size = 12)

p2 <- ggplotly(p2_base, tooltip = "text") %>%
  layout(legend = list(orientation = "h", y = -0.2))

p2
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-956ecd42bedefddcd409" style="width:864px;height:576px;"></div>
<script type="application/json" data-for="htmlwidget-956ecd42bedefddcd409">{"x":{"data":[{"x":[0,0,0,0,0,0.66700000000000004,0,0,0.25,0,1,0,0,0,0,0,0.5,0,0.40000000000000002,0.5,0.59999999999999998,0.33300000000000002,0,0.40000000000000002,0.5,0.5,0.25,0,0,0.25,0.44400000000000001,0.20000000000000001,0.33300000000000002,0.16700000000000001,0.5,0.20000000000000001,0.75,0.27300000000000002,0.5,0.5,0,0.25,0.5,0.33300000000000002,0,0.25,0,0.29999999999999999,0.5,0.125,0.14299999999999999,0.66700000000000004],"y":[0.505,0.47799999999999998,0.48899999999999999,0.432,0.5,0.44900000000000001,0.47299999999999998,0.436,0.55100000000000005,0.42199999999999999,0.54900000000000004,0.46400000000000002,0.46999999999999997,0.48199999999999998,0.47899999999999998,0.51900000000000002,0.51800000000000002,0.45900000000000002,0.39600000000000002,0.432,0.51700000000000002,0.46400000000000002,0.39500000000000002,0.48999999999999999,0.47799999999999998,0.54200000000000004,0.48199999999999998,0.56799999999999995,0.51200000000000001,0.56000000000000005,0.5,0.438,0.57699999999999996,0.40500000000000003,0.48499999999999999,0.55600000000000005,0.61499999999999999,0.49399999999999999,0.48199999999999998,0.45300000000000001,0.48399999999999999,0.39800000000000002,0.45500000000000002,0.51400000000000001,0.40300000000000002,0.47399999999999998,0.47199999999999998,0.55800000000000005,0.55400000000000005,0.54300000000000004,0.51800000000000002,0.48599999999999999],"text":["Lakers - 1980 (Game 2)<br>FG%: 50.5%<br>3P%: 0.0%<br>Result: Loss","Lakers - 1980 (Game 3)<br>FG%: 47.8%<br>3P%: 0.0%<br>Result: Win","Lakers - 1980 (Game 6)<br>FG%: 48.9%<br>3P%: 0.0%<br>Result: Win","Celtics - 1981 (Game 1)<br>FG%: 43.2%<br>3P%: 0.0%<br>Result: Win","Celtics - 1981 (Game 2)<br>FG%: 50.0%<br>3P%: 0.0%<br>Result: Loss","Celtics - 1981 (Game 3)<br>FG%: 44.9%<br>3P%: 66.7%<br>Result: Win","Celtics - 1981 (Game 4)<br>FG%: 47.3%<br>3P%: 0.0%<br>Result: Loss","Celtics - 1981 (Game 5)<br>FG%: 43.6%<br>3P%: 0.0%<br>Result: Win","Celtics - 1981 (Game 6)<br>FG%: 55.1%<br>3P%: 25.0%<br>Result: Win","Lakers - 1982 (Game 2)<br>FG%: 42.2%<br>3P%: 0.0%<br>Result: Loss","Lakers - 1982 (Game 3)<br>FG%: 54.9%<br>3P%: 100.0%<br>Result: Win","Lakers - 1982 (Game 4)<br>FG%: 46.4%<br>3P%: 0.0%<br>Result: Win","Lakers - 1982 (Game 5)<br>FG%: 47.0%<br>3P%: 0.0%<br>Result: Loss","Sixers - 1983 (Game 2)<br>FG%: 48.2%<br>3P%: 0.0%<br>Result: Win","Sixers - 1983 (Game 3)<br>FG%: 47.9%<br>3P%: 0.0%<br>Result: Win","Sixers - 1983 (Game 4)<br>FG%: 51.9%<br>3P%: 0.0%<br>Result: Win","Celtics - 1984 (Game 1)<br>FG%: 51.8%<br>3P%: 50.0%<br>Result: Win","Celtics - 1984 (Game 2)<br>FG%: 45.9%<br>3P%: 0.0%<br>Result: Loss","Celtics - 1984 (Game 3)<br>FG%: 39.6%<br>3P%: 40.0%<br>Result: Loss","Celtics - 1984 (Game 4)<br>FG%: 43.2%<br>3P%: 50.0%<br>Result: Win","Celtics - 1984 (Game 5)<br>FG%: 51.7%<br>3P%: 60.0%<br>Result: Win","Celtics - 1984 (Game 6)<br>FG%: 46.4%<br>3P%: 33.3%<br>Result: Loss","Celtics - 1984 (Game 7)<br>FG%: 39.5%<br>3P%: 0.0%<br>Result: Win","Lakers - 1985 (Game 1)<br>FG%: 49.0%<br>3P%: 40.0%<br>Result: Loss","Lakers - 1985 (Game 2)<br>FG%: 47.8%<br>3P%: 50.0%<br>Result: Win","Lakers - 1985 (Game 3)<br>FG%: 54.2%<br>3P%: 50.0%<br>Result: Win","Lakers - 1985 (Game 4)<br>FG%: 48.2%<br>3P%: 25.0%<br>Result: Loss","Lakers - 1985 (Game 5)<br>FG%: 56.8%<br>3P%: 0.0%<br>Result: Win","Lakers - 1985 (Game 6)<br>FG%: 51.2%<br>3P%: 0.0%<br>Result: Win","Celtics - 1986 (Game 1)<br>FG%: 56.0%<br>3P%: 25.0%<br>Result: Win","Celtics - 1986 (Game 2)<br>FG%: 50.0%<br>3P%: 44.4%<br>Result: Win","Celtics - 1986 (Game 3)<br>FG%: 43.8%<br>3P%: 20.0%<br>Result: Loss","Celtics - 1986 (Game 4)<br>FG%: 57.7%<br>3P%: 33.3%<br>Result: Win","Celtics - 1986 (Game 5)<br>FG%: 40.5%<br>3P%: 16.7%<br>Result: Loss","Celtics - 1986 (Game 6)<br>FG%: 48.5%<br>3P%: 50.0%<br>Result: Win","Lakers - 1987 (Game 1)<br>FG%: 55.6%<br>3P%: 20.0%<br>Result: Win","Lakers - 1987 (Game 2)<br>FG%: 61.5%<br>3P%: 75.0%<br>Result: Win","Lakers - 1987 (Game 3)<br>FG%: 49.4%<br>3P%: 27.3%<br>Result: Loss","Lakers - 1987 (Game 4)<br>FG%: 48.2%<br>3P%: 50.0%<br>Result: Win","Lakers - 1987 (Game 5)<br>FG%: 45.3%<br>3P%: 50.0%<br>Result: Loss","Lakers - 1987 (Game 6)<br>FG%: 48.4%<br>3P%: 0.0%<br>Result: Win","Lakers - 1988 (Game 1)<br>FG%: 39.8%<br>3P%: 25.0%<br>Result: Loss","Lakers - 1988 (Game 2)<br>FG%: 45.5%<br>3P%: 50.0%<br>Result: Win","Lakers - 1988 (Game 3)<br>FG%: 51.4%<br>3P%: 33.3%<br>Result: Win","Lakers - 1988 (Game 4)<br>FG%: 40.3%<br>3P%: 0.0%<br>Result: Loss","Lakers - 1988 (Game 5)<br>FG%: 47.4%<br>3P%: 25.0%<br>Result: Loss","Lakers - 1988 (Game 6)<br>FG%: 47.2%<br>3P%: 0.0%<br>Result: Win","Lakers - 1988 (Game 7)<br>FG%: 55.8%<br>3P%: 30.0%<br>Result: Win","Pistons - 1989 (Game 1)<br>FG%: 55.4%<br>3P%: 50.0%<br>Result: Win","Pistons - 1989 (Game 2)<br>FG%: 54.3%<br>3P%: 12.5%<br>Result: Win","Pistons - 1989 (Game 3)<br>FG%: 51.8%<br>3P%: 14.3%<br>Result: Win","Pistons - 1989 (Game 4)<br>FG%: 48.6%<br>3P%: 66.7%<br>Result: Win"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(27,73,101,1)","opacity":0.80000000000000004,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(27,73,101,1)"}},"hoveron":"points","name":"1980s","legendgroup":"1980s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.29999999999999999,0.57099999999999995,0.29999999999999999,0.438,0.66700000000000004,0.14299999999999999,0,0.33300000000000002,0.33300000000000002,0.66700000000000004,0.46700000000000003,0.33300000000000002,0.25,0.33300000000000002,0.33300000000000002,0.46200000000000002,0.375,0.42899999999999999,0.38500000000000001,0.33300000000000002,0.44400000000000001,0.71399999999999997,0.25,0.27300000000000002,0.35299999999999998,0.29999999999999999,0.33300000000000002,0.29399999999999998,0.36399999999999999,0.438,0.35699999999999998,0.36799999999999999,0.40699999999999997,0.26900000000000002,0.19,0.46700000000000003,0.25,0.115,0.35999999999999999,0.375,0.375,0.375,0.21099999999999999,0.40000000000000002,0.35699999999999998,0.188,0.188,0.36399999999999999,0.33300000000000002,0.34999999999999998,0.40000000000000002,0.40000000000000002,0.16700000000000001,0.38500000000000001,0.33300000000000002,0.38500000000000001],"y":[0.374,0.45600000000000002,0.53100000000000003,0.46200000000000002,0.45700000000000002,0.47499999999999998,0.61699999999999999,0.48199999999999998,0.52500000000000002,0.53800000000000003,0.54900000000000004,0.47699999999999998,0.47399999999999998,0.45500000000000002,0.54800000000000004,0.52100000000000002,0.53100000000000003,0.50600000000000001,0.42299999999999999,0.53000000000000003,0.46800000000000003,0.47599999999999998,0.41899999999999998,0.39000000000000001,0.40000000000000002,0.43099999999999999,0.40699999999999997,0.48499999999999999,0.46600000000000003,0.45800000000000002,0.52000000000000002,0.46400000000000002,0.45500000000000002,0.42999999999999999,0.39000000000000001,0.5,0.40000000000000002,0.377,0.39700000000000002,0.44700000000000001,0.46400000000000002,0.44,0.42099999999999999,0.44400000000000001,0.38300000000000001,0.41499999999999998,0.42499999999999999,0.48699999999999999,0.37,0.38700000000000001,0.50700000000000001,0.47799999999999998,0.42899999999999999,0.44600000000000001,0.46700000000000003,0.40300000000000002],"text":["Pistons - 1990 (Game 1)<br>FG%: 37.4%<br>3P%: 30.0%<br>Result: Win","Pistons - 1990 (Game 2)<br>FG%: 45.6%<br>3P%: 57.1%<br>Result: Loss","Pistons - 1990 (Game 3)<br>FG%: 53.1%<br>3P%: 30.0%<br>Result: Win","Pistons - 1990 (Game 4)<br>FG%: 46.2%<br>3P%: 43.8%<br>Result: Win","Pistons - 1990 (Game 5)<br>FG%: 45.7%<br>3P%: 66.7%<br>Result: Win","Bulls - 1991 (Game 1)<br>FG%: 47.5%<br>3P%: 14.3%<br>Result: Loss","Bulls - 1991 (Game 2)<br>FG%: 61.7%<br>3P%: 0.0%<br>Result: Win","Bulls - 1991 (Game 3)<br>FG%: 48.2%<br>3P%: 33.3%<br>Result: Win","Bulls - 1991 (Game 4)<br>FG%: 52.5%<br>3P%: 33.3%<br>Result: Win","Bulls - 1991 (Game 5)<br>FG%: 53.8%<br>3P%: 66.7%<br>Result: Win","Bulls - 1992 (Game 1)<br>FG%: 54.9%<br>3P%: 46.7%<br>Result: Win","Bulls - 1992 (Game 2)<br>FG%: 47.7%<br>3P%: 33.3%<br>Result: Loss","Bulls - 1992 (Game 3)<br>FG%: 47.4%<br>3P%: 25.0%<br>Result: Win","Bulls - 1992 (Game 4)<br>FG%: 45.5%<br>3P%: 33.3%<br>Result: Loss","Bulls - 1992 (Game 5)<br>FG%: 54.8%<br>3P%: 33.3%<br>Result: Win","Bulls - 1992 (Game 6)<br>FG%: 52.1%<br>3P%: 46.2%<br>Result: Win","Bulls - 1993 (Game 1)<br>FG%: 53.1%<br>3P%: 37.5%<br>Result: Win","Bulls - 1993 (Game 2)<br>FG%: 50.6%<br>3P%: 42.9%<br>Result: Win","Bulls - 1993 (Game 3)<br>FG%: 42.3%<br>3P%: 38.5%<br>Result: Loss","Bulls - 1993 (Game 4)<br>FG%: 53.0%<br>3P%: 33.3%<br>Result: Win","Bulls - 1993 (Game 5)<br>FG%: 46.8%<br>3P%: 44.4%<br>Result: Loss","Bulls - 1993 (Game 6)<br>FG%: 47.6%<br>3P%: 71.4%<br>Result: Win","Rockets - 1994 (Game 1)<br>FG%: 41.9%<br>3P%: 25.0%<br>Result: Win","Rockets - 1994 (Game 2)<br>FG%: 39.0%<br>3P%: 27.3%<br>Result: Loss","Rockets - 1994 (Game 3)<br>FG%: 40.0%<br>3P%: 35.3%<br>Result: Win","Rockets - 1994 (Game 4)<br>FG%: 43.1%<br>3P%: 30.0%<br>Result: Loss","Rockets - 1994 (Game 5)<br>FG%: 40.7%<br>3P%: 33.3%<br>Result: Loss","Rockets - 1994 (Game 6)<br>FG%: 48.5%<br>3P%: 29.4%<br>Result: Win","Rockets - 1994 (Game 7)<br>FG%: 46.6%<br>3P%: 36.4%<br>Result: Win","Rockets - 1995 (Game 1)<br>FG%: 45.8%<br>3P%: 43.8%<br>Result: Win","Rockets - 1995 (Game 2)<br>FG%: 52.0%<br>3P%: 35.7%<br>Result: Win","Rockets - 1995 (Game 3)<br>FG%: 46.4%<br>3P%: 36.8%<br>Result: Win","Rockets - 1995 (Game 4)<br>FG%: 45.5%<br>3P%: 40.7%<br>Result: Win","Bulls - 1996 (Game 1)<br>FG%: 43.0%<br>3P%: 26.9%<br>Result: Win","Bulls - 1996 (Game 2)<br>FG%: 39.0%<br>3P%: 19.0%<br>Result: Win","Bulls - 1996 (Game 3)<br>FG%: 50.0%<br>3P%: 46.7%<br>Result: Win","Bulls - 1996 (Game 4)<br>FG%: 40.0%<br>3P%: 25.0%<br>Result: Loss","Bulls - 1996 (Game 5)<br>FG%: 37.7%<br>3P%: 11.5%<br>Result: Loss","Bulls - 1996 (Game 6)<br>FG%: 39.7%<br>3P%: 36.0%<br>Result: Win","Bulls - 1997 (Game 1)<br>FG%: 44.7%<br>3P%: 37.5%<br>Result: Win","Bulls - 1997 (Game 2)<br>FG%: 46.4%<br>3P%: 37.5%<br>Result: Win","Bulls - 1997 (Game 3)<br>FG%: 44.0%<br>3P%: 37.5%<br>Result: Loss","Bulls - 1997 (Game 4)<br>FG%: 42.1%<br>3P%: 21.1%<br>Result: Loss","Bulls - 1997 (Game 5)<br>FG%: 44.4%<br>3P%: 40.0%<br>Result: Win","Bulls - 1997 (Game 6)<br>FG%: 38.3%<br>3P%: 35.7%<br>Result: Win","Bulls - 1998 (Game 1)<br>FG%: 41.5%<br>3P%: 18.8%<br>Result: Loss","Bulls - 1998 (Game 2)<br>FG%: 42.5%<br>3P%: 18.8%<br>Result: Win","Bulls - 1998 (Game 3)<br>FG%: 48.7%<br>3P%: 36.4%<br>Result: Win","Bulls - 1998 (Game 4)<br>FG%: 37.0%<br>3P%: 33.3%<br>Result: Win","Bulls - 1998 (Game 5)<br>FG%: 38.7%<br>3P%: 35.0%<br>Result: Loss","Bulls - 1998 (Game 6)<br>FG%: 50.7%<br>3P%: 40.0%<br>Result: Win","Spurs - 1999 (Game 1)<br>FG%: 47.8%<br>3P%: 40.0%<br>Result: Win","Spurs - 1999 (Game 2)<br>FG%: 42.9%<br>3P%: 16.7%<br>Result: Win","Spurs - 1999 (Game 3)<br>FG%: 44.6%<br>3P%: 38.5%<br>Result: Loss","Spurs - 1999 (Game 4)<br>FG%: 46.7%<br>3P%: 33.3%<br>Result: Win","Spurs - 1999 (Game 5)<br>FG%: 40.3%<br>3P%: 38.5%<br>Result: Win"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(95,168,211,1)","opacity":0.80000000000000004,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(95,168,211,1)"}},"hoveron":"points","name":"1990s","legendgroup":"1990s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.25,0.46700000000000003,0.41199999999999998,0.33300000000000002,0.21099999999999999,0.58799999999999997,0.46200000000000002,0.25,0.40000000000000002,0.52600000000000002,0.70599999999999996,0.10000000000000001,0.56299999999999994,0.5,0.57899999999999996,0.40000000000000002,0.38500000000000001,0.5,0.222,0.33300000000000002,0.20000000000000001,0.5,0.5,0.33300000000000002,0.154,0.14299999999999999,0.308,0.45800000000000002,0.47099999999999997,0.33300000000000002,0.40000000000000002,0.28599999999999998,0.63600000000000001,0.25,0.41199999999999998,0.28599999999999998,0.36799999999999999,0.41199999999999998,0.111,0.375,0.33300000000000002,0.52600000000000002,0.26300000000000001,0.316,0.64300000000000002,0.44400000000000001,0.36399999999999999,0.36399999999999999,0.5,0.33300000000000002,0.33300000000000002,0.34799999999999998,0.34799999999999998,0.5],"y":[0.51100000000000001,0.47999999999999998,0.5,0.51600000000000001,0.40000000000000002,0.47799999999999998,0.44400000000000001,0.46899999999999997,0.46700000000000003,0.5,0.45100000000000001,0.45800000000000002,0.5,0.54400000000000004,0.52100000000000002,0.49399999999999999,0.48499999999999999,0.41799999999999998,0.28899999999999998,0.47799999999999998,0.45900000000000002,0.46200000000000002,0.39500000000000002,0.40799999999999997,0.42599999999999999,0.46100000000000002,0.42999999999999999,0.46800000000000003,0.433,0.371,0.46300000000000002,0.41299999999999998,0.42599999999999999,0.436,0.41399999999999998,0.48699999999999999,0.51500000000000001,0.44900000000000001,0.44900000000000001,0.45300000000000001,0.48099999999999998,0.41199999999999998,0.42599999999999999,0.42099999999999999,0.52900000000000003,0.34899999999999998,0.45200000000000001,0.42899999999999999,0.49399999999999999,0.46100000000000002,0.46200000000000002,0.51300000000000001,0.41799999999999998,0.438],"text":["Lakers - 2000 (Game 1)<br>FG%: 51.1%<br>3P%: 25.0%<br>Result: Win","Lakers - 2000 (Game 2)<br>FG%: 48.0%<br>3P%: 46.7%<br>Result: Win","Lakers - 2000 (Game 3)<br>FG%: 50.0%<br>3P%: 41.2%<br>Result: Loss","Lakers - 2000 (Game 4)<br>FG%: 51.6%<br>3P%: 33.3%<br>Result: Win","Lakers - 2000 (Game 5)<br>FG%: 40.0%<br>3P%: 21.1%<br>Result: Loss","Lakers - 2000 (Game 6)<br>FG%: 47.8%<br>3P%: 58.8%<br>Result: Win","Lakers - 2001 (Game 1)<br>FG%: 44.4%<br>3P%: 46.2%<br>Result: Loss","Lakers - 2001 (Game 2)<br>FG%: 46.9%<br>3P%: 25.0%<br>Result: Win","Lakers - 2001 (Game 3)<br>FG%: 46.7%<br>3P%: 40.0%<br>Result: Win","Lakers - 2001 (Game 4)<br>FG%: 50.0%<br>3P%: 52.6%<br>Result: Win","Lakers - 2001 (Game 5)<br>FG%: 45.1%<br>3P%: 70.6%<br>Result: Win","Lakers - 2002 (Game 1)<br>FG%: 45.8%<br>3P%: 10.0%<br>Result: Win","Lakers - 2002 (Game 2)<br>FG%: 50.0%<br>3P%: 56.3%<br>Result: Win","Lakers - 2002 (Game 3)<br>FG%: 54.4%<br>3P%: 50.0%<br>Result: Win","Lakers - 2002 (Game 4)<br>FG%: 52.1%<br>3P%: 57.9%<br>Result: Win","Spurs - 2003 (Game 1)<br>FG%: 49.4%<br>3P%: 40.0%<br>Result: Win","Spurs - 2003 (Game 2)<br>FG%: 48.5%<br>3P%: 38.5%<br>Result: Loss","Spurs - 2003 (Game 3)<br>FG%: 41.8%<br>3P%: 50.0%<br>Result: Win","Spurs - 2003 (Game 4)<br>FG%: 28.9%<br>3P%: 22.2%<br>Result: Loss","Spurs - 2003 (Game 5)<br>FG%: 47.8%<br>3P%: 33.3%<br>Result: Win","Spurs - 2003 (Game 6)<br>FG%: 45.9%<br>3P%: 20.0%<br>Result: Win","Pistons - 2004 (Game 1)<br>FG%: 46.2%<br>3P%: 50.0%<br>Result: Win","Pistons - 2004 (Game 2)<br>FG%: 39.5%<br>3P%: 50.0%<br>Result: Loss","Pistons - 2004 (Game 3)<br>FG%: 40.8%<br>3P%: 33.3%<br>Result: Win","Pistons - 2004 (Game 4)<br>FG%: 42.6%<br>3P%: 15.4%<br>Result: Win","Pistons - 2004 (Game 5)<br>FG%: 46.1%<br>3P%: 14.3%<br>Result: Win","Spurs - 2005 (Game 1)<br>FG%: 43.0%<br>3P%: 30.8%<br>Result: Win","Spurs - 2005 (Game 2)<br>FG%: 46.8%<br>3P%: 45.8%<br>Result: Win","Spurs - 2005 (Game 3)<br>FG%: 43.3%<br>3P%: 47.1%<br>Result: Loss","Spurs - 2005 (Game 4)<br>FG%: 37.1%<br>3P%: 33.3%<br>Result: Loss","Spurs - 2005 (Game 5)<br>FG%: 46.3%<br>3P%: 40.0%<br>Result: Win","Spurs - 2005 (Game 6)<br>FG%: 41.3%<br>3P%: 28.6%<br>Result: Loss","Spurs - 2005 (Game 7)<br>FG%: 42.6%<br>3P%: 63.6%<br>Result: Win","Heat - 2006 (Game 1)<br>FG%: 43.6%<br>3P%: 25.0%<br>Result: Loss","Heat - 2006 (Game 2)<br>FG%: 41.4%<br>3P%: 41.2%<br>Result: Loss","Heat - 2006 (Game 3)<br>FG%: 48.7%<br>3P%: 28.6%<br>Result: Win","Heat - 2006 (Game 4)<br>FG%: 51.5%<br>3P%: 36.8%<br>Result: Win","Heat - 2006 (Game 5)<br>FG%: 44.9%<br>3P%: 41.2%<br>Result: Win","Heat - 2006 (Game 6)<br>FG%: 44.9%<br>3P%: 11.1%<br>Result: Win","Spurs - 2007 (Game 1)<br>FG%: 45.3%<br>3P%: 37.5%<br>Result: Win","Spurs - 2007 (Game 2)<br>FG%: 48.1%<br>3P%: 33.3%<br>Result: Win","Spurs - 2007 (Game 3)<br>FG%: 41.2%<br>3P%: 52.6%<br>Result: Win","Spurs - 2007 (Game 4)<br>FG%: 42.6%<br>3P%: 26.3%<br>Result: Win","Celtics - 2008 (Game 1)<br>FG%: 42.1%<br>3P%: 31.6%<br>Result: Win","Celtics - 2008 (Game 2)<br>FG%: 52.9%<br>3P%: 64.3%<br>Result: Win","Celtics - 2008 (Game 3)<br>FG%: 34.9%<br>3P%: 44.4%<br>Result: Loss","Celtics - 2008 (Game 4)<br>FG%: 45.2%<br>3P%: 36.4%<br>Result: Win","Celtics - 2008 (Game 5)<br>FG%: 42.9%<br>3P%: 36.4%<br>Result: Loss","Celtics - 2008 (Game 6)<br>FG%: 49.4%<br>3P%: 50.0%<br>Result: Win","Lakers - 2009 (Game 1)<br>FG%: 46.1%<br>3P%: 33.3%<br>Result: Win","Lakers - 2009 (Game 2)<br>FG%: 46.2%<br>3P%: 33.3%<br>Result: Win","Lakers - 2009 (Game 3)<br>FG%: 51.3%<br>3P%: 34.8%<br>Result: Loss","Lakers - 2009 (Game 4)<br>FG%: 41.8%<br>3P%: 34.8%<br>Result: Win","Lakers - 2009 (Game 5)<br>FG%: 43.8%<br>3P%: 50.0%<br>Result: Win"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(190,233,232,1)","opacity":0.80000000000000004,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(190,233,232,1)"}},"hoveron":"points","name":"2000s","legendgroup":"2000s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.40000000000000002,0.22700000000000001,0.13300000000000001,0.34999999999999998,0.36799999999999999,0.316,0.20000000000000001,0.40899999999999997,0.35299999999999998,0.38100000000000001,0.21099999999999999,0.68400000000000005,0.42299999999999999,0.42099999999999999,0.42899999999999999,0.308,0.38500000000000001,0.53800000000000003,0.32000000000000001,0.52600000000000002,0.44400000000000001,0.33300000000000002,0.47799999999999998,0.57899999999999996,0.375,0.52000000000000002,0.46200000000000002,0.45000000000000001,0.42899999999999999,0.46200000000000002,0.37,0.22900000000000001,0.35299999999999998,0.40000000000000002,0.46200000000000002,0.38200000000000001,0.33300000000000002,0.217,0.47999999999999998,0.23999999999999999,0.41699999999999998,0.37,0.23999999999999999,0.36399999999999999,0.41899999999999998,0.48499999999999999,0.28199999999999997,0.36799999999999999,0.36099999999999999,0.41699999999999998,0.34599999999999997,0.36799999999999999],"y":[0.48699999999999999,0.40799999999999997,0.44700000000000001,0.45100000000000001,0.39700000000000002,0.41799999999999998,0.32500000000000001,0.373,0.47999999999999998,0.40000000000000002,0.39700000000000002,0.56499999999999995,0.5,0.46200000000000002,0.47399999999999998,0.378,0.48099999999999998,0.51900000000000002,0.436,0.49399999999999999,0.40799999999999997,0.52900000000000003,0.42999999999999999,0.46899999999999997,0.439,0.58799999999999997,0.439,0.59399999999999997,0.57099999999999995,0.47399999999999998,0.443,0.39800000000000002,0.40000000000000002,0.46800000000000003,0.47999999999999998,0.435,0.38100000000000001,0.35399999999999998,0.52700000000000002,0.46899999999999997,0.53000000000000003,0.51900000000000002,0.40200000000000002,0.42499999999999999,0.51700000000000002,0.48199999999999998,0.44800000000000001,0.51100000000000001,0.51100000000000001,0.57299999999999995,0.51900000000000002,0.45300000000000001],"text":["Lakers - 2010 (Game 1)<br>FG%: 48.7%<br>3P%: 40.0%<br>Result: Win","Lakers - 2010 (Game 2)<br>FG%: 40.8%<br>3P%: 22.7%<br>Result: Loss","Lakers - 2010 (Game 3)<br>FG%: 44.7%<br>3P%: 13.3%<br>Result: Win","Lakers - 2010 (Game 4)<br>FG%: 45.1%<br>3P%: 35.0%<br>Result: Loss","Lakers - 2010 (Game 5)<br>FG%: 39.7%<br>3P%: 36.8%<br>Result: Loss","Lakers - 2010 (Game 6)<br>FG%: 41.8%<br>3P%: 31.6%<br>Result: Win","Lakers - 2010 (Game 7)<br>FG%: 32.5%<br>3P%: 20.0%<br>Result: Win","Mavericks - 2011 (Game 1)<br>FG%: 37.3%<br>3P%: 40.9%<br>Result: Loss","Mavericks - 2011 (Game 2)<br>FG%: 48.0%<br>3P%: 35.3%<br>Result: Win","Mavericks - 2011 (Game 3)<br>FG%: 40.0%<br>3P%: 38.1%<br>Result: Loss","Mavericks - 2011 (Game 4)<br>FG%: 39.7%<br>3P%: 21.1%<br>Result: Win","Mavericks - 2011 (Game 5)<br>FG%: 56.5%<br>3P%: 68.4%<br>Result: Win","Mavericks - 2011 (Game 6)<br>FG%: 50.0%<br>3P%: 42.3%<br>Result: Win","Heat - 2012 (Game 1)<br>FG%: 46.2%<br>3P%: 42.1%<br>Result: Loss","Heat - 2012 (Game 2)<br>FG%: 47.4%<br>3P%: 42.9%<br>Result: Win","Heat - 2012 (Game 3)<br>FG%: 37.8%<br>3P%: 30.8%<br>Result: Win","Heat - 2012 (Game 4)<br>FG%: 48.1%<br>3P%: 38.5%<br>Result: Win","Heat - 2012 (Game 5)<br>FG%: 51.9%<br>3P%: 53.8%<br>Result: Win","Heat - 2012 (Game 1)<br>FG%: 43.6%<br>3P%: 32.0%<br>Result: Loss","Heat - 2013 (Game 2)<br>FG%: 49.4%<br>3P%: 52.6%<br>Result: Win","Heat - 2013 (Game 3)<br>FG%: 40.8%<br>3P%: 44.4%<br>Result: Loss","Heat - 2013 (Game 4)<br>FG%: 52.9%<br>3P%: 33.3%<br>Result: Win","Heat - 2013 (Game 5)<br>FG%: 43.0%<br>3P%: 47.8%<br>Result: Loss","Heat - 2013 (Game 6)<br>FG%: 46.9%<br>3P%: 57.9%<br>Result: Win","Heat - 2013 (Game 7)<br>FG%: 43.9%<br>3P%: 37.5%<br>Result: Win","Spurs - 2014 (Game 1)<br>FG%: 58.8%<br>3P%: 52.0%<br>Result: Win","Spurs - 2014 (Game 2)<br>FG%: 43.9%<br>3P%: 46.2%<br>Result: Loss","Spurs - 2014 (Game 3)<br>FG%: 59.4%<br>3P%: 45.0%<br>Result: Win","Spurs - 2014 (Game 4)<br>FG%: 57.1%<br>3P%: 42.9%<br>Result: Win","Spurs - 2014 (Game 5)<br>FG%: 47.4%<br>3P%: 46.2%<br>Result: Win","Warriors - 2015 (Game 1)<br>FG%: 44.3%<br>3P%: 37.0%<br>Result: Win","Warriors - 2015 (Game 2)<br>FG%: 39.8%<br>3P%: 22.9%<br>Result: Loss","Warriors - 2015 (Game 3)<br>FG%: 40.0%<br>3P%: 35.3%<br>Result: Loss","Warriors - 2015 (Game 4)<br>FG%: 46.8%<br>3P%: 40.0%<br>Result: Win","Warriors - 2015 (Game 5)<br>FG%: 48.0%<br>3P%: 46.2%<br>Result: Win","Warriors - 2015 (Game 6)<br>FG%: 43.5%<br>3P%: 38.2%<br>Result: Win","Cavaliers - 2016 (Game 1)<br>FG%: 38.1%<br>3P%: 33.3%<br>Result: Loss","Cavaliers - 2016 (Game 2)<br>FG%: 35.4%<br>3P%: 21.7%<br>Result: Loss","Cavaliers - 2016 (Game 3)<br>FG%: 52.7%<br>3P%: 48.0%<br>Result: Win","Cavaliers - 2016 (Game 4)<br>FG%: 46.9%<br>3P%: 24.0%<br>Result: Loss","Cavaliers - 2016 (Game 5)<br>FG%: 53.0%<br>3P%: 41.7%<br>Result: Win","Cavaliers - 2016 (Game 6)<br>FG%: 51.9%<br>3P%: 37.0%<br>Result: Win","Cavaliers - 2016 (Game 7)<br>FG%: 40.2%<br>3P%: 24.0%<br>Result: Win","Warriors - 2017 (Game 1)<br>FG%: 42.5%<br>3P%: 36.4%<br>Result: Win","Warriors - 2017 (Game 2)<br>FG%: 51.7%<br>3P%: 41.9%<br>Result: Win","Warriors - 2017 (Game 3)<br>FG%: 48.2%<br>3P%: 48.5%<br>Result: Win","Warriors - 2017 (Game 4)<br>FG%: 44.8%<br>3P%: 28.2%<br>Result: Loss","Warriors - 2017 (Game 5)<br>FG%: 51.1%<br>3P%: 36.8%<br>Result: Win","Warriors - 2018 (Game 1)<br>FG%: 51.1%<br>3P%: 36.1%<br>Result: Win","Warriors - 2018 (Game 2)<br>FG%: 57.3%<br>3P%: 41.7%<br>Result: Win","Warriors - 2018 (Game 3)<br>FG%: 51.9%<br>3P%: 34.6%<br>Result: Win","Warriors - 2018 (Game 4)<br>FG%: 45.3%<br>3P%: 36.8%<br>Result: Win"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(239,86,117,1)","opacity":0.80000000000000004,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(239,86,117,1)"}},"hoveron":"points","name":"2010s","legendgroup":"2010s","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":47.083437110834375,"r":7.9701120797011216,"b":44.632627646326284,"l":47.023661270236623},"font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243},"title":{"text":"Field Goal % vs Three-Point % by Game, NBA Champions 1980-2018","font":{"color":"rgba(0,0,0,1)","family":"","size":19.128268991282692},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.050000000000000003,1.05],"tickmode":"array","ticktext":["0%","25%","50%","75%","100%"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0%","25%","50%","75%","100%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.9850560398505608,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.72455564360919278,"zeroline":false,"anchor":"y","title":{"text":"Three-Point Percentage","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.27259999999999995,0.63339999999999996],"tickmode":"array","ticktext":["30%","40%","50%","60%"],"tickvals":[0.30000000000000004,0.40000000000000002,0.5,0.60000000000000009],"categoryorder":"array","categoryarray":["30%","40%","50%","60%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.9850560398505608,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.72455564360919289,"zeroline":false,"anchor":"x","title":{"text":"Field Goal Percentage","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"hoverformat":".2f"},"shapes":[],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":12.7521793275218},"title":{"text":"Decade","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"orientation":"h","y":-0.20000000000000001},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"467449363403":{"x":{},"y":{},"colour":{},"text":{},"type":"scatter"}},"cur_data":"467449363403","visdat":{"467449363403":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

```r
# save a self-contained standalone version of this widget as required
saveWidget(p2, "figures/interactive_shooting_profile.html", selfcontained = TRUE)
```

**Interpretation:** Hovering across the points reveals two patterns that line up with the static trend chart but add a layer of detail it can't show on its own.

**The 1980s points cluster tightly on the left side of the chart.** Since teams barely attempted threes during that decade, three-point percentage for those games is either undefined or based on a tiny sample size, which pushes the dark blue dots toward the low end of the x-axis. Despite that clustering on one axis, their field goal percentages are spread out fairly widely on the y-axis, which suggests three-point shooting wasn't yet a meaningful predictor of how efficiently a team scored overall.

**The more recent decades shift noticeably to the right.** As three-point percentage becomes a more central part of a team's offensive identity, the 2000s and especially the 2010s points move rightward across the chart. Most of those points also land at fairly high field goal percentages, which makes sense: a team that is good at scoring efficiently from the field tends to also be good at knocking down threes, rather than one skill coming at the expense of the other.

---

## Dataset 2: FIFA 18 Player Ratings

For the second dataset, the project shifts entirely away from basketball and toward a simple modeling question: among a handful of skill attributes, which ones matter most for a soccer player's overall FIFA 18 rating?

### Cleaning and Filtering

The dataset has separate columns for goalkeeper-specific skills (`gk_diving`, `gk_reflexes`, and similar) and outfield skills. Because goalkeepers are rated on a completely different set of attributes, they were filtered out before fitting any model, so they wouldn't muddy the relationship between outfield skills and overall rating. The cutoff used was `gk_diving < 30`, since outfield players score very low on goalkeeper-specific stats while actual goalkeepers score much higher.


```r
fifa <- read.csv("../data/fifa18.csv")

outfield <- fifa %>% filter(gk_diving < 30)
cat("Outfield players:", nrow(outfield), "out of", nrow(fifa), "total\n")
```

```
## Outfield players: 15145 out of 17076 total
```

### Variable Notes

| Variable | Description |
|---|---|
| `overall` | Player's overall FIFA 18 rating (the response variable) |
| `reactions` | How quickly a player responds to events on the pitch |
| `ball_control` | Ability to control the ball under pressure |
| `short_passing` | Accuracy on short passes |
| `stamina` | Ability to sustain effort over 90 minutes |
| `strength` | Physical strength in duels |
| `vision` | Awareness of teammates' positioning and passing lanes |
| `finishing` | Accuracy when shooting on goal |
| `sprint_speed` | Top running speed |

### Visualizations

#### Plot 3: What Predicts a FIFA 18 Player's Overall Rating? A Linear Model of Eight Skill Attributes

A linear model was fit predicting `overall` rating from eight skill attributes that seemed like reasonable predictors, then `broom::tidy()` was used to pull out the coefficients and their confidence intervals for plotting.


```r
model <- lm(
  overall ~ reactions + ball_control + short_passing + stamina +
    strength + vision + finishing + sprint_speed,
  data = outfield
)

# Model summary stored silently; R-squared is extracted below for the subtitle
model_summary <- summary(model)

coefs <- tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term,
      reactions = "Reactions",
      ball_control = "Ball Control",
      short_passing = "Short Passing",
      stamina = "Stamina",
      strength = "Strength",
      vision = "Vision",
      finishing = "Finishing",
      sprint_speed = "Sprint Speed"
    ),
    term = reorder(term, estimate)
  )

r2 <- round(summary(model)$r.squared, 3)

p3 <- ggplot(coefs, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high, color = estimate > 0),
    size = 0.7, linewidth = 0.9
  ) +
  scale_color_manual(values = c("TRUE" = accent_color, "FALSE" = highlight_color), guide = "none") +
  labs(
    title = "What Predicts a FIFA 18 Player's Overall Rating?",
    subtitle = paste0("Linear model coefficients with 95% confidence intervals (R-squared = ", r2, ")"),
    x = "Effect on Overall Rating (points)", y = NULL,
    caption = "Source: FIFA 18 player attributes, outfield players only (n = 15,145)"
  ) +
  theme_owen()

p3
```

<img src="https://github.com/OwenTelis/dataviz_final_project/blob/main/figures/fifa-model-1.png" style="display: block; margin: auto;" />

**Interpretation:** The coefficient plot makes it easy to see which attributes are actually pulling weight in the model and which ones aren't.

**The model fits surprisingly well for just eight predictors.** It explains about 81% of the variation in overall rating, which is a higher R-squared than expected going in given how simple the model is.

**Reactions is by far the strongest predictor,** with a confidence interval that sits well clear of zero and far ahead of every other attribute on the chart. That actually lines up with something FIFA players say constantly online: that the "reactions" stat is the hidden key driving a player's overall rating more than any single visible skill.

**Ball control and short passing also push the rating up a meaningful amount**, though nowhere near as much as reactions. Stamina, strength, and sprint speed matter too, but their effects are smaller and their confidence intervals sit much closer to zero.

**Vision and finishing actually come out slightly negative** once everything else in the model is held constant, which was the most surprising result of the three datasets. The likely explanation is multicollinearity: those two stats are highly correlated with the other predictors already in the model, so once reactions and ball control are accounted for, there isn't much unique information left in vision or finishing to explain additional variation in overall rating. It's a useful reminder that a negative coefficient in a model with correlated predictors doesn't necessarily mean the raw relationship is negative; it just means that variable isn't adding anything new once the rest of the model is already in place.

---

## Dataset 3: Florida Lakes (Spatial Visualization)

The last dataset is a shapefile of lake polygons across Florida, read in with the `sf` package. A simple state outline from the `maps` package was pulled in alongside it just to give the lakes some geographic context instead of floating in blank space.

### Data Overview


```r
lakes <- st_read("../data/Florida_Lakes/Florida_Lakes.shp", quiet = TRUE)
fl_outline <- st_as_sf(map("state", "florida", plot = FALSE, fill = TRUE))

# A handful of lake polygons have minor digitizing defects (duplicate vertices)
# that the strict spherical (S2) geometry engine sf uses by default can't
# tolerate. Switching to the classic planar (GEOS) engine and repairing the
# geometries here, once, at read-in, means every later operation on
# `lakes` (centroids, joins, plotting) works without re-fixing it each time.
sf::sf_use_s2(FALSE)
lakes <- st_make_valid(lakes)

st_crs(lakes)$input
```

```
## [1] "WGS 84"
```

```r
nrow(lakes)
```

```
## [1] 4243
```

### Variable Notes

| Variable | Description |
|---|---|
| `NAME` | Lake name |
| `COUNTY` | Florida county the lake is located in |
| `SHAPEAREA` | Surface area of the lake polygon |
| `geometry` | Polygon boundary used for spatial plotting |

### Summarizing the Data


```r
lakes_by_county <- lakes %>%
  st_drop_geometry() %>%
  count(COUNTY, sort = TRUE)

head(lakes_by_county, 5)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["COUNTY"],"name":[1],"type":["chr"],"align":["left"]},{"label":["n"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"LAKE","2":"396","_rn_":"1"},{"1":"ORANGE","2":"365","_rn_":"2"},{"1":"POLK","2":"309","_rn_":"3"},{"1":"WASHINGTON","2":"228","_rn_":"4"},{"1":"MARION","2":"199","_rn_":"5"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
largest <- lakes %>% filter(SHAPEAREA == max(SHAPEAREA))
largest %>% st_drop_geometry() %>% select(NAME, COUNTY, SHAPEAREA)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["NAME"],"name":[1],"type":["chr"],"align":["left"]},{"label":["COUNTY"],"name":[2],"type":["chr"],"align":["left"]},{"label":["SHAPEAREA"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Lake Okeechobee","2":"PALM BEACH","3":"1296120234","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
centroid <- st_centroid(largest)
```

There are 4,243 lakes in this shapefile spread across 67 counties. Lake County has the most individual lakes (396), followed closely by Orange County (365), which tracks with how both counties are packed with small natural lakes in central Florida. The biggest lake by far, however, is Lake Okeechobee in Palm Beach County, and it isn't a close contest.

### Visualizations

#### Plot 4: Lakes of Florida: County-Level Lake Density with Individual Lakes Overlaid

The first version of this map colored each lake polygon by its own surface area. The problem is that most of Florida's 4,243 lakes are small enough that their fill color barely registers at state scale, so the chart ended up showing roughly where lakes are, but not the regional concentration pattern that the summary table already hinted at. This version fixes that by adding a county-level choropleth of lake counts underneath the actual lake shapes, so the map can show fine-grained geography and the regional pattern at the same time.


```r
# County-level lake counts via a spatial join (point-in-polygon) rather than a
# name-based join. County name spelling/formatting doesn't always agree between
# independently-sourced files (e.g. "Miami-Dade" vs. "Dade"), so matching on
# actual location instead of text avoids that problem entirely.
fl_counties <- st_as_sf(map("county", "florida", plot = FALSE, fill = TRUE)) %>%
  st_set_crs(st_crs(lakes))

lake_centroids <- st_centroid(lakes)

county_lake_counts <- st_join(fl_counties, lake_centroids) %>%
  st_drop_geometry() %>%
  count(ID, name = "n_lakes")

fl_counties <- fl_counties %>%
  left_join(county_lake_counts, by = "ID")

p4 <- ggplot() +
  geom_sf(data = fl_counties, aes(fill = n_lakes), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "mako", direction = -1, na.value = "grey90",
    name = "Lakes per\nCounty"
  ) +
  geom_sf(data = lakes, fill = "#bcdfe4", color = "#1b4965", linewidth = 0.05, alpha = 0.9) +
  geom_sf(data = largest, fill = highlight_color, color = highlight_color, linewidth = 0.5) +
  annotate(
    "label", x = st_coordinates(centroid)[1] - 1.6, y = st_coordinates(centroid)[2] + 1.0,
    label = "Lake Okeechobee\n(largest lake in FL)",
    size = 3.2, fontface = "bold", color = highlight_color, fill = "white", label.size = 0
  ) +
  labs(
    title = "Lakes of Florida",
    subtitle = paste0("All ", nrow(lakes), " mapped lake polygons, layered over a county-level choropleth of lake counts"),
    caption = "Source: Florida Lakes shapefile; county boundaries from the maps package"
  ) +
  theme_owen_map()

p4
```

<img src="https://github.com/OwenTelis/dataviz_final_project/blob/main/figures/lakes-map-1.png" style="display: block; margin: auto;" />

**Interpretation:** This version answers a question the original chart could only gesture at: not just that lake size varies a lot, but specifically where lakes are concentrated.

**The choropleth background does what coloring individual polygons by area couldn't.** Most lakes are too small to register a perceptible fill color at state scale, so the original area-based encoding mostly dissolved into a sea of similarly dark specks. Aggregating counts up to the county level and using that as the background fill makes the central Florida concentration, led by Lake County (396 lakes) and Orange County (365 lakes), visible at a glance instead of something that had to be read off a table.

**The county counts come from a spatial join, not a name match.** Joining lake centroids to county polygons with `st_join()` means the result depends on where each lake actually sits, not on whether `COUNTY` in the shapefile happens to be spelled and capitalized the same way as the county names baked into the `maps` package boundary data.

**Lake Okeechobee is now highlighted directly on the map** in the report's highlight color, rather than only being called out through a label pointing at an otherwise unremarkable blob. The other 4,242 lake polygons are still drawn in a light water-blue on top of the choropleth, so their individual shapes and locations remain visible: nothing about the fine-grained geography was traded away to get the regional pattern.

---

## Report & Discussion

### What Was Originally Planned?

The original plan when looking was to possibly use only the NBA dataset for everything: an annotated trend chart, an interactive version of essentially the same chart, and a model predicting wins from team stats. After working with it for a bit, it became clear that predicting wins from box score stats *from the same game* is somewhat circular, since a team wins partly because it shot well, so a model built on that data doesn't really reveal anything that wasn't already obvious. The modeling piece was switched over to the FIFA dataset instead, which produced a cleaner and more interesting question.

The cleaning work itself ended up being lighter across all three datasets than expected going in. The NBA data needed two text typos fixed in the team names, plus remembering to use `na.rm = TRUE` for the handful of games with a missing three-point percentage. The FIFA data had no missing values at all, but did require filtering out goalkeepers before the model would make sense. The lakes shapefile needed no cleaning whatsoever, just reading it in correctly with `sf` and pairing it with a state outline so the map would have geographic context.

### What Story Could You Tell With Your Plots?

The clearest story across this whole project is the three-point trend in the NBA data. Going from less than one attempt a game in 1980 to 37 attempts a game for the championship team alone by 2017 is a dramatic shift, and seeing it as a line chart makes it land in a way that simply hearing "the league shoots more threes now" doesn't really capture. The FIFA model tells a smaller but still genuinely interesting story about what a single stat, reactions, hides inside it once correlated predictors are accounted for. The lakes map started out as more of a "here's where things are" reference chart, but after adding the county-level choropleth it tells a real story too: lakes in Florida aren't spread evenly across the state, they're heavily concentrated in a handful of central counties.

### Challenges and Possible Extensions

The biggest difficulty throughout the project was getting annotation labels to avoid overlapping with the data or getting cut off at the edge of the plot. The x and y position of the text labels had to be nudged manually several times in both the NBA chart and the lakes map before they landed somewhere clean. A second difficulty was settling on a CRS and projection setup for the spatial plot that actually lined the lake polygons up with the state outline correctly, since the two layers came from different sources and the coordinate systems had to be double-checked before plotting them together.

The lakes map originally just plotted raw lake polygons colored by area, which is why a county-level choropleth of lake counts (joined spatially rather than by county name) got added afterward: it directly shows the regional concentration that the summary table could only describe. The next step worth trying would be normalizing those counts by county land area to get an actual lake density (lakes per square mile), since a county that's simply bigger will tend to rack up more total lakes even if it isn't unusually lake-dense. For the NBA data, pulling in regular season stats in addition to Finals data would be an interesting follow-up, to see whether the three-point trend looks the same outside of championship-level basketball or whether Finals teams run ahead of or behind the league average.

### How Were the Principles of Data Visualization Applied?

A few core principles came into play repeatedly while designing these four charts:

- **Purposeful, consistent color:** The same dark blue (`theme_owen()`'s accent color) and the same pink/red highlight color were reused across both the NBA trend chart and the FIFA coefficient plot, so a highlighted point or bar means the same thing visually everywhere it appears in the report.
- **Appropriate chart types:** A line chart fit the NBA trend over time. A scatterplot fit the interactive comparison of two continuous variables with extra detail available on hover. A coefficient plot (point-range) fit the model output, since it shows both the estimate and its uncertainty at once. A shaded map fit the spatial lake data.
- **Restrained annotation:** Direct labels were used only on the two charts where they genuinely helped guide interpretation, the 2017 Warriors point and Lake Okeechobee, instead of cluttering every plot with text.
- **An encoding chosen to match what's actually perceivable:** The lakes map originally colored individual lake polygons by surface area, but most lakes are too small for a fill color to register at state scale. Aggregating up to a county-level choropleth was a direct response to that problem, not just the next palette option to try.
- **Minimal, cohesive theming:** Every static plot routes through the same `theme_owen()` or `theme_owen_map()` function, so the whole report reads as one consistent piece of work rather than four separately styled charts.

---

## Conclusion

Three datasets with nothing in common on the surface, basketball box scores, soccer player ratings, and lake polygons, still ended up requiring the same disciplined approach: a consistent visual language, a chart type chosen to fit the shape of the data rather than habit, and annotations used sparingly enough to actually help rather than clutter. The NBA data tells the most dramatic story, with championship teams going from barely shooting threes in 1980 to averaging 37 attempts a game by 2017. The FIFA model tells a quieter but more conceptually interesting story, where reactions dominates the prediction of overall rating and a couple of seemingly important stats, vision and finishing, turn up negative once correlated predictors are controlled for. The lakes map ended up being the chart that changed the most during revision, moving from individual lake polygons colored by area, a fill that barely showed up at state scale, to a county-level choropleth that finally makes the central Florida concentration around Lake and Orange counties visible at a glance. If there's one throughline connecting all three, it's that the right visualization choice almost always comes down to actually looking at the shape of the data first, whether that means a log scale, a point-range plot, a hover tooltip, or, in the lakes map's case, going back and rethinking what should even be encoded, rather than reaching for whatever chart type is most familiar.
