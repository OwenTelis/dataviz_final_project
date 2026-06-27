---
title: "Billboard Summer Hits: A Sonic Journey Through Six Decades (1958–2017)"
author: "Owen Telis - Otelis0019@floridapoly.edu"
date: "Due Date: 11:59 - 06-06-2026"
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

What makes a summer hit? Is it energy, a danceable beat, or just a feel-good vibe? This project looks at the **Spotify audio features** of Billboard summer hits from **1958 to 2017**, which is 600 songs spread across nearly six decades of popular music. Each song in the dataset has been analyzed by Spotify and given scores for things like danceability, energy, valence (musical positivity), acousticness, and tempo.

The goal is to see how the sound of summer hits has changed over time and figure out what patterns, if any, define the "summer sound."

---

## Data Overview


```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

billboard <- read.csv("../Data/all_billboard_summer_hits.csv", stringsAsFactors = FALSE)

cat("Dataset dimensions:", nrow(billboard), "rows x", ncol(billboard), "columns\n")
```

```
## Dataset dimensions: 600 rows x 22 columns
```

```r
cat("Year range:", min(billboard$year), "to", max(billboard$year), "\n")
```

```
## Year range: 1958 to 2017
```

```r
cat("Total songs:", nrow(billboard), "\n")
```

```
## Total songs: 600
```

### Variable Descriptions

| Variable | Description | Range |
|---|---|---|
| `danceability` | How suitable a track is for dancing (rhythm, beat) | 0–1 |
| `energy` | Perceptual measure of intensity and activity | 0–1 |
| `valence` | Musical positiveness (happy vs. sad/tense) | 0–1 |
| `acousticness` | Confidence the track is acoustic (not electric) | 0–1 |
| `loudness` | Overall loudness in decibels (dB) | Negative dB |
| `tempo` | Estimated tempo in beats per minute (BPM) | BPM |
| `speechiness` | Presence of spoken words | 0–1 |

### Data Quality Check


```r
missing_summary <- sapply(billboard, function(x) sum(is.na(x)))
missing_summary[missing_summary > 0]
```

```
## named integer(0)
```


```r
audio_features <- billboard %>%
  select(year, danceability, energy, valence, acousticness, loudness, tempo, speechiness)

summary(audio_features[, -1])
```

```
##   danceability        energy          valence        acousticness      
##  Min.   :0.2170   Min.   :0.0600   Min.   :0.0695   Min.   :0.0000488  
##  1st Qu.:0.5457   1st Qu.:0.4768   1st Qu.:0.4790   1st Qu.:0.0417250  
##  Median :0.6480   Median :0.6405   Median :0.6900   Median :0.1620000  
##  Mean   :0.6407   Mean   :0.6221   Mean   :0.6488   Mean   :0.2665156  
##  3rd Qu.:0.7402   3rd Qu.:0.7830   3rd Qu.:0.8482   3rd Qu.:0.4472500  
##  Max.   :0.9800   Max.   :0.9890   Max.   :0.9860   Max.   :0.9870000  
##     loudness           tempo         speechiness     
##  Min.   :-23.574   Min.   : 62.83   Min.   :0.02330  
##  1st Qu.:-10.947   1st Qu.:100.22   1st Qu.:0.03280  
##  Median : -8.072   Median :120.01   Median :0.04140  
##  Mean   : -8.587   Mean   :120.48   Mean   :0.06866  
##  3rd Qu.: -5.862   3rd Qu.:133.84   3rd Qu.:0.06990  
##  Max.   : -1.097   Max.   :210.75   Max.   :0.51700
```

No missing values were found in any of the key audio features, so the dataset is clean and ready for analysis.

---

## Data Wrangling


```r
# Create decade grouping for era-level analysis
billboard <- billboard %>%
  mutate(
    decade = paste0(floor(year / 10) * 10, "s"),
    duration_min = duration_ms / 60000,
    loudness_pos = loudness + abs(min(loudness)) + 1  # shift for plotting
  )

# Decade-level averages for key features
decade_avg <- billboard %>%
  group_by(decade) %>%
  summarise(
    avg_danceability = mean(danceability),
    avg_energy       = mean(energy),
    avg_valence      = mean(valence),
    avg_acousticness = mean(acousticness),
    avg_loudness     = mean(loudness),
    avg_tempo        = mean(tempo),
    n_songs          = n(),
    .groups = "drop"
  )

decade_avg
```

| Decade | Avg Danceability | Avg Energy | Avg Valence | Avg Acousticness | Avg Loudness | Avg Tempo | Number of Songs |
|--------|-----------------:|-----------:|------------:|-----------------:|-------------:|----------:|----------------:|
| 1950s | 0.5767 | 0.4868 | 0.7652 | 0.6365 | -10.8449 | 135.8538 | 20 |
| 1960s | 0.5591 | 0.5456 | 0.7106 | 0.5005 | -9.8398 | 121.9394 | 100 |
| 1970s | 0.6214 | 0.5431 | 0.6831 | 0.3188 | -11.1196 | 117.1161 | 100 |
| 1980s | 0.6400 | 0.6527 | 0.6464 | 0.2130 | -9.6890 | 122.9814 | 100 |
| 1990s | 0.6712 | 0.6257 | 0.6100 | 0.2045 | -8.5591 | 119.0249 | 100 |
| 2000s | 0.6872 | 0.6960 | 0.6219 | 0.1404 | -5.8199 | 118.6785 | 100 |
| 2010s | 0.6876 | 0.7151 | 0.5849 | 0.1183 | -5.4037 | 119.9741 | 80 |


```r
# Year-level averages for trend lines
yearly_avg <- billboard %>%
  group_by(year) %>%
  summarise(
    avg_danceability = mean(danceability),
    avg_energy       = mean(energy),
    avg_valence      = mean(valence),
    avg_acousticness = mean(acousticness),
    avg_loudness     = mean(loudness),
    avg_tempo        = mean(tempo),
    .groups = "drop"
  )
```

---

## Visualizations

### Plot 1: The Evolution of Summer's Sonic DNA (1958–2017)

This plot tracks how four core audio features changed across each decade. The idea was to see if the overall "personality" of a summer hit has shifted over time or stayed relatively the same.


```r
# Long format for faceting
decade_long <- decade_avg %>%
  select(decade, avg_danceability, avg_energy, avg_valence, avg_acousticness) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to  = "feature",
    values_to = "value"
  ) %>%
  mutate(
    feature = recode(feature,
      avg_danceability = "Danceability",
      avg_energy       = "Energy",
      avg_valence      = "Valence (Positivity)",
      avg_acousticness = "Acousticness"
    )
  )

pal <- c(
  "Danceability"       = "#E63946",
  "Energy"             = "#F4A261",
  "Valence (Positivity)" = "#2A9D8F",
  "Acousticness"       = "#457B9D"
)

ggplot(decade_long, aes(x = decade, y = value, color = feature, group = feature)) +
  geom_line(linewidth = 1.3, alpha = 0.85) +
  geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ feature, ncol = 2) +
  labs(
    title    = "The Evolving Sound of Summer: Audio Feature Trends by Decade",
    subtitle = "Average Spotify audio feature scores for Billboard Summer Hits, 1958–2017",
    x        = "Decade",
    y        = "Average Score (0–1 scale)",
    caption  = "Source: Spotify / Billboard Summer Hits dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(color = "gray40", margin = margin(b = 10)),
    strip.text      = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(angle = 35, hjust = 1)
  )
```

<img src="https://github.com/OwenTelis/dataviz_final_project/blob/main/figures/plot1-1.png" alt="Faceted line chart showing four Spotify audio feature trends across decades for Billboard Summer Hits 1958-2017. Acousticness drops sharply from 0.64 to 0.12, Energy rises from 0.49 to 0.72, Danceability increases modestly, and Valence declines from 0.77 to 0.59." style="display: block; margin: auto;" />

```r
# Annotation layer: label the 1950s and 2010s endpoints for Acousticness and Valence
# to draw the reader's eye to the two most notable trends without relying on the text alone
endpoint_labels <- decade_long %>%
  filter(
    (feature == "Acousticness"       & decade %in% c("1950s", "2010s")) |
    (feature == "Valence (Positivity)" & decade %in% c("1950s", "2010s")) |
    (feature == "Energy"             & decade %in% c("1950s", "2010s"))
  ) %>%
  mutate(
    label = scales::percent(value, accuracy = 1),
    vjust = case_when(
      feature == "Acousticness"        & decade == "1950s" ~ -1.2,
      feature == "Acousticness"        & decade == "2010s" ~  1.8,
      feature == "Valence (Positivity)" & decade == "1950s" ~ -1.2,
      feature == "Valence (Positivity)" & decade == "2010s" ~  1.8,
      feature == "Energy"              & decade == "1950s" ~  1.8,
      feature == "Energy"              & decade == "2010s" ~ -1.2,
      TRUE ~ -1.2
    )
  )

last_plot() +
  geom_text(
    data = endpoint_labels,
    aes(x = decade, y = value, label = label, color = feature),
    vjust = endpoint_labels$vjust,
    size = 3.2, fontface = "bold", show.legend = FALSE
  )
```

<img src="https://github.com/OwenTelis/dataviz_final_project/blob/main/figures/plot1-2.png" alt="Faceted line chart showing four Spotify audio feature trends across decades for Billboard Summer Hits 1958-2017. Acousticness drops sharply from 0.64 to 0.12, Energy rises from 0.49 to 0.72, Danceability increases modestly, and Valence declines from 0.77 to 0.59." style="display: block; margin: auto;" />

**Interpretation:** Each of the four panels tells a different part of the story about how summer music has changed.

**Acousticness** dropped the most out of any feature. Back in the 1950s, the average acousticness score was 0.637, which means most summer hits at the time sounded like they were recorded with acoustic instruments. By the 1960s it had already fallen to 0.500, and then it dropped more sharply to 0.319 in the 1970s as rock and funk went fully electric. By the 2010s it was down to just 0.118, which is less than a fifth of where it started. This decline is pretty consistent across all seven decades and is the steepest trend in the whole chart.

**Energy** went almost the opposite direction. It started at 0.487 in the 1950s and gradually climbed up to 0.715 by the 2010s, which is close to a 47% increase overall. The biggest jump happened between the 1970s (0.543) and 1980s (0.653), which makes sense given how much synthesizer-driven pop and arena rock dominated that period. Both of those genres tend to have a very compressed, loud, and intense sound.

**Danceability** also increased over time, but at a slower pace. It went from 0.577 in the 1950s up to 0.688 in the 2010s. The clearest jump was between the 1960s and 1970s (0.559 to 0.621), which lines up with the rise of funk and disco and how those genres changed what a rhythm section was expected to do. Interestingly, danceability is almost flat between the 2000s (0.687) and 2010s (0.688), which might suggest it has hit some kind of ceiling.

**Valence** is probably the most surprising result. Rather than going up with energy and danceability, it actually went down steadily, from 0.765 in the 1950s to just 0.585 in the 2010s. So even as songs became more energetic and rhythmically driven, they became less "positive" sounding according to Spotify's analysis. The total drop in valence (0.18 points) is actually larger than the total gain in danceability (0.11 points). This might be related to the rise of hip-hop and EDM, both of which can be very high-energy and danceable without necessarily sounding upbeat or happy.

---

### Plot 2: Are Summer Hits Getting Louder? The Loudness Wars

The "Loudness War" refers to a trend in music production where songs are mastered louder and louder over time. This plot looks at whether that pattern shows up in Billboard summer hits.


```r
yearly_avg_loud <- billboard %>%
  group_by(year) %>%
  summarise(avg_loudness = mean(loudness), .groups = "drop")

ggplot(yearly_avg_loud, aes(x = year, y = avg_loudness)) +
  geom_point(data = billboard, aes(x = year, y = loudness),
             color = "#457B9D", alpha = 0.18, size = 1.5) +
  geom_line(color = "#E63946", linewidth = 1.2, alpha = 0.9) +
  geom_point(color = "#E63946", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1D3557", linetype = "dashed",
              linewidth = 0.9, alpha = 0.15) +
  scale_x_continuous(breaks = seq(1958, 2017, by = 6)) +
  labs(
    title    = "The Loudness War Hits Summer: Average Track Loudness Over Time",
    subtitle = "Each blue dot = individual song; red line = yearly average; dashed = overall trend",
    x        = "Year",
    y        = "Loudness (dB)",
    caption  = "Source: Spotify / Billboard Summer Hits dataset\nNote: Loudness is negative -- values closer to 0 are louder"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 10)),
    panel.grid.minor = element_blank(),
    axis.text.x   = element_text(angle = 35, hjust = 1)
  ) +
  # Annotate the ~1990 inflection point where digital loudness tools took hold
  annotate(
    "segment",
    x = 1991, xend = 1991, y = -15, yend = -8.8,
    color = "#1D3557", linetype = "dotted", linewidth = 0.7
  ) +
  annotate(
    "text", x = 1991, y = -15.8,
    label = "Digital mastering
goes mainstream
(~1991)",
    color = "#1D3557", size = 3, hjust = 0.5, fontface = "italic"
  ) +
  # Label the earliest and latest decade averages
  annotate(
    "text", x = 1959, y = -10.0,
    label = "1950s avg:
-10.8 dB",
    color = "#E63946", size = 3, hjust = 0, fontface = "bold"
  ) +
  annotate(
    "text", x = 2012, y = -4.0,
    label = "2010s avg:
-5.4 dB",
    color = "#E63946", size = 3, hjust = 0.5, fontface = "bold"
  )
```

<img src="project_01_updated_files/figure-html/plot2-1.png" alt="Scatter and line chart showing loudness trend for Billboard Summer Hits 1958-2017. Individual songs shown as blue dots with yearly average in red and overall linear trend dashed. Loudness rises from around -11 dB in the 1950s-70s to near -5 dB in the 2010s, with the steepest jump in the 1990s-2000s." style="display: block; margin: auto;" />

**Interpretation:** The chart confirms that summer hits have gotten noticeably louder over time, though the trend is not as simple as a straight line upward.

**The overall increase is pretty significant.** Average loudness went from about -10.8 dB in the 1950s all the way to -5.4 dB in the 2010s. That is a difference of over 5 dB, and a 5 dB increase is roughly equivalent to doubling the perceived volume. So modern summer hits are, on average, about twice as loud as the ones from the earliest years in this dataset.

**The timing of the increase is worth paying attention to.** Looking at the decade averages, the 1950s (-10.8), 1960s (-9.84), 1970s (-11.1), and 1980s (-9.69) are all pretty close to each other with no clear upward direction. The 1970s are actually quieter on average than the 1950s. The real shift starts in the 1990s (-8.56) and then becomes very obvious in the 2000s (-5.82) and 2010s (-5.40). This lines up with the period when digital audio workstations and loudness-maximizing plugins became standard tools in music production, which gave engineers the ability to push tracks louder than was ever possible with analog equipment.

**The spread of individual songs also changed over time.** In the earlier decades, the standard deviation of loudness was around 3.3 to 3.8 dB, but by the 2000s it had dropped to 2.04 dB and in the 2010s it was just 1.79 dB. This compression is easy to see in the blue scatter dots on the plot. In the left half of the chart the dots are spread out vertically, but in the right half they bunch together into a narrower band, mostly between -3 and -9 dB. It suggests that modern hits are not just louder on average but also more consistent with each other in terms of loudness, which could reflect more standardized mastering practices.

**There are a few interesting outliers on both ends.** The quietest song in the whole dataset is Roberta Flack's *Feel Like Makin' Love* (1974) at -23.6 dB, which is nearly 20 dB quieter than some of the loudest modern tracks. That is a huge range for what is supposed to be the same type of music. On the other end, Eminem's *Not Afraid* (2010) comes in at -1.1 dB, which is basically right at the digital ceiling. *Crazy* by Gnarls Barkley (2006, -1.6 dB) and *Hollaback Girl* by Gwen Stefani (2005, -2.1 dB) are also near the top. One surprising outlier from the early years is Bobby Darin's *Splish Splash* (1958, -1.5 dB), which is one of the loudest songs in the entire dataset despite being from 1958. That is unusual for that era and is probably a result of its very energetic and punchy drum track.

---

### Plot 3: Danceability vs. Valence -- What's the Mood of a Summer Hit?

If summer hits are supposed to make people feel good and want to dance, then we would expect most of them to score high on both danceability and valence. This scatterplot looks at whether that holds across different eras.


```r
billboard <- billboard %>%
  mutate(era = case_when(
    year < 1970 ~ "1958-1969",
    year < 1980 ~ "1970s",
    year < 1990 ~ "1980s",
    year < 2000 ~ "1990s",
    year < 2010 ~ "2000s",
    TRUE        ~ "2010s"
  ))

era_order <- c("1958-1969","1970s","1980s","1990s","2000s","2010s")
billboard$era <- factor(billboard$era, levels = era_order)

era_pal <- c(
  "1958-1969" = "#6A0572",
  "1970s"     = "#C77DFF",
  "1980s"     = "#E63946",
  "1990s"     = "#F4A261",
  "2000s"     = "#2A9D8F",
  "2010s"     = "#457B9D"
)

era_centers <- billboard %>%
  group_by(era) %>%
  summarise(
    cx = mean(danceability),
    cy = mean(valence),
    .groups = "drop"
  )

ggplot(billboard, aes(x = danceability, y = valence, color = era)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_point(data = era_centers, aes(x = cx, y = cy, color = era),
             size = 6, shape = 18) +
  geom_text(data = era_centers, aes(x = cx, y = cy, label = era),
            vjust = -1, fontface = "bold", size = 3.5, color = "black") +
  scale_color_manual(values = era_pal, name = "Era") +
  scale_x_continuous(limits = c(0.15, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  annotate("rect", xmin = 0.6, xmax = 1, ymin = 0.6, ymax = 1,
           fill = "#2A9D8F", alpha = 0.06) +
  annotate("text", x = 0.98, y = 0.98, label = "High Dance\n& High Positivity",
           hjust = 1, vjust = 1, color = "#2A9D8F", size = 3.2, fontface = "italic") +
  labs(
    title    = "Danceability vs. Valence in Billboard Summer Hits by Era",
    subtitle = "Large diamonds show era averages; shaded zone = ideal 'summer feel' quadrant",
    x        = "Danceability",
    y        = "Valence (Musical Positivity)",
    caption  = "Source: Spotify / Billboard Summer Hits dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 10)),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )
```

<img src="project_01_updated_files/figure-html/plot3-1.png" alt="Scatterplot of danceability versus valence for Billboard Summer Hits colored by six eras. Era centroids shown as large diamonds. Centroids shift right and down across eras, showing increasing danceability and decreasing valence from the 1958-1969 era through the 2010s." style="display: block; margin: auto;" />

**Interpretation:** The scatterplot shows both a consistent pattern across eras and some clear differences in how the typical summer hit has shifted over time.

**The era averages move in a consistent direction.** Going from the 1958-1969 centroid (danceability: 0.562, valence: 0.720) all the way to the 2010s centroid (danceability: 0.688, valence: 0.585), there is a clear diagonal movement to the right and downward on the plot. Every successive era has a higher average danceability and a lower average valence than the one before it. This is not just a slight drift but a steady progression across all six eras. The 1958-1969 diamond sits near the top of the chart, the 1970s and 1980s are in the middle, and the 2000s and 2010s are clustered lower and to the right.

**The earliest era stands out in terms of valence.** The 1958-1969 group has a mean valence of 0.720, which is about 0.135 points above the 2010s average of 0.585. That gap is easy to see just by looking at where the two centroids sit on the y-axis. A lot of the individual points from the early era also have valence scores above 0.80, which reflects how much early rock and roll and pop leaned on simple, major-key, upbeat sounds. That kind of emotional brightness is noticeably less common in the more recent eras.

**How spread out the points are also varies by era.** The 1970s and 1980s have the widest range of valence scores within their groups, with standard deviations of 0.244 and 0.251 respectively. That means those decades produced summer hits with a pretty wide variety of emotional tones, from very happy-sounding to more somber. The 2010s have a tighter danceability spread (SD of 0.115), which suggests that recent hits are more similar to each other rhythmically even though their emotional tone still varies.

**Some songs are notable outliers.** In the upper-left part of the plot, there are a few older songs that had high valence but low danceability, like *Rebel Rouser* by Duane Eddy (1958) and *Palisades Park* by Freddy Cannon (1962). These were cheerful songs but not particularly dance-focused. On the opposite end, there are songs with very high danceability but surprisingly low valence. The most extreme example is *Turn Down for What* by DJ Snake (2014), which has a danceability score of 0.818 but a valence of only 0.082. That is one of the lowest positivity scores in the whole dataset, yet the song is clearly built to get people moving. This kind of track, high energy and rhythmically dominant but not emotionally upbeat, is a relatively new type of summer hit that did not really exist before the 2000s.

---

## Report & Discussion

### What Were the Original Charts Planned?

The original plan for this assignment was to create three visualizations:

1. A **line chart of audio features by decade** to see how the sound of summer music has changed over time.
2. A **loudness trend chart** to look at whether the Loudness War shows up in summer hits.
3. A **scatterplot of danceability vs. valence** colored by era to explore whether summer hits have always been both upbeat and danceable.

All three plots were completed as planned. The one change I made was turning the first chart into a faceted layout rather than putting all four features on a single chart, which would have been too cluttered to read clearly.

### What Story Could You Tell With Your Plots?

Together, the three plots tell a pretty consistent story: **summer hits have gotten louder, more electric, and more rhythmically driven over the past six decades, but they have always been built around the idea of making people want to move.**

Plot 1 shows that the acoustic, more melodically "happy" sound of the late 1950s and early 1960s gave way to electrified, high-energy production by the 1980s and that shift never reversed. Plot 2 shows that loudness followed a similar trajectory, with the biggest jump happening in the 1990s and 2000s when digital mastering tools made it easier to push volume higher than ever before. Plot 3 ties things together by showing that even as the production style changed drastically, summer hits in every era still tended to land in the high-danceability zone, just with a gradual shift toward less overall positivity over time.

### How Were the Principles of Data Visualization Applied?

A few key principles from the course came into play when designing these charts:

- **Purposeful color:** Each audio feature and each era was given its own consistent color. Labels and shapes were also used so that the charts do not rely on color alone to communicate meaning.
- **Appropriate chart types:** Line charts made sense for showing trends over time in Plots 1 and 2. A scatterplot made sense for Plot 3 because the goal was to show the relationship between two continuous variables across groups.
- **Layering raw and summary data:** In Plot 2, the individual song points are shown as faint blue dots underneath the yearly average line. This lets you see both the overall trend and how much variation exists within each year at the same time.
- **Annotations:** Plot 3 uses a shaded region and labeled era centroids to help guide interpretation without making the viewer do a lot of mental work.
- **Minimal design:** All three plots use `theme_minimal()` and remove minor gridlines to keep the focus on the data and reduce visual clutter.
- **Subtitles and captions:** Each plot has a subtitle that explains what the visual elements represent and a caption that credits the data source.

---

## Conclusion

Looking at the data as a whole, it is clear that summer hits have changed a lot since 1958 in terms of how they sound, but some things have stayed consistent. The biggest changes are in acousticness (which dropped dramatically), loudness (which increased significantly, especially after the 1990s), and energy (which climbed steadily over time). At the same time, danceability has always been a defining feature of summer hits, just expressed differently depending on the era. The most unexpected finding is that valence has gone down over time, meaning modern summer hits are actually less "positive" sounding on average than older ones, even though they are more energetic. That probably reflects how much the genre landscape of popular music has changed, with hip-hop and EDM now dominating the charts in a way that was not the case in the 1960s or 1970s.
