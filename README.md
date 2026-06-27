# Data Visualization and Reproducible Research

> Owen Telis | Otelis0019@floridapoly.edu

The following is a sample of products created during the _"Data Visualization and Reproducible Research"_ course.

---

## Project 01

In the `project_01/` folder you can find a revised analysis of Spotify audio features for Billboard Summer Hits spanning 1958 to 2017. The project uses 600 songs across six decades to explore how the sonic character of popular summer music has changed over time. Three visualizations cover the evolution of key audio features by decade, the Loudness War trend in mastering levels, and the relationship between danceability and emotional positivity across eras. This revision adds direct in-figure annotations to both trend charts so the most important values are readable without cross-referencing the written commentary.

**Evolving Sound of Summer Figure:**

<img src="https://raw.githubusercontent.com/OwenTelis/dataviz_final_project/main/figures/plot1-2.png" width="80%">

---

## Project 02

In the `project_02/` folder you can find an analysis of three unrelated datasets: NBA Finals champion box scores from 1980 to 2018, FIFA 18 player attribute ratings, and a Florida Lakes shapefile. Each dataset drives a different chart type. The NBA data is used for a static trend line and an interactive plotly scatterplot. The FIFA data feeds a linear regression coefficient plot. The lakes shapefile produces a spatial choropleth layered with individual lake polygons. This revision suppresses long console output from the model summary and team name tables, replacing them with clean kable tables.

**Three-Point Revolution NBA Finals:**

<img src="https://raw.githubusercontent.com/OwenTelis/dataviz_final_project/main/figures/nba-static-plot-1.png" width="80%">

---

## Project 03

In the `project_03/` folder you can find an exploration of daily mean temperature records from a Tampa, Florida weather station (COOP ID 80211) covering 1931 through 1936. The dataset comes from FSU's Florida Climate Center and contains 2,103 valid daily observations after removing missing-value placeholders. Four visualizations are included: an interactive daily time series built with plotly, a monthly ridge plot using ggridges, a calendar heatmap, and a seasonal boxplot with individual daily points overlaid. The project also includes a before/after chart redesign comparing a rainbow-palette bar chart with a y-axis starting at zero against a corrected version with a zoomed axis, direct value labels, and a reference line. All figures use viridis family palettes and include alt text.

**Monthly Temp Distribution Tampa Fl 1931-1936:**

<img src="https://raw.githubusercontent.com/OwenTelis/dataviz_final_project/main/figures/plot2-ridges-1.png" width="80%">

---

## Moving Forward

This course pushed me to think more carefully about what a chart is actually doing versus what it looks like it is doing. The redesign exercise in Project 03 made that concrete: a bar chart starting at zero is not more honest, it just makes the data unreadable. I want to keep practicing that instinct of asking whether the encoding matches what is actually perceivable at the scale the chart is drawn.

Going forward I plan to get more comfortable with spatial data and sf, since the Florida lakes map was the part of Project 02 that required the most problem-solving and produced the most interesting result. I also want to explore more ways to use interactivity purposefully rather than just converting a static plot to plotly. The hover tooltip in Project 03 was a real improvement over a static scatterplot for that dataset, and I think there are more cases in my day-to-day BI work where that kind of reader control would add genuine value over a fixed chart.
