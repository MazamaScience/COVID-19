# Can I reproduce the fouth graphic down on:
#   https://blog.datawrapper.de/coronaviruscharts/
#

WORKING_DIR <- path.expand("~/Projects/MazamaScience/COVID-19")

library(data.table)
library(ggplot2)
library(plotly)
library(hrbrthemes)

# ----- Prepare COVID data -------------------------------------------------------

filePath <- file.path(WORKING_DIR, "barcharts/top_countries_2020-03-23.csv")
covid <- readr::read_csv(filePath)

# > names(covid)
# [1] "region"                  "current confirmed cases" "deaths"                  "recovered"              

# Ordered barchart:
#  http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Ordered%20Bar%20Chart

# Pivot to tidy dataframe (used to be called "gather")
tidyCovid <-
  covid %>%
  dplyr::mutate(orderVar = `current confirmed cases`) %>%
  tidyr::pivot_longer(
    c(-region, -orderVar),
    names_to = "status",
    values_to = "count"
  )

# ----- Plot style -------------------------------------------------------------

title <- "Countries with more than 1000 currently confirmed COVID–19 cases"
  
namedColors <- c(
  "current confirmed cases" = "#166071",
  "deaths" = "#17181C",
  "recovered" = "#93BF46"
)

countryCount <- length(sort(unique(tidyCovid$region)))
separator_y <- 0.5:(countryCount - 1.5)

top_9_text <- 
  sort(covid$`current confirmed cases`, decreasing = TRUE)[1:9] %>%
  format(big.mark = ",")
top_9_x <- countryCount:(countryCount - 8)

# ----- Plot -------------------------------------------------------------------

gg <-
  ggplot(tidyCovid, 
         aes(fill = status, 
             y = count, 
             x = forcats::fct_reorder(region, orderVar))) + 
  geom_bar(stat = "identity",
           position = position_stack(reverse = TRUE),
           width = 0.7
  ) +
  scale_fill_manual(values = namedColors) +
  coord_flip() +
  ggtitle(title) +
  theme_ipsum() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification = 0,               # left adjust
        axis.text.x = element_blank(),          # remove x axis labels
        panel.grid.major.x = element_blank(),   # vertical grid lines
        panel.grid.minor.x = element_blank(),   # vertical grid lines
        panel.grid.major.y = element_blank(),   # horizontal grid lines
        panel.grid.minor.y = element_blank()    # horizontal grid lines
  ) +
  # Add back "separator" lines
  geom_vline(xintercept = separator_y, colour = "gray90", size = 0.2) +
  # Remove X and Y axis labels
  xlab("") +
  ylab("") +
  # Add annotations
  annotate("text", x = top_9_x, y = 2800, label = top_9_text, color = "white")

print(gg)

# ----- Plotly version ---------------------------------------------------------

# Use data.table (very easy to order)
dt <- data.table(covid)
setorder(dt, `current confirmed cases`)
# Create y labels
ylabs <- as.character(dt$region)
# title
title <- "Countries with more than 1000 currently confirmed COVID–19 cases"
# Make color theme
cols <- list(
  "current confirmed cases" = "#166071",
  "deaths" = "#17181C",
  "recovered" = "#93BF46"
)
# function to determine whether to display bar text
show_nums <- function(x) ifelse(x >= 5000, 'inside', 'none')

# plot
pp <- plotly::plot_ly(
  data = dt,
  type = 'bar'
) %>%
  add_trace( x = ~`current confirmed cases`,
             y = ~region,
             marker = list(color = cols$`current confirmed cases`),
             name = 'current confirmed cases',
             text = ~`current confirmed cases`,
             textposition = ~show_nums(`current confirmed cases`) ) %>%
  add_trace( x = ~deaths,
             y = ~region,
             marker = list(color = cols$deaths),
             name = 'deaths',
             text = ~deaths,
             textposition = ~show_nums(deaths) ) %>%
  add_trace( x = ~recovered,
             y = ~region,
             marker = list(color = cols$recovered),
             name = 'recovered',
             text = ~recovered,
             textposition = ~show_nums(recovered) ) %>%
  layout( title = list(text = title, font = list(size = 18, color = 'black')),
          legend = list(orientation = 'h', y = 1.05),
          barmode = 'stack',
          xaxis = list(  title = '',
                         zeroline = FALSE,
                         showline = FALSE,
                         showticklabels = FALSE,
                         showgrid = FALSE),
          yaxis = list(  title = '',
                         zeroline = FALSE,
                         showline = FALSE,
                         showticklabels = TRUE,
                         showgrid = TRUE) ,
          margin = list(l = 0, r = 0, b = 0, t = 100) ) %>%
  config(displayModeBar = FALSE)

print(pp)

