library(readxl)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(leaflet)
library(plotly)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

# read in covid mh gaming data
new.y.n <- read_xlsx("data/covid_new.y.n.xlsx") 

# read in game delay and country revenue data
dly.f <- read_xlsx("data/delay.f.xlsx")
dly.f$`Initial Release Date` <- as.Date(dly.f$`Initial Release Date`, tz = "")
dly.f$`Final Release Date` <- as.Date(dly.f$`Final Release Date`, tz = "")
country <- read_xlsx("data/game delay.xlsx", sheet = 2)

# read in twitter data
df_2019 <- read_xlsx("data/df_2019.xlsx")
df_2020 <- read_xlsx("data/df_2020.xlsx")
df_2021 <- read_xlsx("data/df_2021.xlsx")

# read in ratings and genre and games data
games_DT <- read_xlsx("data/games_DT.f.xlsx")
gamesall <- read_xlsx("data/gamesall.f.xlsx")
platform_by_year <- read_xlsx("data/platform_by_year.xlsx")
top_5_genre_rating <- read_xlsx("data/top_5_genre.xlsx")
top_5_genre_meta <- read_xlsx("data/top_5_genre_meta.xlsx")

# read in hardware sales and stock data
sales <- read_xlsx("data/sales.xlsx")
sales$Date <-as.Date(with(sales, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
stock <- read_xlsx("data/stock.xlsx")
stock$Date <-as.Date(with(stock, paste(timestamp, sep = "-")), "%Y-%m-%d")

# creating genre choice
'%!in%' <- function(x, y)
  ! ('%in%'(x, y))
genrelistg <-
  str_split(string = gamesall$new_tags, pattern = "', '")
genrelist2g <- c(unique(genrelistg))
all_geng <- c()
for (i in genrelist2g) {
  for (j in i) {
    if (j %!in% all_geng) {
      all_geng <- c(all_geng, j)
    }
  }
}
all_geng <- c("", all_geng)



# creating platform choice
platlist <- str_split(string = gamesall$new_plat, pattern = "', '")
platlist2 <- c(unique(platlist))
all_plat <- c()
for (i in platlist2) {
  for (j in i) {
    if (j %!in% all_plat) {
      all_plat <- c(all_plat, j)
    }
  }
}

# update games df
gamesall$genre <- genrelistg
gamesall$plat_list <- platlist
gamesall$year <- as.numeric(gamesall$year)

# changing datatable search bar color
callback1 <- c(
  "$('#DataTables_Table_0_length select').css('background-color', 'white');",
  "$('#DataTables_Table_0_filter input').css('background-color', 'white');"
)


# set ggplot bases
wb_imp <- ggplot(new.y.n,
                 aes(x = `well-being impact`))

age <- ggplot(new.y.n,
              aes(x = `age group`))

gend <- ggplot(new.y.n,
               aes(x = gender))

deg <- ggplot(new.y.n,
              aes(x = degree))

impact <- ggplot(new.y.n,
                 aes(x = `Impact Type`))

gm_type <- ggplot(new.y.n,
                  aes(x = `change in game type`))

# avoid NA in plot
new.oc <-
  filter(
    new.y.n,
    `occupation group` == "not currently employed" |
      `occupation group` == "part-time" |
      `occupation group` == "full-time" |
      `occupation group` == "Self-employed" |
      `occupation group` == "Student"
  )

occu <- ggplot(new.oc,
               aes(x = `occupation group`))

# avoid NA in plot
new.gm <-
  filter(
    new.y.n,
    `covid gaming frequency` == "Rarely" |
      `covid gaming frequency` == "Once a month" |
      `covid gaming frequency` == "Several times a month" |
      `covid gaming frequency` == "Once a week" |
      `covid gaming frequency` ==  "Several times a week" |
      `covid gaming frequency` ==  "At least once a day" |
      `covid gaming frequency` ==  "Several times a day"
  )

gm_freq <- ggplot(new.gm,
                  aes(x = `covid gaming frequency`))



# mental health category choices for shiny
mh_cat <-
  c(
    "",
    "age group",
    "degree",
    "frequency",
    "gender",
    "change in game type",
    "occupation",
    "well-being"
  )





# set tooltip info for leaflet
content <- paste(
  "Country: ",
  country$Country,
  "<br/>",
  "Rank 2020: ",
  country$`Rank 2020`,
  "<br/>",
  "Revenue 2020: ",
  paste0("$", as.character(country$`Revenue  2020`), " Billion"),
  "<br/>",
  "Rank 2021: ",
  country$`Rank 2021`,
  "<br/>",
  "Revenue 2021: ",
  paste0("$", as.character(country$`Revenue  2021`), " Billion"),
  "<br/>",
  "Video Game Exports 2020: ",
  paste0(
    "$",
    as.character(
      country$`Video Game Exports (USD Billions) 2020 *excluding video games requiring user payment to play`
    ),
    " Billion"
  ),
  "<br/>",
  "Exports % Change from 2019: ",
  paste0(
    as.character(country$`Exports % change between 2019 and 2020`),
    "%"
  ),
  "<br/>",
  "<br/>",
  "Active Video Gamers Worldwide 2020: ",
  paste0(
    as.character(country$`Number of active video gamers worldwide 2020 (Billions)`),
    " Billion"
  ),
  "<br/>",
  "Online Gamers Worldwide 2020: ",
  paste0(
    as.character(country$`Number of online gamers 2020 (Millions)`),
    " Million"
  ),
  "<br/>",
  "Total National Players 2021 (Estimated): ",
  paste0(
    as.character(country$`Country Player Count Estimate 2021 (Millions)`),
    " Million"
  ),
  "<br/>",
  "Active Video Gamers Worldwide 2021: ",
  paste0(
    as.character(country$`Number of active video gamers worldwide 2021 (Billions)`),
    " Billion"
  ),
  "<br/>",
  "% Internet Users Playing Video Games on Any Device (as of Q3 2021): ",
  paste0(
    as.character(
      country$`% internet users who play video games on any device (as of 3rd quarter) 2021`
    ),
    "%"
  ),
  "<br/>"
)



# ggplot delayed dates and games
dly_plt <-
  ggplot(
    dly.f,
    aes(
      x = `Initial Release Date`,
      y = reorder(Game, `Days Delayed`),
      color = Game,
      shape = `Covid Impact`,
      group = Game,
      text = paste0(
        "Game: ",
        Game,
        "\n",
        "Initial Release Date: ",
        `Initial Release Date`,
        "\n",
        "Final Release Date: ",
        `Final Release Date`,
        "\n",
        "Days Delayed: ",
        `Days Delayed`,
        "\n",
        "Covid Impact: ",
        `Covid Impact`
      )
    )
  ) +
  ggtitle("Delayed Games for 2019-2022 Over Time") +
  ylab(NULL) +
  xlab("Date") +
  geom_point(alpha = 0.9) +
  geom_point(aes(
    x = `Final Release Date`,
    y = Game,
    color = Game,
    group = Game
  ),
  alpha = 0.9) +
  geom_segment(aes(
    x = `Initial Release Date`,
    xend = `Final Release Date`,
    y = Game,
    yend = Game
  ),
  size = 0.9) +
  theme_tufte() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_x_date(labels = scales::date_format("%d/%m-%Y")) +
  coord_cartesian(ylim = c(1, 45))

# plotly of delay data
myplot <- ggplotly(dly_plt, tooltip = "text")
# clean legend titles
for (i in 1:length(myplot$x$data)) {
  if (!is.null(myplot$x$data[[i]]$name)) {
    myplot$x$data[[i]]$name =  gsub("\\(", "", str_split(myplot$x$data[[i]]$name, ",")[[1]][1])
  }
}


# ggplot delay df num days delayed
myplot2 <- ggplot(dly.f,
                  aes(
                    x = `Days Delayed`,
                    y = reorder(Game, `Days Delayed`),
                    fill = Game,
                    group = Game,
                    text = paste0(
                      "Game: ",
                      Game,
                      "\n",
                      "Initial Release Date: ",
                      `Initial Release Date`,
                      "\n",
                      "Final Release Date: ",
                      `Final Release Date`,
                      "\n",
                      "Days Delayed: ",
                      `Days Delayed`,
                      "\n",
                      "Covid Impact: ",
                      `Covid Impact`
                    )
                  )) +
  geom_col() +
  theme_tufte() +
  theme(
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 5)
  ) +
  ggtitle("Number of Days Game Delayed for 2019-2022") +
  ylab("") +
  coord_cartesian(xlim = c(1, 850))

# plotly of delay data
myplot2 <- ggplotly(myplot2, tooltip = "text")


# plotly of hardware sales data

salesplot <-
  ggplot(sales, aes(x = Date, y = Sold, color = Hardware)) + 
  geom_line() + 
  geom_text(aes(
    x = as.Date("2019-11-30", "%Y-%m-%d"),
    y = 1920), label = "Peak NS sales", color = "grey20") + 
  ggtitle("Console Sales from 2019-2022") +
  theme_tufte() 

salesplot <- ggplotly(salesplot)



#plotly of game company stocks

stockplot <-
  ggplot(data = stock, aes(x = Date, y = open, color = stock)) + geom_line() + theme_classic() + geom_vline(xintercept = as.numeric(as.Date("2020-03-27")), linetype =
                                                                                                              4) + ggtitle("Stock changes of top 10 Game companies from 2019-2022") + xlab("Date") + ylab("Price at Open")


stockplot <- ggplotly(stockplot)


# twitter word clouds
wc2019 <-
  wordcloud2(
    df_2019,
    size = 0.4,
    shape = 'circle',
    color = 'random-light',
    backgroundColor = 'black',
    gridSize = 1.8 ,
    rotateRatio = 0.9
  )

wc2020 <-
  wordcloud2(
    df_2020,
    size = 0.3,
    shape = 'triangle-forward',
    color = 'random-light',
    backgroundColor = 'black',
    gridSize = 1.8 ,
    rotateRatio = 0.9
  )

wc2021 <-
  wordcloud2(
    df_2021,
    size = 0.4,
    shape = 'pentagon',
    color = 'random-light',
    backgroundColor = 'black',
    gridSize = 1.8 ,
    rotateRatio = 0.9
  )



#leaflet boundary
m <- leaflet() %>%
  fitBounds(-840,-10,-580, 50)

# add map tile and clustered points
colnames(country)[4] <- "Country"

mm <- m %>% addTiles() %>%
  addCircleMarkers(
    lng = country$longitude,
    lat = country$latitude,
    label = country$Country,
    color = c(
      "#ff0000",
      "#2d00f7",
      "#ff7aa2",
      "#0096c7",
      "#f2542d",
      "#9b5de5",
      "#8ac926",
      "#ff477e",
      "#2c8c99",
      "#70e000",
      "#ffba08"
    ),
    fillColor = c(
      "#ff0000",
      "#2d00f7",
      "#ff7aa2",
      "#0096c7",
      "#f2542d",
      "#9b5de5",
      "#8ac926",
      "#ff477e",
      "#2c8c99",
      "#70e000",
      "#ffba08"
    ),
    popup = content,
    clusterOptions = markerClusterOptions()
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c(
      "#ff0000",
      "#2d00f7",
      "#ff7aa2",
      "#0096c7",
      "#f2542d",
      "#9b5de5",
      "#8ac926",
      "#ff477e",
      "#2c8c99",
      "#70e000",
      "#ffba08"
    ),
    labels = country$Country,
    title = "Top 11 Countries of 2020 & 2021"
  )
