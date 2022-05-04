# load packages
library(rio)
library(rvest)
library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(plotly)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(colorspace)
library(tm)
library(stringr)
library(patchwork)
library(scales)
library(DT)

source("setup.R")

shinyUI(
  dashboardPage(
    title = "Changes in the Video Game Landscape",
    header = dashboardHeader(titleWidth = '100%',
                             title = span(
                               tags$img(
                                 src = "pac_trans.png",
                                 width = '100%',
                                 height = '100%'
                               )
                             )),
    
    dashboardSidebar(
      width = 220,
      sidebarMenu(
        style = "position:fixed;width:220px;",
        #  "Filter Controls",
        # genre1
        conditionalPanel(
          condition = "input.tabselected == 1",
          menuItem(
            tabName = "Genres 1",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "genre1",
              label = "Search for a Genre",
              multiple = F,
              selected = "rpg",
              choice = all_geng,
              options = list(
                create = F,
                placeholder = "Try 'rpg'...",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
            
          )
        ),
        
        
        
        # genre2
        conditionalPanel(
          condition = "input.tabselected == 1",
          menuItem(
            tabName = "Genres 2",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "genre2",
              label = "Search for Another Genre",
              multiple = F,
              choice = all_geng,
              selected = "all",
              options = list(
                create = F,
                placeholder = "Try 'moba'...",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          )
        ),
        
        
        # platform
        conditionalPanel(
          condition = "input.tabselected == 1",
          menuItem(
            tabName = "Platforms",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "platf",
              label = "Search for a Patform",
              multiple = T,
              choice = all_plat,
              selected = "PC",
              options = list(
                create = F,
                placeholder = "Try 'PC'...",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          )
        ),
        
        
        #mental health
        conditionalPanel(
          condition = "input.tabselected == 3",
          menuItem(
            tabName = "Mental Health",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "mh",
              label = "Search for a Population Category",
              multiple = T,
              choice = mh_cat,
              selected = "age group",
              options = list(
                create = F,
                placeholder = "Search for a Population Category",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          )
        ),
        width = 3
      ) # sidebar menu
    ),
    # dashboard sidebar
    
    
    dashboardBody(
      tags$style(
        type = "text/css",
        "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }

    /*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
      ),
      
      shinyDashboardThemes(theme = "grey_dark"),
      # overview tab
      tabsetPanel(
        type = "tabs",
        id = "tabselected",
        selected = 0,
        # Welcome page tab
        tabPanel(
          "Project Overview",
          br(),
          h3("Introduction:"),
          h4(
            "The first electronic game, now referred to as video game, ever released was Tennis for Two introduced on October 18th, 1958. 
            In a short 64 years this one game sparked the creation of a multi-billion dollar industry.
            With this new gaming industry came a plethora of innovations including new genres, advancements in software and hardware capabilities,
            and numerous sub-culture's to name a few. For our project, we will examine the video game landscape and how it has changed over time, 
            particularly with an eye on the past 4 years or so. We will take a deep-dive into the industry by looking at the evolution of perceptions of games,
            popular platforms, country-specific industries, and finally changes in the industry and public sentiment since the beginning of the COVID-19 pandemic. "
          ),
          tags$hr(),
          br(),
          h3("Group Members:"),
          br(),
          # Tobias
          h4("Shengkang 'Tobias' Zhou"),
          p(
            "Tobias was born and raised in a 'traditional' Chinese family; gaming was the “forbidden fruit” of his childhood."
          ),
          p(
            "However, his parents made a mistake when they bought him a Windows PC in high school, because then he actually began to play video games.
           His favorite genres are RPG and fantasy, the top three games being:"
          ),
          tags$div(list(
            h5(
              "Elder Scroll V: Skyrim  (bet on George Martin finish GOT before Bethesda release Elder Scroll VI)"
            ),
            h5(
              "Total War: Warhammer Series  (can only beat the campaign on the beginner difficulty)"
            ),
            h5("Magic the Gathering  (too poor to be a competitive player)")
          )),
          tags$hr(),
          br(),
          # Adrian
          h4("Adrian Varallyay"),
          p(
            "Has had an on and off relationship with console gaming. However, like many during the Covid-19 pandemic, he reconnected with gaming via the Nintendo Switch."
          ),
          p(
            "These days he probably spends more time than he should splatting others in ‘Splatoon 2’."
          ),
          h5("Other favorites include:"),
          tags$div(list(
            h5("Overcooked 2"),
            h5("The Metroid series"),
            h5("The Zelda series")
          )),
          tags$hr(),
          br(),
          # Tazz
          h4("Tazz Orchi"),
          p("Tazz grew up playing whatever free game was installed on her PC until her parents finally bought her a Gameboy Advance SP."),
          p("She spent her younger years playing exclusively Gameboy games until finding a love for PC gaming. 
            She has since built her first PC and enjoys multiplayer games with her friends the most. "),
          h5("Current Favorite Games:"),
          tags$div(list(
            h5("Monster Hunter World"),
            h5("Stardew Valley"),
            h5("Raft")
          )),
          tags$hr(),
          br(),
          # Alan
          h4("Xiuyuan 'Alan' Shen"),
          p(
            "Alan, originally from Hong Kong, now lives in Shanghai. He has fond memories of playing Metal Slug and Tony Hawk's Pro Skater with his dad on the Playstation 1."
          ),
          p(
            "He enjoys strategy, RPG, FPS, and sports games. His current top 3 favorite games are:"
          ),
          tags$div(list(
            h5("The Last of Us"),
            h5("Hades"),
            h5("Hollow Knight")
          )),
          tags$hr(),
          br(),
          
          # citations
          tags$div(
            h3("Citations:"),
            h5("RAWG API"),
            tags$div(list(
              a(href = "https://rawg.io/apidocs#pricing", "Players and Genres")
            )),
            h5("Twitter API"),
            h5("Wikipedia"),
            tags$div(list(
              a(Href = "https://en.wikipedia.org/wiki/2020_in_video_games", "2020 in Video Games"),
              a(Href = " https://en.wikipedia.org/wiki/Video_game_industry#2020s", "Video Game Industry 2020"),
              a(Href = "https://en.wikipedia.org/wiki/2021_in_video_games", "2021 in Video Games"),
              a(Href = "https://en.wikipedia.org/wiki/Impact_of_the_COVID-19_pandemic_on_the_video_game_industry#Hardware_and_software_releases", "Impact of Covid-19")
            )),
            h5("IGN"),
            tags$div(list(
              a(Href = "https://www.ign.com/articles/video-game-delays-2021", "Game Delays in 2021"),
              a(Href = "https://www.ign.com/articles/video-game-delays-2021-are-worse-covid", "Covid Delays are worse"),
              a(Href = "https://www.ign.com/articles/2019/12/12/the-last-of-us-part-2-animal-crossing-new-horizons-and-every-game-delay-announced-in-2019", "Game Delay 2019")
            )),
            h5("PC Gamer"),
            tags$div(list(
              a(Href = "https://www.pcgamer.com/delayed-in-2019-the-games-that-didnt-come-out-this-year-and-why/",  "Delayed 2019")
            )),
            h5("YouTube"),
            tags$div(list(
              a(href = "https://www.youtube.com/watch?v=Wyg8E5fDLrw", "Video: 2019 Delays")
            )),
            
            h5("Game Gavel"),
            tags$div(list(
              a(Href = "https://gamegavel.com/video-game-statistics/", "Game Statistics")
            )),
            h5('VgChartz Console Sales Data'),
            h5('Alpha Vantage Stock API'),
            tags$hr(),
            br(),
            
            h4(
              "We would like to give a special thanks to Professor Matthew Barr, who so kindly shared his data with us. Professor Barr collected data for his study regarding the impact of video gaming on mental health during the Covid-19 pandemic."
            ),
            h5("Dr. Matthew Barr"),
            tags$div(list(
              a(Href = "https://www.researchgate.net/profile/Matthew-Barr-3", "Gaming and Covid-19"),
              br(),
              br()
            ))
          ),value = 0),
        
        # tab 1
        # Video Games and Genres Ratings tab
        tabPanel(
          "Games and Genres Popularity",
          br(),
          h4("User Instructions For This Page:"),
          h6(
            "Select TWO genres and ONE platform in the sidebar panel. These conditions filter the results shown in the first two graphs."
          ),
          tags$hr(),
          br(),
          
          p(
            "On this page, we compare the perceptions of games over the past 11 years (2011-2021) between  players and
               professisonal game critics (metacritic scores). We conducted comparisons from two perspectives: "
          ),
          tags$div(list(
            h5(
              "  1. comparing individual game's rating filtered by genre and platform."
            ),
            h5("  2. comparing the most popular genres over the years.")#,
            #h5("The following data is extracted through RAWG API: https://rawg.io/apidocs#pricing")
          )),
          p(
            "The score for genre is calculated by grouping games by their lable and taking the average of the groups.
           Every genre that has less than 30 games is dropped. The bubble size displays the number of games."
          ),
          p(
            "As the graphs indicate, RPG, Atmospheric and Cooperative games is popular for both players and professional game critics
           over time. The Tactical genre is the only one that appears on the top of the list for metacritic reviews,
           but does not appear for the player scores. In 2015, the Tactical genre is the top genre rated by metacritic,
           including games like Metal Gear Solid V: The Phantom Pain and Wasteland 2: Director's Cut."
          ),
          p(
            "Comedy is a genre that only appears in the player rating table. It includes games such as
           Grand Thief Auto V, Half-life 2 and Portal 2. "
          ),
          p(
            "The Violent and Gore genres are becoming more and more popular since 2018.
           Games like Red Dead Redemption 2 and Seikiro: Shadows Die Twice receive overwhelmingly positive reviews by both
           players and the critics alike."
          ),
          br(),
          h3("Video Game Ratings by Players (out of 5)"),
          br(),
          plotlyOutput("games_rating", height = 550),
          h3("Video Game Ratings by Metacritic (out of 100)"),
          br(),
          plotlyOutput("games_meta", height = 550),
          tags$hr(),
          br(),
          
          h3("Top 5 Genres Rated by Players"),
          br(),
          plotlyOutput("t5_genre_rating", height = 550),
          h3("Top 5 Genres Rated by Metacritics"),
          br(),
          plotlyOutput("t5_genrer_meta", height = 550),
          tags$hr(),
          br(),
          
          h3("Search for a game"),
          p(
            "Below, the data for these visuals are searchable through the provided datatable."
          ),
          br(),
          DT::dataTableOutput("gamesdt", height = 650),
          value = 1
        ),
        
        # Game Counts by Platforms tab
        tabPanel(
          "Games and Platforms",
          br(),
          h3("Platform Game Counts Tracking Over Time"),
          br(),
          p("The first ever home video game console was released in 1972 by Magnavox, just 14 years after the release of the first video game.
            Since then game companies have released numerous platforms on which consumers can play. 
            There are traditional consoles, which are heavily dominated by Microsoft's XBox Series and Sony's PS consoles,
            as well as handheld devices like the ever popular Nintendo Switch."),
          p(
            "The graph below shows the most popular platforms for game releases over the last decade.
            Following a dip in 2019, the most popular release platforms were PC, Sony PS Consoles, and Xbox series consoles.
            By hovering over an individual point you can see the number of games released on the associated platform. 
            In the last 10 years, the most games ever released on one platform was in 2015 when 441 games were released for PC in one year."
          ),
          br(),
          plotlyOutput("platform", height = "550px"),
          tags$hr(),
          br(),
          
          h4("Console Sales Over Time"),
          p(
            "In addition, console sales data indicates that the best selling gaming hardware since 2019 has been the Nintendo Switch console. In late 2020, both the Xbox Series S and PlayStation 5 were released with high demand. Due to the demand and current worldwide chip shortage, it's very difficult to purchase these popular consoles, though constraints have been easing. In addition, 'scalping', the practice of purchasing popular gaming parts and selling much higher than market price, has made it difficult for the average person to purchase the latest technology at an accessible price."
          ),
          br(),
          plotlyOutput("salesplot", height = "550px"),
          br()
        ),
        
        
        
        # Country Map: Ranking by Revenue tab
        tabPanel(
          "Games and Countries: Map",
          br(),
          h3("Map: Top 11 Countries of 2020 and 2021 by Revenue"),
          br(),
          p(
            "The interactive map below currently only displays the top 11 countries of 2019 and 2020, ranked by video game revenue. Eleven countries are displayed as one country changed on the Top 10 list between the 2 years. More countries will be added beyond the top 10 ranked by revenue, as soon as we can gain access to that data."
          ),
          p(
            "Some examples of available data in pop-in include: country name, ranking, revenue, exports, and active palyer info."
          ),
          br(),
          leafletOutput("mymap", height = "600px"),
          br()
        ),
        
        # release date delays tab
        tabPanel(
          "Games and Covid-19",
          br(),
          
          h3("Game Company Stocks Over Time"),
          p(
            "Below we see the change in price of stocks for the top 10 Game companies. In March of 2020, approximately the time that covid-19 began to spread, there was a slight dip in stock prices across all companies followed by a steady rise, particularly for Microsoft. It should be noted that all companies, with the exception of NTES, experienced rapid increase in stock value in the years following the pandemic. "
          ),
          h4("The top 10 game companies are:"),
          tags$div(list(
            p("Microsoft (MSFT)"),
            p("Sony (SONY)"),
            p("Nintendo (NTDOY)"),
            p("Tencent (TCEHY)"),
            p("Activision Blizzard (ATVI-recently acquired by MSFT)"),
            p("Electronic Arts (EA)"),
            p("Bandai Namco (NCBDY)"),
            p("NetEase (NTES)"),
            p("Square Enix (SQNNY)"),
            p("Take-Two Interactive (TTWO)")
          )),
          br(),
          plotlyOutput("stockplot", height = "550px"),
          tags$hr(),
          br(),
          
          h3("Video Game Delays by Dates (2019-2022)"),
          br(),
          p(
            "In the graph below, we see an extensive list of gmes that were delayed between 2019 and 2022. All data points were selected from a larger collection of games because these entries were data complete. Nonetheless, a simple pattern appears, more games were delayed during 2020 through 2022 than in 2019."
          ),
          br(),
          plotlyOutput("dly", height = "650px"),
          br(),
          p(
            "This graph displays the same information as above, but shows it framed as days delayed instead of dates delayed."
          ),
          br(),
          plotlyOutput("dly2",  height = "750px"),
          tags$hr(),
          br(),
          h4("User Instructions For This Tab:"),
          h6(
            "The search field in the sidebar corresponds to the last visualization regarding mental health, see below."
          ),
          tags$hr(),
          br(),
          p(
            "As referenced above in the instructions for the sidebar functionality for this page, below displays the impact of gaming on mental health during the Covid-19 pandemic. Select a category from:"
          ),
          tags$div(list(
            p("Age"),
            p("Gender"),
            p("Occupation"),
            p("Well-Being"),
            p("Academic Degree"),
            p("Gaming Frequency"),
            p("Change in Gaming Frequency"),
            p("Covid Gaming Frequency"),
            p("Change in Gaming Type")
          )),
          p(
            "Each selection will display the distribution of those who responded yes/no to the question whether playing video games during the pandemic affected mental health. Of those who responded yes, the distribution of positive/negative impacts will be visible."
          ),
          br(),
          h3("Impact of Gaming on Mental Health During Covid-19"),
          br(),
          plotOutput("mho", height = "650px"),
          br(),
          
          value = 3
        ),
        
        # twitter data tab
        tabPanel(
          "Games and Twitter Sentiment",
          br(),
          
          p(
            "The wordclouds below are generated from tweets scraped from the Twitter API."
          ),
          p(
            "We chose to look at the top ten most popular/best selling games from 2019, 2020, and 2021, respectively. For each year, we collected tweets mentioning the top ten most popular games for that year."
          ),
          p(
            "After cleaning the data, we were left with around 3000 tweets for each year. As we can see from the 2019 wordcloud, the word 'twitch' appears to be the most frequently used term, with 207 mentions. Similarly, words like 'stream', 'streamer', 'live', 'community', and 'mixer'
            (a former streaming platform), all have relatively high frequency. This signifies the rise in popularity of live streaming most likely caused by people being stuck at home during the lockdown period of the pandemic. This trend continues to be evident in the wordcloud for 2020,
            as many of the same terms are used."
          ),
          p(
            "As for 2021, 'halo' appears to be the most frequently used term with 487 mentions, most likely due to the highly anticipated release of 'Halo: Infinite', the latest installment of the 'Halo' franchise. Nonetheless, words like 'twitch' and 'stream'
            are still very prominent. Generally speaking, it is safe to say from this text analysis that Covid-19 played a major role in pushing the idea of live streaming and platforms, like twitch, into mainstream popularity."
          ),
          
          h3("Wordcloud: 2019"),
          tags$img(
            src = "wc2019.png",
            width = '100%',
            height = '100%'
          ),
          #wordcloud2Output('my_wc19', height = 500),
          tags$hr(),
          br(),
          h3("Wordcloud: 2020"),
          tags$img(
            src = "wc2020.png",
            width = '100%',
            height = '100%'
          ),
          #wordcloud2Output('my_wc20', height = 500),
          tags$hr(),
          br(),
          h3("Wordcloud: 2021"),
          tags$img(
            src = "wc2021.png",
            width = '100%',
            height = '100%'
          ),
          #wordcloud2Output('my_wc21', height = 500),
          br()
        )
        
        
      )
    ) #main panel
  )
)
