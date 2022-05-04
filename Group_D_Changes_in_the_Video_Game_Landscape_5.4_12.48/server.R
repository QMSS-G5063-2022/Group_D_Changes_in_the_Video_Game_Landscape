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

# shiny app
head(gamesall)


shinyServer(function(input, output) {
  # genre, ratings, games input
  df <- reactive({
    req(input$genre1, input$genre2, input$platf)
    gamesall %>%
      filter(str_detect(genre, input$genre1)) %>%
      filter(str_detect(genre, input$genre2)) %>%
      filter(str_detect(plat_list, input$platf))
  })
  
  # MH input
  mh_choice <- reactive({
    req(input$mh)
    input$mh
  })
  
  # game delay input
  delay_choice <- reactive({
    req(input$delay)
    input$delay
  })
  
  # output1: shiny interactive plot: search genre and render games' player rating
  output$games_rating <- renderPlotly({
    g <-
      ggplot(
        data = df(),
        aes(
          x = year,
          y = overall_rating,
          game_name = game_name,
          released_date = released_date
        )
      ) +
      geom_boxplot(fill = "#FE6E89", alpha = 0.2) +
      geom_jitter(
        color = "#FE6E89",
        width = 0.2,
        size = 0.5,
        alpha = 0.8
      ) +
      coord_flip() +
      ggtitle("Player Ratings", subtitle = " - Games") +
      scale_y_continuous(breaks = seq(0.5, 5, by = 0.5)) +
      scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
      xlab("Year") +
      ylab("Player Scored Game Ratings") +
      theme_tufte()
    
    ggplotly(g,
             tooltip = c("game_name", "year", "overall_rating", "released_date"))
  })
  
  # output2: shiny interactive plot: search genre and render games' meta rating
  output$games_meta <- renderPlotly({
    p <-
      ggplot(
        data = df(),
        aes(
          x = year,
          y = metacritic,
          game_name = game_name,
          released_date = released_date
        )
      ) +
      geom_boxplot(fill = "#00BBDD", alpha = 0.2) +
      geom_jitter(
        color = "#00BBDD",
        width = 0.2,
        size = 0.5,
        alpha = 0.8
      ) +
      coord_flip() +
      ggtitle("Metacratic Ratings", subtitle = " - Games") +
      scale_y_continuous(breaks = seq(20, 100, by = 10)) +
      scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
      xlab("Year") +
      ylab("Metacritic Scored Game Ratings") +
      theme_tufte()
    
    ggplotly(p,
             tooltip = c("game_name", "year", "metacritic", "released_date"))
  })
  
  #output 3: datatable for all games
  output$gamesdt <- DT::renderDataTable(
    games_DT,
    elementId = "games_dt",
    option = list(
      order = list(5, "desc"),
      initComplete = JS(
        "function(settings, json) {$(this.api().table().header()).css({'color' : 'white'});}"
      )
    ),
    callback = DT::JS(callback1)
  )
  
  # output 4: plotly interactive plot: genre popularity over the years by player rating
  output$t5_genre_rating <- renderPlotly({
    g1 <-
      ggplot(
        data = top_5_genre_rating,
        aes(
          x = year,
          y = rating,
          fill = genre,
          size = count,
          genre = genre,
          metacritic = metacritics
        )
      ) +
      geom_point(alpha = 0.3) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
      scale_y_continuous(breaks = seq(0, 5, by = 0.2)) +
      labs(fill = "Genre") +
      xlab("Year") +
      ylab("Player Scored Top 5 Genres") +
      theme_tufte()
    
    ggplotly(g1,
             tooltip = c("year", "count", "genre", "metacritic", "rating"))
  })
  
  # output 5: plotly interactive plot: genre popularity over the years by meta rating
  output$t5_genrer_meta <- renderPlotly({
    g3 <-
      ggplot(
        data = top_5_genre_meta,
        aes(
          x = year,
          y = metacritics,
          fill = genre,
          size = count,
          genre = genre,
          rating = rating
        )
      ) +
      geom_point(alpha = 0.3) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
      scale_y_continuous(breaks = seq(68, 80, by = 1)) +
      labs(fill = "Genre") +
      xlab("Year") +
      ylab("Metacritic Scored Top 5 Genres") +
      theme_tufte()
    
    ggplotly(g3,
             tooltip = c("year", "count", "genre", "metacritics", "rating"))
  })
  
  # output 6: plotly interactive: platform popularity over the year by game counts
  output$platform <- renderPlotly({
    g5 <-
      ggplot(data = platform_by_year, aes(x = year, y = count, color = platform)) +
      geom_line() +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
      labs(fill = "Platform") +
      ggtitle("Counts of Released Games per Platform Between 2011 to 2021") +
      xlab("Year") +
      ylab("Counts of Games") +
      theme_tufte()
    
    ggplotly(g5, tooltip = c("year", "count", "platform"))
  })
  
  # output 7: mental health
  output$mho <- renderPlot({
    if (mh_choice() == "age group") {
      age +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ `age group`) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Age Group') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16)
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
      
    } else if (mh_choice() == "change in game type") {
      gm_type +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ `change in game type`) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Change in Game Type Played During Pandemic') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16)
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
      
    } else if (mh_choice() == "degree") {
      deg +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ degree) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Academic Degree') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16)
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
      
    } else if (mh_choice() == "frequency") {
      gm_freq +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ `covid gaming frequency`) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Gaming Frequency During Pandemic') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16)
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
      
    } else if (mh_choice() == "gender") {
      gend +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ gender) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Gender') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16),
          panel.spacing = unit(1, "lines")
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
      
    } else if (mh_choice() == "occupation") {
      occu +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ `occupation group`) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Occupation Status') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16),
          panel.spacing = unit(1, "lines")
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
      
    } else {
      wb_imp +
        geom_density(aes(fill = `Impact Type`), alpha = 0.5) +
        coord_flip() +
        facet_wrap(~ `well-being impact`) +
        labs(title = "Video Gaming Impact on Well-being During Covid-19 Pandemic",
             subtitle = 'by Well-being Impact Type') +
        theme_tufte() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 16)
        ) +
        scale_fill_discrete_qualitative(palette = "Set 3")
    }
  })
  
  # output 8: delay
  output$dly <- renderPlotly({
    myplot
  })
  # output 9: delay2
  output$dly2 <- renderPlotly({
    myplot2
  })
  # output 10: map
  output$mymap <- renderLeaflet({
    mm
  })
  # output 11: sales data
  output$salesplot <- renderPlotly({
    salesplot
  })
  # output 12: stock data
  output$stockplot <- renderPlotly({
    stockplot
  })
  #output 13: wordcloud
  output$my_wc19 <- renderWordcloud2({
    wc2019
  })
  output$my_wc20 <- renderWordcloud2({
    wc2020
  })
  output$my_wc21 <- renderWordcloud2({
    wc2021
  })
})
