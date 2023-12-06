library(shiny)
library(tidytuesdayR)
library(bslib)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(geomtextpath)
library(lubridate)
library(magrittr)
library(GGally)


tuesdata <- tidytuesdayR::tt_load(2023, week = 48)

drwho_episodes <- tuesdata$drwho_episodes
drwho_directors <- tuesdata$drwho_directors
drwho_writers <- tuesdata$drwho_writers


#Breaking down the episodes per year
year_counts <- drwho_episodes %>% 
  group_by(year = lubridate::floor_date(first_aired, "year")) %>%
  count() %>% mutate(Year = str_split_i(as.character(year), "-", 1))

year_rating <- drwho_episodes %>% 
  group_by(year = lubridate::floor_date(first_aired, "year")) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>% mutate(Year = str_split_i(as.character(year), "-", 1))


rating_by_epi <- drwho_episodes %>% mutate(Year = str_split_i(as.character(first_aired), "-", 1))

year_viewers <- drwho_episodes %>% 
  group_by(year = lubridate::floor_date(first_aired, "year")) %>%
  summarise(avg_viewers = mean(uk_viewers, na.rm = TRUE)) %>% mutate(Year = str_split_i(as.character(year), "-", 1))


viewers_by_epi <- drwho_episodes %>% mutate(Year = str_split_i(as.character(first_aired), "-", 1))


monthly_breakdown <- drwho_episodes %>% mutate(Month = str_split_i(as.character(first_aired), "-", 2)) %>% 
  mutate(full_month = case_when(Month == "01" ~ "January",
                                Month == "02" ~ "Febuary",
                                Month == "03" ~ "March",
                                Month == "04" ~ "April",
                                Month == "05" ~ "May", 
                                Month == "06" ~ "June", 
                                Month == "07" ~ "July",
                                Month == "08" ~ "August",
                                Month == "09" ~ "September",
                                Month == "10" ~ "October", 
                                Month == "11" ~ "November", 
                                Month == "12" ~ "December")) %>%
  mutate(full_month = factor(full_month)) %>% 
  mutate(full_month = fct_relevel(full_month, c("January", "Febuary" ,"March", 
                                                "April", "May", "June", 
                                                "July", "August", "September", 
                                                "October", "November", "December"))) %>%
  arrange(full_month) %>% mutate(Year = str_split_i(as.character(first_aired), "-", 1))

year_code = c("2005", "2006", "2007", "2008", "2009", 
              "2010", "2011", "2012", "2013", "2014", 
              "2015", "2016", "2017", "2018", "2019", "2020", 
              "2021", "2022")

full_df <- merge(drwho_writers, drwho_episodes, by = "story_number")
full_df <- merge(full_df, drwho_directors, by = "story_number")
full_df %<>% select(-c(era, duration, type, production_code, episode_title, serial_title, story_number, writer, director)) %>% 
  mutate(Month = str_split_i(as.character(first_aired), "-", 2)) %>% 
  mutate(full_month = case_when(Month == "01" ~ "January",
                                Month == "02" ~ "Febuary",
                                Month == "03" ~ "March",
                                Month == "04" ~ "April",
                                Month == "05" ~ "May", 
                                Month == "06" ~ "June", 
                                Month == "07" ~ "July",
                                Month == "08" ~ "August",
                                Month == "09" ~ "September",
                                Month == "10" ~ "October", 
                                Month == "11" ~ "November", 
                                Month == "12" ~ "December")) %>%
  mutate(full_month = factor(full_month)) %>% 
  mutate(full_month = fct_relevel(full_month, c("January", "Febuary" ,"March", 
                                                "April", "May", "June", 
                                                "July", "August", "September", 
                                                "October", "November", "December"))) %>%
  arrange(full_month) %>% mutate(Year = str_split_i(as.character(first_aired), "-", 1)) %>% mutate(episode_number = as.numeric(episode_number)) %>% 
  mutate(Year = as.numeric(Year)) %>% mutate(rating = as.numeric(rating)) %>% mutate(uk_viewers = as.numeric(uk_viewers))

#this will be a good opportunity to try building connections across datasets, a skill that may become useful as we build out tmato

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "lux"),
  # Application title
  titlePanel("Doctor Who!?"),
  
  navlistPanel(
    id = "tabset",
    "",
    tabPanel("Yearly Breakdown", plotOutput("year_histogram"), 
             plotOutput("year_rating_histo"), plotOutput("year_viewers_histo")),
    
    tabPanel("Monthly Breakdown",
             fluidRow(
      column(6,
             selectInput("year_code", "Year", choices = year_code)
      )), 
      plotOutput("year_select_rating_histo"), plotOutput("year_select_viewers_histo")),
    
    tabPanel("Random Associations", 
             fluidRow(
               column(6,
                      selectInput("param_1", "X Axis", choices = colnames(full_df))
               )), 
             fluidRow(
               column(6,
                      selectInput("param_2", "Y Axis", choices = colnames(full_df), selected = "episode_number")
               )), plotOutput("associations")
             )
  )
)
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    selected <- reactive({
     monthly_breakdown %>%
        dplyr::filter(Year == input$year_code)
    })
  
    selected_1 <- reactive({
      full_df %>%
        dplyr::select(input$param_1)
    })
    
    selected_2 <- reactive({
      full_df %>%
        dplyr::select(input$param_2)
    })
    

    # Render the histogram
    output$year_histogram <- renderPlot({
      year_counts %>% ggplot(aes(x = Year, y = n)) + geom_bar(stat = "identity", fill="#37a7e7", color="#e9ecef") +
        theme_bw() + theme(axis.text.x =  element_text(angle = 90, vjust = 0.5)) +
        xlab("Year") + ylab("Number of Episodes") +
        ggtitle("Episodes per Year")
    })
    
    output$year_rating_histo <- renderPlot({
      year_rating %>% ggplot(aes(x = Year, y = avg_rating)) + geom_bar(stat = "identity", fill="#1CECD9", color="#e9ecef") +
        geom_point(data = rating_by_epi, aes(x = Year, y = rating), position = "jitter", alpha = 0.5) + 
        theme_bw() + theme(axis.text.x =  element_text(angle = 90, vjust = 0.5)) +
        xlab("Year") + ylab("Show Ratings") +
        ggtitle("Show Ratings")
    })
    
    output$year_viewers_histo <- renderPlot({
      year_viewers %>% ggplot(aes(x = Year, y = avg_viewers)) + geom_bar(stat = "identity", fill="#1CEC97", color="#e9ecef") +
        geom_point(data = viewers_by_epi, aes(x = Year, y = uk_viewers), position = "jitter", alpha = 0.5) + 
        theme_bw() + theme(axis.text.x =  element_text(angle = 90, vjust = 0.5)) +
        xlab("Year") + ylab("Show Viewers") +
        ggtitle("Show Viewers")
    })
    
   
    output$year_select_rating_histo <- renderPlot({
      monthly_breakdown %>% ggplot(aes(x = full_month, y = rating, fill = full_month)) + geom_violin() +
        geom_jitter(alpha = 0.35, color = "#F66193", width = 0.27) + geom_point(data = selected(), size = 8, color = "#E11818") +
        theme_bw() + theme(axis.text.x =  element_text(angle = 90, vjust = 0.5)) +
        xlab("Month") + ylab("Rating") +
        ggtitle("Rating per Month") + scale_fill_viridis_d(option = "G", guide = "none")
    })
    
    
    output$year_select_viewers_histo <- renderPlot({
      monthly_breakdown %>% ggplot(aes(x = full_month, y = uk_viewers, fill = full_month)) + geom_violin() +
        geom_jitter(alpha = 0.35, color = "#F66193", width = 0.27) + geom_point(data = selected(), size = 8, color = "#E11818") +
        theme_bw() + theme(axis.text.x =  element_text(angle = 90, vjust = 0.5)) +
        xlab("Month") + ylab("Viewers") +
        ggtitle("Viewers per Month") + scale_fill_viridis_d(option = "G", guide = "none")
    })

    output$associations <- renderPlot({
      
      df <- cbind(selected_1(), selected_2())
      
      df %>% ggplot(aes(x = .[[1]], y = .[[2]])) + geom_point() + xlab(input$param_1) + 
        ylab(input$param_2)
    })
    
  }
  # Run the application 
  shinyApp(ui = ui, server = server)
  