#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
install.packages('shiny')
#install.packages('rsconnect')
#library(rsconnect) 
#rsconnect::setAccountInfo(name='jessica-rojas', token='F027A3F0B7E2D3AD1E8961133567247E', secret='3YzSDBOhTWaefho4n5nAAsHR9Df/hEZ5CX1cNd2x')
#rsconnect::deployApp('/cloud/project')

# Look at cocktails with fruits in them, for fun!
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

fruit_list <- tolower(c("Kiwi", "Banana", "Apricot", "Avocado", "Cocos", "Clementine", "Mandarine", "Orange","Cranberry","Cherry","BLackcurrant","Lime", "Lemon", "Peach", "Plum", "Raspberry", "Strawberry", "Pineapple", "Pomegranate","Limon","Apple"))

fruitycocktails <- cocktails %>%
    mutate(ingredient = tolower(ingredient)) %>%
    filter(ingredient %in% fruit_list) %>%
    select(drink) %>%
    unique()

# Filter out fruity drinks
fdrinks <- cocktails %>%
    group_by(drink) %>%
    right_join(fruitycocktails)

fdrinks2 <- fdrinks %>% 
    mutate(ingredient = tolower(ingredient)) %>%
    filter(ingredient %in% fruit_list) %>% 
    mutate(alcoholic = tolower(alcoholic))


ui <- fluidPage(  
    titlePanel("The Community of Fruity Drinks"),
    sidebarLayout(
        sidebarPanel(pickerInput("ingredient","Select Fruit:", 
                                 choices=c(unique(fdrinks2$ingredient)),options = list(`actions-box` = TRUE),multiple = TRUE),
                     pickerInput("alcoholic","Select Type:", 
                                 choices=c(unique(fdrinks2$alcoholic)),options = list(`actions-box` = TRUE),multiple = TRUE),
        sliderInput(session,inputId = "input$n",
            label = "Number of Drinks",
            min = 0,
            max = 45,
            value = 0, step=1)),
         mainPanel(tabsetPanel(type = "tabs",
                               tabPanel("Plot", plotlyOutput("plot2")),
                               tabPanel("Summary", plotlyOutput("plot3")),
                               tabPanel("Table", plotlyOutput("plot4"))
            ))))
 
server <- function(input, output,session) {
    observe({
        print(input$ingredient)})
    
    d <- reactive({
       fdrinks2 %>%
            filter(ingredient %in% input$ingredient,alcoholic %in% input$alcoholic) %>%
                       select(one_of(c("ingredient", input$ingredient))) %>%
                       gather(ingredient, drink)})

    output$plot2 <- renderPlotly({
        
       d() %>%
                  distinct(drink,ingredient) %>% #subset to distinct drinks
                  group_by(ingredient) %>%
                  count() %>%
                  ggplot(aes(y=reorder(ingredient,desc(-n)),x=n,fill=ingredient))+
                  geom_point(size=4,aes(colour=ingredient),show.legend=FALSE) +
                  xlab('Number of Drinks') +
                  ylab('Fruit') +
                  coord_flip() +
                  theme(legend.position = "none") +
                  theme_minimal()
    
        #output$plot3 <- renderPlotly({ 
            #d() %>% 
            #mutate(alcoholic = tolower(alcoholic)) %>% 
           # distinct(drink,ingredient,alcoholic) %>% #subset to distinct drinks
            #group_by(ingredient,alcoholic) %>%
            #count() %>%
           # ggplot(aes(x=reorder(ingredient,desc(-n)),y=n,fill=alcoholic))+
           # geom_col() +
           # geom_text(aes(label=n),position=position_stack(0.5))+
           # xlab('Fruit') +
           # ylab('Number of Drinks') +
           # coord_flip() +
           # scale_fill_hue('') +
            #theme(legend.position = 'bottom') +
           # theme_minimal()
        
        })} #)}
 
shinyApp(ui, server)
