
library(shiny)
#setwd("C:/Users/Karen/Desktop/Jupyter/1040Project2")
crel <- readRDS("crel.Rds")
harry_wc <- readRDS("harry.Rds")
ron_wc <- readRDS("ron.Rds")
hermione_wc <- readRDS("hermione.Rds")
dumbledore_wc <- readRDS("dumbledore.Rds")
hagrid_wc <- readRDS("hagrid.Rds")
ginny_wc <- readRDS("ginny.Rds")
neville_wc <- readRDS("neville.Rds")
mcgonagall_wc <- readRDS("mcgonagall.Rds")
fred_wc <- readRDS("fred.Rds")
george_wc <- readRDS("george.Rds")
snape_wc <- readRDS("snape.Rds")
sirius_wc <- readRDS("sirius.Rds")
draco_wc <- readRDS("draco.Rds")
voldemort_wc <- readRDS("voldemort.Rds")
hedwig_wc <- readRDS("hedwig.Rds")

ui <- fluidPage(
  
  #App Title
  titlePanel("Harry Potter Character Relationship Analysis"),
  
  #Sidebar Layout (inputs and outputs)
  sidebarLayout(
    
    #Sidebar layout for inputs
    sidebarPanel(
      
      #Select Viewpoint character
      selectInput("char1", "Select a viewpoint character.", 
                  c("harry", "ron", "hermione", "dumbledore", "hagrid", "ginny", "neville", "mcgonagall", 
                    "fred", "george", "snape", "sirius", "draco", "voldemort", "hedwig"), selected = NULL),
      
      #Select Target Character
      selectInput("char2", "Select a target character (different from viewpoint character)", 
                  c("ron", "harry", "hermione", "dumbledore", "hagrid", "ginny", "neville", "mcgonagall", 
                    "fred", "george", "snape", "sirius", "draco", "voldemort", "hedwig"), selected = NULL),
      
      #Additional Info
      helpText("Note: The plot visualizes the relationship scores based on how often the characters appear together in each book.
               The wordcloud is based on 3grams generated that includes the viewpoint character."),
      
      actionButton("update", "Update View")
      
      #For Wordcloud
      #sliderInput("freq", "Minimum Frequency:", min=1, max = 50, value = 15),
      #sliderInput("max", "Maximum Number of Words:", min = 1, max = 300, value = 100)
      
    ),
    
    #Main Panel for outputs
    mainPanel(
      
      #Set up 2 tabs for line plot and word cloud
      tabsetPanel(
        tabPanel("Character Relationship Scores", 
                 h5("The scores in the graph below indicate how strong the relationship is between the two characters based on how frequently the target character appears in the text surrounding the viewpoint character."),
                 h5("A high score indicates that there is a strong relationship between the two characters, while a low score indicates that there is a weak relationship."),
                 plotOutput("crelplot")),
        tabPanel("Viewpoint Character Wordcloud", 
                 h5("Word Cloud below are based on 3grams Analysis of the viewpoint character"),
                 plotOutput("wcloud",width = "100%",height=700))
      )
    )
  )
)


server <- function(input, output) {
  
  #Output for Character Relationship Score tab
  library(ggplot2)
  library(stringi)
  
  vchar <- eventReactive(input$update, {
    input$char1
  })
  
  tchar <- eventReactive(input$update, {
    input$char2
  })
  
  cchar <- eventReactive(input$update, {
    stri_c(vchar(),"_",tchar())
  })
  
  xvar <- "Book"
  
  cplot <- eventReactive(input$update, {
    ggplot(data=crel,aes_string(x=xvar, y=cchar(), group=1))+geom_line(color="red")+
      geom_point(size=3, color="red")+labs(x="", y = "Relationship Score", title =cchar())+
      geom_text(aes_string(label=cchar()), vjust=1, hjust=1)
  })
  
  output$crelplot <- renderPlot({cplot()})
  
  #Output for Wordcloud tab
  library(wordcloud)
  library(tm)
  library(RColorBrewer)
  
  wchar <- eventReactive(input$update, {
    stri_c(vchar(),"_wc")
  })
  
  wctxt <- eventReactive(input$update, {
    switch(wchar(),
           "harry_wc"=harry_wc,
           "ron_wc"=ron_wc,
           "hermione_wc"=hermione_wc,
           "dumbledore_wc"=dumbledore_wc,
           "hagrid_wc"=hagrid_wc,
           "ginny_wc"=ginny_wc,
           "neville_wc"=neville_wc,
           "mcgonagall_wc"=mcgonagall_wc,
           "fred_wc"=fred_wc,
           "george_wc"=george_wc,
           "snape_wc"=snape_wc,
           "sirius_wc"=sirius_wc,
           "draco_wc"=draco_wc,
           "voldemort_wc"=voldemort_wc,
           "hedwig_wc"=hedwig_wc)
  }, ignoreNULL = FALSE)
  
  wc <- eventReactive(input$update, {
    suppressWarnings(wordcloud(wctxt(), excludeWords = NULL, max.words=100, 
                               scale = c(8,1), colors=brewer.pal(4, "Dark2")))
  })
  
  output$wcloud <- renderPlot({wc()})
  
}


shinyApp(ui=ui, server=server)