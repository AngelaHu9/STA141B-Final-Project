library(shiny)
library(scales)
country <- fromJSON("https://api.covid19api.com/countries")%>%
  arrange(Country,Slug,ISO2)%>%
  select(Country)
df = fromJSON("https://api.covid19api.com/all")
library(lubridate)
confirmed<-data%>%
  select(Country,Confirmed,Date)%>%
  mutate(Date,Date = gsub("T00:00:00Z","",Date))%>%
  mutate(Date,Date=as.Date(Date,"%Y-%m-%d"))

confirmed$Month <- as.Date(cut(confirmed$Date,
                         breaks = "month"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("COVID-19 Monthly Confirmed Cases across the World"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country","country",country),
      checkboxGroupInput("statistics", "Choose statistics:",
                         choiceNames =
                           list("min", "max", "mean", "median"),
                         choiceValues =
                           list("min", "max", "mean", "median")
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("summary")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    c <- input$country
    confirmed %>%
      filter(Country == c)
  })
  output$distPlot <- renderPlot({
    ggplot(data(),
           aes(Month, Confirmed)) +
      stat_summary(fun = sum, # adds up all observations for the month
                   geom = "bar",fill = "#FF6666") +
    scale_x_date(
      labels = date_format("%Y-%m"),
      breaks = "1 month")
  })
  

  
  output$summary <- renderTable({
    d <- data()
    ta <- NULL
    if ("min" %in% input$statistics) {
      ta <- bind_cols(ta, summarize(d, min = min(Confirmed, na.rm = TRUE)))
    }
    if ("max" %in% input$statistics) {
      ta <- bind_cols(ta, summarize(d, max = max(Confirmed, na.rm = TRUE)))
    }
    if ("mean" %in% input$statistics) {
      ta <- bind_cols(ta, summarize(d, mean = mean(Confirmed, na.rm = TRUE)))
    }
    if ("median" %in% input$statistics) {
      ta <- bind_cols(ta, summarize(d, median = median(Confirmed, na.rm = TRUE)))
    }
    ta
  })
}

shinyApp(ui = ui, server = server)

