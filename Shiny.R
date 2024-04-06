install.packages("data.table")
library(data.table)
install.packages("ggplot2")
library(ggplot2)
install.packages("shiny")
library(shiny)
install.packages("DT")
library(DT)

##############################   WCZYTYWANIE DANYCH  ##################################################################################################
y1987 <- as.data.table(read.csv("1987.csv.bz2"))
y1988 <- as.data.table(read.csv("1988.csv.bz2"))
y1989 <- as.data.table(read.csv("1989.csv.bz2"))
y1990 <- as.data.table(read.csv("1990.csv.bz2"))
y1991 <- as.data.table(read.csv("1991.csv.bz2"))
y1992 <- as.data.table(read.csv("1992.csv.bz2"))
y1993 <- as.data.table(read.csv("1993.csv.bz2"))
y1994 <- as.data.table(read.csv("1994.csv.bz2"))
y1995 <- as.data.table(read.csv("1995.csv.bz2"))
y1996 <- as.data.table(read.csv("1996.csv.bz2"))
y1997 <- as.data.table(read.csv("1997.csv.bz2"))
y1998 <- as.data.table(read.csv("1998.csv.bz2"))
y1999 <- as.data.table(read.csv("1999.csv.bz2"))
y2000 <- as.data.table(read.csv("2000.csv.bz2"))
y2001 <- as.data.table(read.csv("2001.csv.bz2"))
y2002 <- as.data.table(read.csv("2002.csv.bz2"))
y2003 <- as.data.table(read.csv("2003.csv.bz2"))
y2004 <- as.data.table(read.csv("2004.csv.bz2"))
y2005 <- as.data.table(read.csv("2005.csv.bz2"))
y2006 <- as.data.table(read.csv("2006.csv.bz2"))
y2007 <- as.data.table(read.csv("2007.csv.bz2"))
y2008 <- as.data.table(read.csv("2008.csv.bz2"))

years <- list(y1987, y1988, y1989, y1990, y1991, y1992, y1993, y1994, y1995, y1996, y1997,
              y1998, y1999, y2000, y2001, y2002, y2003, y2004, y2005, y2006, y2007, y2008)
carriers <- as.data.table(read.csv("carriers.csv"))
planes <- as.data.table(read.csv("plane-data.csv"))

################################### Ilośc lotów dla danych linii lotniczych ###############################################################################
#    PRZYKŁAD w 2000 roku - zwykly wykres
dt_y2000 <- as.data.table(years[[14]])
carrier_counts <- dt_y2000[, .N, by = UniqueCarrier] # zliczanie lotów dla danych przewożników


ggplot(carrier_counts, aes(x = UniqueCarrier, y = N)) + # Tworzenie wykresu słupkowego
  geom_bar(stat = "identity", fill = "purple") +
  ggtitle("Number of Flights by Carrier in 2000") +
  xlab("Carrier") +
  ylab("Number of Flights")


############################################SHINY APP - ile lotów #################################################################3######################

# Tworzenie serwera
server <- function(input, output) {
  output$carrier_plot <- renderPlot({
    dt_filtered <- years[[((input$year_slider)-1986)]]
    carrier_counts <- dt_filtered[, .N, by = UniqueCarrier] # Zliczanie lotów dla każdego przewoźnika
    #Tworzenie bar plota
    ggplot(carrier_counts, aes(x = UniqueCarrier, y = N)) +
      geom_bar(stat = "identity", fill = "purple") +
      ggtitle(paste("Number of Flights by Carrier in", input$year_slider)) +
      xlab("Carrier") +
      ylab("Number of Flights")
  })
  
  # lista przewoźników (pokazuje tylko te które są w danym roku/wykresie)
  output$carrier_table <- renderDataTable({ 
    carriers_subset <- carriers[Code %in% unique(years[[((input$year_slider)-1986)]][, UniqueCarrier])]
    carriers_subset
  })
  
  #tworzenie Summary (który przewoźnik najmniej/najwiecej lotów w danym roku)
  output$carrier_summary <- renderTable({
    dt_filtered <- years[[((input$year_slider)-1986)]] #Odpowiedni rok
    
    carrier_counts <- dt_filtered[, .N, by = UniqueCarrier]# Zliczanie lotów dla każdego przewoźnika
    
    max_carrier <- carrier_counts[which.max(N), UniqueCarrier]#Który
    min_carrier <- carrier_counts[which.min(N), UniqueCarrier]
    max_flights <- carrier_counts[which.max(N), N]#Ile
    min_flights <- carrier_counts[which.min(N), N]
    max_airline <- carriers[max_carrier == Code, Description]#Pelna nazwa
    min_airline <- carriers[min_carrier == Code, Description]
    
    # Tworzenie ramki z wynikami do wyswietlenia
    summary_data <- data.frame(
      Rok = input$year_slider,
      Przewoźnik_Najwięcej = max_carrier,
      Maksymalna_Liczba_Lotów = max_flights,
      Przewoźnik_Najmniejszy = min_carrier,
      Minimalna_Liczba_Lotów= min_flights
    )
    
    summary_data
  })
}

# Tworzenie UI
ui <- fluidPage(
  titlePanel("Number of Non-cancelled Flights by Carrier"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year_slider",
        "Year:",
        min = 1987,
        max = 2008,
        value = 2000,
        step = 1,
      )
    ),
    mainPanel(
      plotOutput("carrier_plot"),
      dataTableOutput("carrier_table"),
      h2("Summary"),
      tableOutput("carrier_summary")
    )
  )
)

shinyApp(ui = ui, server = server)


#############################################ENGINE TYPES Z SUWAKIEM###############################################################################
#(W KOŃCU UŻYTE COŚ INNEGO BEZ SUWAKA ŻEBY BYŁO LEPIEJ WIDZIEĆ NA RAZ) można zignorować

filtered_planes <- planes[!is.na(year) & year != "" & year != "None"  & year != "0000"]
min_year <- min(filtered_planes$year)
max_year <- max(filtered_planes$year)

# tWORZENIE UI
ui <- fluidPage(
  titlePanel("Planes by Year and Engine Type"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year", 1946, 2008, value = 1948, step = 1)
    ),
    mainPanel(
      plotOutput("engineTypePlot")
    )
  )
)

# TWORZENIE SERWERA
server <- function(input, output) {
  filtered_data <- reactive({
    filtered_planes[year == input$year & !is.na(engine_type) & engine_type != ""]
  })
  
  plane_counts <- reactive({
    data <- filtered_data()
    engine_counts <- data[, .N, by = .(year, engine_type)]
    return(engine_counts)
  })
  
  output$engineTypePlot <- renderPlot({
    data <- plane_counts()
    
    # Tworzenie bar plota
    ggplot(data, aes(x = engine_type, y = N, fill = engine_type)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Plane Count by Engine Type for Year", input$year),
           x = "Engine Type", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)

#################### WYKRESY - ENGINE TYPES (PODZIELONE NA DWA GRAFY 1946-1982, 1983-2008) #########################################################
filtered_planes <- planes[!is.na(year) & year != "" & year != "None" & year != "0000" & !is.na(engine_type) & engine_type != "" & engine_type != "None"]

# Tworzenie UI
ui <- fluidPage(
  titlePanel("Planes by Year and Engine Type"),
  mainPanel(
    h3("Total Number of Planes Created:"),
    verbatimTextOutput("totalPlanes"),
    br(),
    h3("Engine Type Summary (1946-1982):"),
    DT::dataTableOutput("engineTypeTable1"),
    br(),
    plotOutput("engineTypePlot1"),
    br(),
    h3("Engine Type Summary (1983-2008):"),
    DT::dataTableOutput("engineTypeTable2"),
    br(),
    plotOutput("engineTypePlot2")
  )
)

# SERWER
server <- function(input, output) {
  engine_counts_all <- filtered_planes[, .N, by = engine_type] # Ile których in total
  
  
  # dla lat 1946-1982
  filtered_planes1 <- filtered_planes[year >= 1946 & year <= 1982]
  engine_counts1 <- filtered_planes1[, .N, by = engine_type]#Ile 
  # dla lat 1983-2008
  filtered_planes2 <- filtered_planes[year >= 1983 & year <= 2008]
  engine_counts2 <- filtered_planes2[, .N, by = engine_type]#Ile 
  
  # Ile w sumie samolotów
  total_planes <- nrow(filtered_planes)
  output$totalPlanes <- renderPrint({
    total_planes
  })
  
  # Ile których tabelka 1946-1982
  output$engineTypeTable1 <- DT::renderDataTable({
    DT::datatable(engine_counts1, options = list(pageLength = 10))
  })
  
  #  # Ile których tabelka 1983-2008
  output$engineTypeTable2 <- DT::renderDataTable({
    DT::datatable(engine_counts2, options = list(pageLength = 10))
  })
  
  
  output$engineTypePlot1 <- renderPlot({
    data <- filtered_planes1
    # bar plot 1946-2008
    ggplot(data, aes(x = engine_type, fill = engine_type)) +
      geom_bar() +
      labs(title = "Plane Count by Engine Type (1946-1982)",
           x = "Engine Type", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  output$engineTypePlot2 <- renderPlot({
    data <- filtered_planes2
    # bar plot 1983-2008
    ggplot(data, aes(x = engine_type, fill = engine_type)) +
      geom_bar() +
      labs(title = "Plane Count by Engine Type (1983-2008)",
           x = "Engine Type", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}
shinyApp(ui = ui, server = server)



################ENGINE TYPES OVER YEARS wszystkie, WYKRESY z SUMMARY SHINY APP ###########################################################################################################
filtered_planes <- planes[!is.na(year) & year != "" & year != "None" & year != "0000" & !is.na(engine_type) & engine_type != "" & engine_type != "None" & engine_type != " None"& engine_type != " None "]

# UI
ui <- fluidPage(
  titlePanel("Planes by Year and Engine Type"),
  mainPanel(
    h3("Total Number of Planes Created:"),
    verbatimTextOutput("totalPlanes"),
    br(),
    h3("Engine Type Summary:"),
    DT::dataTableOutput("engineTypeTable"),
    br(),
    plotOutput("engineTypePlot")
  )
)

# SERWER
server <- function(input, output) {

  engine_counts_all <- filtered_planes[, .N, by = engine_type] # Ile samolotów z danym enginem in total
  
  # Ile samolotów in total
  total_planes <- nrow(filtered_planes)
  output$totalPlanes <- renderPrint({
    total_planes
  })
  
  # Summary, ile czego
  output$engineTypeTable <- DT::renderDataTable({
    DT::datatable(engine_counts_all, options = list(pageLength = 10))
  })
  
  # PLOT
  output$engineTypePlot <- renderPlot({
    data <- filtered_planes
    # Tworzenie bar plota
    ggplot(data, aes(x = engine_type, fill = engine_type)) +
      geom_bar() +
      labs(title = "Plane Count by Engine Type",
           x = "Engine Type", y = "Count") +
      facet_wrap(~ year, scales = "free") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "Engine Type"))
  })
}

shinyApp(ui = ui, server = server)
