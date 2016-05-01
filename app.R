library(shiny)

Pace <- function(x){
  if (x >= 3600)
    format(as.POSIXct('0001-01-01 00:00:00') + x, "%H:%M:%S")
  else
    format(as.POSIXct('0001-01-01 00:00:00') + x, "%M:%S")}

outSeconds <- function(x){
  if (!is.numeric(x) && !is.na(x)) stop("x must be an integer in seconds")
  unlist(lapply(x, function(i) {
    if (is.na(i))
      NA
    else {
      HH = i %/% 3600; MM = (i%%3600) %/% 60
      if (MM < 10) MM = paste(c(0, MM), collapse = "")
      paste(c(HH, MM),collapse = ":")
    }}))
}

SectoTime <- function(x){format(as.POSIXct('0001-01-01 00:00:00') + x, "%I:%M %p")}

toTime <- function(x){
  sapply(x, function(i){
    ((i + 4*3600) %% (12*3600)) + 3600
  })
}

val <- function(sec, var){
  model = lm(var~Time, data = df)
  newdata = data.frame(Time = sec)
  predict(model, newdata)
}

nms <- c( "Lyon Ridge",        "Red Star Ridge",     "Duncan Canyon", 
          "Robinson Flat",     "Miller's Defeat",    "Dusty Corners", 
          "Last Chance",       "Devil's Thumb",      "El Dorado Creek", 
          "Michigan Bluff",    "Foresthill",         "Dardanelles (Cal-1)",
          "Peachstone (Cal-2)","Rucky Chucky",       "Green Gate",
          "Auburn Lake Trails","Brown's Bar",        "Highway 49",
          "No Hands Bridge",   "Robie Point",        "Finish")


df <- read.csv("wserAll.csv")


ui <- fluidPage(
  titlePanel("Western States 100 Mile Foot Race"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "hour", 
                   label = "Choose the desired hour finish", 
                   value = 24, min = 1, max = 30),
      sliderInput(inputId = "min",
                   label = "Choose the desired minute finish",
                   value = 0, min = 0, max = 59), width = 4),
    mainPanel(tableOutput("predict"))
  ),

  dataTableOutput("time")
)

server <- function(input, output){
  output$predict <- renderTable({
    
    hour = input$hour
    minute = input$min
    seconds = hour*3600 + minute*60
    
    raw.predict = sapply(df[,8:27], sec = seconds, val)
    chart = data.frame(c(raw.predict, seconds))
    names(chart) = "raw"
    
    chart$`Aid Stations` = nms
    chart$Milage = c(10.50,	16,	23.8,	29.7,	34.4,	38,	43.3,	47.8,	52.9,	55.7,	62,	65.7,	70.7,	78,79.8,85.2,89.9,	93.5,	96.8,	98.9, 100.2)
    chart$`Overall Time` = outSeconds(chart$raw)
    chart$`Time of Day` = sapply((chart$raw + 5*3600), SectoTime)
    
    # calculate pace
    section = numeric(21); section[1] = chart$raw[1]/chart$Milage[1]
    section[2:21] = (chart$raw[2:21]-chart$raw[1:20])/
      (chart$Milage[2:21]-chart$Milage[1:20])
    
    chart$`Section Pace` = sapply(section, Pace)
    chart$`Overall Pace` = sapply((chart$raw/chart$Milage), Pace)
    
    #make it look nicer
    rownames(chart) = c(names(df[,8:27]), "Finish")
    rownames(chart) = NULL
    chart$raw = NULL
    
    chart
    })
  output$time <- renderDataTable({df})
}

shinyApp(ui = ui, server = server)

