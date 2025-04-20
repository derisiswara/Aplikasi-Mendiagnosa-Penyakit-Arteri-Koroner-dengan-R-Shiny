library(randomForest)
library(shiny)
library(tidymodels)

# Define UI ----
ui <- fluidPage(
  
  # Title Panel
  titlePanel("Aplikasi Mendiagnosa Penyakit Arteri Koroner"),
  
  # Add some padding and spacing for better appearance
  fluidRow(  
    column(4,
           numericInput("num1",h5("Usia (age)"),value = 1, min = 0),
           selectInput("select2", h5("Jenis Kelamin (sex)"),choices = list("Perempuan" = 0, "Laki-laki" = 1), selected = 1),  
           selectInput("select3", h5("Jenis Nyeri Dada (cp)"),
                       choices = list("Angina Tipikal" = 1, "Angina Atypical" = 2, "Nyeri Non-Anginal" = 3, "Asimptomatik" = 4), selected = 1),
           numericInput("num4",h5("Tekanan Darah (trestbps)"),value = 120, min = 0),
           numericInput("num5",h5("Kolesterol (choi)"),value = 200, min = 0),
           selectInput("select6", h5("Gula Darah Puasa (fbs)"),choices = list("Tidak" = 0, "Ya" = 1), selected = 1)
    ),  
    column(4,
           selectInput("select7", h5("EKG pada Istirahat (restecg)"),
                       choices = list("Normal" = 0, "Kelainan ST-T" = 1, "Hipertrofi Ventrikel Kiri" = 2), selected = 1),
           numericInput("num8",h5("Denyut Jantung Maksimal (thalach)"),value = 150, min = 0),
           selectInput("select9", h5("Angina pada Latihan (exang)"),choices = list("Tidak" = 0, "Ya" = 1), selected = 1),
           numericInput("num10",h5("Depresi ST pada Latihan (oldpeak)"),value = 1, min = 0),
           selectInput("select11", h5("Kemiringan Segmen ST (slope)"),
                       choices = list("Menanjak" = 1, "Datar" = 2, "Menurun" = 3), selected = 1),
           selectInput("select12", h5("Jumlah Pembuluh Darah (ca)"),
                       choices = list("0" = 0, "1" = 1, "2" = 2, "3" = 3), selected = 0)
    ),
    column(4,
           selectInput("select13", h5("Thallium Stress Test (thai)"),
                       choices = list("Normal" = 3, "Defek Tetap" = 6, "Defek yang Dapat Dipulihkan" = 7), selected = 3),
           actionButton("submitBtn", "Submit", class = "btn-primary"),
           # Initially hide the output area
           uiOutput("value")
    )
  ),
  
  # Add some spacing at the bottom
  tags$br()
)

# Load the model
model_old = readRDS("rf_R.rda")

# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive expression to prepare input data
  mydata <- reactive({
    test <- data.frame(input$num1, input$select2, input$select3, input$num4, input$num5, input$select6, 
                       input$select7, input$num8, input$select9, input$num10, input$select11, input$select12, 
                       input$select13)
    names(test) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", 
                     "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai")
    
    test <- transform(
      test,
      age = as.integer(age),
      sex = factor(sex, levels = c("0", "1")),
      cp = factor(cp, levels = c("1", "2", "3", "4")),
      trestbps = as.integer(trestbps),
      choi = as.integer(choi),
      fbs = factor(fbs, levels = c("0", "1")),
      restecg = factor(restecg, levels = c("0", "1", "2")),
      thalach = as.integer(thalach),
      exang = factor(exang, levels = c("0", "1")),
      oldpeak = as.numeric(oldpeak),
      slope = factor(slope, levels = c("1", "2", "3")),
      ca = factor(ca, levels = c("0", "1", "2", "3")),
      thai = factor(thai, levels = c("3", "6", "7"))
    )
    
    return(test)
  })
  
  # Reactive expression for prediction
  data <- eventReactive(input$submitBtn, {
    test <- mydata()
    # Predict using the model
    pred = predict(model_old, test)
    
    # Print result based on prediction
    if (pred == 1) {
      return("Terdiagnosa Penyakit Arteri Koroner")
    } else {
      return("Tidak Terdiagnosa Penyakit Arteri Koroner")
    }
  })
  
  # Render the prediction output only after clicking the submit button
  output$value <- renderUI({
    req(data())  # Make sure data is available before rendering
    div(
      h4("Hasil Diagnosa:"),
      p(data())
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
