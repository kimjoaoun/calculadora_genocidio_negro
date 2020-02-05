library(shiny)
library(shinyWidgets)
library(glue)


# UI-Part   
ui <- fluidPage(

  ## Estética
  shinyWidgets::setBackgroundColor(color = "#560510"), # BG Colour: Black or Dark Blue
  tags$head(tags$style("body {color:white;}")), # Letter Colour
  tags$head(tags$style(".well {background-color:#6C0B0B;}")), # SB Colour

  ## Funcionamento
titlePanel(h3("Calculadora do Genocidio Negro",  #Titulo
         align = "center"),
  ),
  shiny::h4("No Brasil, a cada 23 minutos um jovem negro é assassinado. [Mapa da Violência, 2014]", 
            align = "center"), #Subtitulo.
  
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
        
        knobInput(
            inputId = "quantidade",
            label = "Selecione a quantidade:",
            value = 1,
            min = 1,
            max = 300,
            displayPrevious = TRUE, 
            lineCap = "round",
            fgColor = "#960B0B",
            inputColor = "#CC0000"
        ),
        
      selectInput(
        inputId = "enquantovc",
        label = "Enquanto você...",
        choices = c(
          "assou esses bolo(s)",
          "correu 1K distancia",
          "assistiu essa quantidade de aulas",
          "lavou essas louças",
          "fez essas refeições",
          "trabalhou esses dias",
          "pediu pizzas",
          "ficou esse tempo nas redes sociais",
          "voou essas vezes Rio-SP"
        ),
        selected = "Selecione uma atividade",
        multiple = FALSE
      ),
    ),
    mainPanel(strong(textOutput("text")),
              br(),
              uiOutput("imagem"),
              br(),
              textOutput("text2")),
  )


)



# Server-side
server <- function(input, output) {
    timetable = tibble::tribble(~action, ~time_min, ~img_name,
                    "assou esses bolos(s)", 40, "bolo.png",
                    "correu 1K distancia", 6, "corredor.png",
                    "assistiu essa quantidade de aulas", 2, "aulas.png",
                    "lavou essas louças", 10, "loucas.png",
                    "fez essas refeições", 60, "comida.png",
                    "trabalhou esses dias", 480, "work.png",
                    "pediu pizzas", 15, "pizza.png",
                    "ficou esse tempo nas redes sociais", 180, "social_media.png",
                    "voou essas vezes Rio-SP", 45, "aviao.png")
    
    
    output$text <- renderText({
        quantidade = input$quantidade
        n_assas = round((as.numeric(timetable[grep(pattern = input$enquantovc, x = timetable$action), 2])/23)*quantidade,
                        digits = 1)
        
        glue('Enquanto você {input$enquantovc}, foram assasinadas {n_assas} pessoas negras.')
    })
     output$text2 <- renderText({
         tempo2 = (timetable[grep(pattern = input$enquantovc, x = timetable$action), 2])*input$quantidade
         quant = input$quantidade
         glue("No Brasil, enquanto você faz essa atividade cotidiana por {quant} vezes, o que demoraria {tempo2} minutos isso é o que acontece nas periferias.")
     })
     
     output$imagem <- renderUI({
         img_loc = timetable[grep(pattern = input$enquantovc, x = timetable$action), 3]
         img(src = img_loc, height = "200px", style="display: block; margin-left: auto; margin-right: auto;")
     })
}

# Run the application
shinyApp(ui = ui, server = server)
