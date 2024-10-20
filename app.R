rm(list=ls())

library(jsonlite)
library(tidyr)
library(shiny)
library(ggplot2)
library(magick)

url <- "https://github.com/ciciyuminliang/436hw2/raw/main/archive.zip"
download.file(url, destfile = "archive.zip")
unzip("archive.zip")
cardsImgs <- fromJSON("tarot-images.json")
cardsImgs_df <- as.data.frame(cardsImgs$cards)


# meanings→ meanings_light 和 meanings_shadow
cardsImgs_df <- cardsImgs_df %>%
  unnest_wider(meanings, names_sep = ".")
colnames(cardsImgs_df) <- gsub("\\$", ".", colnames(cardsImgs_df))
#print(colnames(cardsImgs_df)) to check


ui <- fluidPage(
  titlePanel("Your Tarot Reading🔮 "),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select Your Reading"),
      
      # show your selection is 1 or 3
      radioButtons("num_cards", "Number of cards to display:", 
                   choices = list("One card (Overall fortune)" = 1, 
                                  "Three cards (Past, Present, Future)" = 3)),
      
      # show selected date time
      dateInput("reading_date", "Choose a date for the reading:", value = Sys.Date()),
      
      # click
      actionButton("draw_cards", "Draw Cards"),
      
      textOutput("reading_type")
    ),
    
    mainPanel(
      
      plotOutput("tarot_cards", height = "500px"),
      
      
      uiOutput("explanation_text"),
      
      
      textOutput("chosen_date"),
      
      h4("Explanation"),
      p("🔮 This Tarot card reading application allows you to choose either one card for an overall fortune, 
        or three cards representing your Past, Present, and Future. The cards are drawn randomly from 
        the Tarot deck, and each card comes with a unique meaning, providing insight based on your selection."),
      p("🔮 To use this application, first choose whether you'd like a single card or a three-card reading. 
        You can also select a date for the reading. After making your selections, click 'Draw Cards' to 
        reveal your Tarot reading."),
      p("🔮 Take it easy! This Tarot reading is for entertainment purposes only. Have fun and enjoy! 😊"),
      p("*If you have some questions,please feel free to contact myu259@wisc.edu")
      
    )
  )
)

server <- function(input, output, session) {
  
  #random select
  cards_selected <- eventReactive(input$draw_cards, {
    num <- as.numeric(input$num_cards)  # number of cards
    selected_cards <- cardsImgs_df[sample(nrow(cardsImgs_df), num), ]  
    selected_cards$position <- ifelse(runif(num) > 0.5, "upright", "reversed")  #reverse！！
    selected_cards  
  })
  
  
  output$reading_type <- renderText({
    if (input$num_cards == 1) {
      "You have selected one card for an overall fortune reading."
    } else {
      "You have selected three cards for a Past, Present, and Future reading."
    }
  })
  
  # date select
  output$chosen_date <- renderText({
    paste("Your tarot reading for the date:", as.character(input$reading_date))
  })
  
  # draw cards
  output$tarot_cards <- renderPlot({
    cards <- cards_selected()
    
    if (is.null(cards)) {
      return()
    }
    
    num <- nrow(cards)
    par(mfrow = c(1, num), mar = c(1, 1, 2, 1))  # 设置1行布局，减少边距使图片更大
    
    for (i in 1:num) {
      img_path <- file.path("cards", cards$img[i])
      
      
      if (!file.exists(img_path)) {
        stop("Image file does not exist at: ", img_path)
      }
      
     
      img <- image_read(img_path)
      if (cards$position[i] == "reversed") {
        img <- image_rotate(img, 180)  # reverse 180
      }
      
      #show pictures
      plot(as.raster(img), main = "", frame.plot = FALSE, axes = FALSE, asp = 1)
    }
  })
  
  # use different meanings
  output$explanation_text <- renderUI({
    cards <- cards_selected()
    
    if (is.null(cards)) {
      return()
    }
    
    num <- nrow(cards)
    explanation <- ""
    
    if (num == 1) {
      meaning <- if (cards$position[1] == "upright") {
        if (!is.null(cards$meanings.light[[1]])) {  
          #delete c()
          paste(unlist(cards$meanings.light[[1]]), collapse = ", ")
        } else {
          "No light meaning available"
        }
      } else {
        if (!is.null(cards$meanings.shadow[[1]])) {
          paste(unlist(cards$meanings.shadow[[1]]), collapse = ", ")
        } else {
          "No shadow meaning available"
        }
      }
      explanation <- paste("The card you selected represents your overall fortune: ",
                           "<strong>", cards$name[1], " (", cards$position[1], ")</strong>.", "<br>", 
                           "This card suggests the following: ", 
                           "<strong>", meaning, "</strong>.")
    } else {
      times <- c("Past", "Present", "Future")
      explanation <- lapply(1:3, function(i) {
        meaning <- if (cards$position[i] == "upright") {
          if (!is.null(cards$meanings.light[[i]])) {
            paste(unlist(cards$meanings.light[[i]]), collapse = ", ")
          } else {
            "No light meaning available"
          }
        } else {
          if (!is.null(cards$meanings.shadow[[i]])) {
            paste(unlist(cards$meanings.shadow[[i]]), collapse = ", ")
          } else {
            "No shadow meaning available"
          }
        }
        paste("Your <strong>", times[i], "</strong> is <strong>", cards$name[i], " (", cards$position[i], ")</strong>.<br>",
              "This card suggests: <strong>", meaning, "</strong>.<br><br>")
      })
      
      explanation <- paste(unlist(explanation), collapse = "")
    }
    
    HTML(explanation)
  })
  
}


shinyApp(ui = ui, server = server)


