library(shiny)
library(tidyverse)
library(fmsb)
library(ggplot2)
library(DT)

load("spotify_mpd_01.RData")

# filter cols, NA 
new_track_attr <- track_attr |> 
  select(track_id, artists, track_name, danceability, energy, loudness,
         acousticness, valence, tempo, track_genre) |>
  drop_na()

#normalize function
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  # return ((x - min(x)) / (max(x) - min(x)))
}

#scaled tracks
scaled_tracks <- new_track_attr |>
  ungroup() |>
  mutate(across(c(danceability, energy, loudness, acousticness, valence, tempo), normalize))

# matrix
track_matrix <- scaled_tracks |>
  select(danceability, energy, loudness, acousticness, valence, tempo) |>
  as.matrix()

#norm_tracks 
norm_tracks <- sqrt(rowSums(track_matrix^2))

#id?
search_choices <- setNames(new_track_attr$track_id, 
                           paste(new_track_attr$track_name, "-", new_track_attr$artists))

ui <- fluidPage(
  titlePanel("Music Recommender"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "search_select",
        label = "Search and Select 3 songs:",
        choices = NULL, #we will load in server
        multiple = T, # Set to TRUE for multi-selection
        options = list(
          placeholder = 'Start typing to search...',
          maxItems = 3
        )
      ),
      actionButton("btn_recommend", "Get Recommendation", class = "btn-primary btn-lg", width = "100%"),
      uiOutput("warning_msg")
    ),
    
    mainPanel(
      # h4("You selected:"),
      # verbatimTextOutput("selected_value"),
      # h4("Recommendations"),
      # tableOutput("resultsTable"), 
      # uiOutput("chooseResultUi"),
      # plotOutput("radarCharts")
      
      tabsetPanel(
        tabPanel("recommendation result", 
                 h3("Top 10 Recommendations"),
                 DTOutput("resultsTable")
        ),
        tabPanel("Music Profile", 
                 h4("Music Profile"),
                 uiOutput("chooseResultUi"),
                 h4("Compared to Your Songs"),
                 plotOutput("radarCharts"),
                 h4("More Info"),
                 textOutput("artist"),
                 textOutput("genre"),
                 textOutput("explicit"),
                 textOutput("key"),
                 textOutput("timesig"),
                 textOutput("duration")
        ),
        tabPanel("Vector DNA", 
                 h4("Feature Vector Comparison (Parallel Coordinates)"),
                 plotOutput("parCoordPlot"),
                 p("Each line represents one song"),
                 p("RED: my mean vec, BLUE: rec songs")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # loading
  updateSelectizeInput(session, "search_select", choices = search_choices, server = TRUE)
  
  recommendation_data <- eventReactive(input$btn_recommend, {
    
    cat("button ok\n")
    
    # input validation
    
    output$warning_msg <- renderUI({
      req(input$btn_recommend)
      
      if (length(input$search_select) != 3) {
        div(style = "color: red; font-weight: bold; margin-top: 10px; text-align: center;",
            paste("Currently selected", length(input$search_select), ". Please select 3 songs"))
      } 
    })
    
    req(length(input$search_select) >= 3)
    
    # user_input
    selected_ids <- input$search_select
    
    # target vector
    target_vector <- scaled_tracks |>
      filter(track_id %in% selected_ids) |>
      select(danceability, energy, loudness, acousticness, valence, tempo) |>
      colMeans(na.rm = TRUE)
    
    cat("vector ok\n")
    
    # cosine similarity
    dot_product <- track_matrix %*% target_vector
    norm_target <- sqrt(sum(target_vector^2))
    cosine_scores <- as.vector(dot_product / (norm_tracks * norm_target))
    
    cat ("cosine ok\n")
    
    # result
    rec_result <- new_track_attr |>
      ungroup() |>
      mutate(similarity_score = cosine_scores) |>
      filter(!track_id %in% selected_ids) |>
      arrange(desc(similarity_score)) |>
      head(10) |>
      mutate(
        spotify_url = paste0(
          "https://open.spotify.com/search/",
          URLencode(paste(track_name, artists), reserved = TRUE)
        ),
        track_name = paste0(
          '<a href="', spotify_url, '" target="_self">', track_name, '</a>'
        )
      )
    
    return(list(table = rec_result, target_profile = target_vector))
  })
  
  # text output
  output$result_text <- renderText({
    req(recommendation_data()) # wait for button
    "Top 10 Recommendations"
  })
  
  # table
  output$resultsTable <- renderDT({
    req(recommendation_data())
    df <- recommendation_data()$table
    
    # hide spotify_url column in DT (0-based indexing)
    url_col_idx <- which(names(df) == "spotify_url") - 1
    
    datatable(
      df,
      escape = FALSE,   # IMPORTANT: render <a> as link
      rownames = FALSE,
      options = list(
        pageLength = 10,
        columnDefs = list(list(targets = url_col_idx, visible = FALSE))
      )
    ) |>
      formatStyle(
        'similarity_score',
        background = styleColorBar(c(0, 1), 'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  
  #result dropdown
  output$chooseResultUi = renderUI({
    options = recommendation_data()$table
    options = new_track_attr |> filter(track_id %in% options$track_id)
    selectInput("resultSelect", "Select Result", choices = options$track_name)
    #choices = paste(options$track_name, " - ", options$artists))
  })
  
  #reactive value of user input
  user_input_rv = reactive({
    new_input = input$search_select
    track_attr |> filter(track_id %in% new_input) |> 
      select(track_id, artists, track_name, popularity, danceability, energy, loudness,
        acousticness, valence, tempo, track_genre, key, duration_ms, explicit, 
        time_signature)
  })
  
  #reactive of result select
  result_rv = reactive({
    resultInput = input$resultSelect
    compared = recommendation_data()$table
    compared = track_attr |> filter(track_id %in% compared$track_id) |>
      select(track_id, artists, track_name, popularity, danceability, energy, loudness,
             acousticness, valence, tempo, track_genre, key, duration_ms, explicit, 
             time_signature)
    compared = compared |> filter(track_name == resultInput) #|> select(-liked, -is_genre, -predicted_score, -spotify_url)
    compared
  })
  
  # radar
  output$radarCharts = renderPlot({
    req(input$resultSelect)
    used_colors = c("red", "lightgreen", "darkgrey", "lightskyblue")
    radar_vars = c("popularity", "danceability", "energy", "loudness",
                   "acousticness", "valence", "tempo")

    # ungroup(): track_attr is grouped by track_id, and select() on a
    # grouped_df always keeps the grouping column -- which would add a
    # meaningless track_id axis to the radar.
    compared = rbind(result_rv(), user_input_rv()) |>
      ungroup() |>
      select(track_name, all_of(radar_vars))

    # Axis max/min must come from numeric columns only. apply() pushes the
    # frame through as.matrix(), which coerces everything to character when
    # any column is character, making max/min a lexicographic comparison.
    # loudness is negative for most tracks, so its axis rendered inverted:
    # quiet songs plotted as loud.
    num = compared |> select(-track_name)
    maxrow = as.data.frame(t(apply(num, 2, max)))
    minrow = as.data.frame(t(apply(num, 2, min)))
    max_min = rbind(maxrow, minrow)
    max_min$track_name = c("Max", "Min")

    compared = rbind(max_min[, names(compared)], compared) |>
      mutate(across(-track_name, as.numeric)) |> column_to_rownames("track_name")
    radarchart(compared, pcol = used_colors)
    legend("bottomleft", legend = rownames(compared[-c(1, 2),]),
           bty = "n", pch = 20, col = used_colors)
  })
  
  #Info Outputs
  output$artist = renderText({
    req(input$resultSelect)
    df = result_rv()
    paste("Artist(s):", str_replace_all(df$artists, ";", " and "))
  })
  
  output$genre = renderText({
    req(input$resultSelect)
    df = result_rv()
    paste("Genre: ", df$track_genre)
  })
  
  output$explicit = renderText({
    req(input$resultSelect)
    df = result_rv()
    paste("Explicit: ", df$explicit)
  })
  
  output$key = renderText({
    req(input$resultSelect)
    df = result_rv()
    numkey = c("C", "C#", "D", "D#", "E", "F", "F#", "G", 
            "G#", "A", "A#", "B")
    # Spotify pitch class is 0-11 (0 = C). levels = 1:12 rendered key 0 as
    # NA and labelled every other key a semitone flat.
    paste("Key:", as.character(factor(df$key, levels = 0:11, labels = numkey)))
  })
  
  output$timesig = renderText({
    req(input$resultSelect)
    df = result_rv()
    paste0("Time Signature: ", df$time_signature, "/4")
  })
  
  output$duration = renderText({
    req(input$resultSelect)
    df = result_rv()
    paste("Duration:", round((df$duration_ms) / 1000), "s")
  })
  
  # Parallel Coordinates Chart
  output$parCoordPlot <- renderPlot({
    req(recommendation_data())
    
    # data 
    rec_table <- recommendation_data()$table
    target_vec <- recommendation_data()$target_profile
    
    # my vec
    target_df <- as.data.frame(t(target_vec)) |>
      mutate(Type = "My Profile", track_name = "User Average")
    
    # 10 recs
    rec_vectors <- scaled_tracks |>
      filter(track_id %in% rec_table$track_id) |>
      select(danceability, energy, loudness, acousticness, valence, tempo, track_name) |>
      mutate(Type = "Recommended")
    
    # merge data
    plot_data <- bind_rows(target_df, rec_vectors)
    
    # pivot_longer(wide -> long)
    plot_data_long <- plot_data |>
      pivot_longer(cols = c(danceability, energy, loudness, acousticness, valence, tempo),
                   names_to = "Feature",
                   values_to = "Value")
    
    
    ggplot(plot_data_long, aes(x = Feature, y = Value, group = track_name, color = Type, size = Type, alpha = Type)) +
      geom_line() +
      geom_point() +
      
      scale_color_manual(values = c("My Profile" = "red", "Recommended" = "steelblue")) +
      scale_size_manual(values = c("My Profile" = 2, "Recommended" = 0.8)) +  
      scale_alpha_manual(values = c("My Profile" = 1, "Recommended" = 0.5)) + 
      
      theme_minimal() +
      ylim(0, 1) +
      labs(title = "Visualizing Song Vectors",
           subtitle = "Do the recommended songs follow your taste pattern?",
           x = "Audio Features",
           y = "Normalized Value (0-1)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"))
  })
  
}

shinyApp(ui, server)
