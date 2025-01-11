library(shiny)
library(tidyverse)
library(bslib)
library(htmltools)
library(shinyWidgets)

pitches = c("Fastball", 
            "Four-Seam", 
            "Sinker", 
            "Slider", 
            "Sweeper", 
            "Curveball", 
            "Changeup", 
            "Splitter", 
            "Cutter")

pitch_colors = c("Fastball" = '#d7191c', 
                 "Four-Seam" = '#d7191c', 
                 "Sinker" = "#fdae61", 
                 "Slider" = "#A020F0", 
                 "Sweeper" = "magenta",
                 "Curveball" = '#2c7bb6', 
                 "Changeup" = '#90EE90',
                 "Splitter" = '#90EE32',
                 "Cutter" = "pink")
# list.files(".")
# CHANGE THE LINE BELOW TO POINT TO THE FILE PATH OF THE TRACKMAN DATA YOU WANT TO GENERATE A PITCH REPORT FROM
df <- read_csv("./Documents/app_state_analytics/trackman_analytics/20220308-JimPerry-1.csv") 

df <- df %>% 
  filter(is.na(HorzBreak) == F) %>% 
  mutate(
    
    PitchType = TaggedPitchType,
    
    Pitcher = str_replace_all(Pitcher, "(\\w+), (\\w+)", "\\2 \\1"),
    
    inZone = case_when(
      between(PlateLocHeight, 1.6, 3.4) & between(PlateLocSide, -0.71, 0.71) ~ 1,
      T ~ 0
    ),
    
    Chase = case_when(
      inZone == 0 & PitchCall %in% c('FoulBall', 
                                     'FoulBallNotFieldable',
                                     'InPlay',
                                     'StrikeSwinging') ~ 1,
      T ~ 0
    ),
    
    CustomGameID = paste0(
      Date,
      ": ",
      str_sub(AwayTeam, 1,3),
      " @ ",
      str_sub(HomeTeam, 1,3)
    )
  )

home_plate_segments <- data.frame(
  x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
  y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
  xend = c(0.71, 0.71, 0, -0.71, -0.71, 0),
  yend = c(0.15, 0.3, 0.5, 0.3, 0.15, 0.15)
)

# UI ####

ui <- page_sidebar(
  title = "Pitch Report Shiny App",
  sidebar = sidebar(
    title = "Select Pitcher/Game",
    selectInput(
      inputId = "PitcherInput", 
      label = "Select Pitcher", 
      choices = sort(unique(df$Pitcher)),
      selectize = T
    ),
    pickerInput(
      inputId = "GameInput",
      label = HTML("Select Game<br>(Selects all by default)"),
      choices = unique(df$CustomGameID),
      options = list(`actions-box` = TRUE),
      multiple = T
    ),
    pickerInput(
      inputId = "BatterHand",
      label = HTML("Select Batter Hand<br>(Selects both by default)"),
      choices = unique(df$BatterSide),
      selected = unique(df$BatterSide),
      options = list(`actions-box` = TRUE),
      multiple = T
    )
  ),
  navset_tab(
    nav_panel(title = "Movement & Metrics", plotOutput("movement"), tableOutput("metrics")),
    nav_panel(title = "Clock", plotOutput("clock")),
    nav_panel(title = "Velo Distribution", plotOutput("velo_density")),
    nav_panel(title = "Locations", plotOutput("locations")),
    nav_panel(title = "Whiffs, Chase, Called Strikes", 
              plotOutput("whiffs"), 
              plotOutput("chase"), 
              plotOutput("calledStrikes")
    )
  )
)

server <- function(input, output, session){
  
  observeEvent(input$PitcherInput,
               
               updatePickerInput(
                 session,
                 inputId = "GameInput",
                 choices = sort(unique(df$CustomGameID[df$Pitcher == input$PitcherInput])),
                 selected = sort(unique(df$CustomGameID[df$Pitcher == input$PitcherInput]))
               )
               
  )
  
  output$movement <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(
          Pitcher == input$PitcherInput,
          CustomGameID %in% c(input$GameInput),
          BatterSide %in% c(input$BatterHand)
        )
    })
    
    dataFilter() %>%
      ggplot(aes(HorzBreak, InducedVertBreak)) +
      geom_point(aes(color = PitchType), na.rm = TRUE, alpha = .9, size = 2.5) + 
      scale_x_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) + 
      scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) +
      scale_color_manual(values = pitch_colors) + 
      geom_hline(yintercept = 0, linetype = 2) + 
      geom_vline(xintercept = 0, linetype = 2) + 
      theme_bw(base_size = 20) + 
      labs( 
        title = paste0(input$PitcherInput, ": Pitch Movement"), 
        y = "Induced Vertical Break", 
        x = "Horizontal Break", 
        color = "Pitch Type" 
      ) +
      theme( 
        plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
        legend.position = "right",
        legend.text = element_text(size = 12)
      ) +
      coord_fixed() + 
      guides(color = guide_legend(override.aes = list(size = 3))) 
    
  })
  
  
  output$clock <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand)
        )
    })
    
    
    degree_to_clock <- function(degree) {
      labels <- c("6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00")
      labels[((degree / 30) %% 12) + 1]
    }
    
    labels_df <- data.frame(
      angle = c(100, 100, 100, 100),
      radius = c(1000, 1500, 2000, 2500),
      label = c("1000 rpm", "1500 rpm", "2000 rpm", "2500 rpm")
    )
    
    dataFilter () %>%
      group_by(PitchType) %>%
      summarise(
        avgSpinAxis = mean(SpinAxis),
        avgSpin = mean(SpinRate)
      ) %>%
      ggplot(aes(avgSpinAxis, avgSpin)) +
      geom_col(aes(fill = PitchType), width = 5, color = "black") +
      scale_fill_manual(values = pitch_colors) +
      geom_text(data = labels_df, aes(x = angle, y = radius, label = label), color = "black", size = 3) +
      coord_polar(start = pi) + 
      scale_x_continuous(
        limits = c(0, 360), 
        breaks = seq(0, 360, 30), 
        labels = degree_to_clock(seq(0, 360, 30)), 
      ) +
      theme_minimal(base_size = 15) + 
      theme(
        
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.5),
        axis.title =  element_blank() 
      )
    
  })
  
  
  output$metrics <- renderTable({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand)
        )
    })
    
    dataFilter() %>%
      filter(CustomGameID %in% c(input$GameInput), BatterSide %in% c(input$BatterHand)) %>%
      group_by(PitchType) %>%
      summarize(
        Total = n(),
        Velo = round(mean(RelSpeed, na.rm = T), 1),
        Max = round(max(RelSpeed, na.rm = T), 1),
        iVB = round(mean(InducedVertBreak, na.rm = T), 1),
        HB = round(mean(HorzBreak, na.rm = T), 1),
        VAA = round(mean(VertApprAngle, na.rm = T), 1),
        HAA = round(mean(HorzApprAngle, na.rm = T), 1),
        Spin = round(mean(SpinRate, na.rm = T)),
        `Height (rel)` = round(mean(RelHeight), 1),
        `Side (rel)` = round(mean(RelSide), 1),
        Extension = round(mean(Extension, na.rm = T), 1),
        `Whiff%` = paste0(round(sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging",
                                                                                          "InPlay",
                                                                                          "FoulBallFieldable",
                                                                                          "FoulBallNotFieldable")) * 100), "%"),
        `CSW%` = paste0(round(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled")) / n() * 100), "%")
      )
  }, digits = 1)
  
  output$velo_density <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand)
        )
    })
    
    mean_velo <- dataFilter() %>%
      group_by(PitchType) %>%
      summarise(mean_speed = round(mean(RelSpeed, na.rm = TRUE), 1))
    
    dataFilter() %>%
      ggplot(aes(RelSpeed, color = PitchType, fill = PitchType)) +
      scale_color_manual(values = pitch_colors) +
      scale_fill_manual(values = pitch_colors) +
      geom_density(alpha = 1, fill = NA, show.legend = FALSE, trim = T) +
      geom_density(alpha = 0.4, color = NA, show.legend = FALSE, trim = T) +
      facet_wrap(~PitchType, ncol = 1, scales = "free_y", strip.position = "left") + 
      geom_text(
        data = mean_velo, 
        aes(
          x = mean_speed, 
          label = sprintf("%.2f", mean_speed)
        ),
        y = 0, 
        vjust = -0.5, 
        color = "black", 
        size = 3
      ) +
      theme(axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            strip.text.y.left = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "gray", size = 0.1),
            panel.grid.minor.y = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
      labs(y = NULL, x = "Pitch Velo") # plot labels
    
  })
  
  output$locations <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand))
    })
    
    dataFilter() %>%
      ggplot(aes(PlateLocSide, PlateLocHeight)) + 
      geom_point(na.rm = TRUE) +
      facet_wrap(~PitchType, ncol = 4) + 
      stat_density2d(geom = 'raster', aes(fill = after_stat(density)), contour = F, na.rm = TRUE) +
      ylim(0,5) + 
      xlim(-3, 3) +
      scale_fill_gradientn(colours = c(
        "#f7f7f7", 
        "#fff7bc",
        "#fee391",
        "#fec44f",
        "#fe9929",
        "#ec7014",
        "#cc4c02",
        "#993404",
        "#662506"
      )) +
      
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black") +
      
      labs(title = paste0(input$PitcherInput, ": Pitch Location (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") + 
      
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
            strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
            strip.text = element_text(size = 15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
            panel.background = element_rect(fill = "#f7f7f7")) +
      guides(fill = "none") +
      coord_fixed() +
      geom_segment( # this draws in a plate, helps visualize the pov
        data = home_plate_segments, 
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "black"
      )
  })
  
  
  output$whiffs <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand)
        )
    })
    
    dataFilter() %>%
      filter(PitchCall == "StrikeSwinging") %>%
      ggplot(aes(PlateLocSide, PlateLocHeight)) +
      geom_point(aes(color = PitchType), na.rm = TRUE, size = 3, alpha = 0.85) +
      scale_color_manual(values = pitch_colors) +
      ylim(0,5) +
      xlim(-3, 3) +
      annotate("rect", xmin = -.71, xmax = .71,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black") +
      labs(title = paste0(input$PitcherInput, ": Whiff's (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
            strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
            strip.text = element_text(size = 15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
            panel.background = element_rect(fill = "#f7f7f7")) +
      guides(fill = "none") +
      coord_fixed() +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black")
    
  })
  
  output$chase <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand)
        )
    })
    
    dataFilter() %>%
      filter(Chase == 1) %>%
      ggplot(aes(PlateLocSide, PlateLocHeight)) +
      geom_point(aes(color = PitchType), na.rm = TRUE, size = 3, alpha = 0.85) +
      scale_color_manual(values = pitch_colors) +
      ylim(0,5) +
      xlim(-3, 3) +
      annotate("rect", xmin = -.71, xmax = .71,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black") +
      labs(title = paste0(input$PitcherInput, ": Chase's (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
            strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
            strip.text = element_text(size = 15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
            panel.background = element_rect(fill = "#f7f7f7")) +
      guides(fill = "none") +
      coord_fixed() +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black")
  })
  
  output$calledStrikes <- renderPlot({
    
    dataFilter <- reactive({
      df %>%
        filter(Pitcher == input$PitcherInput,
               CustomGameID %in% c(input$GameInput),
               BatterSide %in% c(input$BatterHand)
        )
    })
    
    dataFilter() %>%
      filter(PitchCall == "StrikeCalled") %>%
      ggplot(aes(PlateLocSide, PlateLocHeight)) +
      geom_point(aes(color = PitchType), na.rm = TRUE, size = 3, alpha = 0.85) +
      scale_color_manual(values = pitch_colors) +
      ylim(0,5) +
      xlim(-3, 3) +
      annotate("rect", xmin = -.71, xmax = .71,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black") +
      labs(title = paste0(input$PitcherInput, ": Called Strikes (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
            strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
            strip.text = element_text(size = 15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
            panel.background = element_rect(fill = "#f7f7f7")) +
      guides(fill = "none") +
      coord_fixed() +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black")
  })
}

shinyApp(ui, server)
runApp("Projects")