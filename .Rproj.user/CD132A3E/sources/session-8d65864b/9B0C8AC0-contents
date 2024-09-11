library(shiny)
library(leaflet)
library(bslib)
library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(akima)

#TODO Add Leaflet map of buoy location,
#TODO Add value boxes of current swell, direction, and period
#TODO Add more graphs of seasonal swell, direction, period
#TODO Add to Github

# Load in Data 
# Reading Erddap from direct link
current_time <- format(Sys.time(), "T%H:%M:%SZ", tz = "UTC") %>% 
  str_split_1(pattern = ":")

current_date <- Sys.Date()
erddap_url <- glue("https://erddap.sensors.axds.co/erddap/tabledap/edu_ucsd_cdip_142.json?time%2Clatitude%2Clongitude%2Cz%2Csea_surface_wave_mean_period%2Csea_surface_wave_period_at_variance_spectral_density_maximum%2Csea_surface_wave_significant_height%2Csea_surface_wave_from_direction%2Cstation&time%3E=2000-01-01T04%3A45%3A00Z&time%3C={current_date}{current_time[1]}%3A{current_time[2]}%3A{current_time[3]}")

# res = GET(erddap_url)

# full_erddap_list <- fromJSON(rawToChar(res$content))
# full_erddap_data <- as.data.frame(full_erddap_list$table$rows)
# colnames(full_erddap_data) <- full_erddap_list$table$columnNames
# 
# # Filter and mutate to get non-blank wave periods
# filtered_erddap_data <- full_erddap_data %>%
#   filter(!is.na(sea_surface_wave_mean_period)) %>%
#   mutate(
#     time = as.Date(time),
#     year_month = lubridate::floor_date(time, unit = "month"),
#     across(starts_with("sea"), as.numeric),
#     sea_surface_wave_significant_height_ft = sea_surface_wave_significant_height * 3.28084,
#     month = lubridate::month(time, label = TRUE)
#   ) %>%
#   relocate(c("year_month", "month"), .after = time) %>%
#   mutate(
#     season = case_when(
#       month %in% c("Dec", "Jan", "Feb") ~ "Winter",
#       month %in% c("Mar", "Apr", "May") ~ "Spring",
#       month %in% c("Jun", "Jul", "Aug") ~ "Summer",
#       TRUE ~ "Autumn"
#     )
#   )
# 
# # Summarize by month----
# summarized_erddap_data <- filtered_erddap_data  %>%
#   group_by(month) %>%  # group
#   group_split() %>% # split
#   lapply(., function(x) {
#     grouped_frame <- with(
#       x,
#       interp(
#         x = sea_surface_wave_from_direction,
#         y = sea_surface_wave_significant_height_ft,
#         z = sea_surface_wave_mean_period,
#         #extrap = TRUE,
#         nx = 100,
#         ny = 100,
#         duplicate = "mean"
#       )
#     ) %>%
#       interp2xyz() %>%
#       as.data.frame()
# 
#     group_month <- x %>%
#       pull(month) %>%
#       unique()
# 
#     final_frame <- grouped_frame %>%
#       mutate(month = group_month) %>%
#       filter(!is.na(z))
# 
#   }) %>%
#   bind_rows() %>%
#   rename(
#     sea_surface_wave_from_direction = x,
#     sea_surface_wave_significant_height_ft = y,
#     sea_surface_wave_mean_period = z
#     )
# 
# unique_months <- unique(summarized_erddap_data$month)

# Define UI for application that draws a histogram
ui <- page_fillable(
  navset_card_tab( 
    nav_panel("Wave Rose", 
              id = "wave_rose_tab",
              layout_columns(
                page_sidebar(
                  bg = "4cc9f0",
                  title = "Filters",
                  open = "desktop",
                  fill = TRUE,
                  sidebar = sidebar(
                    title = "Controls",
                    selectInput(
                      'month_selecter',
                      'Months Filter',
                      selected = unique_months,
                      choices = unique_months,
                      multiple=TRUE,
                      selectize=TRUE
                    )
                  ),
                ),
                plotOutput("wave_polar_plot", width = "100%", height = "100%", fill = TRUE)
              ),
              ), 
    nav_panel("Wave Time Series", "Page B content"), 
    nav_panel("Third", "Page C content"), 
    nav_spacer(),
    nav_menu( 
      "Other links", 
      nav_panel("D", "Panel D content"), 
      "----", 
      "Description:", 
      nav_item( 
        a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab"
)

server <- function(input, output) {
  
  data <- reactive({
    summarized_erddap_data %>% 
      filter(month %in% input$month_selecter)
  })
  
  output$wave_polar_plot <- renderPlot({
    ggplot(data(), 
           aes(x = sea_surface_wave_from_direction, y = sea_surface_wave_significant_height_ft, z = sea_surface_wave_mean_period)) +
      geom_contour_filled(alpha = 0.75, breaks = c(2, 4, 6, 8, 10, 12, 14, 16)) + 
      scale_x_continuous(limits = c(180, 360),
                         breaks = seq(0, 360, by = 45),
                         minor_breaks = seq(0, 360, by = 15),
                         labels = function(x) paste0(x, " °")) +
      coord_radial(r.axis.inside = TRUE, expand = FALSE, start = pi, end = 2 * pi) +
      scale_y_continuous(labels = function(x) paste0(x, " ft")) +
      labs(
        fill = "Period (Sec)",
        #x = "Direction (Degrees)",
        #y = "Mean Significant Wave Height (Ft)",
        #title = "CDIP San Francisco Bar, CA Buoy"
      ) +
      facet_wrap(~ month, nrow = 2) +
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title.position = "bottom",
            legend.title = element_text(hjust = 0.5),
            legend.key.height= unit(1, 'cm'),
            legend.key.width= unit(2, 'cm'),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()
      ) +
      guides(fill = guide_colorsteps(
        ticks = TRUE, 
        show.limits = TRUE,
        even.steps = FALSE,
        frame.linewidth = 0.55, 
        frame.colour = "black", 
        ticks.colour = "black",
        ticks.linewidth = 0.3)
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
