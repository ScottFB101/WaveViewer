library(shiny)
library(leaflet)
library(bslib)
library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(akima)
library(bsicons)

#TODO make text bigger on polar graph
#TODO Add Leaflet map of buoy location,
#TODO Add value boxes of current swell, direction, and period
#TODO Add more graphs of seasonal swell, direction, period
  #Make this a whole page of scrollable graphs
#TODO add maestro (package) script that automatically loads data during early hours

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

unique_months <- unique(summarized_erddap_data$month)

first_date_of_data_collection <- min(filtered_erddap_data$time)
last_date_of_data_collection <- max(filtered_erddap_data$time)

####Median Wave Stats processing----
median_stats_height_per_month_day <- filtered_erddap_data %>%
  group_by(month_day) %>% 
  summarize(
    median_wave_height = median(sea_surface_wave_significant_height_ft),
    median_period = median(sea_surface_wave_mean_period)
  ) %>% 
  mutate(
    month_day = paste("2024", month_day, sep = "-"),
    month_day = as.Date(month_day, format = "%Y-%m-%d")
  )

####Current Wave Stats processing----
current_wave_stats <- filtered_erddap_data %>% 
  arrange(desc(time)) %>% 
  slice(1)

####Define Wave Time Series Cards----
wave_series_cards <- list(
  card(
    card_header(glue("Median Wave Height (Ft) by Date ({first_date_of_data_collection}-{last_date_of_data_collection})")),
    plotlyOutput("interactive_wave_height_plot"),
    full_screen = TRUE
  ),
  card(
    card_header(glue("Median Wave Period (Seconds) by Date ({first_date_of_data_collection}-{last_date_of_data_collection})")),
    plotlyOutput("interactive_wave_period_plot"),
    full_screen = TRUE
  )
)

####Define Current Wave Stat Value Boxes----
current_wave_stat_value_boxes <- list(
  value_box(
    title = "Current Wave Height",
    value = paste0(round(current_wave_stats$sea_surface_wave_significant_height_ft, 2), "ft"),
    showcase = bs_icon("tsunami")
  ),
  value_box(
    title = "Current Wave Period",
    value = paste0(round(current_wave_stats$sea_surface_wave_mean_period, 2), "s"),
    showcase = bs_icon("water")
  ),
  value_box(
    title = "Current Wave Direction",
    value = paste0(round(current_wave_stats$sea_surface_wave_from_direction, 2), "°"),
    showcase = bs_icon("compass")
  )
)

#####Define UI----
ui <- page_fillable(
  navset_card_tab( 
    #Nav Panel Wave Time Series
    nav_panel(
      "Current/Historical Swell Stats", 
      layout_columns(
        fill = FALSE,
        !!!current_wave_stat_value_boxes
      ),
      !!!wave_series_cards
    ),
    #Wave Rose nav panel
    nav_panel(
      "Wave Rose", 
              id = "wave_rose_tab",
              #Column layouts
              layout_columns(
                col_widths = c(2, 10),
                #Page Sidebar
                page_sidebar(
                  title = "Filters",
                  open = "desktop",
                  fill = TRUE,
                  #Sidebar
                  sidebar = sidebar(
                    title = "Controls",
                    #Select Input
                    selectInput(
                      'month_selecter',
                      'Months Filter',
                      selected = unique_months,
                      choices = unique_months,
                      multiple=TRUE,
                      selectize=TRUE
                    ),
                    #Action Button
                    actionButton("update_months", "Update Months")
                  ),
                ),
                #Plot Output in first Nav Panel
                plotOutput("wave_polar_plot", width = "100%", height = "100%", fill = TRUE)
              ),
              ), 
    nav_panel("Third", "Page C content"), 
    nav_spacer(),
    nav_menu( 
      "Link to Date Source", 
      nav_item( 
        a("CDIP Data", href = "https://erddap.sensors.axds.co/erddap/info/edu_ucsd_cdip_142/index.html", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab"
)

####Define Server----
server <- function(input, output) {

  # Store the selected months, initially set to default values
  selected_months <- reactiveVal(unique_months)
  
  # Update the selected months only when the action button is clicked
  observeEvent(input$update_months, {
    selected_months(input$month_selecter)
  })
  
  # Reactive data based on selected months
  data <- reactive({
    summarized_erddap_data %>% 
      filter(month %in% selected_months())  # Use the reactive value for selected months
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
  }) %>% bindCache(data())
  
  output$interactive_wave_height_plot <- renderPlotly({
    plot_ly(data = median_stats_height_per_month_day, 
            x = ~month_day, 
            y = ~median_wave_height, 
            type = 'scatter', 
            mode = 'markers', 
            #marker = list(color = '00b4d8', size = 5),
            color = I('#00b4d8')
    ) %>%
      layout(
        xaxis = list(
          title = 'Date',
          tickformat = '%b<br>%d',  # Format as abbreviated month
          dtick = 'M1'        # 1-month interval for x-axis
        ),
        yaxis = list(
          title = ''
        ),
        showlegend = FALSE,
        font = list(family = 'Times New Roman', size = 14, color = '#000000')
      ) %>% 
      add_lines(
        x = ~month_day, 
        y = ~fitted(loess(median_wave_height ~ as.numeric(month_day))),
        line = list(color = '#0077b6', width = 2),
        name = 'Smoothed Line'
      )
  })
  
  output$interactive_wave_period_plot <- renderPlotly({
    plot_ly(data = median_stats_height_per_month_day, 
            x = ~month_day, 
            y = ~median_period,
            type = 'scatter', 
            mode = 'markers', 
            #marker = list(color = '00b4d8', size = 5),
            color = I('#00b4d8')
    ) %>%
      layout(
        xaxis = list(
          title = 'Date',
          tickformat = '%b<br>%d',  # Format as abbreviated month
          dtick = 'M1'        # 1-month interval for x-axis
        ),
        yaxis = list(
          title = ''
        ),
        showlegend = FALSE,
        font = list(family = 'Times New Roman', size = 14, color = '#000000')
      ) %>% 
      add_lines(
        x = ~month_day, 
        y = ~fitted(loess(median_period ~ as.numeric(month_day))),
        line = list(color = '#0077b6', width = 2),
        name = 'Smoothed Line'
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
