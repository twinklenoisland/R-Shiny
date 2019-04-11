## app.R ##
setwd('/home/rym/DSBA/ESSEC/MarketingAnalytics/Project')

library(shiny)
library(shinydashboard)
library(ggplot2)

source('info.R')
source('RFM_segment.R')
source('demographic_analysis.R')
source('campaign_response_seg.R')

db = dbConnect(MySQL(), user='rym', password='', dbname='ma_charity_full', host='127.0.0.1', port=3306)

month_day = '0625'
this_year = 2018
donors_infos = donors_info(db, this_year, month_day)
donations_infos = donations_info(db, this_year, month_day)

ui <- dashboardPage(
	dashboardHeader(title = "Charity dashboard"),
	dashboardSidebar(
		width = 150,
		sidebarMenu(
			id = "sidebar", 
			menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
			menuItem("Donations", icon = icon("hand-holding-usd"), tabName = "donations"),
			menuItem("Demography", icon = icon("users"), tabName = "demography"),
			menuItem("Campaigns", icon = icon("comments-dollar"), tabName = "campaigns")
		)
	),
	dashboardBody(
		fluidRow(
			box(title = "2018 Key figures", width = 12, height = 35),
			valueBox(donors_infos[[1]], "New donors", icon = icon("users"), width = 3),
			valueBox(paste(round(donors_infos[[2]], 2), "%"), "Progress from last year", icon = icon("users"), width = 3),
			valueBox(paste(round(donations_infos[[1]], 2)), "Donations amount", icon = icon("euro-sign"), width = 3),
			valueBox(paste(round(donations_infos[[2]], 2), "%"), "Progress from last year", icon=icon("euro-sign"), width=3)
		),
    
		tabItems(
			tabItem(
				tabName = "dashboard",
				fluidRow(
					box(
						title = 'Segment Size Evolution',
						plotOutput("plot1_size", height = 250),
						width = 6
					),
					box(
						title = '2018 segments - prefix distribution',
						plotOutput("plot1_prefix", height = 250),
						width = 6
					)
				)
			),
      
			tabItem(tabName = "donations",
				fluidRow(
					selectInput(
						inputId = "year", 
						label = 'Donation Frequency and Amount',
						choices = c("2018", "2017", "2016", "2015", "2014")
					)
				),
				fluidRow(
					box(plotOutput("plot2_freq", height=230), width=6),
					box(plotOutput("plot2_amt", height=230), width=6)
				)
			),

			tabItem(tabName = "demography",
				fluidRow(
					selectInput(
						inputId = "segment", 
						label = 'Segment count',
						choices = c("Active top", "Active bottom", "Warm", "Cold", "Lost")
					)
				),
				fluidRow(
					box(plotOutput("plot3_region", height=250), width = 10)
				)
			),

			tabItem(tabName = "campaigns",
				column(
					width = 6,
					fluidRow(
						selectInput(
							inputId = "year_camp", 
							label = 'Campaign Response by Year',
							choices = c("2018", "2017", "2016", "2015", "2014")
						)
					),
					fluidRow(
						box(plotOutput("plot4_year", height=230), width=12)
					)

				),
				column(
					width = 6,
					fluidRow(
						selectInput(
							inputId = "segment_camp", 
							label = 'Campaign Response by Segment',
							choices = c("Active top", "Active bottom")
						)
					),
					fluidRow(
						box(plotOutput("plot4_segment", height=230), width=12)
					)
				)
			)

		)
	)
)

server <- function(input, output) {
  #Initialization data
  segments = init_segments(db)
  contacts = init_demog(db, segments)
  responses = init_responses(db, segments)
  
  output$plot1_size <- renderPlot({
    yearly_segment_size_plot(segments)
  })

  output$plot1_prefix <- renderPlot({
    prefix_count_plot(contacts)
  })
  
  output$plot2_freq <- renderPlot({
    freq_plot(segments, input$year)
  })

  output$plot2_amt <- renderPlot({
    amt_plot(segments, input$year)
  })

  output$plot3_region <- renderPlot({
   region_count_plot(contacts, input$segment, 10)
  })

  output$plot4_year <- renderPlot({
   year_response_plot(responses, input$year_camp)
  })

  output$plot4_segment <- renderPlot({
   segment_response_plot(responses, input$segment_camp)
  })
}

shinyApp(ui, server)