options(stringsAsFactors = F)
library(ggplot2);library(dplyr);library(pipeR);library(shiny);library(lubridate);library(corrplot);library(ggthemr);library(reshape2);library(data.table);library(rCharts)
'data/advanced_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_advanced 
'data/per_game_player_stat_table_names.csv' %>>% read.csv() %>>% (data.frame(.,stringsAsFactors = F)) %>>% tbl_df()-> table_names_pg 

rbind(table_names_advanced[,c(1:3)],table_names_pg[,c(1:3)]) -> all_names
all_names %>>% arrange(desc(name)) %>>%
	filter(!name %in% c('Player','Team ID',"Blocks Per Game")) %>>%
	select(name) %>>% (.$name) -> bin_options
all_names[!duplicated(all_names$column_name),] -> all_names
all_names[c(3,47:5),'name']  %>>% (.$name) -> bin_options
c("PG","SG","G","SF","PF","F","C") -> positions
all_names %>>% 
	filter(!name %in% c("Player","Games Played","Games Started","Team ID","Position")) %>>%
	arrange(name) %>>%
	select(name) %>>% (.$name) -> rank_options
rank_options %>>% as.character() -> rank_options
all_names %>>% 
	filter(!name %in% c("Player","Team ID","Position","Steal %","Block %","Turnover %","Assist %","Usage","Total Rebound %","Defensive Rebound %","Offensive Rebound %","True Shooting %","Effective Field Goal %","Field Goal %","3PT %","2PT Shot %","Free Throw %")) %>>%
	arrange(name) %>>%
	select(name) %>>% (.$name) -> rank_plot_chart
rank_plot_chart %>>% as.character() -> rank_plot_chart


c("Winshare Total","Points Per Game","Turnovers Per Game","Blocks Per Game","Steals Per Game","Assists Per Game","Total Rebounds Per Game", "Usage Rate","Player Efficency Rating") -> selected_plot
Sys.Date() >= "2014-10-28" -> in_season
ifelse(in_season,
			 years_possible <- 2015:1951,
			 years_possible <- 2014:1951)
shinyUI(
	fluidPage(responsive = T,
						title = 'NBA Player Analysis by Alex Bresler',
						sidebarLayout(fluid = T,
													sidebarPanel(
														span="span4",
														tags$head(
															tags$head(includeScript("google-analytics.js")),
															tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.min.css"),
															#tags$link(rel="stylesheet", type="text/css", href="styles_black_orange.css"),
															tags$style(type="text/css",
																				 "label {font-size: 9px;}",
																				 ".recalculating {opacity: 2.0;}"
															),
															tags$style(type="text/css", "select { max-width: 600px; }"),
															tags$style(type="text/css", "textarea { max-width: 800px; }"),
															tags$style(type="text/css", ".jslider { max-width: 500px; }"),
															tags$style(type='text/css', ".well { max-width: 450px; }")),
														p(img(src = "http://www.logodesignlove.com/images/classic/nba-logo.jpg", height = 30, width = 30),
															" Player Stat Analyzer"
														),
														fluidRow(
															column(8, offset = 0,
																		 selectInput("season_end", "NBA Season [1951-52 to 2014-15]",choices = years_possible, selected = 2014,multiple = F,
																		 ))
														),
														fluidRow(
															column(5, offset = 0,
																		 numericInput("min_minutes", "Minimum Total Minutes:",
																		 						 min=0, max=4000, value=0,step = 1)),
															column(5, offset = 0,
																		 numericInput("min_games", "Minimum Games Played:",
																		 						 min=0, max=82, value=0,step = 1))
														),
														conditionalPanel(condition = "input.analysis_tabs == 'Rank Plot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput("sort", "Select Item to Rank",choices = rank_options, multiple = F,selected = 'Points Per Game'
																						 				 )
																						 	),
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="plot_var",  multiple = TRUE, selectize = T,
																						 				 						label="Plot/Chart Variables:",
																						 				 						choices=rank_options,
																						 				 						selected = selected_plot))),
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 numericInput("players", "Players to Show:",
																						 				 						 min=10, max=100, value=55,step = 1)),
																						 	column(6, offset = 0,
																						 				 								 selectInput(inputId="position_selection",  multiple = TRUE, selectize = T,width = "100%",
																						 				 								 						label="Positions",
																						 				 								 						choices = positions,
																						 				 								 						selected = positions))
																						 	),
																						 downloadButton("downloadPlotPDF", "Rank Plot")
														),
														conditionalPanel(condition = "input.analysis_tabs == 'XY Plot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput("rc_xvar", "Select X Variable",choices = rank_plot_chart, multiple = F,
																						 				 						selected = 'Field Goal Attempts Per Game'
																						 				 )
																						 	),
																						 	column(6, offset = 0,
																						 				 selectInput("rc_yvar", "Select Y Variable",choices = rank_plot_chart, multiple = F,
																						 				 						selected = 'Points Per Game'
																						 				 )
																						 	)
																						 ),
																						 fluidRow(
																						 	
																						 	conditionalPanel(condition = "input.rc_facet == 'TRUE'",
																						 									 column(6, offset = 0,
																						 									 			 selectInput("rc_bin", "Select Bin Variable",choices = rank_plot_chart, multiple = F,
																						 									 			 						selected = 'Age'
																						 									 			 ))
																						 	),
																						 	column(6, offset = 0,
																						 				 selectInput("rc_facet", "Facet by Position",choices = c(TRUE,FALSE), multiple = F,
																						 				 						selected = TRUE
																						 				 )
																						 	)
																						 ),
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput("log_x", "Log X Variable",choices = c(TRUE,FALSE), multiple = F,
																						 				 						selected = FALSE
																						 				 )
																						 	),
																						 	column(6, offset = 0,
																						 				 selectInput("log_y", "Log Y Variable",choices = c(TRUE,FALSE), multiple = F,
																						 				 						selected = FALSE
																						 				 )
																						 	)
																						 )
														),
														conditionalPanel(condition = "input.analysis_tabs == 'Boxplot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput("sort_boxplot", "Select Item to Rank",choices = rank_options, multiple = F,selected = 'Points Per Game'
																						 				 )),
																						 	column(6, offset = 0,
																						 				 selectInput("bin", "Select Grouping Variable",choices = bin_options, multiple = F,selected = 'Age'
																						 				 )
																						 	)
																						 )
														),
														conditionalPanel(condition = "input.analysis_tabs == 'Density Plot'",
																						 fluidRow(	
																						 	column(8, offset = 0,
																						 				 selectInput("sort_density", "Select Item to Rank",choices = rank_options, multiple = F,selected = 'Points Per Game'
																						 				 ))),
																						 fluidRow(
																						 	column(5, offset = 0,
																						 				 sliderInput("min_quantile", "Minimum Distribution Quartile",format = '#0.00%',
																						 				 						min=0, max=1, value=.02,step = .01)),
																						 	column(5, offset = 0,
																						 				 sliderInput("max_quantile", "Maximum Distribution Quartile:",format = '#0.00%',
																						 				 						min=0, max=1, value=.95, step = .01))
																						 )),
														conditionalPanel(condition = "input.analysis_tabs === 'Rank Table'",
																						 fluidRow(
																						 	column(7, offset = 0,
																						 			condition="input.season_end==2015",
																						 				 								 selectInput(inputId="position_selection_table",  multiple = TRUE, selectize = T,width = "100%",
																						 				 								 						label="Positions",
																						 				 								 						choices = positions,
																						 				 								 						selected = positions)
																						 				 ),
																						 	column(6, offset = 0,
																						 				 selectInput("sort_table", "Select Item to Rank",choices = rank_options, multiple = F,selected = 'Points Per Game'
																						 				 )
																						 	)
																						 	
																						 ),fluidRow(
																						 	column(7, offset = 0,
																						 				 selectInput(inputId="plot_var_table",  multiple = TRUE, selectize = T,
																						 				 						label="Plot/Chart Variables:",
																						 				 						choices=rank_options,
																						 				 						selected = selected_plot))
																						 ),
																						 downloadButton("downloadCSV", "Selected Data")
														),
														conditionalPanel(condition = "input.analysis_tabs === 'Correlation Plot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="plot_var_cor",  multiple = TRUE, selectize = T,
																						 				 						label="Plot/Chart Variables:",
																						 				 						choices=rank_options,
																						 				 						selected = selected_plot)),
																						 	column(6, offset = 0,
																						 				 								 selectInput(inputId="position_selection_cor",  multiple = TRUE, selectize = T,width = "100%",
																						 				 								 						label="Positions",
																						 				 								 						choices = positions,
																						 				 								 						selected = positions)
																						 	)), 
																						 downloadButton("downloadCOR", "Correlation Plot")
																						 	),
														conditionalPanel(condition = "input.analysis_tabs === 'Kohonen Plot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 numericInput("xdim", "X Dimension Length:",
																						 				 						 min=1, max=20, value=12,step = 1)),
																						 	column(6, offset = 0,
																						 				 numericInput("ydim", "Y Dimension Length:",
																						 				 						 min=1, max=20, value=12,step = 1))
																						 ),
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="scaled_per_minute_kp",  multiple = F,selectize = T,
																						 				 						label="Use Per Minute Stats",
																						 				 						choices=c(T,F),
																						 				 						selected = F)),
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="clusters",  multiple = F,selectize = T,
																						 				 						label="Kohonen Clusters",
																						 				 						choices=1:20,
																						 				 						selected = 13)
																						 				 
																						 	)
																						 ),
																						 p(a(href ="http://en.wikipedia.org/wiki/Self-organizing_map", "Kohonen Self Organizing Maps",target = "_blank")," modeled after ",a(href ="http://www.sloansportsconference.com/wp-content/uploads/2012/03/Alagappan-Muthu-EOSMarch2012PPT.pdf", "2012 Sloan Sports Analytics Presentation",target = "_blank"),", recommend minimum of 10 clusters and no more than 225 groups [x dimension * y dimension].")
														),
														conditionalPanel(condition = "input.analysis_tabs === 'Kohonen Table'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 numericInput("xdim_t", "X Dimension Length:",
																						 				 						 min=1, max=20, value=12,step = 1)),
																						 	column(6, offset = 0,
																						 				 numericInput("ydim_t", "Y Dimension Length:",
																						 				 						 min=1, max=20, value=12,step = 1))
																						 ),
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="scaled_per_minute_t",  multiple = F,selectize = T,
																						 				 						label="Use Per Minute Stats",
																						 				 						choices=c(T,F),
																						 				 						selected = F)),
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="clusters_t",  multiple = F,selectize = T,
																						 				 						label="Kohonen Clusters",
																						 				 						choices=1:20,
																						 				 						selected = 13)
																						 				 
																						 	)
																						 ),
																						 p(a(href ="http://en.wikipedia.org/wiki/Self-organizing_map", "Kohonen Self Organizing Maps",target = "_blank")," modeled after ",a(href ="http://www.sloansportsconference.com/wp-content/uploads/2012/03/Alagappan-Muthu-EOSMarch2012PPT.pdf", "2012 Sloan Sports Analytics Presentation",target = "_blank"),", recommend minimum of 10 clusters and no more than 225 groups [x dimension * y dimension].")
														),
														conditionalPanel(condition = "input.analysis_tabs == 'Cluster Plot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput("cluster_xvar", "Select X Variable",choices = rank_plot_chart, multiple = F,
																						 				 						selected = 'Field Goal Attempts Per Game'
																						 				 )
																						 	),
																						 	column(6, offset = 0,
																						 				 selectInput("cluster_yvar", "Select Y Variable",choices = rank_plot_chart, multiple = F,
																						 				 						selected = 'Points Per Game'
																						 				 )
																						 	)
																						 ),
																						 fluidRow(
																						 	
																						 	conditionalPanel(condition = "input.cluster_facet == 'TRUE'",
																						 									 column(6, offset = 0,
																						 									 			 selectInput("cluster_bin", "Select Bin Variable",choices = rank_plot_chart, multiple = F,
																						 									 			 						selected = 'Age'
																						 									 			 ))
																						 	),
																						 	column(6, offset = 0,
																						 				 selectInput("kmeans_cluster_facet", "Facet by Position",choices = c(TRUE,FALSE), multiple = F,
																						 				 						selected = TRUE
																						 				 )
																						 	)
																						 ),
																						 selectInput("clusters_kmeans", "Kmeans Clusters",choices = 2:12, multiple = F,
																						 						selected = 6
																						 )
														),
														conditionalPanel(condition = "input.analysis_tabs === 'MDS Plot'",
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 selectInput("sort_mds", "Select Item to Rank",choices = rank_options, multiple = F,selected = 'Points Per Game'
																						 				 )),
																						 	column(6, offset = 0,
																						 				 selectInput(inputId="mds_scaled_per_minute",  multiple = F,selectize = T,
																						 				 						label="Use Per Minute Stats",
																						 				 						choices=c(TRUE,FALSE),
																						 				 						selected = FALSE)
																						 	)
																						 ),
																						 fluidRow(
																						 	column(6, offset = 0,
																						 				 numericInput("min_player_rank", "Minimum Players to Rank:",
																						 				 						 min=1, max=500, value=125,step = 1))
																						 )
														),
														helpText(h6
																		 (
																		 	p("An",a(href ="https://twitter.com/abresler", "Alex Bresler",target = "_blank")," Data Analysis")
																		 	,p('May take up to 30 seconds to load due to real-time scraping'))
														)
													),
													mainPanel(
														tabsetPanel(id = 'analysis_tabs',type = 'pills',
																				tabPanel('Rank Plot', 
																								 plotOutput("win_share_plot",height = 800, width = "100%")),
																				tabPanel('Rank Table', 
																								 dataTableOutput("table")),
																				tabPanel('XY Plot', showOutput('xy_chart','polycharts')
																				),
																				tabPanel('MDS Plot',
																								 plotOutput("mds_plot",height = 850,width = "100%")),
																				tabPanel('Kohonen Plot',
																								 plotOutput("kohonen_plot",height = 850,width = "100%")),
																				tabPanel('Kohonen Table',
																								 dataTableOutput("kohonen_table")),
																				tabPanel('Boxplot',
																								 plotOutput("boxplot",height = 850,width = "100%")),
																				tabPanel('Density Plot', 
																								 plotOutput("dist_plot",height = 800,width = "100%")),
																				tabPanel('Correlation Plot', 
																								 plotOutput("corr_plot",height = 600, width = "100%")
																				),
																				tabPanel('Cluster Plot', showOutput('cluster',lib = 'polycharts')
																				)
														)
													)
						)
	))