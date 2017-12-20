library(shiny)
library(ggplot2)
library(dplyr)


indSer <- read.csv("Industry_Series.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
	titlePanel("Employment and Salaries by Industry in Vermont Counties: 2009 - 2015"),
	sidebarLayout(
		sidebarPanel(
			   selectInput("geoInput", "Statewide, or by County",
						sort(unique(indSer$Geo)),
						selected = "Vermont")

			),
			
			
			
			tabsetPanel(type = "tabs",
					tabPanel("Average Salary", plotOutput("avesalplot")),
					tabPanel("Number of Employees ", plotOutput("numempplot")),
					tabPanel("Table", tableOutput("table"))
			)	
		)
	)

server <- function(input, output) {


filtered <- reactive({
	
	indSer %>%
		filter(Geo == input$geoInput
				
	)
})
 
output$avesalplot <- renderPlot ({
	ggplot(filtered(), aes(Year, aveEmpSal, col = Sector)) +
		geom_line() +
		labs(title ="Average Salary by Sector", x = "Year", y = "Average Salary") +
		coord_cartesian(xlim = c(2009, 2015), ylim = c(0, 80000)) +
		guides(col = guide_legend(ncol = 1), size = FALSE, alpha = FALSE)+
		scale_color_brewer(palette = "Paired") +
		theme_classic()
	
  
})

output$numempplot <- renderPlot ({

	ggplot(filtered(), aes(Year, Emp, col = Sector,)) +
		geom_line()+
		labs(title ="Number of Employees by Sector", x = "Year", y = "Number of Employees") +
		coord_cartesian(xlim = c(2009, 2015)) +
		guides(col = guide_legend(ncol = 1), size = FALSE, alpha = FALSE) +
		scale_color_brewer(palette = "Paired") +
		theme_classic()
		
		
})

output$table <- renderTable({
filtered()
 })

}

shinyApp(ui = ui, server = server)
