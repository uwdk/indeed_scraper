#scrape indeed jobs page and condense to remove duplicates (sponsored ads)

library(xml2)
library(rvest)
library(data.table)
library(magrittr)


# ----
#FUTURE - scrape min results; loop to count results and re-scrape until min met
scrape_indeed <- function(keyword, location = NULL, sort_by = "date"){#, min_results = 50

	keyword_compress <- gsub(" ", "+", keyword)

	#FUTUTE - adjust pages here
	pages_to_scrape <- 0:4

	url <- paste0("https://www.indeed.com/m/jobs?q=", keyword_compress, "&l=", location, "&start=", pages_to_scrape*10)

	#sort by date or keyword
	if(sort_by == "date"){url %<>% paste0(., "&sort=date")}

	# ----
	pg_list <- lapply(url, read_html)
}

parse_indeed_scrape_results <- function(pg){

	job_id <- pg %>% html_nodes("h2") %>% html_children() %>% html_attr("href") %>% strsplit("jk=") %>% sapply(tail, 1)

#NOTE - issue is company isn't tagged in html code - need to parse out and drop non-company items
	company <- pg %>% as.character() %>% strsplit(" - <span") %>% .[[1]] %>% strsplit("\n", fixed = TRUE) %>% sapply(tail, 1) %>% .[-length(.)]

	location <- pg %>% html_nodes(".location") %>% html_text

	job_title <- pg %>% html_nodes(".jobTitle") %>% html_text

	posted <- pg %>% html_nodes(".date") %>% html_text

	table <- data.table(
		job_id,
		job_title,
		company,
		location,
		posted
	)
}

# ----

library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Indeed postings"),

    sidebarLayout(
      sidebarPanel(
		textInput("keyword_id", "Enter keyword search"),
		textInput("location_id", "Enter location (optional)"),

		radioButtons("sort_input", "Top results by", choices = c("keyword", "date"), selected = "keyword"),

		actionButton("go", "Retrieve postings"),

		downloadButton("download", "Download csv")
      ),

  mainPanel(DTOutput("results"))
)
)


server <- function(input, output) {

	results_output <- eventReactive(input$go, {

		scrape_results <- scrape_indeed(input$keyword_id, input$location_id, input$sort_input)

		results <- lapply(scrape_results, parse_indeed_scrape_results) %>% rbindlist() %>% unique()

		#add data
		results[, posting_url:= paste0("https://www.indeed.com/m/viewjob?jk=", job_id)]

		results[, timestamp_scrape:= Sys.time()]

		#parse posted - format _d _h
		results[posted %like% "day", posted_sort:= sprintf(
			"%02d d", as.numeric(gsub("[[:punct:][:alpha:]]", "", posted)))]

		results[posted %like% "hour", posted_sort:= sprintf(
			"0 d %02d h", as.numeric(gsub("[[:punct:][:alpha:]]", "", posted)))]

		results[posted %like% "month", posted_sort:= posted]

		return(results)
	})

	#display results
	table_output <- reactive({
	results_output()[, .(
		title = paste0("<a href=\"", posting_url, "\"target=\"_blank\">", job_title, "</a>"),
		company,
		location,
		posted_sort
		)]
	})

	output$results <- renderDT({
	DT::datatable(table_output(), escape = FALSE, rownames = FALSE, selection = "none", class = "compact stripe", options = list(pageLength = 75))
	})

	output$download <- downloadHandler(
		filename = function() {paste("indeed", input$keyword_id, input$location_id, Sys.Date(), sep = "_") %>% gsub(" ", "_", .) %>% paste0(., ".csv")}
		,
		content = function(file) {fwrite(results_output(), file, quote = TRUE)}
		)

}

# ----

#export
# fwrite(results, paste0("indeed_results_", today(), ".csv"), quote = TRUE)

#needs to be last line
# runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
shinyApp(ui = ui, server = server)
