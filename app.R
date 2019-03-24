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

	url <- paste0("https://www.indeed.com/jobs?q=", keyword_compress, "&l=", location, "&start=", pages_to_scrape*10)

	#sort by date or keyword
	if(sort_by == "date"){url %<>% paste0(., "&sort=date")}

	# ----
	# pg <- read_html(url)
	pg_list <- lapply(url, read_html)
}

parse_indeed_scrape_results <- function(pg){

	job_id <- pg %>% html_nodes("div") %>% html_attr("data-jk") %>% .[!is.na(.)]

	company <- pg %>% html_nodes(".company") %>% html_text %>% trimws

	location <- pg %>% html_nodes(".location") %>% html_text

	#find items that are jobTitle
	job_title_items <- pg %>% html_nodes("a") %>% html_attr("data-tn-element") %>% grep("jobTitle", .)

	job_title <- pg %>% html_nodes("a") %>% html_attr("title") %>% as.character() %>% .[job_title_items]

	#find items that are date - can't use .date b/c not all postings list dates
	# date_items <- pg %>% html_nodes(".date")

	#FUTURE - parse out date
	#id to link
	# pg %>% html_nodes("div") %>% html_attr("id") %>% grep("recJobLoc_", .)
	# #parse window results
	# pg %>% html_nodes("script") %>% html_text %>% .[22]

	table <- data.table(
		job_id,
		job_title,
		company,
		location#,
		# posted
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


# options(DT.options = list(pageLength = 20))

server <- function(input, output) {

	results_output <- eventReactive(input$go, {

		scrape_results <- scrape_indeed(input$keyword_id, input$location_id, input$sort_input)

		results <- lapply(scrape_results, parse_indeed_scrape_results) %>% rbindlist() %>% unique()

		#add data
		results[, posting_url:= paste0("https://www.indeed.com/viewjob?jk=", job_id)]

		results[, timestamp_scrape:= Sys.time()]

		return(results)
	})

	#display results
	table_output <- reactive({
	results_output()[, .(
		title = paste0("<a href=\"", posting_url, "\"target=\"_blank\">", job_title, "</a>"),
		company,
		location#,
		# posted
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
