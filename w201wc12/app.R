# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    http://shiny.rstudio.com/articles/gadget-ui.html
cat('\014')
library(shiny)
library(miniUI)
library(wordcloud2)
library(data.table)
library(magrittr)
dtm<-readRDS('./data/dtm.rds')
uri<-readRDS('./data/uri.rds')
# Define UI for application that draws a word cloud
ui<-miniPage(
	tags$style(
		type="text/css",
		".shiny-output-error { visibility: hidden; }",
		".shiny-output-error:before { visibility: hidden; }"
	),
	miniContentPanel(
		fillCol(
			flex = c(NA,1,NA)
			,selectInput(inputId = 'citekey',label = NULL,choices = names(uri),selected = names(uri[1]),width = '100%')
			,wordcloud2Output("plot",height = '80%')
			,uiOutput("slider1")
		))
)

server<-function(input, output) {

	output$slider1 <- renderUI({
		ck<-uri[input$citekey]
		sliderInput(
			"freq"
			,label=NULL #"Word frequency:"
			, min=dtm[ck,min(freq)], max=dtm[ck,max(freq)], value=dtm[ck,c(quantile(freq,probs = .5)+1,max(freq))]
			,width = '100%',ticks = F,pre='Count:'
		)
	})

	output$plot <- renderWordcloud2({
		ck<-uri[input$citekey]
		ix<-if(is.null(input$freq)) {dtm[ck][.N,freq]} else {input$freq[1]:input$freq[2]}
		ix<-intersect(ix,dtm[ck,freq]) %>% sort %>% rev
		p<-dtm[.(ck,ix)][order(-freq)]
		wordcloud2(data=p[,.(word,freq)]%>%as.data.frame(),shuffle=F,color = p[,c],size = .5)
	})

}

shinyApp(ui, server)
