library(DT)
library(shiny)
library(googleVis)
library(leaflet)
library(maps)
library(plotly)

shinyServer(function(input, output){

    #reactive function for selected SR
    selected_SR=reactive({
        srdata_top10%>%
            filter(issue_type==input$selected)%>%
            group_by(issue_type)%>%
            summarise(n=n())
    })
    
    # data prep for gauge chart
    gauge_month=reactive({
        y=filter(x,month %in% input$month & !is.na(month))%>%group_by(by=case_owner,ontime)%>%summarise(total=n())
        y=rename(y,owner='by')
        z=group_by(y,by=owner)%>%mutate(percent=round(total/sum(total)*100))%>%ungroup()
        z$by=NULL
        pass=filter(z,ontime=='pass')%>%select(owner,percent)
        #fail=filter(z,ontime=='fail')%>%select(owner,percent)
        #pending=filter(z,ontime=='pending')%>%select(owner,percent)
    })
    
    # On-time indicator for each department for top10 requests
     output$gauge <- renderGvis({
         gauge_month()%>%gvisGauge( 
                  options=list(min=0, max=100, greenFrom=80,
                  greenTo=100, yellowFrom=60, yellowTo=80,
                  redFrom=0, redTo=60, width=600, height=150,title="asfsfsdfasdfsa"))
        
    })
    
     #show time series plot for 2019
     output$time <- renderGvis({
       gvisCalendar(srs_bydate, 
                    datevar="date_created", 
                    numvar="total",
                    options=list(
                      title=paste("Total Service Requests for 2019: : ",
                                  prettyNum(nrow(srdata_top10), scientific=FALSE, big.mark=',')),
                      height="automatic",
                      width="automatic",
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 11,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}"))
     })
     

    #reactive function for selected SR for map
    selected_SR2=reactive({
      filter(srdata_top10,month %in% input$month & issue_type %in% input$selected)
    })
    
    #leaflet map showing SRs
    output$mymap <- renderLeaflet({
        selected_SR2()%>%leaflet() %>%
            setView(-80.304508,25.824274,zoom=9) %>%
            addTiles() %>%
            addMarkers(~longitude, ~latitude, popup = ~info,
                       options = popupOptions(closeButton = T),
                       clusterOptions = markerClusterOptions())
    })
    
    #reactive function for bar chart - to filter cities by issue_type and month
    iss_by_city=reactive({
      
      filter(srdata_top10,month %in% input$month & issue_type %in% input$selected)%>%group_by(by=issue_type,city)%>%summarise(total=n())%>%top_n(10,city)
    })
    
    output$plotlybar <- renderPlotly({
      l <- list(
        font = list(
          family = "sans-serif",
          size = 8,
          color = "#000"))
      iss_by_city()%>%plot_ly(x = ~city, y = ~total,color = ~by)%>%
        layout(autosize = F, width = 600, height = 300,title = 'Monthly Requests per City',automargin = TRUE,legend=l)
      
    })
    
    #reactive function for all SRs
    all_SRs=reactive({
        srdata_top10%>%
            #filter(ticket_status=="OPEN")%>%
            group_by(issue_type)%>%
            summarise(n=n())
    })
    
    #data for monthly SRs
    monthly_SRs=reactive({
      #bar_data=filter(srdata_top10,month %in% input$month)%>%group_by(by=issue_type)%>%summarise(n=n())
      
        filter(srdata_top10,month %in% c("Jan","Feb"))%>%group_by(by=month,issue_type)%>%summarise(total=n())%>%top_n(10,issue_type)
      
    })
    
    # bar chart for monthly SRs
    output$bar=renderGvis({
          gvisBarChart(monthly_SRs(),xvar = 'by',
                      option=list(title="Service Request Type",
                                    #fill="method_received",
                                    #legend="{ position: 'bottom' }",
                                    height=250,
                                    colors="['green']"))
           
    })
    
    # data for pie chart of method of source for SRs
    piesource=reactive({
      
        filter(srdata_top10,month==input$month)%>%group_by(by=method_received)%>%summarise(n())%>%top_n(10)
    })
    
    # pie chart for method of source
    output$sourcepie=renderGvis({
        gvisPieChart(piesource(), 
                    options=list(width=600,
                    height=200,
                    slices="{4: {offset: 0.2}, 0: {offset: 0.3}}",
                    #title=paste0(input$selected, " per City"),
                    #legend='none',
                    pieSliceText='label'
                    #pieHole=0.5
                    ))
        
    })
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
      data_table %>%
            filter(issue_type == input$selected)%>%
            datatable(., rownames=FALSE,
              options = list(lengthMenu = list(c(25, 50,100, -1), c('25', '50','100', 'All')),
              pageLength = 25,width=800))
            #options=list(lengthChange = FALSE,width=600)
            #formatStyle("issue_type",background="skyblue",fontWeight='bold')
            
    })
    
    
    # show statistics using infoBox
    
    output$maxBox <- renderInfoBox({
        #max_value <- max(state_stat[,input$selected])
        max_value <-nrow(srdata_top10)
        max_state <- 
            state_stat$state.name[state_stat[,input$selected] == max_value]
        infoBox(max_state, max_value, icon = icon("hand-o-up"))
    })
    output$minBox <- renderInfoBox({
        min_value <- min(state_stat[,input$selected])
        min_state <- 
            state_stat$state.name[state_stat[,input$selected] == min_value]
        infoBox(min_state, min_value, icon = icon("hand-o-down"))
    })
    output$avgBox <- renderInfoBox(
        infoBox(paste("AVG.", input$selected),
                mean(state_stat[,input$selected]), 
                icon = icon("calculator"), fill = TRUE))
})