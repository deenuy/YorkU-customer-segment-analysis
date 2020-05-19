library(shiny)
library(shinydashboard)
library(dplyr)
source('assoModel.R')

# Define UI for application that draws a histogram
ui = dashboardPage(
    #The title of the dashboard
    dashboardHeader(
        title = "SuperStore Dashboard",
        titleWidth = 230
    ),
    dashboardSidebar(
        #Sidebar content of the dashboard
        sidebarMenu(
            menuItem("Analytic Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Consumer Preference Recommender", tabName = "widgets", icon = icon("bar-chart-o"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dashboard", 
                h2("Analytic Dashboard"),
                fluidRow(
                    valueBoxOutput("value1"),
                    valueBoxOutput("value2"),
                    valueBoxOutput("value3")
                ),
                fluidRow(
                  box(
                    title = "Performance by Segment",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    selectInput(inputId = "segmentInput",
                                label = "Customer Segment Type",
                                choices = c("Segment","Category","Region"),
                                selected = "Category"),
                    plotOutput("profitBySegment", width = "100%")
                  )
                ),
                fluidRow(),
                fluidRow(
                      column(1,
                             selectInput(inputId = "xInput",
                                         label = "X-Axis",
                                         choices = c("Sales","Discount","Days to Ship" = "Discount"),
                                         selected = "Sales"),
                             br()
                      ),
                      column(2,
                             #offset = 1,
                             selectInput(inputId = "yInput",
                                         label = "Y-Axis",
                                         choices = c("Profit Ratio" = "Profit"),
                                         selected = "Profit")
                      ),
                      column(3,
                             selectInput(inputId = "segmentTypeInput",
                                         label = "Segment Type",
                                         choices = c("Segment","Category","Sub Category" = "Sub.Category"),
                                         selected = "Sub.Category")
                      ),
                      column(4,
                             #offset = 1,
                             selectInput(inputId = "regionInput",
                                         label = "Region",
                                         choices = c("US",
                                                     "Central",
                                                     "East", 
                                                     "West", 
                                                     "South"),
                                         selected = "US")
                      ),
                      title = "Rename this box",
                      status = "primary",
                      solidHeader = TRUE, 
                      collapsible = TRUE,
                      width = 12,
                      plotOutput("bubbleChart", width = "100%")
                ),
                fluidRow(),
                br(),
                br()
                # fluidRow(
                #     box(
                #         title = "Profit % by Segments Table",
                #         status = "primary",
                #         solidHeader = TRUE, 
                #         collapsible = TRUE,
                #         #Input section
                #         selectInput(inputId = "segmentTabInput",
                #                     label = "Select Segment",
                #                     choices = c("Segment","Category","Region"),
                #                     selected = "Segment"),
                #         #Output section
                #         tableOutput("profitBySegmentTable")
                #     )
                # )
            ),
            
            tabItem(
                tabName = "widgets",
                h2("Consumer Preference Recommender"),
                fluidRow(),
                fluidRow(
                    box(
                        title = "Consumer Preference Recommender System",
                        status = "primary",
                        solidHeader = TRUE, 
                        collapsible = TRUE,
                        
                        #Input section
                        selectInput("input_item", "Item #1", choices = df_basket_filtered$rules$lhs, selected = df_basket_filtered$rules$lhs[2], selectize = TRUE),
                        h3("Other Items you might be Interested:"),
                        #Output section
                        tableOutput("productRecommender")
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    
    observeEvent(input$switchtab, {
    newtab <- switch(input$tabs,
      "dashboard" = "widgets",
      "widgets" = "dashboard"
    )
    updateTabItems(session, "tabs", newtab)
    })

    #Construct the dataset for first box inputs
    supstore_df_byProfit = reactive({
        
            supstore_df_new %>%
            dplyr::group_by(Profit_pct_range, !!sym(input$segmentInput)) %>%
            dplyr::summarise(Orders = n_distinct(Order.ID)) %>%
            dplyr::mutate(Orders.pct = round( (Orders/sum(Orders) * 100), 1)) %>%
            arrange(desc(Orders.pct))
    })
    
    #Plot for first box
    output$profitBySegment <- renderPlot({
        ggplot(supstore_df_byProfit(),
               aes(reorder(Profit_pct_range, -Orders), Orders, fill=!!sym(input$segmentInput))) + 
            geom_bar(position="stack", stat="identity") +
            scale_fill_manual(values = cbPalette)+
            theme_pubclean() +
            labs(x="Profit Range", y="Orders", title="Distribution of Orders by Profit%") +
            geom_text(aes(label=paste0(Orders.pct,"%",sep=" ")), size=3.2, color='black', fontface='bold', position = position_stack(vjust = 0.5))
        })
    
    #Construct the dataset for second box inputs
    supstore_df_byBox2Input =  reactive({
      
        supstore_df_new %>%
        dplyr::filter(Order.Date > "2014-01-01" & Order.Date <"2017-12-30") %>%
        dplyr::group_by(!!sym(input$segmentTypeInput)) %>%
        dplyr::summarise(
                  Orders = n_distinct(Order.ID),
                  Average.y = round(mean(!!sym(input$yInput))),
                  Average.x = round(mean(!!sym(input$xInput))),
                  total_x = round(sum(!!sym(input$xInput)))
                  ) %>%
        dplyr::mutate(
               yx_ratio = round((Average.y/Average.x) * 100)
               )
    })

    #Plot for second box - Initiate a ggplot bubble chart
    output$bubbleChart <- renderPlot({

      ggplot(supstore_df_byBox2Input(),
             aes(x = total_x, y = yx_ratio)) +
          geom_point(aes(color = !!sym(input$segmentTypeInput), size = Orders), alpha = 0.6) +
          geom_text(aes(label = !!sym(input$segmentTypeInput)) ) + 
          labs(x='Sales', y='Profit Ratio') +
          theme(legend.position="none") +
          scale_size(range = c(1, 50)) +
          scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = "%")) +
          scale_x_continuous(labels = scales::unit_format(unit = "k", prefix = "$", scale = 1e-3))
    })

    #segmentTabInput
    supstore_df_byProfit2 = reactive({
        
        supstore_df_new %>%
            dplyr::group_by(Profit_pct_range, !!sym(input$segmentTabInput)) %>%
            dplyr::summarise(Orders = n_distinct(Order.ID)) %>%
            dplyr::mutate(Orders.pct = round( (Orders/sum(Orders) * 100), 1)) %>%
            arrange(desc(Orders.pct))
        })   
    
    #Plot2
    output$profitBySegmentTable <- renderTable({
        head(supstore_df_byBox2Input(), 5)
        })
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            formatC(sales.revenue$total_revenue, format="d", big.mark=','),
            paste('Sales Revenue'),
            icon = icon("usd",lib='glyphicon'),
            color = "purple")  
    })
    output$value2 <- renderValueBox({ 
        valueBox(
            formatC(length(unique(supstore_df_new$Customer.ID)), format="d", big.mark=','),
            'Total Customers',
            icon = icon("stats",lib='glyphicon'),
            color = "green")  
    })
    output$value3 <- renderValueBox({
        valueBox(
            formatC(avg.basket.price$avg.price, format="d", big.mark=','),
            paste('Average Basket Spend'),
            icon = icon("usd",lib='glyphicon'),
            color = "yellow")   
    })
    
    #RHS Recommender Result
    results = reactive({
        df_basket_filtered$rules %>%
        filter(stri_detect_fixed(lhs, input$input_item)) %>%
        select(rhs)
        })
    
    #Recommender Output
    output$productRecommender = renderTable({
        head(results())
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
