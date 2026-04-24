# Kevin Hon Yin Hau - 2026

# Load the Shiny package
library(shiny)
library(htmlwidgets)
library(rhandsontable)
library(shinythemes)
library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyr)
library(DiagrammeR)

default_tbl2 <- data.frame(
  Code = c("11","01","10","00"), 
  Total  = c(1000, 100, 100, 100),
  PayNext = c(970, 40, 60, 30))


default_tbl3 <- data.frame(
  Code = c("111","110","101","100","011","010","001","000"), 
  Total  = c(1000, 100, 100, 100, 100, 100, 100, 100),
  PayNext = c(970, 40, 60, 30, 70, 20, 30, 5))

default_tbl4 <- data.frame(
  Code = c("1111","1101","1011","1001","0111","0101","0011","0001", "1110","1100","1010","1000","0110","0100","0010","0000"), 
  Total  = c(1000, 100, 100, 100, 100, 100, 100, 100, 1000, 100, 100, 100, 100, 100, 100, 100),
  PayNext = c(970, 40, 60, 30, 70, 20, 30, 5, 900, 30, 50, 20, 60, 10, 20, 1))



getModelDeclineCurve <- function(tbl, numMth){
  
  paidOrNotPaid <- function(startCode, payCodeTbl, numMth){
    tbl <- data.table(step = 0, code=startCode, payNum=0, prob=1)
    resultList <- rep(0,numMth)
    lenCode <- nchar(startCode)
    for(i in 1:numMth){
      tbl <- suppressMessages(tbl %>% inner_join(payCodeTbl, c("code"="code")) %>% 
        mutate(code = paste(substr(code,2,lenCode),"1",sep=""), payNum = payNum + 1,
               prob = prob * paidPerc) %>% 
        union_all(tbl %>% inner_join(payCodeTbl, c("code"="code")) %>% 
                    mutate(code = paste(substr(code,2,lenCode),"0",sep=""),
                           prob = prob * notPaidPerc)) %>% 
        mutate(step = step+1) %>% select(-paidPerc, -notPaidPerc) %>% 
        group_by(step, code, payNum) %>% summarise(prob = sum(prob)) %>% 
        arrange(desc(payNum), code) %>% ungroup(step, code, payNum))
      
      resultList[i] <- tbl %>% summarise(ep = sum(payNum*prob)) %>% pull(ep)
    }
    data.table(startCode = startCode,
               numStep = 1:numMth, 
               expectedVal = resultList)
  }
  
  result <- paidOrNotPaid(tbl$code[1], data.table(tbl), numMth)
  
  for(i in 2:nrow(tbl)) {
    result <- result %>% union_all(paidOrNotPaid(tbl$code[i],tbl,numMth))
  }
  
  return(result %>% 
           left_join(result %>% 
                       mutate(numStep = numStep + 1,
                              expectedValLast = expectedVal) %>% 
                       select(startCode, numStep, expectedValLast), 
                     by=c("startCode"="startCode","numStep"="numStep")) %>% 
           mutate(curve = expectedVal - coalesce(expectedValLast,0)) %>% 
           select(-expectedValLast)
  )
  
}

getCodeTbl <- function(dt){
  data.frame(code = dt$Code, 
             total = as.integer(dt$Total),
             paidPerc =as.integer(dt$PayNext)/as.integer(dt$Total), 
             notPaidPerc = 1-as.integer(dt$PayNext)/as.integer(dt$Total), 
             stringsAsFactors = FALSE) %>% arrange(code)
}

# Define UI for application
ui <- fluidPage(
  # Application title
  theme = shinytheme("cosmo"),
  titlePanel(HTML("Dynamic Stochastic Model <br><small>Kevin Hau</small>")),
  
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      actionButton("Run", "Run"),
      
      # Input: Slider for the number of steps ----
      sliderInput(inputId = "hist",
                  label = "Number of historical steps:",
                  min = 2,
                  max = 4,
                  value = 2),
      
      sliderInput(inputId = "mths",
                  label = "Number of steps forward:",
                  min = 6,
                  max = 24,
                  value = 6),
      
      # Input: Table
      rHandsontableOutput("table", width = "400px", height = "500px")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(id="Tabs", type = "pills",
                  tabPanel("Charts", 
                           h3(textOutput("expectedVal")),
                           plotlyOutput("distPlot", width = "auto"),
                           h3(textOutput("curve")),
                           plotlyOutput("ratePlot", width = "auto")
                           ),
                  tabPanel("Results", 
                          h3(textOutput("ExpectedVal")),
                          dataTableOutput("resultTbl"),
                          
                          h3(textOutput("Curve")),
                          dataTableOutput("curveTbl")
                          ),
                  tabPanel("About", 
                           h4(htmlOutput("PayLink")), br(),
                           htmlOutput("Desc"), br(), 
                           DiagrammeROutput("diag"),br(),
                           htmlOutput("ToDo")
                  ))
      
    ))
)


server <- function(input, output, session) {
  
  # reactive value to track table validity
  is_table_ok <- reactiveVal(FALSE)
  
  observeEvent(input$hist,{
  # Render the rhandsontable
    output$table <- renderRHandsontable({
      
      if(input$hist ==2){
        default_tbl <- default_tbl2
      } else if (input$hist ==3){
        default_tbl <- default_tbl3
      } else if (input$hist ==4){
        default_tbl <- default_tbl4
      }
      
      rhandsontable(default_tbl,
                    rowHeaders = NULL
                    )  %>%
        hot_col("Total", format = "0a", halign = "htCenter") %>%
        hot_col("PayNext", format = "0a", halign = "htCenter") %>% 
        hot_col("Code", readOnly = TRUE, halign = "htCenter") %>% 
        hot_cols(colWidths = c(50, 80, 80),
                 manualColumnMove = FALSE,
                  manualColumnResize = TRUE)
    })
  })
  
  
  
  observeEvent(input$Run,{
  
    table_object <- hot_to_r(input$table)
    
    # check if table contains only valid (non-NA) numeric
    # check if all the "Total" > "PayNext"
    
    flag <- all(!is.na(table_object[,2:3])) & all(table_object[,2] > table_object[,3])
    
    
    if(flag == T){
      
      y <- getCodeTbl(hot_to_r(input$table))
      
      result <- getModelDeclineCurve(y, input$mths)
      
      overall <- suppressMessages(
        result %>% inner_join(
          y %>% mutate(proportion = total / sum(y$total)), by=c("startCode" = "code")) %>% 
        mutate(
          startCode = "ALL",
          expectedVal = expectedVal * proportion,
          curve = curve * proportion
        ) %>% group_by(startCode, numStep) %>% 
        summarise(expectedVal = sum(expectedVal),
                  curve = sum(curve)))
      
      result <- result %>% union_all(overall) %>% 
        mutate(expectedVal = round(expectedVal, 3),
               curve = round(curve, 3)
        ) %>% rename("Num_Steps" = "numStep")
      
      output$expectedVal <- renderText("Expected Value")
      
      binStep = floor(max(result$Num_Steps) / 5)
      maxRng = (binStep + as.integer(max(result$Num_Steps) %% 5 != 0)) * 5
      
      
      output$distPlot <- renderPlotly({
        ggplot(result, aes(x=Num_Steps, y=expectedVal, label=startCode)) + 
          geom_line(col = "dark blue",linewidth=1) + facet_wrap(~startCode) + 
          labs(title="", x = "Num Steps", y = "") + 
          theme(legend.position = "none", ) +
          scale_x_continuous(limits = c(0, maxRng),
                             breaks=seq(0, maxRng, 
                                        by = binStep))
      })
      
      output$curve <- renderText("Decline / Growth Curve")
      
      output$ratePlot <- renderPlotly({
        ggplot(result, aes(x=Num_Steps, y=curve, label=startCode)) + 
          geom_line(col = "purple",linewidth=1) + facet_wrap(~startCode) + 
          labs(title="", x = "Num Steps", y = "") + 
          theme(legend.position = "none", ) +
          scale_x_continuous(limits = c(0, maxRng),
                             breaks=seq(0, maxRng, 
                                        by = binStep))
      })
      
      output$ExpectedVal <- renderText("Expected Value")
      
      output$resultTbl <- renderDataTable({
        pivot_wider(result[,-4], names_from = startCode, values_from = expectedVal)}, 
         options = list(pageLength = 25, dom = "t") )
      
      output$Curve <- renderText("Decline / Growth Curve")
      
      output$curveTbl <- renderDataTable({
        pivot_wider(result[,-3], names_from = startCode, values_from = curve)}, 
         options = list(pageLength = 25, dom = "t"))
      
    } 
  })
  
  
  output$PayLink <- renderText({
    HTML(
      '<br><b><a href="https://www.paypal.me/KevHauR">∫ KevHau d(coffee) = formula </a></b><br>')
  })
  
  output$Desc <- renderText({
    HTML(
      
      "<h4>Objective:</h4>
      This algorithm model a stochastic process that combines the forward-branching structure of a binomial tree 
      with the memory-retaining dynamics of a recurrent neural network (similar to <a herf='https://en.wikipedia.org/wiki/Hopfield_network'>Hopfield network</a> when historical step (k) = 3), 
      while computing expected values at every step under binary (0/1) outcomes.<br>
      
      <h4>Applications:</h4>
      <ul>
      <li> Monthly payment (utilities / phone / insurance) </li>
      <li> Repayments of loan / mortgage (beyond the scope of this app if the term is shorter than 24) </li>
      <li> Binomial Option (beyond the scope of this app) </li>
      </ul>
      
      <h4>Instructions:</h4>
      <ul>
        <li>Choose the number of historical time steps and the number of future steps you want to forecast.</li>
        <li>Enter the total population for each event code across the historical period, plus the expected number of events for the next upcoming step.</li>
        <li>Click <strong>Run</strong> to compute the expected values and visualize the resulting growth or decline curve.</li>
      </ul>
      
      
      <h4>Details:</h4>
      
      In a binomial tree, each node’s probability depends only on the immediately previous state. 
      Here, the transition probability at step k is derived from a grouped summary of multiple earlier steps—effectively treating the history as a compact state vector whose influence decays or aggregates in a controlled manner.<br><br>
      This is similar to the recurrent connections in a Hopfield network, where the network’s energy landscape stores and retrieves patterns from past activations. 
      However, unlike Hopfield neural network (which typically performs deterministic or probabilistic retrieval), the algorithm injects a forward-looking expected-value layer. <br><br>
      This expectation is propagated forward, turning the process into a hybrid recurrent stochastic process that remembers past configurations while maintaining an analytically tractable martingale-like property.<br><br>
      
      <h4>Illustration:</h4>
      Below show a simple example of using last 2 historical steps from one of the starting branch to the next 3 steps. The further the graphs goes, each number of payment will remain maximum four of the codes
      
      
      ")
  })
  
  
  output$diag <- renderDiagrammeR({
    mermaid('
    graph LR
            
    A["11"] --> |"Paid"|B["11"]
    
    A --> |"Not"|C["10"]
    B --> D["11"]
    
    B --> E["10"]
    C --> F["01"]
    C --> G["00"]
    
    D --> H["11"]
    D --> I["10"]
    E --> J["01"]
    E --> K["00"]
    F --> L["11"]
    F --> M["10"]
    G --> N["01"]
    G --> O["00"]
    
    subgraph start
      A
    end
    subgraph Paid:1
      B
    end
    subgraph Paid:0
      C
    end
    subgraph Paid:2
      D
    end
    subgraph Paid:1
      E
      F
    end
    subgraph Paid:0
      G
    end
    subgraph Paid:3
      H
    end
    subgraph Paid:2
      I
      J
      L
    end
    
    subgraph Paid:1
      K
      M
      N
    end
    subgraph Paid:0
      O
    end
    
    class B,D,F,H,J,L,N paid
    class C,E,G,I,K,M,O not


    classDef paid fill:#dfd,stroke:#0f0,stroke-width:1px,rx:4,ry:4
    classDef not fill:#fdd,stroke:#f00,stroke-width:1px,rx:4,ry:4

    
    ')
  })
  
  output$ToDo <- renderText({
    HTML("<h4>TO DO (based on funding): </h4>
          <ui>
            <li>Different option of input methods</li>
            <li>Extend length of steps forward</li>
            <li>Extend length of historical steps</li>
            <li>Include the option of different set of parameters for each step up to the cycle of 12 steps</li>
            <li>Include length of terms for loans</li>
            <li>Include breakdown of number of events by each step</li>
          </ui><br><br><br><br>
         ")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


