library(shiny)
library(ggplot2)
library(latex2exp)
library(markdown)
library(gridExtra)



ui <- fluidPage(withMathJax(),
                titlePanel('Comparing IS curves'),
                sidebarLayout(
                  sidebarPanel(
                    p(
                      'How does the IS curve change when we change the behavior of consumers or investors? Suppose
					  $$ C(Y,T) = a + b \\times (Y-T) $$
					  $$PI(r) = d - h \\times r $$
					  and taking T and G as exogenous
					  
					  The equation for the IS curve is
					  
					  $$ Y = \\frac{1}{1-b} \\times (a - bT + d - hr + G ) $$'),
                    
                    p('Below you can pick high, medium or low values for each parameter. 
                      The initial curve has medium values for each. 
                      When you have made your selection, hit the plot button!'),
                    actionButton("plot", "Plot"),
                    h3("Parameters"),
                    radioButtons(
                      "a",
                      "Autonomous consumption  (\\(a\\))",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = 1),
                    radioButtons(
                      "b",
                      "Marginal propensity to consume (MPC) (\\(b\\))",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = .5),
                    radioButtons(
                      "d",
                      "autonomous investment (\\(d\\))",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = 1),
                    radioButtons(
                      "h",
                      "Responsiveness of planned investment to interest rates  (\\(h\\))",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = 1),
                    radioButtons(
                      "r",
                      "Real interest rate  (\\(r\\)) - only affects the PE graph!",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = 1),
                    radioButtons(
                      "Tax",
                      "Taxes minus transfers  (\\(T\\))",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = 1),
                    radioButtons(
                      "G",
                      "Government spending  (\\(G\\))",
                      choices = list("low" = .5, 
                                     "medium" = 1,
                                     "high" = 2), selected = 2)
                  ),
                  mainPanel(
                    h1("Partial equilibrium:"),
                    plotOutput('PE_plot'),
                    h1("IS curve"),
                    plotOutput('IS_plot')
                  )
                )
)



server <- function(input, output) {
  
  output$PE_plot <- renderPlot({
    input$plot
    isolate({
      a<-as.numeric(isolate(input$a))
      ggplot(data.frame(Y = c(0,4000)), aes(x=Y)) +
        stat_function(
          fun = function(Y)
            a *250  + as.numeric(isolate(input$b)) * .2 * ( Y - (as.numeric(isolate(input$Tax))  *100)) + 500*as.numeric(isolate(input$d)) - 50*as.numeric(isolate(input$h)) * (1 *as.numeric(isolate(input$r)) ) + 100*as.numeric(isolate(input$G)),
          #geom = "line",
          #aes(colour = "Initial Depreciation", linetype = "Initial Depreciation"),
          #  linetype = "dashed",
          colour = "red",
           size = 1.25
        ) +
           stat_function(
           fun = function(Y)
             250 + .2*(Y - 100) + 500 - 50* 1 + 100,
      #     geom = "line",
      #     #aes(colour = "Initial Depreciation", linetype = "Initial Depreciation"),
      #     #  linetype = "dashed",
           size = 1.25
       ) +
       stat_function(
         fun = function(Y)
           Y,
         geom = "line",
      #   #aes(colour = "Initial Depreciation", linetype = "Initial Depreciation"),
           linetype = "dashed",
      colour = "gray",
         size = 1.25
       )+
         theme_classic()+
        ylab('Planned Spending (red line is with selected values)')+
        xlim(c(0,4000)) + ylim(c(0,4000))
        #theme(legend.position = "left")
        
        
      
    })
    
  })
  
  output$IS_plot <- renderPlot({
    input$plot
    isolate({
      a<-as.numeric(isolate(input$a))
      b<-as.numeric(isolate(input$b))
      d<-as.numeric(isolate(input$d))
      h<-as.numeric(isolate(input$h))
      Tax<-as.numeric(isolate(input$Tax))      
      G<-as.numeric(isolate(input$G))
      mult <- (1-as.numeric(isolate(input$b)))^(-1)
      
      ggplot(data.frame(rr = c(-5,15)), aes(x=rr)) +
         stat_function(
           fun = function(r)
             (1/(1-.2*b) ) *(250*a - (.2*b)*(100*Tax) + (500*d) - (50*h)* r + (100*G)),
        #   #geom = "line",
        #   #aes(colour = "Initial Depreciation", linetype = "Initial Depreciation"),
        #   #  linetype = "dashed",
           colour = "red",
           size = 1.25
         ) +
       stat_function(
          fun = function(r)
            (1/(1-.2) ) *(250 - (.2)*(100) + (500) - (50)* r + (100)),
          #     geom = "line",
          #     #aes(colour = "Initial Depreciation", linetype = "Initial Depreciation"),
          #     #  linetype = "dashed",
          size = 1.25
        ) +
        geom_vline(xintercept = as.numeric(isolate(input$r)))+
        geom_vline(xintercept = 1)+
        
        coord_flip()+
        theme_classic()+
      ylim(c(0,4000)) + xlim(c(-5,15))
      #theme(legend.position = "left")
      
      
      
    })
    
  })
  

}

shinyApp(ui = ui, server = server)




