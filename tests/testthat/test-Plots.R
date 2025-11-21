if (FALSE) {
# These tests must be inspected manually ,so if (FALSE)
library(shiny)


variables <- list(
                  normal = list(quote(rnorm(N)), quote(rnorm(N, 50, 20)), quote(rnorm(N, -20, 200))),
                  missing = list(quote(NULL)),
                  poisson = list(quote(rpois(N, 2)), quote(rpois(N, 20))),
                  factor = list(quote(gl(4, N/4, N)), quote(gl(3, N/3, N)), quote(sample(LETTERS[1:7], N, replace=T, 7:1))),
                  derived = list(quote(as.numeric(x) * 5.5 + rnorm(N, 0, 1)),
                                 quote(as.numeric(x) * -22 + rchisq(N, 2) * 20),
                                 quote(x * y)))

shinyApp(ui = sidebarLayout(sidebarPanel = sidebarPanel(width = c(2,10),numericInput('seed', 'seed', value = 1, min = 1, max = 50, step = 1),
                                                        sliderInput('Nx', label = 'log(sample size, 10)', min = 1, max = 7, step = .5, value = 2),
                                                        checkboxInput('lm', 'lm', value = FALSE),
                                                        checkboxInput('conditional', 'conditional', value = FALSE),
                                                        checkboxInput('smooth', 'smooth', value = FALSE),
                                                        sliderInput('aspect', 'log(aspect, 2)', min = -2, max = 2, step = .1, value = 0),
                                                        selectInput('jitter', 'jitter', choices = c('none', 'x', 'y', 'xy')),
                                                        selectInput('log', 'log', choices =  c('none', 'x', 'y', 'xy')),
                                                        textInput('quantiles', 'Quantiles', value = ''),
                                                        textInput('xlab', 'xlab', value = 'none'),
                                                        textInput('ylab', 'ylab', value = 'none'),
                                                        textInput('main', 'main', value = 'none'),
                                                        textInput('sub', 'sub', value = 'none'),
                                                        selectInput('x', 'x', choices = variables),
                                                        selectInput('y', 'y', choices = variables),
                                                        selectInput('col', 'color', choices = variables[c(2,1,3:length(variables))]),
                                                        selectInput('cex', 'point size', choices = variables[c(2,1,3:length(variables))]),
                                                        sliderInput('height', 'Plot height', min = 400, max = 1200, value = 1000, step = 100),
                                                        sliderInput('width', 'Plot width', min = 400, max = 1800, value = 1000, step = 100)),
                            mainPanel = fluidPage(plotOutput('draw', inline = TRUE), textOutput('expr'))),
         server <- function(input, output) {
             expr <- reactiveVal()
             expr(quote(draw(1:10)))
             
             plotH <- reactive(input$height)
             plotW <- reactive(input$width)
             
             output$draw <- renderPlot(height = \() plotH(), width = \() plotW(),
                                       {
                 args <- as.list(input)
                 args$Nx <- args$height <- args$width <- args$seed <- NULL
                 
                 args <- Filter(Negate(is.null), args)
                 
                 args$aspect <- 2^args$aspect
                 
                 args$quantiles <- local({
                     parse <- try(parse(text = paste0('c(', input$quantiles, ')')), silent = TRUE)
                     
                     if (class(parse)[1] != 'try-error') eval(parse) else c()
                 })
                 
                 N <- 10^input$Nx
                 
           
                 
                 if (args$jitter == 'none') args$jitter <- ''
                 if (args$log == 'none') args$log <- ''
                 if (grepl('x', args$log)) args$x <- paste0('abs(', args$x, ') + 1')
                 if (grepl('y', args$log)) args$y <- paste0('abs(', args$y, ') + 1') 
                 
                 set.seed(input$seed)
                 
                 x <- eval(parse(text = args$x))
                 y <- eval(parse(text = args$y))
                 
                 args$x <- args$y <- NULL
                 # args$x <- rlang::parse_expr(args$x)
                 # args$y <- rlang::parse_expr(args$y)
                 col <- rlang::parse_expr(args$col)
                 args$col <- if (!is.null(col)) col
                 
                 cex <- rlang::parse_expr(args$cex)
                 args$cex <- if (!is.null(cex)) {
                   rlang::expr(if (is.factor(!!cex)) cex else abs(!!cex) + 1)
                 }
                 
                 args <- Filter(\(x) x != 'none', args)
                 
                 
                 curexpr <- rlang::expr(draw(x =x , y = y, !!!args))
                 expr(curexpr)
                 rlang::eval_tidy(curexpr)
                 
                 })
             
             output$expr <- renderText({deparse(expr())})
             
             
             })
         }