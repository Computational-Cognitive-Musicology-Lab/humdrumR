
test_that('Plot errors', {
  # expect_error()
})



if (FALSE) {
  # generates many thousands of plots with different parameters (too many to be usable,really)
  
  
  aspects <- c(2, 16/9, 4/3, 1)
  
  
  
  options <- list(lm = c(TRUE, FALSE),
                  conditional = c(TRUE, FALSE),
                  normalReference = c(TRUE, FALSE),
                  legend = c(TRUE, FALSE),
                  margin = c(.15, .2, .4),
                  smooth = c(TRUE, FALSE),
                  showCounts = c(TRUE, FALSE),
                  mean = c(TRUE, FALSE),
                  global_stats = c(TRUE, FALSE),
                  center = c(TRUE, FALSE),
                  heat = c(TRUE, FALSE),
                  col = c('red', 'skyblue'), 
                  alpha = c(.9, .3),
                  aspect = unique(c(aspects, 1/aspects)),
                  jitter = c('', 'x', 'y', 'xy'),
                  log = c('', 'x', 'y', 'xy'),
                  quantiles = list(.5, c(.25, .75), c(.025, .25, .5, .75, .975)),
                  title = list(NULL, 'Title here', "Two-line\nTitle here.", "Very loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong title."),
                  subtitle = list(NULL, 'Subtite here', "Two-line\nSubtitle here.", "Very loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong subtitle."),
                  xlabel = list(NULL, 'X label here', 'Two-line\nX-label here', 'Very very loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong X-label here.'),
                  ylabel = list(NULL, "Y label here", 'Two-line\nY-label here', 'Very very loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong Y-label here.'))
  
  
  Ns <- c(20, 200, 2000)
  
  for (N in Ns) {
    
    
    args <- list(normal = list(rnorm(N, 50, 20), rnorm(N, -20, 200)), 
                 missing = list(NULL), 
                 poisson = list(rpois(N, 20)), 
                 chisq = list(rchisq(N, 2), rchisq(N, 12)), 
                 categorical = list(gl(4, N/4, N)))#, gl(3, N/3, N), sample(LETTERS[1:7], N, replace=T, 7:1)))
    
    
    
    args$derived <- Map(Filter(is.numeric, unlist(args, recursive = FALSE)) |> tail(n = -1), 
                        Filter(is.numeric, unlist(args, recursive = FALSE)) |> head(n = -1), 
                        f= \(x, y) {  
                          list(#as.numeric(x) * 5.5 + rnorm(N, 0, 1),  
                            as.numeric(x) * -22 + rchisq(N, 2),  
                            x * y) }) |> unlist(recursive = FALSE)
    
    
    args <- c(options, args)
    
    args <- with(args, {
      list(density = list(x = c(normal = normal, poisson = poisson, chisq = chisq), 
                          y = missing,
                          col = c(col, categorical = categorical),
                          alpha = alpha,
                          conditional = conditional,
                          mean = mean, global_stats = global_stats,
                          smooth = smooth,
                          log = c('', 'x'),
                          title = title, subtitle = subtitle, 
                          xlabel = xlabel, ylabel = ylabel,
                          quantiles = quantiles)
      )
      
    })
    
    
    lapply(names(args),
           \(test) {
             argFrame <- do.call('expand.grid', c(args[[test]], list(stringsAsFactors = FALSE)))
             
             print(nrow(argFrame))
             for (i in 1:nrow(argFrame)) {
               curArgs <- lapply(argFrame[i, ], \(row) if (is.list(row)) row[[1]] else row) |>
                 Filter(f = Negate(is.null))
               
               argExpr <- sapply(argFrame[i, ], \(row) if (is.list(row)) names(row)[1] else row[[1]])
               call <- paste(paste0(names(argFrame),'=',argExpr), collapse=';')
               do.call('draw',curArgs) |> 
                 drawToFile(filename = paste0('tests/testthat/testPlots/', test, '_N',N, '_', call, '.png'), overwrite = TRUE)
             }
             
           })
  }
  
}



if (TRUE) {
# These tests must be inspected manually ,so if (FALSE)



variables <- list(
                  normal = list(quote(rnorm(N)), quote( rnorm(N, seq(1, 50, length.out = N), 20)), quote(rnorm(N, -20, 200))),
                  missing = list(quote(NULL)),
                  poisson = list(quote(rpois(N, 2)), quote(rpois(N, 20))),
                  factor = list(quote(gl(4, N/4, N)), quote(gl(3, N/3, N, labels = c('Group 1', 'Group 2', 'Group 3'))), quote(sample(LETTERS[1:7], N, replace=T, 7:1))),
                  derived = list(quote(as.numeric(x) * 5.5 + rnorm(N, 0, 1)),
                                 quote(as.numeric(x) * -22 + rchisq(N, 2) * 20)))

aspects <- c(2, 16/9, 4/3, 1)

plotArguments <- list(lm = c(TRUE, FALSE),
                      conditional = c(TRUE, FALSE),
                      normalReference = c(TRUE, FALSE),
                      legend = c(TRUE, FALSE),
                      margin = c(.15, .2, .4),
                      smooth = c(TRUE, FALSE),
                      showCounts = c(TRUE, FALSE),
                      mean = c(TRUE, FALSE),
                      global_stats = c(TRUE, FALSE),
                      center = c(TRUE, FALSE),
                      heat = c(TRUE, FALSE),
                      aspect = unique(c(aspects, 1/aspects)),
                      jitter = c('', 'x', 'y', 'xy'),
                      log = c('', 'x', 'y', 'xy'),
                      quantiles = list(.5, c(.25, .75), c(.025, .25, .5, .75, .975)),
                      xlab = list(NULL, 'X label here'),
                      ylab = list(NULL, "Y label here"))
                      
                      



library(shiny)

shinyApp(ui = sidebarLayout(sidebarPanel = sidebarPanel(width = c(2,10),numericInput('seed', 'seed', value = 1, min = 1, max = 50, step = 1),
                                                        sliderInput('Nx', label = 'log(sample size, 10)', min = 1, max = 7, step = .5, value = 2),
                                                        sliderInput('aspect', 'log(aspect, 2)', min = -2, max = 2, step = .1, value = 1),
                                                        selectInput('x', 'x', choices = variables),
                                                        selectInput('y', 'y', choices = variables),
                                                        checkboxInput('lm', 'lm', value = FALSE),
                                                        checkboxInput('conditional', 'conditional', value = FALSE),
                                                        checkboxInput('center', 'center', value = FALSE),
                                                        checkboxInput('smooth', 'smooth', value = FALSE),
                                                        checkboxInput('normalReference', 'normalReference', value = FALSE),
                                                        checkboxInput('showCounts', 'showCounts', value = FALSE),
                                                        checkboxInput('showPoints', 'showPoints', value = FALSE),
                                                        checkboxInput('global_stats', 'global_stats', value = FALSE),
                                                        checkboxInput('mean', 'mean', value = FALSE),
                                                        checkboxInput('line', 'line', value = FALSE),
                                                        checkboxInput('heat', 'heat', value = FALSE),
                                                        checkboxInput('violin', 'violin', value = FALSE),
                                                        selectInput('jitter', 'jitter', choices = c('none', 'x', 'y', 'xy')),
                                                        selectInput('log', 'log', choices =  c('none', 'x', 'y', 'xy')),
                                                        textInput('quantiles', 'Quantiles', value = ''),
                                                        textInput('xlab', 'xlab', value = 'none'),
                                                        textInput('ylab', 'ylab', value = 'none'),
                                                        textInput('main', 'main', value = 'none'),
                                                        textInput('sub', 'sub', value = 'none'),
                                                        selectInput('col', 'color', choices = variables[c(2,1,3:length(variables))]),
                                                        selectInput('cex', 'point size', choices = variables[c(2,1,3:length(variables))]),
                                                        sliderInput('height', 'Plot height', min = 400, max = 1600, value = 1000, step = 100),
                                                        sliderInput('width', 'Plot width', min = 400, max = 3000, value = 1800, step = 100)),
                            mainPanel = fluidPage(textOutput('expr'), plotOutput('draw', inline = TRUE))),
         server <- function(input, output) {
             expr <- reactiveVal()
             expr(quote(draw(1:10)))

             plotH <- reactive(input$height)
             plotW <- reactive(input$width)

             output$draw <- renderPlot(height = \() plotH(), width = \() plotW(),
                                       {
                 args <- reactiveValuesToList(input)
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


                 curexpr <- rlang::expr({ plot <- draw(x =x , y = y, !!!args); show(plot)})
                 expr(curexpr)
                 rlang::eval_tidy(curexpr)

                 })

             output$expr <- renderText({deparse(expr())})


             })
}
