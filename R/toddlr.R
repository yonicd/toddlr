#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tw PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname toddlr
#' @export 
#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniTitleBarButton miniContentPanel
#' @importFrom slickR slickROutput renderSlickR
#' @importFrom dplyr filter left_join mutate between n count select slice
toddlr <- function(tw = toc) {
  
  tw <- tw%>%tweet_threading_fw()
  
  # gadget UI ----
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = 'toddlr: "Toddler in Chief" Thread Analytics Dashboard',
                           left = miniUI::miniTitleBarButton("qt", "Quit")
    ),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::sliderInput(
            inputId = 'date',
            label = 'Status Dates',
            min = min(tw$created_at),
            max = max(tw$created_at),
            drag = TRUE,
            value = range(tw$created_at)
          ),
          shiny::selectizeInput(
            inputId = 'prox',
            label = 'Select Circle of Trust',
            choices = c('himself','intel','friends','congress','staff','GOP','allies'),
            selected = c('himself','intel','friends','congress','staff','GOP','allies'),
            multiple = TRUE
          ),
          shiny::sliderInput(
            inputId = 'slickslide',
            label = 'Last Status to Show',
            min = 1,
            max = 30,
            value = 5
          ),
          slickR::slickROutput('slick'),
          width = 3
        ),
        mainPanel = shiny::mainPanel(
          shiny::plotOutput('plot',height = 800),
          width = 9
        )
      )
    )
  )
  
  # gadget Server -----
  server <- function(input, output, session) {
    
    shiny::observeEvent(input$qt, {
      shiny::stopApp()
    })
    
    plot_dat <- shiny::eventReactive(c(input$date,input$prox),{
      
      tw <- tw%>%
        create_whoami()%>%
        create_prox()%>%
        dplyr::filter(prox %in% input$prox )
      
      ret_plots <- tw%>%
        toddlr_status()%>%
        dplyr::left_join(
          tw%>%toddlr_stats(),
        by = 'ym')
      
      ret_plots <- ret_plots%>%
        dplyr::mutate(
          now = dplyr::between(
            x = as.Date(sprintf('%s-01',ym),format = '%Y-%m-%d'),
            left = as.Date(sprintf('%s-01',strftime(input$date[1],'%Y-%m')),format = '%Y-%m-%d'),
            right = as.Date(sprintf('%s-01',strftime(input$date[2],'%Y-%m')),format = '%Y-%m-%d')
          ),
          nn=cumsum(n),
          i = 1:dplyr::n()
        )
      
      ret_filter <- tw%>%
        dplyr::filter(
          dplyr::between(
            x = created_at,
            left = input$date[1],
            right = input$date[2]
          )
        )

      ret_snippets <- ret_filter%>%
        dplyr::count(prox,whoami)
      
      ret_twe <- ret_filter%>%
        dplyr::select(screen_name,status_id)
      
      list(time = ret_plots , snips = ret_snippets, twe_dat = ret_twe)
    })
    
    output$plot <- shiny::renderPlot({

      toddlr_plots(plot_dat())
      
    })
    
    shiny::observeEvent(c(input$date,input$prox,input$slickslide),{
      output$slick <- slickR::renderSlickR({
        
        all_dat <- plot_dat()
        
        all_dat$twe_dat%>%
          dplyr::slice(1:input$slickslide)%>%
          toddlr_slick()
    })
    
    })
  }
  
  # Run Gadget ----
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
  
}
