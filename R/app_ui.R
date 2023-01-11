#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyBS bsPopover
#' @importFrom shinydashboard dashboardPage
#' @importFrom dashboardthemes shinyDashboardThemeDIY
#' @importFrom htmltools tags span br hr HTML
#' @importFrom shinyWidgets awesomeRadio actionBttn downloadBttn
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),



    # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = tags$a(href='https://github.com/DerekVanBerkel/PIVOT',
                                     tags$img(src='www/PPGIS_logo3.png',height="85%", width="105%")), titleWidth = '300'),

      shinydashboard::dashboardSidebar(
        width = 300,

        shinydashboard::sidebarMenu(
          ## a slider to the sidebar to resolves the need for
          ##scrolling the map
          class = "sidebar",
          style = "height: 90vh; overflow-y: auto;",

          tags$p(style = "font-size: 12px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
                 htmltools::span("This PPGIS tool is designed to support" ,br(), "community planning efforts, helping users" ,br(), "to explore their communities through" ,br(), "spatial data. By following the steps below," ,br(), "users can upload their own spatial data and" ,br(), "create categories that outline regions of the" ,br(), "map for planning purposes.")),

          hr(),

          ## this is radio button where you choose which category to add to the map
          awesomeRadio("radioInt",
                       label = tags$p(style = "font-size: 16px;",
                                      htmltools::span("Choose categories and click on the map to prioritize"),
                                      htmltools::span(icon("info-circle"), id = "icon4", style = "color: blue")),
                       status= "success",
                       choices=golem::get_golem_options("mapping_categories"),
                       selected = 'No Category'),
          shinyBS::bsPopover("icon4", "Choose a mapping category", golem::get_golem_options("editable_map_info_icon_message"), trigger = "hover", placement = "bottom"),


          hr(),
          ## We define the header outside of the widget as it also prompted the download,
          ## which interfered with the information button
          tags$p(style = "font-size: 16px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
                 htmltools::span("Select the variable you want to",br(), "view"),
                 ##this defines the in-line info icon
                 htmltools::span(icon("info-circle"), id = "icon1", style = "color: blue; 15px;")
          ),
          ## this function defines the info-circle text using the shinyBS library
          shinyBS::bsPopover("icon1", "Map Variable", golem::get_golem_options("basemap_info_icon_message"), trigger = "hover", placement = "bottom"),

          selectInput("field",label = NULL ,'N/A'),


          hr(),
          fluidRow(column(11, actionBttn(
            inputId = "go",
            label = "Download Map",
            color = "success",
            size = "md",
            style = "unite",
            icon = icon("camera"),
            block = TRUE
          ))),

          fluidRow(column(11,downloadBttn(
            outputId = "download_shp",
            label = "Download Map Data",
            style = "unite",
            color = "success",
            icon = icon("download")))),


          hr(),

          ## This section of code adds the authors, images and link to the sidepanel
          HTML(paste0(
            "<br>",
            "<a href='https://seas.umich.edu/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='www/UM_SEAS_Logo.png'; width = '186'></a>",
            "<br>"
          )),

          HTML(paste0(
            "<br>",
            "<a href='https://glisa.umich.edu/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='www/GLISA_logo_standard_0.png'; width = '186'></a>",
            "<br>"
          )),

          HTML(paste0(
            "<script>",
            "var today = new Date();",
            "var yyyy = today.getFullYear();",
            "</script>",
            "<p style = 'text-align: center;'><small>&copy; - <a href='https://seas.umich.edu/research/faculty/derek-van-berkel' target='_blank'>DerekVanBerkel.com</a> - <script>document.write(yyyy);</script></small></p>",
            "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.linkedin.com/in/tgestabrook/' target='_blank'>ThomasEstabrook.com</a> - <script>document.write(yyyy);</script></small></p>",
            "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.linkedin.com/in/rahul-agrawal-bejarano-5b395774/' target='_blank'>RahulAgrawalBejarano.com</a> - <script>document.write(yyyy);</script></small></p>",
            "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.researchgate.net/profile/Nathan-Fox-8' target='_blank'>NathanFox.com</a> - <script>document.write(yyyy);</script></small></p>"))
        )
      ),
      ## this code add the new color theme defined at shinyDashboardthemesDIY()
      shinydashboard::dashboardBody(dashboardthemes::shinyDashboardThemeDIY(

        ### general
        appFontFamily = "Helvetica"
        ,appFontColor = "rgb(0,0,0)"
        ,primaryFontColor = "rgb(0,0,0)"
        ,successFontColor = "rgb(0,0,0)"
        ,warningFontColor = "rgb(0,0,0)"
        ,dangerFontColor = "rgb(0,0,0)"
        ,bodyBackColor = "rgb(248,248,248)"

        ### header
        ,logoBackColor = "rgb(30,47,68)"

        ,headerButtonBackColor = "rgb(30,47,68)"
        ,headerButtonIconColor = "rgb(255,255,255)"
        ,headerButtonBackColorHover = "rgb(210,210,210)"
        ,headerButtonIconColorHover = "rgb(0,0,0)"

        ,headerBackColor = "rgb(30,47,68)"
        ,headerBoxShadowColor = "#aaaaaa"
        ,headerBoxShadowSize = "2px 2px 2px"

        ### sidebar
        ,sidebarBackColor = dashboardthemes::cssGradientThreeColors(
          direction = "down"
          ,colorStart = "rgb(255,255,255)"
          ,colorMiddle = "rgb(235,237,240)"
          ,colorEnd = "rgb(64,64,65)"
          ,colorStartPos = 0
          ,colorMiddlePos = 50
          ,colorEndPos = 100
        )
        ,sidebarPadding = 0

        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = 0
        ,sidebarMenuBorderRadius = 0

        ,sidebarShadowRadius = "3px 5px 5px"
        ,sidebarShadowColor = "#aaaaaa"

        ,sidebarUserTextColor = "rgb(255,255,255)"

        ,sidebarSearchBackColor = "rgb(55,72,80)"
        ,sidebarSearchIconColor = "rgb(153,153,153)"
        ,sidebarSearchBorderColor = "rgb(55,72,80)"

        ,sidebarTabTextColor = "rgb(255,255,255)"
        ,sidebarTabTextSize = 13
        ,sidebarTabBorderStyle = "none none solid none"
        ,sidebarTabBorderColor = "rgb(35,106,135)"
        ,sidebarTabBorderWidth = 1

        ,sidebarTabBackColorSelected = dashboardthemes::cssGradientThreeColors(
          direction = "right"
          ,colorStart = "rgba(44,222,235,1)"
          ,colorMiddle = "rgba(44,222,235,1)"
          ,colorEnd = "rgba(0,255,213,1)"
          ,colorStartPos = 0
          ,colorMiddlePos = 30
          ,colorEndPos = 100
        )
        ,sidebarTabTextColorSelected = "rgb(0,0,0)"
        ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

        ,sidebarTabBackColorHover = dashboardthemes::cssGradientThreeColors(
          direction = "right"
          ,colorStart = "rgba(44,222,235,1)"
          ,colorMiddle = "rgba(44,222,235,1)"
          ,colorEnd = "rgba(0,255,213,1)"
          ,colorStartPos = 0
          ,colorMiddlePos = 30
          ,colorEndPos = 100
        )
        ,sidebarTabTextColorHover = "rgb(50,50,50)"
        ,sidebarTabBorderStyleHover = "none none solid none"
        ,sidebarTabBorderColorHover = "rgb(75,126,151)"
        ,sidebarTabBorderWidthHover = 1
        ,sidebarTabRadiusHover = "0px 20px 20px 0px"

        ### boxes
        ,boxBackColor = "rgb(255,255,255)"
        ,boxBorderRadius = 5
        ,boxShadowSize = "0px 1px 1px"
        ,boxShadowColor = "rgba(0,0,0,.1)"
        ,boxTitleSize = 16
        ,boxDefaultColor = "rgb(210,214,220)"
        ,boxPrimaryColor = "rgba(44,222,235,1)"
        ,boxSuccessColor = "rgba(0,255,213,1)"
        ,boxWarningColor = "rgb(244,156,104)"
        ,boxDangerColor = "rgb(255,88,55)"

        ,tabBoxTabColor = "rgb(255,255,255)"
        ,tabBoxTabTextSize = 14
        ,tabBoxTabTextColor = "rgb(0,0,0)"
        ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
        ,tabBoxBackColor = "rgb(255,255,255)"
        ,tabBoxHighlightColor = "rgba(44,222,235,1)"
        ,tabBoxBorderRadius = 5

        ### inputs
        ,buttonBackColor = "rgb(245,245,245)"
        ,buttonTextColor = "rgb(0,0,0)"
        ,buttonBorderColor = "rgb(200,200,200)"
        ,buttonBorderRadius = 5

        ,buttonBackColorHover = "rgb(235,235,235)"
        ,buttonTextColorHover = "rgb(100,100,100)"
        ,buttonBorderColorHover = "rgb(200,200,200)"

        ,textboxBackColor = "rgb(255,255,255)"
        ,textboxBorderColor = "rgb(200,200,200)"
        ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "rgb(245,245,245)"
        ,textboxBorderColorSelect = "rgb(200,200,200)"

        ### tables
        ,tableBackColor = "rgb(255,255,255)"
        ,tableBorderColor = "rgb(240,240,240)"
        ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1

      ),
                    ## here is the map
                    leafletOutput('PPGISmap', width='100%', height='650')
                    ## this fixes the location of the download screenshot and data button

      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PPGISr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
