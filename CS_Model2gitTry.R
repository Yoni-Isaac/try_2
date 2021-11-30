tictoc::tic()
rm(list=ls())
library(cli)
library(shiny)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(shinyBS)
library(shinyLP)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(leafpm)
library(htmlwidgets)
#library(rgeos) # source rgeos_0.5-8.zip, Technical debt!!!!!
library(sp)
library(lwgeom)
library(sf)
library(maptools)
library(mapview)
library(mapedit)
library(Orcs)
library(rgdal)
library(dplyr)
library(readxl)
library(googledrive)
library(data.table)
library(raster)
library(DT)
library(stringr)
library(openxlsx)
library(readr)
library(ggplot2)
library(ggnewscale)
library(plotly)
library(officer)
library(grid)
library(utils)
library(aws.s3)
library(tibble)
library(proj4)
library(purrr)
library(tidyverse)
library(shinyalert)
library(shinyWidgets)
library(cowplot)
library(magick)
library(slickR)

################## App Directory ########################
# G:\Geohydrology\Apps\CS_Model_V01
################## App Directory ########################

# Source Tools Scrips and functions ============================================================
message("Source Tools Scrips and functions")

Background_path="G:/Geohydrology" # %%%%%%%%% Change while moved to unplugged  %%%%
# Prodact_path=paste0(Background_path,"/Geohydrology/Apps/CS_Model_V01/Products")
source('scripts/Geohydrology_Functions_V2.R', encoding = 'UTF-8')
debugSource('scripts/CS_Model_Code_V37.R', encoding = 'UTF-8') #debugSource
source('scripts/Horizons_Model_Code_V7.R', encoding = 'UTF-8') #debugSource
source('scripts/Maps_Code_V2.R', encoding = 'UTF-8') #debugSource
options(shiny.maxRequestSize = Inf)
options(shiny.trace = F)
options(shiny.fullstacktrace = T)
options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
options(shiny.reactlog=T) 
Sys.setlocale("LC_ALL", "Hebrew")
modeldialog_status="Inactive"
`%notin%` <<- Negate(`%in%`)
charts=NULL
# Clean before deploy: "C:\Users\yoniis\AppData\Local\Temp"

# Set Aotintication File =======================================================================
message("Set Aotintication File")
Password_df=data.frame(username = c("Security","Gov","Academy"),
                       Password = c("hydro2020@1","hydro2020@2","hydro2020@3"),
                       auth_lvl = c("Security","Gov","Academy"),
                       stringsAsFactors = FALSE
)

# Get Base Layers ==============================================================================
message("Get Base Layers")
# Local Path -----------------------------------------------------------------------------------
transforms_pth= 'data/Background_layers/Transforms/TJ_FAULTS_ll.shp'
basemap_pth=paste0("data/Background_layers/BaseMaps/")
design_pth='data/Design_elements'
well_pth='data/Background_layers/Wells'
dem_pth = "data/DEMs"

# Set Base Layers ===============================================================================
message("Set Base Lyers")

# TJ Faluts 
transforms_ll=sf::st_read(transforms_pth)
transforms_shp=sf::as_Spatial(st_zm(transforms_ll, drop = TRUE, what = "ZM"))
crs(transforms_shp)=sp::CRS("+proj=longlat +datum=WGS84")
transforms_df=transforms_shp %>% as.data.frame(.) %>% subset(.,,"ObjectID")

# Libraries Boundaries
library_dms=sf::st_read(paste0(dem_pth,"/library_dms_V2",".shp"))%>% 
  dplyr::filter(.,basin != "national")

# Geology - Base map
geology_50=sf::st_read(paste0(basemap_pth,"geology_50_V6.shp"))
geology_50_proxy=geology_50[1,]
geoIDX_50=read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "geoINDEX50")
geopal_50=colorFactor(palette = geoIDX_50$hex,
                      domain = geoIDX_50$Code,
                      ordered=T,
                      na.color = "#808080")

geology_200=sf::st_read(paste0(basemap_pth,"geology_200_V4.shp"))
geology_200_proxy=geology_200[1,]
geoIDX_200=read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "geoINDEX200")
geopal_200=colorFactor(palette = geoIDX_200$hex,
                       domain = geoIDX_200$Code,
                       ordered=T,
                       na.color = "#808080")

# National Tiles
hebrew_str <- locale(encoding = "ISO-8859-8")
localtiles_df=readr::read_csv(paste0(design_pth,"/localtiles_V1.csv"),locale=hebrew_str) 


# Geology - Tiles Grid
geogrid= dplyr::filter(sf::st_read(paste0(basemap_pth,"geogrid_V10.shp")),!is.na(resoid) & resoid!=0,)

# Virtual Wells - Template
Virtual_int=fread(paste0(well_pth,"/Virtual_Wells.csv"),colClasses=c("character","character","character",
                                                                     "double","double","double","character","character","character","double","double","double",
                                                                     "character","character","integer"))
Virtual_dt=Virtual_int
INDEX_DEMs = as.data.frame(read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),
                                      sheet = "Index_DEMs",
                                      col_types=c("text","text","text","text","text")))

cs_ids=read.csv(paste0(design_pth,"/cs_ids_V1.csv"))

# Horizons
tab_raw=NULL
tab_tbl=NULL
horizons_db=NULL

# Additional Layers
additional_layers_df=read.csv(paste0(design_pth,"/additional_layers_ids_V1.csv"))
additional_layers_lst=list()

# Visualization
noHide_status<<-F
zoom_old=10

# Info System =================================================================================

msgactionBttn = function(infoId,color,c_label){
  ab = actionBttn(inputId=infoId,
                  style="gradient",
                  color = color,
                  size="xs",
                  label = c_label,
                  icon = icon("info-circle")) 
  return(ab)
}

creditactionBttn = function(infoId,color,c_label,icon){
  ab = actionBttn(inputId=infoId,
                  style="gradient",
                  color = color,
                  size="md",
                  label = c_label,
                  icon = icon) 
  return(ab)
}

msgalart = function(c_title,c_text){
  sa=shinyalert(
    title = c_title,
    text = c_text,
    closeOnEsc = T,
    closeOnClickOutside = T,
    html = T,
    type = "",
    showConfirmButton = F,
    showCancelButton = F,
    #confirmButtonText = "OK",
    #confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = T
  )
  return(sa)
}
# Run App ######################################################################################
message("################################### Run App ###################################")
tictoc::toc()
# UI ===========================================================================================
ui <- fluidPage(
  # Busy Status
  add_busy_spinner(
    spin = "double-bounce",
    color = "#112446",
    timeout = 100,
    position ="bottom-left",
    onstart = TRUE,
    margins = c(10, 10),
    height = "50px",
    width = "50px"
  ),
  
  tags$style(HTML("
                  #mainmap {
                  position: absolute;
                  
                  top: 35px;
                  }
                  ")),
  # Browser title
  list(tags$head(HTML('<link rel="icon", href="app_icon.jpg",
                        type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="CSMS 2.1.1"
      )
  ),
  
  fluidRow(
    # Main panel for displaying outputs ------------------------------------------------------------
    message("Main panel for displaying outputs"),
    div(
      navbarPage(title=div(img(src="app_icon_small_black.jpg"), "Cross Section Model System"),
                 inverse = F, # for diff color view
                 theme = shinytheme("flatly"),
                 
                 tabPanel("Home", icon = icon("home"),
                          span(actionButton("info", 
                                            icon = icon("info-circle"),
                                            style="color: #fff; background-color: #2c3e50; border-color: #2e6da4",
                                            width = "120px",
                                            label = HTML("<span style='font-size:1.1em 'text-align:justify'><br />Info (Inactive)</span>")
                          ),
                          style = "position:absolute;right:2em;"),
                          HTML("<div style='vertical-align:middle; text-align:center'> 
                                  <img src='app_panel_1.jpg' width='1400' height='500' alt='This is alternate text'></img>
                             </div>"),
                          HTML("<div style='vertical-align:middle; text-align:center'> 
                                  <h2 style='text-align:middle;'>Welcome to CSMS<sup>®</sup> Softwar, Explore & Analysis your deep data.</h2>
                             </div>"),
                          slickROutput("slickr", width="1400px")#,
                 ),
                 
                 tabPanel("Map", icon = icon("location-arrow"),
                          # Sidebar panel for Aoturization -------------------------------------------------------------
                          message("Sidebar panel for Aoturization"),
                          fluidRow(
                            hr(),
                            leafletOutput("mainmap",height = "1950px",width = "3400px"),
                            column(width = 5, style = "background-color:#2c3e50; opacity: 0.8;",
                                   # Control actionButton Column --------------------------------------------------------------
                                   column(width = 3,offset = 1,
                                          passwordInput("PW", creditactionBttn(infoId="Password_info",color="default",c_label="Registered User",icon=img(src = "IHS_logo.jpg",
                                                                                                                                                         height = 36, width = 38)),placeholder=NULL),
                                          # Geohydrology Layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          uiOutput("modelUI"),
                                          # Additional Layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          selectInput("additional_layers_type",
                                                      creditactionBttn(infoId="additional_layers_type_info",color="default",c_label="Additional Layers",
                                                                       icon= tags$i(class = "fas fa-layer-group", style="font-size: 34px; color: gray")),
                                                      multiple=F,
                                                      selected=NULL,
                                                      choices=additional_layers_df$type),
                                          fileInput('additional_layers',
                                                    label="",
                                                    accept = c(
                                                      '.tif',
                                                      ".shp",".dbf",".sbn",".sbx",".shx",".prj"
                                                    ),
                                                    multiple=T
                                          )
                                   ),
                                   column(width = 3,offset = 0,
                                          selectInput("geomap",
                                                      creditactionBttn(infoId="geomap_info",color="default",c_label="Geological Maps",
                                                                       icon=img(src = "GSI_logo.jpg", height = 34, width = 34)),
                                                      multiple=F,
                                                      selected="Regional-Low",
                                                      choices=c("Local-High","Regional-Low")),
                                          selectInput("country",
                                                      creditactionBttn(infoId="country_info",color="default",c_label="Country",
                                                                       icon= tags$i(class = "fas fa-flag", style="font-size: 34px; color: grey")),
                                                      multiple=F,
                                                      selected="Israel",
                                                      choices=c("Israel","Indefinite")),
                                          actionButton("login", # Refresh Button
                                                       icon = tags$i(class = "fas fa-redo", style="font-size: 40px"),
                                                       style="color: #000000; background-color: #FFFFFF; border-color: #FFFFFF",
                                                       width = "70px",
                                                       label =""
                                          )
                                   ),
                                   column(width = 3,offset = 0.5,
                                          fileInput('file1',
                                                    creditactionBttn(infoId="Refresh_info",color="default",c_label="Upload wells",
                                                                     icon= tags$i(class = "fas fa-file-upload", style="font-size: 34px; color: gray")),
                                                    accept = c(
                                                      'text/csv',
                                                      'text/comma-separated-values',
                                                      'text/tab-separated-values',
                                                      'text/plain',
                                                      '.csv',
                                                      '.tsv'
                                                    )
                                          )
                                   ),
                                   column(width = 3,offset = 0.5,
                                          fileInput('csgrid',
                                                    creditactionBttn(infoId="csgrid_info",color="default",c_label="Upload CS Grid",
                                                                     icon= tags$i(class = "fas fa-file-upload", style="font-size: 34px; color: brown")),
                                                    accept = c(
                                                      'text/csv',
                                                      'text/comma-separated-values',
                                                      'text/tab-separated-values',
                                                      'text/plain',
                                                      '.csv',
                                                      '.tsv',
                                                      '.shp'
                                                    )
                                          )
                                   )
                            )
                          ),
                          class = "span1"), 
                 tabPanel("Wells Meta Data",  icon = icon("database"),
                          fluidRow(
                            # Buffer Selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            numericInput("Buffer",  msgactionBttn(infoId="Buffer_info",color="default",c_label="Search strip width around the CS [km]:"),
                                         min = 0.01, max = 2, value = 0.4,step=0.01)
                          ),
                          DTOutput("CS_model_system")),
                 tabPanel("Cross Section",  icon = icon("google-wallet"),
                          # Sidebar panel for Inputs ------------------------------------------------------------
                          message("Sidebar panel for Inputs"),
                          fluidPage(
                            column(2,
                                   # Initiation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   tags$head(tags$script(src = "message-handler.js")),
                                   actionButton("Get",
                                                icon = icon("google-wallet"),
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                width = "200px",
                                                label = HTML("<span style='font-size:1.3em;'><br />Run</span>")
                                   ),
                                   titlePanel("Model Parameters"),
                                   
                                   # Build Solids ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   checkboxInput("Build_Solids", msgactionBttn(infoId="Build_Solids_info",color="default",c_label="Build_Solids:"),value = F),
                                   
                                   # Cross Section Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   radioButtons(inputId="CS_type",label=  msgactionBttn(infoId="CS_type_info",color="default",c_label="Cross Section Type:"),
                                                selected = NULL,
                                                choices=c("Groups" = "groups",
                                                          "Formations" = "formations",
                                                          "Materials" = "materials")),
                                   # Surface geology ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput(inputId="geological_cs_surf",creditactionBttn(infoId="geological_cs_surf_info",color="default",c_label="Surface Geology",
                                                                                             icon=img(src = "GSI_logo.jpg", height = 18, width = 18)),
                                               multiple=F,
                                               selected="Geomap",
                                               choices=c("Geomap"="geomap",
                                                         "Geomap With Free Colors"="geomap_free_colors",
                                                         "Blind"="blind"),
                                   ),
                                   # Measurement Year ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput(inputId="measurement_year",label =  msgactionBttn(infoId="measurement_year_info",color="default",c_label="Measurement Year:"),
                                                min = 1970, max = 2020, value = 2018,step=1),
                                   # Season ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput(inputId="season",label =  msgactionBttn(infoId="season_info",color="default",c_label="Measurement Season:"),
                                               multiple=F,
                                               selected="",
                                               choices=c("","Summer-Autumn","Winter-Spring"),
                                   ),
                                   # Resolution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Resolution",  msgactionBttn(infoId="Resolution_info",color="default",c_label="Horizontal Grid Resolution:"),
                                                min = 10, max = 1000, value = 50,step=10),
                                   # vertical Resolution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("vertical_resolution",  msgactionBttn(infoId="vertical_resolution_info",color="default",c_label="Vertical Grid Resolution:"),
                                                min = 1, max = 500, value = 50,step=5),
                                   # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   textInput("title_CS",  msgactionBttn(infoId="title_CS_info",color="default",c_label="Title:"),value="Test CS"),
                                   # ID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("cs_id",  msgactionBttn(infoId="cs_id_info",color="default",c_label="Cross Section ID:"),
                                               multiple=F,
                                               selected="A",
                                               choices=cs_ids$cs_id,
                                   ),
                                   # Transforms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   #checkboxInput("transforms","Transforms:",value = T),
                                   # Projection Line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   checkboxInput("Projection_Line", msgactionBttn(infoId="Projection_Line_info",color="default",c_label="Match wells to line:"),value = F),
                                   # label_size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("label_size",  msgactionBttn(infoId="label_size_info",color="default",c_label="Label Size:"),
                                                min = 10, max = 90, value = 50,step=10),
                                   # Interval_between_labeles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Interval_between_labeles",  msgactionBttn(infoId="Interval_between_labeles_info",color="default",c_label="Labeles Interval & Upper Limit:"),
                                                min = 50, max = 400, value = 200,step=10),
                                   # Lower Limit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Lower_Limit",  msgactionBttn(infoId="Lower_Limit_info",color="default",c_label="Lower Limit:"),
                                                min = -4000, max = 0, value = NA,step=100), 
                                   # Surface Smooth ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Surface_Smooth",  msgactionBttn(infoId="Surface_Smooth_info",color="default",c_label="Surface Smooth:"),
                                                min = 0, max = 2, value = 0,step=0.1),
                                   # DEM Line Thickness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("DEM_line_thickness",  msgactionBttn(infoId="DEM_line_thickness_info",color="default",c_label="DEM Line Thickness:"),
                                                min = 0, max = 2, value = 1,step=0.1)#,
                                   
                            ),
                            # Slider Panel for Preview
                            column(9,offset=0,
                                   shinyjs::useShinyjs(),
                                   textOutput("cs_messages"),
                                   plotOutput("cs_chart",height = "1200px",width = "2140px"),
                                   fluidRow(downloadButton('cs_download_ppt', 'Download Editable'),
                                            downloadButton('cs_download_html', 'Download Interactive')
                                   )
                                   
                            )
                          ),
                 ),
                 tabPanel("Build Geology Model",icon = icon("accusoft"),
                          fluidPage(
                            
                            column(width = 2,
                                   # Classifier - Point Size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("hpoint_size",  msgactionBttn(infoId="hpoint_size_info",color="primary",c_label="Point Size:"),
                                                min = 1, max = 10, value = 1,step=1),
                                   # Classifier - add Values Between points by interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("h_int", msgactionBttn(infoId="h_int_info",color="primary",c_label="Join Points:"),
                                               multiple=F,
                                               choices=c("linear","spline","NaN"),
                                               selected="linear"),
                                   # Classifier - Set the density of the interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("h_res",  msgactionBttn(infoId="h_res_info",color="primary",c_label="Join Density [m]:"),
                                                min = 0, max = 1000, value = 10,step=1),
                                   # Classifier - Select what to class ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("Select_horizon_by", msgactionBttn(infoId="Select_horizon_by_info",color="primary",c_label="Classifer:"),
                                               multiple=F,
                                               choices=c("Wells","Hydrogeology libraries"),
                                               selected="Wells"),
                                   # Classifier - Select Horizon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("Select_horizon", msgactionBttn(infoId="Select_horizon_info",color="primary",c_label="Select horizon:"),
                                               multiple=F,
                                               choices=INDEX_DEMs$f_name),
                                   # Classifier - Eraser sensitivity [m]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Eraser_sensitivity", msgactionBttn(infoId="Eraser_sensitivity_info",color="primary",c_label="Eraser sensitivity [m]:"),
                                                min = 0, max = 1000, value = 10,step=5),
                                   fluidRow(
                                     # Classifier - Create /Add to Horizon DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     actionButton(inputId = "add2hdb",
                                                  icon = icon("database"),
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                  width = "204px",
                                                  label = HTML("<span style='font-size:1.3em;'><br />Create/Add to DB</span>")
                                     )
                                   ),
                                   # Classifier - Download Horizon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   fluidRow(
                                     downloadButton("download_horizon",
                                                    icon = icon("accusoft"),
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                    width = "100px",
                                                    label = HTML("<span style='font-size:1.3em;'><br />Download Horizons</span>")
                                     )  
                                   )
                                   
                            ),
                            # Slider Panel for Edit Horizon
                            column(9,
                                   plotOutput("cs_tagging",height = "680px",width = "2140px",click = "plot_click"),
                                   DT::dataTableOutput("info")
                            )
                            
                          )
                 )
      )
      ,
      class="span7")
  )
)

# Server ======================================================================================
server <- function(input, output, session) {
  # Load Info System ---------------------------------------------------------------------------
  observeEvent(input$info,once = F,priority=1, handlerExpr={
    shinyalert(
      title = "Welcome to CSMS<sup>®</sup> Software",
      text = "<h4 style='text-align:justify;'>This software is the interrogation system of the National Geohydrology Database of Israel.</h4>
            <p style='text-align:justify;'><b>The Geohydrology National Database</b> includes geological data from 9,000 groundwater wells, geotechnical wells, oil & gas wells. About 70 geological spatial layers that maps the subsoil at a national and regional level and about 120 hydrological layers that maps the groundwater and salinity levels in the various aquifers in Israel.</p>.
            <p style='text-align:justify;'><b>CSMS software</b> allows examination and presentation of the data, this by creation of standard Cross Sction along lines defined by the user. The information present by geological and hydrogeological cross sections. The sections can be exported for further editing and updating as Power Point files (<a href='https://csexamp.s3.us-east-2.amazonaws.com/Editable_CS-Exmp_V2.pptx'>see example</a>) or HTML files (<a href='https://csexamp.s3.us-east-2.amazonaws.com/Interactive_CS_exmp_V3.html'>see example</a>)</p>.
            <p style='text-align:justify;'><em><b>Until 01.01.2022, the software is in beta version and free to use without the need to enter a password</b></em>. Fore initial training, and to obtain sample files for building a personal database, please contact <b>CRS hydrology</b> by email: <a href='mailto:CRShydrology@gmail.com'>CRShydrology@gmail.com</a>.</p>.
            <p style='text-align:justify;'>However, in order to gain access to the Geohydrology National Database, please contact the <b>Hydrogeology Division of the Water Authority</b> with a data request fee by email: <a href='mailto:YakovL20@water.gov.ill'>YakovL20@water.gov.il</a> </p>.
            <h4 style='text-align:center;'>Clicking OK constitutes acceptance of the <a href='https://csexamp.s3.us-east-2.amazonaws.com/terms-of-use-CSMS_V1.pdf'>terms of use</a> of the software </h4>",
      closeOnEsc = F,
      closeOnClickOutside = F,
      html = T,
      type = "info",
      showConfirmButton = T,
      showCancelButton = F,
      confirmButtonText = "OK",
      confirmButtonCol = "#1274a1",
      timer = 0,
      imageUrl = "",
      animation = T,
      immediate=T
    )})
  message("Load Info System")
  # Galery Slider ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$slickr <- renderSlickR({
    imgs_df <- list.files("www/", pattern=".png", full.names = TRUE) %>% as.data.frame() %>%
      dplyr::rename(.,"filepth"=".") %>% 
      mutate(Id=as.numeric(gsub('www/app_animation_|.png', '',filepth))) %>% 
      dplyr::arrange(.,Id)
    
    slickR(imgs_df$filepth)+ settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 1200, speed=5000)
  })
  
  
  # Argument Info System ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$geomap_info,once = F, handlerExpr={msgalart("Geology maps grid","<p style='text-align:left'>You can choose between two grids:
                                                                 <p style='text-align:left'>1.<b>Low-resolution</b> grid of 1:200,000 square meters.</p>
                                                                 <p style='text-align:left'>2.<b>High resolution grid</b> of 1:50,000 square meters.</p>
                                                                 <p style='text-align:left'>The low-resolution grid allows you to load up to 6,000 boreholls and up to 2 geological tiles in each view. The high-resolution grid allows you to load up to 15 boreholls at most and load only one geologycal tile per view.</p>")})
  observeEvent(input$country_info,once = F, handlerExpr={msgalart("Country","<p style='text-align:justify;'>Country Selection defines the coordinate system as a local (metric) system and displays background layers belonging to the selected country.</p>")})
  observeEvent(input$Password_info,once = F, handlerExpr={msgalart("Password","<p style='text-align:justify;'>In order to gain access to the Geohydrology National Database, please contact the <b>Hydrogeology Division of the Water Authority</b> with a data request fee by email: <a href='mailto:YakovL20@water.gov.il'>YakovL20@water.gov.il</a> </p>")})
  observeEvent(input$additional_layers_type_info,once = F, handlerExpr={msgalart("Additional Layers","<p style='text-align:justify;'>Additional layers can be loaded for display on the geoloy model. Also, using this parameter you can override general parameters provided by the software database, such as the Height Model (DTM) in local databases. The layer type must be set using the <b>Additional layer type</b> parameter. Full details on the type of layers that can be displayed appear when the layer type is selected. </p>")})
  
  observeEvent(input$Refresh_info,once = F, handlerExpr={msgalart("Refresh wells data base","<p style='text-align:justify;'>Access to the database can be obtained by requesting data from the <b>Hydrogeology Division of the Water Authority</b>by email: <a href='mailto:YakovL20@water.gov.il'>YakovL20@water.gov.il</a>. It is also possible to build a database independently that includes wells that do not exist in the <b>National Database</b>, an example of the database can be downloaded at the <a href='https://csexamp.s3.us-east-2.amazonaws.com/Geology_Description_Examp.csv'>link</a>. The meta-data file appears in the <a href='https://csexamp.s3.us-east-2.amazonaws.com/Geology_Description_Metadata.csv'>link</a>. For further explanations and coding system files, please contact us by email: <a href='mailto:CRShydrology@gmail.com'>CRShydrology@gmail.com</a>. </p>")})
  observeEvent(input$Gridfile_info,once = F, handlerExpr={msgalart("Upload Grid of Cross Sections - Inactive","<p style='text-align:justify;'>Dev Database</a>. </p>")})
  
  observeEvent(input$Background_info,once = F, handlerExpr={msgalart("Active only for authorized users after entering a password","Hydrogeology libraries contain spatial, dynamic and static, data in each of the listed areas. The data displayed on the cross section by dott-lines (<a href='https://csexamp.s3.us-east-2.amazonaws.com/Background_info-Exmp_V1.pptx'>see example</a>).")})
  observeEvent(input$Build_Solids_info,once = F, handlerExpr={msgalart("Active only After selection of hydrogeology library and active only for a single library","Solids are representation of two-dimensional surfaces between the layers in the subsoil according to the order of the layers")})
  observeEvent(input$CS_type_info,once = F, handlerExpr={msgalart("Cross Section Type",
                                                                  "<p style='text-align:left'>The Cross section can be represented in three levels:</p>
                                                                          <p style='text-align:left'>1. Geological groups such as <i>'Jaffa'</i>, <i>'Mount Scopus'</i> and <i>'Judea'</i>.</p>
                                                                          <p style='text-align:left'>2. Geological formations such as <i>'Bina'</i>, <i>'Kfar Shaul'</i> and <i>'Moza'</i>.</p>
                                                                          <p style='text-align:left'>3. Lithological composition of the rock along the log, such as <i>'sand'<i>, </i>'claly sand'</i>, <i>'limestone'</i>, etc.</p>
                                                                          <p style='text-align:left'>The overall list of categories appears in the <a href='https://csexamp.s3.us-east-2.amazonaws.com/CS_type_info-Exmp_V1.xlsx'>following link</a>.</p>")})
  observeEvent(input$geological_cs_surf_info,once = F, handlerExpr={msgalart("Surface geology along the Cross Section","<p style='text-align:left'>With this parameter the surface geology along the section can be obtained on the basis of 1: 50,000 maps and 1: 200,000 maps. When there is no high-resolution data, a free color palette can be used to highlight the differences between the Formations")})
  observeEvent(input$cs_id_info,once = F, handlerExpr={msgalart("Cross Section ID","The Cross section Identified by tags marked with <b>X</b> and <b>X'</b> at the beginning and end of the cross-section (respectively). It is customary to display the cross-section from <b>A</b> in alphabetical order until the last cross-sections in the study area</p>")})
  observeEvent(input$title_CS_info,once = F, handlerExpr={msgalart("Cross Section Title","The Cross-Section title is defined in this parameter, usually while specifying the study area and the direction of the section, for example: <i><b>'Depth section across the western mountain aquifer from northwest to southeast'</i></b>.")})
  observeEvent(input$season_info,once = F, handlerExpr={msgalart("Season","<p style='text-align:left'>Water-Levels & Water-Quality (in this version salinity only) are presented according to the following division:</p>
                                                                          <p style='text-align:left'>1. Last value - when the argument is not marked.</p>
                                                                          <p style='text-align:left'>2. Last value for months 1-5 when <b>Winter-Spring</b> option selected.</p>
                                                                          <p style='text-align:left'>3. Last value for months 6-12 when <b>Summer-Autumn</b> option selected.</p>")})
  observeEvent(input$measurement_year_info,once = F, handlerExpr={msgalart("Measurement Year","<p style='text-align:left'>Water-Levels & Water-Quality (in this version salinity only) are presented according to the following division:</p>
                                                                          <p style='text-align:left'>1. Last value - when the argument is not marked.</p>
                                                                          <p style='text-align:left'>2. Last value for the selected year.</p>")})
  observeEvent(input$Projection_Line_info,once = F, handlerExpr={msgalart("Projection Line (Match wells to line)","In selecting of this parameter the elevations of the well (including the geological units and the perforations) will be attached to the surface. If the projection creates a significant distortion (over then 10 m' diffrance of the elevation at distance of less than 100 m from the cross section), the text box at the  bottom of the well log will be painted to red.")})
  observeEvent(input$label_size_info,once = F, handlerExpr={msgalart("Label Size","This parameter controls the size of all the alphanumeric components in the Cross-Section such as the axis titles, the caption on the axis, the titles on the logs in each well, and so on.")})
  observeEvent(input$Interval_between_labeles_info,once = F, handlerExpr={msgalart("Interval Between Labeles","This parameter controls the vertical distance between all the alphanumeric components in the Cross-section such as the axis titles, the caption on the axes, the titles on the logs and so on. It is recommended to use about <b>200-300</b> in Cross_Section in a mountainous area and <b>50-100</b> in coastal area.")})
  observeEvent(input$Lower_Limit_info,once = F, handlerExpr={msgalart("Lower Limit","This parameter controls the base of the Cross-Section (at a metric height above sea level) Any Well that crosses the base height is marked with the word <b>'cut'</b> in diagonal writing at the base of the log.")})
  observeEvent(input$Surface_Smooth_info,once = F, handlerExpr={msgalart("Surface Smooth","This parameter controls the level of smoothness of the spatial data (surface and other subterranean spatial data such as geological formations). This parameter is <b>especially important when the cross-section does not pass in a straight line</b> and a gap is created between the point information and spatial information as can be seen in <a href='https://csexamp.s3.us-east-2.amazonaws.com/Surface_Smooth_info-Exmp_V1.pptx'>the example</a>.")})
  observeEvent(input$Buffer_info,once = F, handlerExpr={msgalart("Buffer width","By this paramter you can to set the search zone around of wells around the cross section. <b>Changing the parameter changes the search area even without the need to run the CS again.</b>")})
  observeEvent(input$Resolution_info,once = F, handlerExpr={msgalart("Resolution","By this paramter you can to set the resoloution in Y axis.")})
  observeEvent(input$vertical_resolution_info,once = F, handlerExpr={msgalart("vertical Resolution","By this paramter you can to set the resoloution in X axis.")})
  observeEvent(input$DEM_line_thickness_info,once = F, handlerExpr={msgalart("DEM Line Thickness","By this paramter you can to set the thickness of all spatial layers in the Cross-Section.")})
  observeEvent(input$Update_points_info,once = F, handlerExpr={msgalart("Update Points","Show the new horizon points on the  cross section")})
  observeEvent(input$Select_horizon_info,once = F, handlerExpr={msgalart("Select Geology or hydrology horizon","Select a horizon from one of the horizons shown in the section and rebuild it.")})
  observeEvent(input$Select_horizon_by_info,once = F, handlerExpr={msgalart("Select the data source for the classification","Classification can be based either on existing data from existing hydrogeological libraries or on data that exists in wells and does not exist in existing hydrogeological libraries.")})
  observeEvent(input$Eraser_sensitivity_info,once = F, handlerExpr={msgalart("Erase Point form the cross section","With this parameter it is possible to control the number of points to erase. Choose a <b>larger</b> distance to delete more points at once.")})
  observeEvent(input$hpoint_size_info,once = F, handlerExpr={msgalart("Set horizon points size","Use this variable to determine the size of the point on the cross section.")})
  observeEvent(input$h_int_info,once = F, handlerExpr={msgalart("Enable interpolation between points along the horizon","Use this variable to select whether, and what type of interpolation to use to connect points along the horizon.")})
  observeEvent(input$h_res_info,once = F, handlerExpr={msgalart("Set the resolution of the interpolation between points along the horizon","Use this variable to set the resolution of the interpolation.")})
  
  # Open login ----------------------------------------------------------------------------------------------
  message("Open login")
  geo_v=reactive({input$geomap})
  mode_v=reactive({input$PW})
  db_v=reactive({input$file1})
  hz_v=reactive({input$file2})
  output$loginUI=renderUI({
    if(as.character(mode_v())=="" & (as.character(db_v())=="" | as.character(hz_v())=="")){
      actionButton("login", 
                   icon = icon("unlock"),
                   style="color: #fff; background-color: '#337ab7'; border-color: #2e6da4",
                   width = "65px",
                   label = HTML("<span style='font-size:1.3em 'text-align:justify'><br />Login</span>")
      )
    }
    if(as.character(mode_v())=="" & as.character(db_v())!=""){dgree='#337ab7'}
    if(as.character(mode_v())!="" & as.character(db_v())!=""){dgree='#337ab7'}
  })
  
  
  # Load Geohydrology librarys ------------------------------------------------------------------------------
  message("Load Geohydrology librarys")
  
  output$modelUI=renderUI({
    # UI For registered user ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(as.character(mode_v()) %in% Password_df$Password){
      verticalLayout(
        useShinyalert(),
        selectInput(inputId="Background",label = msgactionBttn(infoId="Background_info",color="default",c_label="Subsurface Librarys: "),
                    multiple=T,
                    choices=c("National"="national",
                              "Coastal"="coastal",
                              "Westren Mt."="westren",
                              "Eastren Mt."="eastren",
                              "Shkhem-Tubas"="Shkhem_Tubas",
                              "Negev-Arava"="negev_arava",
                              "Arava-Sinai"="Arava_Sinai",
                              #"Western Galilee"="western_galilee",
                              "Golan-Kinneret"="golan_knneret",
                              "Hermon-Hula"="hermon_hula",
                              "Carmel"="carmel",
                              "Jerusalem"="jerusalem"),
                    selected = "national",)
      )}
    # UI For Public users ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else{
      verticalLayout(
        useShinyalert(),
        selectInput(inputId="Background",label = msgactionBttn(infoId="Background_info",color="warning",c_label="Subsurface libraries: "),
                    multiple=T,
                    choices=c("National"="national"),
                    selected = "national",))}
  }) # Close Mode
  # Load Base Map ------------------------------------------------------------------------------
  message("Load Base Map")
  output$mainmap <- renderLeaflet({
    leaflet() %>% 
      setView(lng=35.2,lat=32.55,zoom=10) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  # Get Wells DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data <- reactive({ 
    req(input$file1) 
    inFile=input$file1 
    df=fread(inFile$datapath,,
             colClasses=c(materials="character",formations="character",groups="character")) %>% 
      dplyr::group_by(name) %>% 
      mutate(max_depth=max(bot_layer,na.rm = T)) %>%  as.data.table(.)
    return(df)
  })
  
  # Load Wells -----------------------------------------------------------------------------
  observeEvent((input$login | !is.null(input$file1)), {
    # Clean exist data
    Virtual_dt<<-Virtual_int
    charts<<-NULL
    horizons_db<<-NULL
    {
      if(!is.null(input$file1)){
        message("Rander Wells Map")
        # Build Initial Wells map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Geology_Description_ss =data() %>% mutate(well_id=as.character(shd_id)) %>% 
          group_by(well_id) %>%
          mutate(max_depth=max(bot_layer,na.rm = T)) %>% 
          subset(.,,c("well_id","name","name_he","Longitude","Latitude","elv","max_depth",
                      "materials","formations","groups","top_layer","bot_layer","LEVEL_DES","LEVEL_DES_CD","AQUIFER_CD","SOURCE_DES"))%>% 
          as.data.table(.,key="well_id")
        
        CS_model_system = unique(Geology_Description_ss,by="well_id") 
        
        # Set Spatial layer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        if(input$country=="Indefinite" | max(CS_model_system$Longitude)<100){
          CS_model_system_sf = st_as_sf(CS_model_system, coords = c("Longitude", "Latitude"), crs =4326,remove=F)
        } else {
          crs_id=subset(localtiles_df,country==input$country,crs)
          CS_model_system_sf<-st_as_sf(CS_model_system,
                                       coords = c("Longitude", "Latitude"), crs =as.numeric(crs_id),remove=T) %>% 
            st_transform(.,crs =4326)
          ll_df=as_tibble(st_coordinates(CS_model_system_sf))
          CS_model_system_sf$Longitude=ll_df$X
          CS_model_system_sf$Latitude=ll_df$Y
          CS_model_system<-st_drop_geometry(CS_model_system_sf)
          Geology_Description_ss<-dplyr::select(Geology_Description_ss,-c("Longitude", "Latitude")) %>% 
            left_join(.,subset(CS_model_system,,c("well_id","Longitude","Latitude")))
          
          messeges_str="The coordinate system has been converted from a local projection to an international projection.
                        There may be a deviation of few meters of the wells location."
          showModal(modalDialog(
            title = "Projection seting: ",
            messeges_str,
            easyClose = TRUE,
            footer = NULL
          ))
          
        }
        # Set DB's to Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assign("Geology_Description_ss",Geology_Description_ss,envir = .GlobalEnv)
        assign("CS_model_system_sf",CS_model_system_sf,envir = .GlobalEnv)
        assign("CS_model_system",CS_model_system,envir = .GlobalEnv)
        
        # Get type of Geological grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
        if(as.character(geo_v())=="Regional-Low"){geogrid$resoid=200}
        
        output$mainmap <- renderLeaflet({
          loc=as.data.frame(st_coordinates(st_centroid(st_union(CS_model_system_sf))))
          
          # Load Initial Wells map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          viewdif=c(lng=loc$X+0.1,lat=loc$Y-0.4,zoom=10.2)
          if(input$country=="Indefinite"){
            wellsmap(CS_model_system,geogrid,transforms_shp,viewdif)
          } else {
            localtiles_dfs=subset(localtiles_df,country==input$country,)
            wellsmap_n(CS_model_system,geogrid,transforms_shp,viewdif,localtiles_dfs)
          }
          
        }) 
      }
    } 
    observeEvent(input$Background,{
      req(input$Background!="national")
      # Get Active Libraries Dimensions -------------------------------------------------
      library_dms_act=dplyr::filter(library_dms,basin %in% input$Background) %>% 
        dplyr::filter(.,basin != "national")
      proxy_mainmap=leafletProxy(
        mapId = "mainmap",
        session = session
      ) %>%
        clearGroup(group="libraris") %>% 
        addPolygons(data=library_dms_act,
                    color= "black",
                    fillColor= "gray",
                    label = NULL,
                    fill=F,
                    weight = 6,
                    fillOpacity =0.2,
                    smoothFactor = 3,
                    group="libraris") # %>%
      # addLabelOnlyMarkers(
      #   data=CS_model_system,
      #   labelOptions=labelOptions(textsize = "30px",
      #                             sticky=T),
      #   layerId=NULL,
      #   lat=~Latitude,
      #   lng=~Longitude,
      #   label=~name,
      #   group = "welnames")
      # 
    })
    
    # Show wells labeles in noHide mode ---------------------------------------------------
    observeEvent(input$mainmap_zoom,{
      req(isTRUE(exists("CS_model_system")))
      # print(input$mainmap_zoom)
      # print(paste0("zoom_old:",zoom_old))
      req((input$mainmap_zoom>=14.5 & zoom_old<14.5) | # Drill in
            (input$mainmap_zoom<14.5 & zoom_old>=14.5)   # Drill out
      )
      zoom_old<<-input$mainmap_zoom
      if (input$mainmap_zoom>=14.5){
        message("no Hide names")
        proxy_mainmap=leafletProxy(
          mapId = "mainmap",
          session = session
        ) %>%
          clearGroup(group="welnames") %>% 
          addLabelOnlyMarkers(
            data=CS_model_system,
            labelOptions=labelOptions(textsize = "15px",
                                      opacity=0.5,
                                      noHide = T,
                                      sticky=T),
            layerId=NULL,
            lat=~Latitude,
            lng=~Longitude,
            label=~name,
            group = "welnames")
      }
      if (input$mainmap_zoom<14.5){
        message("Back to normal view")
        proxy_mainmap=leafletProxy(
          mapId = "mainmap",
          session = session
        ) %>%
          clearGroup(group="welnames") %>% 
          addLabelOnlyMarkers(
            data=CS_model_system,
            labelOptions=labelOptions(textsize = "30px",
                                      noHide = F,
                                      sticky=T),
            layerId=NULL,
            lat=~Latitude,
            lng=~Longitude,
            label=~name,
            group = "welnames")
      }
      
    })
  }) # Close Login
  
  
  # Build Geology Selected Layer ------------------------------------------------------------------
  observeEvent(input$mainmap_draw_new_feature, {
    
    feature_type = input$mainmap_draw_stop$shape
    
    # Select by Polygon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(feature_type %in% c("Rectangle","Poly")) {
      message("Select By Polygon")
      #get the coordinates of the polygon
      polygon_coordinates=input$mainmap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon=Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      #drawn_polygon_sp=SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      drawn_polygon_st=st_as_sf(SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))) %>% st_set_crs(.,4326)
      selected_gotiles=st_intersection(geogrid,drawn_polygon_st)
      
      # use intersect to identify selected items
      #selected_gotiles=raster::intersect(sf::as_Spatial(st_zm(geogrid, drop = TRUE, what = "ZM")),drawn_polygon_sp) # st_intersection(geogrid,st_as_sf(drawn_polygon_sp)) #
      # Check the selection 
      maxtiles=ifelse(input$geomap=="Regional-Low",36,4)
      if(nrow(selected_gotiles)<maxtiles){
        # Define selected tiles
        if(input$geomap == "Regional-Low"){
          selected_gotiles$resoid=200
          geology_200_fltr=dplyr::filter(geology_200,geoid %in% selected_gotiles$geoid)
          geology_50_fltr=geology_50[0,]
        } else {
          selected_gotiles_200=dplyr::filter(selected_gotiles,resoid==200)$geoid
          geology_200_fltr=dplyr::filter(geology_200,geoid %in% dplyr::filter(selected_gotiles,resoid==200)$geoid)
          geology_50_fltr=dplyr::filter(geology_50,geoid %in% dplyr::filter(selected_gotiles,resoid==50)$geoid)
        }
        
        # Build Proxy Layers for empty selection
        if(NROW(geology_200_fltr)==0){geology_200_fltr=geology_200_proxy}
        if(NROW(geology_50_fltr)==0){geology_50_fltr=geology_50_proxy}
        
        # Set base proxy map
        proxy_basemap=leafletProxy(
          mapId = "mainmap",
          session = session
        ) 
        
        proxy_mainmap=proxy_basemap %>% 
          clearGroup(group="geology_tiles") %>% 
          addPolygons(data=geology_200_fltr,
                      color= ~geopal_200(Code),
                      fillColor= ~geopal_200(Code),
                      label = ~Name_Eng,
                      fill = T,
                      weight = 0,
                      fillOpacity = 0.7,
                      smoothFactor = 1,
                      group="geology_tiles") %>%
          clearGroup(group="welnames") %>% 
          addLabelOnlyMarkers(
            data=CS_model_system,
            labelOptions=labelOptions(textsize = "15px",
                                      opacity=0.5,
                                      noHide = F,
                                      sticky=T),
            layerId=NULL,
            lat=~Latitude,
            lng=~Longitude,
            label=~name,
            group = "welnames") 
        
        
        # # Render View
        # proxy_basemap=leafletProxy(
        #   mapId = "mainmap",
        #   session = session,
        # ) %>%
        #   # clearGroup(group="geology") %>% 
        #   addPolygons(data=geology_200_fltr,
        #               color= ~geopal_200(Code),
        #               fillColor= ~geopal_200(Code),
        #               label = ~Name_Eng,
        #               fill=T,
        #               weight = 0,
        #               fillOpacity = 0.7,
        #               smoothFactor = 1,
        #               group = "geology" # ,
        #               # options = pathOptions(pane = "geoview")
        #   ) %>%
        #   addPolygons(data=geology_50_fltr,
        #               color= ~geopal_50(Code),
        #               fillColor= ~geopal_50(Code),
        #               label = ~Name_Eng,
        #               fill=T,
        #               weight = 0,
        #               fillOpacity = 0.7,
        #               smoothFactor = 1,
        #               group = "geology" # ,
        #               # options = pathOptions(pane = "geoview")
        #   )   
        # 
      } else {
        showModal(
          modalDialog(
            title = "There seems to be some issues with your selection: ",
            "Error: You have selected too many geological tiles, select up to 36 ''Regional-Low'' tiles or up to 4 ''Local-high'' tiles",
            easyClose = T,
            size="l",
            footer = NULL
          )
        )  
      }
    }
    
    
  })
  
  
  # Additional layers ----------------------------------------------------------------------
  # Set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$additional_layers_type,{
    req(str_count(input$additional_layers_type)>1) 
    msg_s=as.character(subset(additional_layers_df,type==input$additional_layers_type,mssg))
    showModal(
      modalDialog(
        title = "Layer settings: ",
        msg_s,
        easyClose = T,
        size="l",
        footer = NULL
      )
    )
  })
  # Load ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$additional_layers,{
    # Get files
    req(input$additional_layers)
    req(str_count(input$additional_layers_type)>1) 
    inFile=input$additional_layers
    # Set base proxy map
    proxy_basemap=leafletProxy(
      mapId = "mainmap",
      session = session
    )
    # Digital Terrain Model (raster)
    if(inFile$type=="image/tiff" & input$additional_layers_type=="Digital Terrain Model (raster)"){
      DTM_rst=raster(inFile$datapath)
      if(as.character(crs(DTM_rst))!="+proj=longlat +datum=WGS84 +no_defs"){
        DTM_rst=projectRaster(DTM_rst, crs = 4326)
      }
      additional_layers_lst$DTM_rst=DTM_rst
      additional_layers_lst<<-additional_layers_lst
    }
    # Digital Terrain Model (contours)
    else if (any(str_detect(inFile$name,".shp")) & input$additional_layers_type=="Digital Terrain Model (contours)") {
      shp_path <- reactive({input$additional_layers})
      user_shp <- Read_Shapefile(shp_path)
      user_shp=user_shp() %>% st_transform(.,crs=4326)
      colnames(user_shp)<-c("elv","geometry")
      proxy_mainmap=add_element(main_map=proxy_basemap,
                                ad_lyr=user_shp,
                                type=input$additional_layers_type)
      # Build DTM raster (Inactive)
      showModal(
        modalDialog(
          title = "Layer settings: ",
          "Inactive",
          easyClose = T,
          size="l",
          footer = NULL
        )
      )
    }
  })
  
  # Select Wells & Faluts List  ------------------------------------------------------------
  data_slc=reactive({
    message("Select Wells & Faluts List")
    # use the draw_stop event to detect when users finished drawing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    req(input$mainmap_draw_stop)
    feature_type <- input$mainmap_draw_stop$shape
    
    # Select By Line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(feature_type=="Line") {
      message("Select By Line")
      # Get the coordinates of the polyline
      print("get the coordinates of the polygon")
      polyline_coordinates <- input$mainmap_draw_new_feature$geometry$coordinates
      polyline_coordinates_mt <- Reduce(rbind,polyline_coordinates)
      polyline_coordinates_df <-as.matrix.data.frame(polyline_coordinates_mt) 
      # Fix Orientation
      polyline_coordinates_tbl=as.data.frame(polyline_coordinates_df) %>% 
        rownames_to_column(.) %>% dplyr::arrange(desc(rowname)) %>% 
        dplyr::select(-rowname) %>% 
        as.matrix.data.frame(.)
      
      # Transform them to an sp 
      print("transform them to an sp polylin")
      drawn_polyline <- Orcs::coords2Lines(polyline_coordinates_tbl, ID = "A")
      # Transform them to an sf 
      drawn_polyline_st=st_as_sf(drawn_polyline)
      st_crs(drawn_polyline_st)=4326
      
      # Selected wells
      print("Build Input Parameters to th CS Model")
      drawn_polygon = st_buffer(drawn_polyline_st, dist=input$Buffer*1000)
      CS_wells_coordinates=st_as_sf(CS_model_system,coords = c("Longitude","Latitude"),crs=4326)
      CS_model_system_slc=setDF(st_drop_geometry(st_intersection(CS_wells_coordinates,drawn_polygon)))
      Geology_Descriptions_slc=Geology_Description_ss[Geology_Description_ss$well_id %in% CS_model_system_slc$well_id, ]  
      
      # selected Faults
      transforms_st=st_as_sf(transforms_shp) ;st_crs(transforms_st)=4326
      junc_points=(st_intersection(drawn_polyline_st,transforms_st))
      if(nrow(junc_points)>0){
        # Split fault with several meeting points with the CS line       
        for (i in 1:nrow(junc_points)){
          junc_points$type[i]=class(junc_points$geometry[i])
        }
        junc_points_multi=dplyr::filter(junc_points,type=="sfc_MULTIPOINT")
        junc_points_pnt=dplyr::filter(junc_points,type!="sfc_MULTIPOINT")
        if(nrow(junc_points_multi)>0){
          junc_points_pnt2=st_cast(junc_points_multi,"POINT")
          junc_points_all=bind_rows(junc_points_pnt,junc_points_pnt2)
        } else {
          junc_points_all=junc_points_pnt
        }
        transforms_fltr=as.data.frame(st_coordinates(junc_points_all))[,1:2] 
      } else{
        transforms_fltr=transforms_st[0,]
      }
      
      if(nrow(transforms_fltr)>0){
        colnames(transforms_fltr)=  c("Longitude", "Latitude")
      } else{print("No active Faults")}
      
      # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      write.csv(Geology_Descriptions_slc, paste0(Background_path,'/Apps/External_Data/Geology_Descriptions_slc.csv'))  #  Local Test file
      write.csv(transforms_fltr, paste0(Background_path,'/Apps/External_Data/transforms.csv'),row.names = F)
      st_write(st_as_sf(drawn_polyline), paste0(Background_path,'/Apps/External_Data/drawn_polyline.csv'),layer_options = "GEOMETRY=AS_WKT",delete_dsn=TRUE)
      # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      # print to Tab the name of the CS_wells
      print("print to Tab the name of the CS_wells")
      CS_model_system_ss=subset(CS_model_system_slc,,c("name","max_depth","LEVEL_DES","SOURCE_DES","AQUIFER_CD"))
      colnames(CS_model_system_ss)=c("Name","Depth [m]","Descriptions","Source","Aquifer")
      
      if(nrow(CS_model_system_ss)<2 & NROW(Virtual_dt)==1){
        showModal(
          modalDialog(
            title = "Wrong point selection: ",
            "Error: It is not possible to run the cross-section since less than two wells or virtual points have been selected along the cross section.",
            easyClose = T,
            size="l",
            footer = NULL
          )
        )
        modeldialog_status="Active"
      }
      # Populate Results List
      tablet_filter<<-"inactive"
      print("Populate Results List")
      slc_lst=list("Geology_Descriptions_slc"=Geology_Descriptions_slc,"transforms_fltr"=transforms_fltr,"CS_model_system_ss"=CS_model_system_ss,"drawn_polyline"=st_as_sf(drawn_polyline))
      return(slc_lst)
    }
  })
  # Render Wells Data Table ----------------------------------------------------------------
  message("Render Wells Data Table")
  output$CS_model_system <- renderDT(
    data_slc()[["CS_model_system_ss"]],
    filter = "top"
  )
  
  observeEvent(input$CS_model_system_search_columns,{
    req(!is.null(input$CS_model_system_rows_current))
    tablet_filter<<-"active"
  })
  
  # Create Virtual Wells ----------------------------------------------------------------
  Virtual_cre=observeEvent(input$mainmap_draw_new_feature,{
    
    feature_type <- input$mainmap_draw_new_feature$geometry$type
    
    if(feature_type=="Point") {
      message("Create Virtual Wells")
      # Build Virtual point
      Virtual_dt_i <-tibble(Longitude=as.numeric(input$mainmap_draw_new_feature$geometry$coordinates[1]),
                            Latitude=as.numeric(input$mainmap_draw_new_feature$geometry$coordinates[2])) %>%
        mutate(well_id=as.character(-999+nrow(Virtual_dt)),
               name=paste0("Virtual Point - ",nrow(Virtual_dt)),
               name_he=paste0("Virtual Point - ",nrow(Virtual_dt)),
               materials=-999,
               formations=-999,
               groups=-999,
               top_layer=0,
               bot_layer=50,
               max_depth=999,
               SOURCE_DES=NA,
               LEVEL_DES=NA,
               AQUIFER_CD=NA)
      
      # Get markers elevation
      dem_pth = "data/DEMs"
      DTM_rst=raster(paste0(dem_pth,"/DTM.tif"))
      Virtual_sp_i = SpatialPointsDataFrame(coords = subset(Virtual_dt_i,,c(Longitude,Latitude)), data = Virtual_dt_i,proj4string = CRS("+proj=longlat +datum=WGS84"))
      elevations = raster::extract(DTM_rst,Virtual_sp_i,cellnumbers=TRUE,sp=TRUE,along=T)
      Virtual_sp_i$elv=elevations$DTM
      # Load to Virtual data frame
      Virtual_dt<<-rbind(Virtual_dt,subset(as.data.table(Virtual_sp_i),,names(Virtual_dt)))
      print(Virtual_dt)
      return(Virtual_dt)
    }
  })
  
  
  # Run Cross Section Model ###############################################################################
  observeEvent(input$Get,
               {if(!is.null(input$file1) & is.null(input$file2)){
                 {
                   # Get Well DT & CS line ----------------------------------------------------------------
                   Geology_Descriptions_slc=data_slc()[["Geology_Descriptions_slc"]]
                   drawn_polyline=data_slc()[["drawn_polyline"]]
                   message("Rander Cross Section Model")
                   # Inputs Test --------------------------------------------------------------------------
                   messeges_lst=list();i_msg=1
                   if(is.null(input$mainmap_draw_stop)){
                     messeges_lst[[i_msg]]="Error 1: No line marked along the map.";i_msg=i_msg+1
                   }
                   else {
                     if(NROW(Geology_Descriptions_slc)==0 & NROW(Virtual_dt)==1 ){
                       messeges_lst[[i_msg]]="Error 2: No wells or Virtual points were selected for the Cross Section.";i_msg=i_msg+1
                     }
                     else if (all(str_detect(Geology_Descriptions_slc$LEVEL_DES,"Lithology")==T) && input$CS_type!="materials"  & NROW(Virtual_dt)==1){
                       messeges_lst[[i_msg]]="Error 3: Cross Section type does not match the type of the selected wells.";i_msg=i_msg+1
                     }
                     else if (length(input$Background)>1 && input$Build_Solids==T){
                       messeges_lst[[i_msg]]="Warning 1: You have selected more than two libraries and choose ''solid build'' option, this may create distortions in the Cross Section view.";i_msg=i_msg+1}
                   }
                   if(!is.null(charts)) {
                     if(charts$cs_data$cs_id==input$cs_id){
                       messeges_lst[[i_msg]]="Warning 2: Cross section with this ID already exists in the system, the existing cross section will be replace.";i_msg=i_msg+1
                     }
                   }
                   
                   if(length(messeges_lst)>0 & modeldialog_status=="Inactive"){
                     messeges_str=as.character(unlist(messeges_lst))
                     showModal(modalDialog(
                       title = "There seems to be some issues with the data you entered: ",
                       messeges_str,
                       easyClose = TRUE,
                       footer = NULL
                     ))
                   }
                   # Filter by manual selecting ----------------------------------------------------
                   if(length(input$CS_model_system_rows_current)<length(table(Geology_Descriptions_slc$well_id)) & tablet_filter=="active"){
                     wells_md_fltr=as.data.frame(data_slc()[["CS_model_system_ss"]])[input$CS_model_system_rows_current,]
                     Geology_Descriptions_slc=dplyr::filter(Geology_Descriptions_slc,name %in% wells_md_fltr$Name,)
                   }
                   if(nrow(Virtual_dt)>1) {
                     Geology_Descriptions_slc=bind_rows(Geology_Descriptions_slc,Virtual_dt[2:nrow(Virtual_dt),])
                     rm(Virtual_dt)
                   }
                   # Progress Bar Initiation 
                   withProgress(message = 'Cross Section building in progress',
                                detail = 'This may take a while...', value = 0, min=0,max=360,
                                expr = {
                                  incProgress(1/360)
                                  #  Load Parameters --------------------------------------------------------------
                                  message("Load Parameters")
                                  CS_Prameters<-list(
                                    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MODEL PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                    drawn_polyline=drawn_polyline,
                                    Geology_Descriptions_slc=Geology_Descriptions_slc,
                                    transforms_st=data_slc()[["transforms_fltr"]],
                                    title_CS=input$title_CS,
                                    cs_id=input$cs_id,
                                    CS_type=input$CS_type,
                                    season=input$season,
                                    measurement_year=input$measurement_year,
                                    Projection_Line=input$Projection_Line,
                                    Buffer=input$Buffer,
                                    label_size=input$label_size,
                                    Interval_between_labeles=input$Interval_between_labeles,
                                    Lower_Limit=input$Lower_Limit,
                                    Surface_Smooth=input$Surface_Smooth,
                                    Resolution=input$Resolution,
                                    vertical_resolution=input$vertical_resolution,
                                    DEM_line_thickness=input$DEM_line_thickness,
                                    Background=input$Background,
                                    Build_Solids=input$Build_Solids,
                                    Virtual_Wells=input$Virtual_Wells,
                                    geological_cs_surf=input$geological_cs_surf,
                                    country=input$country,
                                    geology_200=geology_200,
                                    geology_50=geology_50,
                                    additional_layers_lst=additional_layers_lst
                                    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MODEL PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                  )
                                  
                                  # Run Model ----------------------------------------------------------------------------
                                  message("Run Model")
                                  withCallingHandlers({
                                    shinyjs::html("cs_messages", "")
                                    charts<<-cs_model(in_param=CS_Prameters)
                                  },
                                  message = function(m) {
                                    shinyjs::html(id = "cs_messages", html = m$message, add = F)
                                  }
                                  )
                                  # Render Preview Visualization ---------------------------------------------------------
                                  output$cs_chart <- renderPlot({
                                    req(charts$cs_preview)
                                    charts$cs_preview
                                  })
                                }) # End Progress Bar
                   # Download Cross Sections -------------------------------------------------------------
                   output$cs_download_ppt <-  downloadHandler(
                     message("Download Cross Sections"),
                     filename = function() { 
                       paste("Editable_CS-", Sys.Date(), ".pptx", sep="")
                     },
                     content = function(file) {
                       print(charts$cs_ppt, target = file)
                     })
                   output$cs_download_html <-  downloadHandler(
                     message("Download Cross Sections"),
                     filename = function() { 
                       paste("Interactive_CS-", Sys.Date(), ".html", sep=""
                       )
                     },
                     content = function(file) {
                       htmlwidgets::saveWidget(charts$cs_html,selfcontained = T, file=file)
                     })  
                   # Geology Horizons System ##############################################################
                   output$cs_tagging<-renderPlot({
                     req(charts$cs_raw)
                     if(!is.null(horizons_db)){
                       # Get Junction Points --------------------------------------------------------------
                       nodes_links_df=nodes_linker(current_line=st_as_sf(charts$cs_data$CS_line),horizons_db)
                       # Render CS ------------------------------------------------------------------------
                       cols_classf<<-ColourExpreation()
                       cs_tagging<<-charts$cs_raw+geom_text(data=nodes_links_df,
                                                            aes(x=Distance,y=Elevation,color=Horizon,size=7,label=ID)) +
                         scale_color_manual(values=cols_classf,guide = FALSE)
                       cs_tagging
                     } else {
                       cs_tagging<<-charts$cs_raw
                       cs_tagging}
                     
                     
                   })
                   # Update Selection list ----------------------------------------------------------------
                   observe({
                     by_v=reactiveVal({input$Select_horizon_by})
                     if(as.character(by_v())=="Wells"){
                       cs_horizons<<-dplyr::distinct(charts$cs_data$wells_plot_df,f_name,.keep_all = T)
                       cols_classf<<-ColourExpreation()
                     }
                     if(as.character(by_v())=="Hydrogeology libraries"){
                       cs_horizons<<-dplyr::distinct(charts$cs_data$DEM_plot_df,f_name,.keep_all = T)
                       cols_classf<<-ColourExpreation_DEM()
                     }
                     if (is.null(cs_horizons)){cs_horizons <- character(0)}
                     
                     updateSelectInput(session,
                                       inputId="Select_horizon",
                                       label = "Select horizon:",
                                       choices = as.character(cs_horizons$f_name)
                     )
                   })
                   # Set Initial Table --------------------------------------------------------------------
                   if(!is.null(tab_raw)==T){tab_raw=NULL}
                   clickposition_history <- reactiveVal(data.frame(Distance = numeric(),Longitude=numeric(),
                                                                   Latitude=numeric(),Range=numeric(),
                                                                   Elevation = numeric(),Horizon=character(),
                                                                   method=character(),ID=character()))
                   # Add selected box Column ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                   if(nrow(clickposition_history())==0){
                     output$info = DT::renderDataTable({
                       tab_raw <<- clickposition_history()
                       slc_tbl<-DT::datatable(tab_raw,
                                              class="cell-border",
                                              selection='none',escape=F)
                       slc_tbl
                     }) 
                   }
                   # Add new point ========================================================================
                   observeEvent(input$plot_click, {
                     print(tab_raw)
                     new_point_ll=horizonewpnt(
                       Select_horizon=input$Select_horizon,
                       cs_horizons=cs_horizons,
                       plot_click=input$plot_click,
                       DEM_plot_df=charts$cs_data$DEM_plot_df
                     )
                     new_point_ll$ID=paste0(as.character(input$cs_id),as.character(input$cs_id),"'")
                     # Check point class ------------------------------------------------------------------
                     if(nrow(tab_raw)>0){
                       tab_check=tab_raw %>%
                         mutate(int_dist=((Distance-new_point_ll$Distance)^2+(Elevation-new_point_ll$Elevation)^2)^0.5,
                                pnt2erase=ifelse(int_dist<input$Eraser_sensitivity & Horizon==input$Select_horizon,1,0)) %>% 
                         dplyr::filter(.,pnt2erase==0)
                       
                       if (nrow(tab_check)<nrow(tab_raw)) {
                         message("Delete Point") # Delete Point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                         tab_raw<<-dplyr::distinct(dplyr::select(tab_check,-int_dist,-pnt2erase),Distance,Elevation,ID,.keep_all = T) %>% arrange(desc(Distance))
                       } else {
                         message("Add Point") # Join to Current DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                         tab_raw<<-dplyr::distinct(rbind(tab_raw, new_point_ll),Distance,Elevation,ID,.keep_all = T) %>% arrange(desc(Distance))
                       }
                     } else { # New Point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                       tab_raw<<-new_point_ll
                     }
                     # Connect between points -------------------------------------------------------------
                     if(input$h_int!="NaN" & nrow(tab_raw)>1){
                       # Fill horizons by group 
                       horizons2fill=as.data.frame(t(table(tab_raw$Horizon))) %>% rename_all(~(c("ID","Horizon","n")))
                       tab_lst=list()
                       for (i in 1:nrow(horizons2fill)){
                         n_horizon=horizons2fill$n[i]
                         act_horizon=filter(tab_raw,Horizon==horizons2fill$Horizon[i])
                         if (n_horizon>1) {
                           tab_lst[[i]]=fill_horizons(
                             tab_raw=act_horizon,
                             res=input$h_res,
                             int_mathos=input$h_int
                           )
                         }else{
                           tab_lst[[i]]=subset(act_horizon,,c("Elevation","Distance","Horizon","method","ID"))
                         }
                       }
                       tab<<-Reduce(rbind,tab_lst)  
                     } else{tab<<-subset(tab_raw,,c("Elevation","Distance","Horizon","method","ID"))}
                     
                     
                     # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                     write.csv(tab_raw, paste0(Background_path,'/Apps/External_Data/tab_raw.csv'))  #  Local Test file
                     write.csv(charts$cs_data$DEM_plot_df, paste0(Background_path,'/Apps/External_Data/cs_pnts_dt.csv'))  #  Local Test file
                     write.csv(charts$cs_data$wells_plot_df, paste0(Background_path,'/Apps/External_Data/wells_plot_df.csv'))  #  Local Test file
                     # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                     
                     # Render Cross Section & and table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     cols_DEMs=ColourExpreation_DEM()
                     output$cs_tagging <- renderPlot({
                       req(cs_tagging)
                       tab_view=tab_raw
                       if(input$h_int!="NaN" & nrow(tab_raw)>1){
                         common_cols= c("Elevation","Distance","Horizon","method","ID")
                         tab_view=rbind(subset(tab_raw,,common_cols),subset(tab,,common_cols))
                       } 
                       cs_tagging+geom_point(data=tab_view,
                                             shape = 21,
                                             colour = "black",
                                             stroke = 0,
                                             aes(x=Distance,y=Elevation,fill=Horizon,
                                                 size=ifelse(method=="manual",as.numeric(input$hpoint_size),
                                                             0.5*as.numeric(input$hpoint_size))
                                             )) +
                         scale_fill_manual(values=cols_classf,guide = FALSE)
                       
                     })
                     # Render Table
                     output$info = DT::renderDataTable({
                       tab_tbl = dplyr::filter(tab_raw,Horizon==input$Select_horizon)
                       slc_tbl<-DT::datatable(tab_tbl,
                                              class="cell-border",
                                              selection='none',escape=F)  
                       slc_tbl
                     })  
                   })
                   # Add to Data Base --------------------------------------------------------------
                   # Edit 12082021 ###########
                   observeEvent(input$add2hdb,{
                     req(tab_raw)
                     fill_horizons_coord=coordinate_horizons(
                       fill_horizons=tab,
                       DEM_plot_df=charts$cs_data$DEM_plot_df,
                       max_range=10
                     )
                     horizons=combine_sources(
                       wells=charts$cs_data$wells_plot_df,
                       manual_pnt=tab_raw[ID==paste0(as.character(input$cs_id),as.character(input$cs_id),"'"),],
                       intp_pnt=fill_horizons_coord
                     )
                     # Create
                     if(is.null(horizons_db)){
                       horizons_db<<-dplyr::distinct(horizons,Horizon,Distance,Elevation,ID,.keep_all = T)
                     } else {
                       horizons_db<<-rbind(horizons_db,horizons)
                     }
                     # Clean temporal
                     fill_horizons_coord=NULL
                     horizons=NULL
                     tab_raw<-NULL
                     tab<-NULL
                     print(horizons_db[method=="manual",])
                   })
                   # Edit 12082021 ###########
                   # Download Horizon --------------------------------------------------------------
                   output$download_horizon <-downloadHandler(
                     req(!is.null(tab_raw)),
                     message("Download Horizons"),
                     filename = function() { 
                       paste("Horizons_CS-",paste0(as.character(input$cs_id),as.character(input$cs_id),"'-"), Sys.Date(), ".csv", sep="")
                     },
                     content = function(file) {
                       req(tab)
                       # Build horizon Interpolated File
                       if(input$h_int!="NaN" & nrow(tab_raw)>1){
                         fill_horizons_coord=coordinate_horizons(
                           fill_horizons=tab,
                           DEM_plot_df=charts$cs_data$DEM_plot_df,
                           max_range=10
                         )
                         
                         horizons=combine_sources(
                           wells=charts$cs_data$wells_plot_df,
                           manual_pnt=tab_raw,
                           intp_pnt=fill_horizons_coord
                         )
                       }
                       # Build Manual Horizon File 
                       if(input$h_int=="NaN"){
                         horizons=combine_m_sources(
                           wells=charts$cs_data$wells_plot_df,
                           manual_pnt=tab_raw
                         )
                       }
                       fwrite(horizons,file)
                     })
                 }  # End Cross Section
               }
               }) # End of Cross Section Rendering
  
  
} # End of Server --------------------------------------------------------------------------

shinyApp(ui, server,options = list(port=7990))



