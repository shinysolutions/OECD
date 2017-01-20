
shinyUI(bootstrapPage(
  tagList(
    tags$head(

      tags$style("a:hover {text-decoration: none;} .col-sm-12, .form-group.shiny-input-container {width: 800px;} #uiCNT{float:right; width: 200px;} #home {font-size: 150px;   } .input{width: 1000px} .inputLeft {width: 800px; float:left;} .input, .output, .download {margin-left: 20px;} .download{margin-top: 300px;}")

    )
  ),


  
  div(class = "input",
      div(class = "inputLeft",
        HTML( "<div class='col-sm-12' style='padding: 0px;'><a href='home.html'><h2>Organisation and Functions of the Centre of Government, OECD 2014</h2></a> <p>The dataset provides information on the roles and organisation of the administrative structure at the centre of government [referred to for convenience as the “Centre
of Government (CoG)] that supports the collective work of the executive and the Prime Minister or President in OECD member and partner countries.</p></div> "),
        uiOutput("uiTopic"),
        uiOutput("uiSubTopic")
      ),
      uiOutput("uiCNT")

  ),
  
  div(class = "output",
      # uiOutput("uiTitle"),
      plotOutput("plot1", width = "1000px", height = "600px")
  ),
  
  div(class = "download",
      downloadButton('downloadReport', 'Download Current'),
      downloadButton('downloadAll', 'Download All')
  )
  
  # sidebarPanel(
  #   # uiOutput("uiCountry"),
  #   uiOutput("uiTopic"),
  #   uiOutput("uiSubTopic"),
  #   # radioButtons('format', 'Document format', c('PDF', 'PPT'),inline = TRUE),
  #   downloadButton('downloadReport', 'Download Current'),
  #   downloadButton('downloadAll', 'Download All')
  # ),
  # mainPanel(
  #   
  #   uiOutput("uiTitle"),
  #   plotOutput("plot1", width = "1000px", height = "600px")
  # )
  

))
