source("/home/metsalu/ShinyApps/upload_webtool/global.R")
#source("/home/metsalu/ShinyApps/upload_webtool_test/global.R")
h = 800
pcaPlot = plotOutput("pca", height = "100%", width = "100%")

shinyUI(fluidPage(
	titlePanel("ClustVis: a web tool for visualizing clustering of multivariate data (BETA)"),
	sidebarLayout(
		sidebarPanel(
			conditionalPanel(condition = "input.tabs1 == 'Introduction'",
				h4("Introduction")
			),
			conditionalPanel(condition = "input.tabs1 == 'Data upload'",
				h4("Enter data"),
				h5("Choose data input type:"),
				radioButtons("uploadDataInput", "", 
          list("Load sample data" = 1, "Upload file" = 2, "Paste data" = 3, 
				  "Import public dataset from ArrayExpress" = 5, "Load saved settings" = 4,  
					"Import prepared gene expression matrix" = 6)),
				conditionalPanel(condition = "input.uploadDataInput == '1'",
					h5("Choose dataset:"),
					radioButtons("uploadSampleData", "", 
						list("NKI breast cancer dataset (PAM50 genes)" = "nki.csv",
							"Wisconsin Diagnostic Breast Cancer" = "wdbc.csv",
							"Iris flowers" = "iris.csv"
						)
					)
				),
				conditionalPanel(condition = "input.uploadDataInput == '2'",
					h5("Upload delimited text file: "),
					fileInput("uploadFile", "", multiple = FALSE)
				),
				conditionalPanel(condition = "input.uploadDataInput == '3'",
					h5("Paste data below:"),
					tags$textarea(id = "uploadCopyPaste", rows = 10, cols = 5, ""),
					actionButton('uploadClearTextButton', 'Clear data')
				),
				conditionalPanel(condition = "input.uploadDataInput == '4' || input.uploadDataInput == '6'",
					h5("Settings ID:"),
					textInput("uploadSettingsId", "")
				),
				conditionalPanel(condition = "input.uploadDataInput == '5'",
					h5("Choose dataset:"),
					selectInput("uploadOrganism", "Array organism:", choices = uploadOrganismList, 
                      selected = "Homo sapiens", selectize = useSelectize),
					numericInput("uploadMinAnnoTracks", "Minimum number of annotations:", value = 1, min = 0),
					#initially empty to allow faster loading:
					selectizeInput("uploadPbDataset", "Dataset:", choices = NULL, selected = NULL,
						options = list(placeholder = 'search for a dataset')),
					HTML('<p>You can type some keywords separated by space (e.g. affy breast) and then select a dataset; use backspace to delete.</p>')
				),
				conditionalPanel(condition = "input.uploadDataInput == '5'",
				  h5("Filter annotations:"),
				  numericInput("uploadMinAnnoLevels", "Minimum number of levels for annotations:", value = 2, min = 1),
				  numericInput("uploadMaxAnnoLevels", "Maximum number of levels for annotations:", value = 8, min = 1)
				),
				
				conditionalPanel(condition = "input.uploadDataInput == '1' || input.uploadDataInput == '2' || input.uploadDataInput == '3'",
					checkboxGroupInput("uploadGuessSep", "", c("detect delimiter" = TRUE), TRUE),
					conditionalPanel(condition = "input.uploadGuessSep == ''",
						radioButtons("uploadFileSep", "Delimiter:", list("Comma" = 1, "Tab" = 2, "Semicolon" = 3)),
						HTML('<p>Data in <a href = "http://en.wikipedia.org/wiki/Delimiter-separated_values">delimited text files </a> can be separated by comma, tab or semicolon. For example, Excel data can be exported in .csv (comma separated) or .tab (tab separated) format. </p>')
					),
					
					checkboxGroupInput("uploadGuessAnno", "", c("detect annotation rows" = TRUE), TRUE),
					conditionalPanel(condition = "input.uploadGuessAnno == ''",
						numericInput("uploadNbrAnnoRows", "Number of annotation rows:", value = -1, min = -1, step = 1)
					)
				),
				
				h5("Data matrix reshape:"),
				checkboxGroupInput("uploadColumnFiltering", "", c("filter columns" = TRUE), FALSE),
				conditionalPanel(condition = "input.uploadColumnFiltering != ''",
					uiOutput("uploadColumnFilters")
				),
				checkboxGroupInput("uploadMatrixTranspose", "", c("transpose matrix" = TRUE), FALSE),
        
				conditionalPanel(condition = "input.uploadDataInput == '5' || input.uploadDataInput == '6'",
				  h5("Row filtering:"),
				  radioButtons("uploadRowFiltering", "", list("Subset a pathway" = 1, "Cluster genes" = 2, "Choose one cluster" = 3)),
				  conditionalPanel(condition = "input.uploadRowFiltering == '1'",
				    selectizeInput("uploadPbPathway", "Pathway:", choices = NULL, selected = NULL,
				      options = list(placeholder = 'search for a pathway')),
				    HTML('<p>You can type some keywords separated by space (e.g. kegg estrogen) and then select a pathway; use backspace to delete.</p>')
				  ),
				  conditionalPanel(condition = "input.uploadRowFiltering == '2' || input.uploadRowFiltering == '3'",
  				  numericInput("uploadNbrClusters", "Number of k-means clusters:", 
              value = 100, min = 2, max = 600),
  				  conditionalPanel(condition = "input.uploadRowFiltering == '3'",
  				    numericInput("uploadClusterId", "Cluster ID to choose:", value = 1, min = 1, max = 600)
  				  )
				  )
				)
			),
			conditionalPanel(condition = "input.tabs1 == 'Data pre-processing'",
				h4("Pre-processing options"),
				checkboxGroupInput("procAnno", "Annotations to keep:", choices = "Annotation", selected = "Annotation"),
				selectInput("procMethodAgg", "Collapse columns with similar annotations:", 
					choices = procMethAgg, selectize = useSelectize),
				checkboxGroupInput("procCentering", "", c("row centering" = TRUE), TRUE),
				selectInput("procScaling", "Row scaling:", choices = procScalings, selected = "uv", selectize = useSelectize),
				selectInput("procMethod", "PCA method:", choices = procMeth, selected = "svdImpute", selectize = useSelectize)
			),
			conditionalPanel(condition = "input.tabs1 == 'PCA'",
				h4("PCA options"),
				checkboxGroupInput("pcaChangeDataOptions", "", c("change data options" = TRUE), FALSE),
				conditionalPanel(condition = "input.pcaChangeDataOptions != ''",
				  h5("Data options"),
				  selectInput("pcaPcx", "Principal component on x-axis:", 
					  choices = "1", selected = "1", selectize = useSelectize),
				  selectInput("pcaPcy", "Principal component on y-axis:", 
					  choices = "1", selected = "1", selectize = useSelectize),
				  checkboxGroupInput("pcaSwitchDir", "", 
					  choices = c("switch direction of x-axis" = 1, "switch direction of y-axis" = 2),
					  selected = c(FALSE, FALSE)),
				  br()
				),
        
				checkboxGroupInput("pcaChangeDisplayOptions", "", c("change display options" = TRUE), FALSE),
				conditionalPanel(condition = "input.pcaChangeDisplayOptions != ''",
				  h5("Display options"),
				  checkboxGroupInput("pcaChangeColorOptions", "", c("change coloring options" = TRUE), FALSE),
				  conditionalPanel(condition = "input.pcaChangeColorOptions != ''",
				    h6("Coloring options"),
				    checkboxGroupInput("pcaAnnoColor", "Color grouping:", 
                             choices = "Annotation", selected = "Annotation"),
				    selectInput("pcaColor", "Color scheme:", 
                      colQualitative, "Set1", selectize = useSelectize),
				    checkboxGroupInput("pcaShowEllipses", "", c("show ellipses around groups" = TRUE), TRUE),
				    conditionalPanel(condition = "input.pcaShowEllipses != ''",
				     numericInput("pcaEllipseConf", "Confidence level for ellipses:", 
                         value = 0.95, min = 0.5, max = 0.9999, step = 0.001),
				      sliderInput("pcaEllipseLineWidth", "Line width for ellipses:", 
                        min = 0.1, max = 3, value = 1, step = 0.1),
				      selectInput("pcaEllipseLineType", "Line type for ellipses:", 
                        lineTypeList, selectize = useSelectize)
				    ),
            br()
				  ),
          
				  checkboxGroupInput("pcaChangeShapeOptions", "", c("change shape options" = TRUE), FALSE),
				  conditionalPanel(condition = "input.pcaChangeShapeOptions != ''",
				    h6("Shape options"),
				    checkboxGroupInput("pcaAnnoShape", "Shape grouping:", 
                             choices = "Annotation", selected = "Annotation"),
				    selectInput("pcaShape", "Shape scheme:", shapeList, "various", selectize = useSelectize),
            br(), br()
				  ),
          
				  sliderInput("pcaPlotWidth", "Plot width:", value = 20, min = 5, max = 50, step = 0.1),
				  numericInput("pcaPlotRatio", "Plot ratio (height / width):", 
                       value = 0.8, min = 0.01, max = 100, step = 0.01),
				  sliderInput("pcaPointSize", "Point size:", min = 1, max = 9, value = 5, step = 0.1),

				  selectInput("pcaLegendPosition", "Legend position:", c("none", "right", "bottom", "left", "top"), "right", selectize = useSelectize),
				  br(), br() #double br() needed after dropdown
				),
        
				checkboxGroupInput("pcaChangeLabelOptions", "", c("change plot labels" = TRUE), FALSE),
				conditionalPanel(condition = "input.pcaChangeLabelOptions != ''",
				  h5("Plot labels"),
				  sliderInput("pcaFontSize", "Font size:", min = 10, max = 30, value = 20, step = 1),
				  textInput("pcaAxisLabelPrefix", "Prefix for axes' labels:", "PC"), 
				  checkboxGroupInput("pcaShowVariance", "", c("show variance explained" = TRUE), TRUE),
				  checkboxGroupInput("pcaShowSampleIds", "", c("show sample IDs" = TRUE), FALSE)
				)
			),
      
			conditionalPanel(condition = "input.tabs1 == 'Heatmap'",
				h4("Heatmap options"),
				checkboxGroupInput("hmChangeClusteringOptions", "", c("change clustering options" = TRUE), FALSE),
				conditionalPanel(condition = "input.hmChangeClusteringOptions != ''",
				  h5("Clustering options"),
          selectInput("hmClustDistRows", "Clustering distance for rows:", 
                      clustDists, "correlation", selectize = useSelectize),
				  conditionalPanel(condition = "input.hmClustDistRows != 'no clustering'",
					  selectInput("hmClustMethodRows", "Clustering method for rows:", 
                        clustMethods, "average", selectize = useSelectize),
					  selectInput("hmTreeOrderingRows", "Tree ordering for rows:", treeOrderings, selectize = useSelectize)
				  ),
				  selectInput("hmClustDistCols", "Clustering distance for columns:", 
                      clustDists, "correlation", selectize = useSelectize),
				  conditionalPanel(condition = "input.hmClustDistCols != 'no clustering'",
				    selectInput("hmClustMethodCols", "Clustering method for columns:", 
                        clustMethods, "average", selectize = useSelectize),
            selectInput("hmTreeOrderingCols", "Tree ordering for columns:", treeOrderings, selectize = useSelectize)
				  ),
				  conditionalPanel(condition = "input.hmClustDistCols == 'no clustering'", div()), #to force one empty row
          br()
			  ),
        
			  checkboxGroupInput("hmChangeDisplayOptions", "", c("change display options" = TRUE), FALSE),
				conditionalPanel(condition = "input.hmChangeDisplayOptions != ''",
				  h5("Display options"),
				  checkboxGroupInput("hmAnno", "Annotations:", choices = "Annotation", selected = "Annotation"),
				  sliderInput("hmPlotWidth", "Plot width:", value = 25, min = 15, max = 50, step = 0.1),
				  numericInput("hmPlotRatio", "Plot ratio (height / width):", value = 0.8, min = 0.01, max = 100, step = 0.01),
				  numericInput("hmColorRangeMax", "Color range maximum:", value = 5, step = 0.001),
				  numericInput("hmColorRangeMin", "Color range minimum:", value = -5, step = 0.001),
				  selectInput("hmColorScheme", "Color scheme:", schemeListHM, selected = "RdBu", selectize = useSelectize),
				  checkboxGroupInput("hmRevScheme", "", c("reverse color scheme" = TRUE), TRUE),
				  selectInput("hmCellBorder", "Cell border:", c("no border", "grey60", "grey", "black"), "grey60", selectize = useSelectize),
          br(), br()
		    ),
        
				checkboxGroupInput("hmChangeLabelOptions", "", c("change plot labels" = TRUE), FALSE),
				conditionalPanel(condition = "input.hmChangeLabelOptions != ''",
				  h5("Plot labels"),
				  checkboxGroupInput("hmShowNumbers", "", c("show numbers" = TRUE), FALSE),
				  conditionalPanel(condition = "input.hmShowNumbers != ''",
					  sliderInput("hmFontSizeNumbers", "Font size of numbers:", 
                        value = 15 * 0.8, min = 1 * 0.8, max = 25 * 0.8, step = 1),
					  sliderInput("hmPrecisionNumbers", "Precision of numbers:", value = 2, min = 0, max = 4, step = 1)
				  ),
				  checkboxGroupInput("hmShowRownames", "", c("show row names" = TRUE), TRUE),
				  conditionalPanel(condition = "input.hmShowRownames != ''",
					  sliderInput("hmFontSizeRownames", "Font size of row names:", value = 15, min = 1, max = 25, step = 1)
				  ),
				  checkboxGroupInput("hmShowColnames", "", c("show column names" = TRUE), TRUE),
				  conditionalPanel(condition = "input.hmShowColnames != ''",
					  sliderInput("hmFontSizeColnames", "Font size of column names:", value = 15, min = 1, max = 25, step = 1)
				  ),
				  checkboxGroupInput("hmShowAnnoTitles", "", c("show annotation titles" = TRUE), TRUE)
			  ),
        br()
			),

			conditionalPanel(condition = "input.tabs1 == 'Export'",
				h4("Export options"),
				textInput("exportSaveDeleteSettingsId", "ID for settings:")
			),
			conditionalPanel(condition = "input.tabs1 == 'Help'",
				h4("Help"),
				a("General", href = "#general"), br(),
				a("Data upload", href = "#upload"), br(),
				a("Public dataset from MEM", href = "#mem"), br(),
				a("List of datasets available", href = "#datasets"), br(),
				a("List of pathways available", href = "#pathways"), br(),
				a("Data pre-processing", href = "#processing"), br(),
				a("PCA and heatmap", href = "#pca_heatmap"), br(),
				a("Interpreting the output", href = "#interpretation"), br(),
				a("Mathematical basis", href = "#mathematics")
			),
			conditionalPanel(condition = "input.tabs1 == 'News'",
				h4("News")
			),
			
			bsTooltip("uploadDataInput", "Which input to use: example dataset, upload a file, copy-paste from file, import from MEM (collection of public datasets from ArrayExpress), use saved settings or use a custom pre-saved dataset.", tooltipPlace),
			bsTooltip("uploadGuessSep", "Whether delimiter (separator between the columns) is detected automatically or provided by the user.", tooltipPlace),
			bsTooltip("uploadGuessAnno", "Whether number of annotation rows is detected automatically or provided by the user. Annotations should be placed as the first rows in the file. See help tab for more information.", tooltipPlace),
			bsTooltip("uploadOrganism", "Organism of the microarray platform.", tooltipPlace),
			bsTooltip("uploadMinAnnoTracks", "You can filter out less informative datasets, e.g. those that have no annotations.", tooltipPlace),
			bsTooltip("uploadMinAnnoLevels", "You can filter out less informative annotations, e.g. those that are constant.", tooltipPlace),
			bsTooltip("uploadMaxAnnoLevels", "You can filter out less informative annotations, e.g. those that have different value for each sample.", tooltipPlace),
			bsTooltip("uploadColumnFiltering", "You can select a subset of columns using annotation groups.", tooltipPlace),
			bsTooltip("uploadMatrixTranspose", "You can transpose the data matrix. Annotations (if any) will be lost in this case.", tooltipPlace),
			bsTooltip("uploadRowFiltering", "How to limit the number of rows shown - take one pathway, cluster the genes using k-means and show cluster centers or choose one specific cluster from k-means clustering.", tooltipPlace),
			bsTooltip("uploadNbrClusters", "Number of clusters after appying k-means clustering.", tooltipPlace),
			bsTooltip("uploadClusterId", "Which cluster to visualize more closely. The previous option should be run first to identify a cluster of interest.", tooltipPlace),
			
			bsTooltip("procAnno", "Which annotations to keep for further analysis, especially important if collapsing similar annotations.", tooltipPlace),
			bsTooltip("procMethodAgg", "Whether to collapse columns with similar annotations before further analysis and if so, which function to use.", tooltipPlace),
			bsTooltip("procCentering", "Whether to subtract mean from each row or use original data. This only applies to the values on heatmap, rows are always centered before calculating principal components.", tooltipPlace),
			bsTooltip("procScaling", "Which scaling to use for each row. See pcaMethods R package for more details.", tooltipPlace),
			bsTooltip("procMethod", "Which method to use for calculating principal components. Principal components are also used to impute missing values. See pcaMethods R package for more details.", tooltipPlace),
			
			bsTooltip("pcaChangeDataOptions", "Choose which principal components to show and whether to change the direction.", tooltipPlace),
			bsTooltip("pcaChangeDisplayOptions", "Change the way how results are visualized, except textual labels.", tooltipPlace),
			bsTooltip("pcaChangeColorOptions", "Change options related with color grouping.", tooltipPlace),
			bsTooltip("pcaChangeShapeOptions", "Change options related with shape grouping.", tooltipPlace),
			bsTooltip("pcaChangeLabelOptions", "Change the textual labels of the plot.", tooltipPlace),
			bsTooltip("pcaSwitchDir", "Axes can be mirrored, e.g. to make the plot easier to visually compare with PCA plot made with another tool. This does not change the meaning of the plot.", tooltipPlace),
      #quote in the tooltip not working! (e.g. axes')
			bsTooltip("pcaShowVariance", "Whether to show percentage of variance explained added to the labels of the axes.", tooltipPlace),
			bsTooltip("pcaAnnoColor", "Color grouping shown in the legend. Multiple annotations can be chosen; in this case, they are combined.", tooltipPlace),
			bsTooltip("pcaAnnoShape", "Shape grouping shown in the legend. Multiple annotations can be chosen; in this case, they are combined.", tooltipPlace),
			bsTooltip("pcaPlotWidth", "In arbitrary units.", tooltipPlace),
			bsTooltip("pcaPlotRatio", "This applies to the plotting area, i.e. to the rectangle where points are situated (it does not take axis labels and legend into account).", tooltipPlace),
			bsTooltip("pcaColor", "Coloring used for the groups. It is automatically turned off if there are more than 8 groups. See http://colorbrewer2.org/ for more details.", tooltipPlace),
			bsTooltip("pcaShape", "Shapes used for the groups.", tooltipPlace),
			bsTooltip("pcaShowSampleIds", "Whether to show sample IDs on the plot. This is mainly for finding points of interest from the plot, not necessarily publication quality.", tooltipPlace),
			bsTooltip("pcaShowEllipses", "Whether to show prediction ellipses around groups. These are only shown for groups with at least 3 points. See FactoMineR R package for more details.", tooltipPlace),
			bsTooltip("pcaEllipseConf", "Confidence level used for calculating prediction ellipses. This determines their size - a new observation will fall inside the ellipse with this probability. See FactoMineR R package for more details.", tooltipPlace),
			bsTooltip("pcaLegendPosition", "Where legend is placed, relative to the plot.", tooltipPlace),
			
			bsTooltip("hmChangeClusteringOptions", "Change clustering distance, linkage and tree ordering.", tooltipPlace),
			bsTooltip("hmChangeDisplayOptions", "Change the way how results are visualized, except textual labels.", tooltipPlace),
			bsTooltip("hmChangeLabelOptions", "Change the textual labels of the plot.", tooltipPlace),
			bsTooltip("hmClustDistRows", "Which distance measure to use for calculating how far two rows (or clusters of rows) are from each other.", tooltipPlace),
			bsTooltip("hmClustDistCols", "Which distance measure to use for calculating how far two columns (or clusters of columns) are from each other.", tooltipPlace),
			bsTooltip("hmClustMethodRows", "Which linkage criterion to use for clustering rows.", tooltipPlace),
			bsTooltip("hmClustMethodCols", "Which linkage criterion to use for clustering columns.", tooltipPlace),
			bsTooltip("hmTreeOrderingRows", "How to order branches of the clustering tree of rows to make it visually more attractive.", tooltipPlace),
			bsTooltip("hmTreeOrderingCols", "How to order branches of the clustering tree of columns to make it visually more attractive.", tooltipPlace),
      bsTooltip("hmAnno", "Which annotations are shown on top of the heatmap.", tooltipPlace),
			bsTooltip("hmColorRangeMax", "Maximum of the color range. Values above that will appear as white. If you have a range containing zero, you may consider changing minimum and maximum into opposite numbers to make sure that zero is exactly in the middle.", tooltipPlace),
			bsTooltip("hmColorRangeMin", "Minimum of the color range. Values below that will appear as white. If you have a range containing zero, you may consider changing minimum and maximum into opposite numbers to make sure that zero is exactly in the middle.", tooltipPlace),
			bsTooltip("hmColorScheme", "Coloring used for the heatmap. Sequential palettes are better suitable for ordered data that progress from low to high (e.g. percentage between 0 and 100). Diverging palettes put equal emphasis on mid-range critical values and extremes at both ends of the data range, they are better suitable for data with both negative and positive values (e.g. difference from some reference). See http://colorbrewer2.org/ for more details.", tooltipPlace),
			bsTooltip("hmRevScheme", "Whether to switch the ends of the color scheme.", tooltipPlace),
			bsTooltip("hmCellBorder", "Color of the border around each cell.", tooltipPlace),
			bsTooltip("hmShowNumbers", "Whether to show numeric value in each cell.", tooltipPlace),
			bsTooltip("hmPrecisionNumbers", "How many decimal places to show for numbers in the cells.", tooltipPlace),
			bsTooltip("hmPlotWidth", "In arbitrary units.", tooltipPlace),
			bsTooltip("hmPlotRatio", "This applies to dimensions of the whole image file.", tooltipPlace),
			bsTooltip("hmShowAnnoTitles", "Whether to show annotation titles for the tracks above the heatmap.", tooltipPlace),
			
			bsTooltip("exportSaveSettingsButton", "You can save settings to create a link with data and options pre-loaded.", tooltipPlace),
			bsTooltip("exportDeleteSettingsButton", "You can delete settings that you have saved before (for data privacy).", tooltipPlace),
			
			tags$head(
				tags$style(type = "text/css", ".jslider { max-width: 220px; }"),
				tags$style(type = "text/css", ".span4 { max-width: 265px; }"),
				tags$style(type = "text/css", "h1 { font-size: 30px; }"),
				tags$style(type = "text/css", str_c(".shiny-image-output { position:absolute; }")),
				tags$style(type = "text/css", str_c(".shiny-plot-output { position:absolute; }")),
				tags$style(type = "text/css", str_c(".rChart { height: ", h, "px; }"))
			)
		),
	
		mainPanel(
			tabsetPanel(
				tabPanel("Introduction", 
					p("This web tool allows users to upload their own data and easily create Principal Component Analysis (PCA) plots and heatmaps. Data can be uploaded as a file or by copy-pasteing it to the text box. Data format is shown under \"Help\" tab."),
					p("The tool is inspired by discussions in ", 
					a("PREDECT", href = "http://predect.eu/", target = "_blank"),
					" project and borrows some code from ", 
					HTML("<a href='http://boxplot.tyerslab.com/' target='_blank'>BoxPlotR</a>."),
					"Several ", a("R", href = "http://www.r-project.org/", target = "_blank"), " packages are used internally, including ", 
					"shiny, ggplot2, pheatmap, RColorBrewer, FactoMineR, pcaMethods, shinyBS and others. It is developed in ", 
					a("BIIT", href = "http://biit.cs.ut.ee/", target = "_blank"), 
					"Research Group. The source code of ClustVis is available in ",
					HTML("<a href='https://github.com/taunometsalu/ClustVis' target='_blank'>GitHub</a>."),
          "Please submit bug reports and feature requests to GitHub issues page or send to tauno.metsalu [at] ut.ee."),
					br(),
					img(src = "frontPage/frontPage.png"),
          #http://stackoverflow.com/questions/26058909/r-shiny-avoid-scrollbars-when-using-googlevis-charts-in-tabpanels
					style = "overflow:hidden;"
				),
				# Data upload tab
				tabPanel("Data upload",
				  p(textOutput("uploadWarnings")),
					conditionalPanel(condition = "input.uploadDataInput == '5'",
						p("Number of annotation levels in the original data before filtering:"), 
						tableOutput("uploadAnnoLevelsTable")
					),
					uiOutput("uploadAnnoInfo"),
					tableOutput("uploadAnnoTable"),
					uiOutput("uploadDataInfo"),
					tableOutput("uploadDataTable"),
					p("Number of NAs in rows before removing:"),
					p(tableOutput("uploadNAsRows")),
					p("Number of NAs in columns before removing:"),
					p(tableOutput("uploadNAsCols"))
				),
				tabPanel("Data pre-processing",
					uiOutput("procPcaVarInfo"),
					tableOutput("procPcaVarTable"),
					uiOutput("procMatPcaInfo"),
					tableOutput("procMatPca"),
					uiOutput("procPcaLoadingsInfo"),
					tableOutput("procPcaLoadings")
				),
				tabPanel("PCA", 
					downloadButton("exportDownloadPdfPCA", "Download PDF-file"),
					downloadButton("exportDownloadEpsPCA", "Download EPS-file"),
					downloadButton("exportDownloadSvgPCA", "Download SVG-file"),
					br(), br(),
					htmlOutput("legendPCA"),
					pcaPlot
				),
				tabPanel("Heatmap",
					downloadButton("exportDownloadPdfHeatmap", "Download PDF-file"),
					downloadButton("exportDownloadEpsHeatmap", "Download EPS-file"),
					downloadButton("exportDownloadSvgHeatmap", "Download SVG-file"),
					br(), br(),
					htmlOutput("legendHeatmap"),
					imageOutput("heatmap", height = "100%", width = "100%")
				),
				tabPanel("Export",
					actionButton("exportSaveSettingsButton", "Save settings"),
					br(),
					conditionalPanel(condition = "input.exportSaveSettingsButton",
						uiOutput("exportSaveSettingsText")
					),
					actionButton("exportDeleteSettingsButton", "Delete settings"),
					p(),
					conditionalPanel(condition = "input.exportDeleteSettingsButton",
						uiOutput("exportDeleteSettingsText")
					),
					downloadButton("exportDownloadInitialData", "Download initial data as CSV-file"), br(), 
					downloadButton("exportDownloadProcessedData", "Download processed data as CSV-file"), br(), 
					downloadButton("exportDownloadPCAscores", "Download PCA scores as CSV-file"), br(), 
					downloadButton("exportDownloadPCAloadings", "Download PCA loadings as CSV-file"), br(), 
					downloadButton("exportDownloadPCAvartable", "Download PCA explained variance as CSV-file")
				),
				tabPanel("Help",
					h5("General", id = "general"),
					p("You can move through the analysis steps by going to each of the tabs from left to right. All tabs work in a similar way: you can choose settings from the left panel, image or table on the right will automatically renew after that. Sometimes, it can take seconds to load. When moving from one tab to another, settings are saved automatically."),
          p("The idle timeout (the time when browser session ends if user is inactive) is set to 30 minutes from server side but this can be overridden by browser configuration. To save uploaded data and selected settings, you can use a button on the 'Export' tab, a link is given to recover the settings later. This can also be used to send a link to a collaborator to show the same view. There is no planned expiration time for the links, users can delete the settings if they are concerned about the privacy. Though, when version of ClustVis changes, old saved settings may not be fully compatible with the new version if e.g. there are some new features. "),
					h5("Data upload", id = "upload"),
					p("We aimed for a simple input data format. Each column represents one object (e.g. sample), there are optional annotation rows on top followed by dimension rows with numeric data. Format of the input file is shown on the image below. Data sets without annotations can be uploaded as well (on the example image, without rows 2-4)."),
					img(src = "helpTab/uploadTableArrows.png", width = "100%"),
					p("In addition, it is possible to load settings that you have saved earlier (including data, drop-down settings etc.) or import data from",  
            a("MEM", href = "http://biit.cs.ut.ee/mem/", target = "_blank"), 
            " which has a collection of public datasets from ",
            HTML("<a href='http://www.ebi.ac.uk/arrayexpress/' target='_blank'>ArrayExpress</a>."), 
            " The latter case is described more closely in the next section. It is also possible to use one of the testing datasets to get an overview of the web tool."),
          #URL() instead of HTML() adds unwanted space before dot
					p("If your dataset is not uploaded correctly (no rows are shown), please check the following:"),
          HTML("<ul><li>Make sure file is chosen for upload or pasted to the text box.</li>
              <li>Make sure all rows have equal number of columns. In case of doubt, it is safer to open the data in a spreadsheet program and copy-paste from there rather than choosing a file for upload.</li>
              <li>Make sure there are no duplicate row or column names.</li>
              <li>If automatic detection of the delimiter is not working correctly, try to set it manually (uncheck the 'detect delimiter' checkbox).</li>
              <li>If automatic detection of the annotation rows is not working correctly, try to set it manually (uncheck the 'detect annotation rows' checkbox).</li></ul>"),
					p("For user-uploaded datasets, ClustVis automatically detects both delimiter and number of annotation rows from the data by default. To find delimiter, it counts for each possible delimiter (comma, tabulator, semicolon) how many times it appears on each row. We use the heuristic where minimum is taken over all rows and the delimiter with the greatest score is chosen as the right one."),
					p("When finding number of annotation rows, two situations may occur. If numeric data contains integers only (i.e. there are no fractional numbers), the last row that contains any non-numeric values is considered the last row of annotations. If data also includes fractional numbers, it can occur that the last row of annotations has integer-valued annotations (e.g. grade coded with integers: 1, 2, 3) and will be incorrectly classified as numeric data. To avoid this, a row is found where all values are numeric, there is at least one fractional number and all following rows contain only numeric values. The last row before this is considered as the last row of annotations."),
          p("The situation is depicted on the following images where there are three annotation lines and the green and red line show automatic detection. On the left, the numeric matrix contains non-integer values and the last annotation row is detected correctly. On the right, the numeric matrix contains only integers and the last annotation row cannot be detected automatically."),
					img(src = "helpTab/helpMatrix.png", width = "100%"),
					h5("Public dataset from MEM", id = "mem"),
          p("If you select this option, you can choose one dataset at a time. There are three options to filter the number of rows shown:"),
          HTML("<ul><li>Select one KEGG or Reactome pathway or GO biological process. In case of some rare platforms, it can happen that gene IDs don't convert correctly and no data is shown.</li>
              <li>Cluster the genes using k-means. The number of clusters is provided by the user. Cluster ID and number of genes in each cluster is shown on the heatmap labels.</li>
              <li>Choose one of the k-means clusters. This options should be preceded by clustering with k-means and choosing a cluster of interest from the heatmap.</li></ul>"),
					p("If it happens that some names are very long and make the plot small, you can manually increase the width of the plot. We decided not to truncate the names automatically because sometimes the start of the name is important, sometimes the end and it is hard to decide it automatically. "),
					p("You can search for dataset and pathway by typing one or more keywords to the search box and then select from drop-down list that appears. The keywords can be about body part, dataset ID or any other word that will appear in the experiment title. All available datasets are summarized in the table below."),
					h5("List of datasets available", id = "datasets"),
          dataTableOutput('helpDatasetTable'),
					h5("List of pathways available", id = "pathways"),
					dataTableOutput('helpPathwayTable'),
          h5("Data pre-processing", id = "processing"),
					p("On this tab, you can choose the method that is used for PCA. This method is also used for imputing missing values to the heatmap and it also determines, for example, whether values on the heatmap are centered or not. Number of components returned depends on the dimensions of the input data matrix. If there are more observations (n) than dimensions (d) then d principal components are calculated. Otherwise, the number of principal components is n."),
					h5("PCA and heatmap", id = "pca_heatmap"),
					p("These are the main tabs, allowing you to generate and customize PCA plot and heatmap. Each individual setting is described more precisely with a tooltip that appears if you hover over with the mouse. To download an image, you can use one of the buttons above the plot. The following color palettes from ColorBrewer are available:"),
					img(src = "helpTab/colorBrewer.png"),
					p("If there are more than eight groups on the PCA plot, coloring is turned off because human eye cannot distinguish so many colors easily. In this case, shapes should be enough for separating the groups. The following shapes are used:"),
					img(src = "helpTab/shapes.png"),
					p("If there are more groups than available shapes, some groups are not shown."),
					h5("Interpreting the output", id = "interpretation"),
					p("Principal Component Analysis performs a linear transformation to turn multivariate data into a form where variables are uncorrelated (see Jolliffe, Ian. Principal component analysis. John Wiley & Sons, Ltd, 2002). These new uncorrelated variables are called Principal Components and they are ordered descending based on the variance explained. Thus, first two components show the data from the angle of most variability, i.e. they create the most \"widespread\" 2D projection. They also approximate the distances between points. Thus, if ellipses on the PCA plot are not overlapping, these groups form separate clusters."),
					p("However, one should be careful when first components describe just a small proportion of the total variation. In this case, approximating original data with 2D projection can be misleading. "),
					p("The opposite happens when there are only two dimensions in the original data, resulting in 100% of the variation to be explained by the two components. In this case, PCA doesn't make much sense and making a simple scatterplot would be better to interpret in most cases. "),
					p("Sometimes, first components are related with technical variation such as batch effect, in this case, it makes sense to look at further components that can be attributed to more informative sources of variability."),
					p("Heatmap is another popular way to visualize a data matrix. Values in the matrix are color coded and optionally, rows and/or columns are clustered. When looking at the annotations on top of the heatmap, one can get an overview which annotated groups are separated better than others. "),
					p("When reading the clustering on heatmap, attention should be paid which objects are merged into clustering tree first, not the exact order of rows and/or columns. Any two branches can be swapped without changing the meaning of the tree."),
					p("An example output and its interpretation is shown below:"),
					img(src = "helpTab/interpretation.png", width = "100%"),
					h5("Mathematical basis", id = "mathematics"),
          p("Calculation of principal components is thoroughly explained in the book by Ian Jolliffe (see Jolliffe, Ian. Principal component analysis. John Wiley & Sons, Ltd, 2002). "),
					p("Hierarchical clustering of the heatmap starts with calculating all pairwise distances. Objects with the smallest distance are merged in each step. Clustering method defines how to go from object level to cluster level when calculating distance between two clusters."),
					HTML("Available clustering distances:<ul><li>correlation - Pearson correlation subtracted from 1</li><li>Euclidean - square root of sum of square distances</li><li>maximum - greatest absolute difference between coordinates</li><li>Manhattan - sum of the absolute differences</li><li>Canberra - weighted Manhattan distance</li><li>binary - matrix is binarized (non-zero to 1, zero to 0), number of bits which are 1/0 or 0/1 divided by number of bits which are 0/1, 1/0 or 1/1</li></ul>"),
					#http://support.minitab.com/en-us/minitab/17/topic-library/modeling-statistics/multivariate/item-and-cluster-analyses/linkage-methods/
					HTML("Available linkage methods:<ul><li>single linkage - using two closest objects from two clusters to be merged</li><li>complete linkage - using two farthest objects</li><li>average - average distance of all possible pairs</li><li>McQuitty - average distance of the two clusters (to be merged) to the cluster of interest</li><li>median - median distance of all possible pairs</li><li>centroid - distance between cluster means</li><li>Ward linkage - using sum of squared differences from points to centroids as the distance</li></ul>"),
					br(),
					br(),
					br()
				),
				tabPanel("News",
					h5("Version history:"),
					p("31st October 2014 - first version online."),
					p("12th November 2014 - option to save settings added; and some other new features."),
					p("14th November 2014 - option to download plot in different formats added; line type and width for ellipses added."),
					p("21st November 2014 - import from MEM added; some new example datasets; improved help page; number of rows and columns is now shown on upload tab."),
					p("4th December 2014 - improved search for MEM datasets (you don't have to choose platform or pathway database first). Filtering option of less informative annotations and sample filtering added."),
					p("15th December 2014 - column filtering for all import options added; number of annotations added to dataset name; more flexible plot width."),
					p("18th December 2014 - you can aggregate similar annotations on 'pre-processing' tab; you can hide row and/or column names on heatmap; you can set minimum number of annotations when choosing from the list of public datasets."),
					p("19th December 2014 - output interpretation added to the help page; better default size for row and column names on the heatmap."),
					p("6th February 2015 - heatmap tree ordering options added; it is possible to choose different linkage method for rows and columns of heatmap; you can choose color range of the heatmap manually; heatmap annotation titles can be switched off; organism filtering added when importing public datasets; some bug fixes."),
					p("9th February 2015 - some optimization and help text added when importing dataset from ArrayExpress."),
					p("1st April 2015 - major revision based on comments from reviewers: some example datasets removed; it is possible to cluster whole gene expression dataset first using k-means or select one k-means cluster; some warning messages added; Bayesian PCA removed; PCA and heatmap options grouped; percentages shown together with axis labels; color and shape can be changed independently on PCA plot; help page improved a lot; example captions added for PCA plot and heatmap; new export options added; heatmap default color changed."),
					p("6th April 2015 - small improvements related with option 'import prepared gene expression matrix'."),
					p("17th April 2015 - second revision based on comments from reviewers: number of species increased to 17; more informative error messages for data upload; number of NAs in rows and columns is shown during upload; help page improved (including list of all datasets and pathways)."),
					p("21st April 2015 - option to transpose the data matrix added under 'Data upload'; maximal heatmap dimension increased to 1200; a small bug fixed related with uploading a dataset without annotations.")
				),
				id = "tabs1"
			)
		) #, fluid = FALSE
	)
))


