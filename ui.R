#Author: Tauno Metsalu
#Copyright: 2018 University of Tartu

#source("/srv/shiny-server/global.R")

h = 800
pcaPlot = uiOutput("pca", height = "100%", width = "100%")
hmPlot = uiOutput("heatmap", height = "100%", width = "100%")

#no quotes allowed in tooltips:
jitterPlotTypeTooltip = "Whether to show violin plot or box plot together with points. This is informative if columns with similar annotations are collapsed (see data pre-processing tab)."
jitterSeparateOverlappingTooltip = "Whether to shift points horizontally (jittering) or make points transparent to distinguish overlapping points. This may be needed if columns with similar annotations are collapsed (see data pre-processing tab)."

jitterPlotCondition = function(type, negative = FALSE){
  if(type == "hm"){
    ids = c("hmRow", "hmCol", "hmCell")
  } else if(type == "pca"){
    ids = c("pcaCol", "pcaCell")
  } else {
    stop("Such type not found!")
  }
  cond = str_c(str_c(str_c("input.", type, "ClickedType == '"), ids), "'", collapse = " || ")
  if(negative) cond = str_c("!(", cond, ")")
  cond
}

jitterPlotOptions = function(type){
  conditionalPanel(condition = jitterPlotCondition(type),
    h4("Plot options"),
    radioButtons(str_c(type, "JitterPlotType"), "Plot type:", selected = "dots", 
      choices = list("Points only" = "dots", 
                     "Violin plot with points" = "violin dots", "Box plot with points" = "box dots", 
                     "Violin plot only" = "violin", "Box plot only" = "box")),
    conditionalPanel(condition = str_c("input.", type, "JitterPlotType == 'dots' || input.", type, "JitterPlotType == 'violin dots' || input.", type, "JitterPlotType == 'box dots'"),
      radioButtons(str_c(type, "JitterSeparateOverlapping"), "Separate overlapping points:", 
        selected = "no", choices = c("no", "jittering", "transparency")),
      conditionalPanel(condition = str_c("input.", type, "JitterSeparateOverlapping == 'jittering'"),
        sliderInput(str_c(type, "JitterJitteringWidth"), "Jittering width:", 
          min = 0, max = 0.5, value = 0.25, step = 0.01)
      )
    ),
    conditionalPanel(condition = str_c("input.", type, "JitterSeparateOverlapping == 'transparency'"),
      sliderInput(str_c(type, "JitterTransparency"), "Transparency:", 
                  min = 0, max = 1, value = 0.5, step = 0.05)
    )
  )
}

footer = h6("Metsalu, Tauno and Vilo, Jaak. ", a("Clustvis: a web tool for visualizing clustering of multivariate data using Principal Component Analysis and heatmap.", href = "http://nar.oxfordjournals.org/content/43/W1/W566.full", target = "_blank"), "Nucleic Acids Research, 43(W1):W566–W570, 2015. doi: 10.1093/nar/gkv468.")

titlePrefix = "ClustVis"
titleMiddle = ": a web tool for visualizing clustering of multivariate data (BETA)"
titleFull = str_c(titlePrefix, titleMiddle, titleSufix)

shinyUI(
fluidPage(
  useShinyjs(),
  #https://cran.r-project.org/web/packages/shinyjs/vignettes/overview.html
  #http://stackoverflow.com/questions/13338780/how-to-make-twitter-bootstrap-tooltips-have-multiple-lines
  extendShinyjs(text = jsCode),
  titlePanel(
    fluidRow(
      img(src = "frontPage/logo_small.png", width = 125, style = "margin-top:-10px; margin-right:-10px"),
      str_c(titleMiddle, titleSufix), style = "margin-left:0px;"
    )
  ),
  sidebarLayout(
		sidebarPanel(
			conditionalPanel(condition = "input.tabs1 == 'Introduction'",
				h4("Introduction"),
        #oterwise tooltips don't work for some reason:
				tags$div(
				  HTML('<button type="button" class="btn btn-default" data-toggle="tooltip" data-original-title="button" title="" data-placement="right" id="right" style="display: none;">button</button>')
				),
				tags$head(HTML("<script type='text/javascript'>//<![CDATA[
  			  $(document).ready(function(){
            $('[data-toggle=\"tooltip\"]').tooltip({container: 'body'});
          });//]]> 
				  </script>"))
			),
			conditionalPanel(condition = "input.tabs1 == 'Data import'",
				h4("Enter data"),
				h5("Choose data input type:"),
				radioButtons("uploadDataInput", NULL, uploadInputOptions),
				conditionalPanel(condition = "input.uploadDataInput == '1'",
					h5("Choose dataset:"),
					radioButtons("uploadSampleData", NULL, 
						list("NKI breast cancer dataset (PAM50 genes)" = "nki.csv",
							"Wisconsin Diagnostic Breast Cancer" = "wdbc.csv",
							"Iris flowers" = "iris.csv"
						), selected = "nki.csv"
					)
				),
				conditionalPanel(condition = "input.uploadDataInput == '2'",
					h5("Upload delimited text file: "),
					fileInput("uploadFile", NULL, multiple = FALSE),
					p(str_c('Maximum file size is ', maxUploadMB, ' MB.'))
				),
				conditionalPanel(condition = "input.uploadDataInput == '3'",
					h5("Paste data below:"),
					tags$textarea(id = "uploadCopyPaste", rows = 10, cols = 5, ""),
					actionButton('uploadClearTextButton', 'Clear data')
				),
				conditionalPanel(condition = "input.uploadDataInput == '4' || input.uploadDataInput == '6'",
					h5("Settings ID:"),
					textInput("uploadSettingsId", NULL)
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
				
				conditionalPanel(condition = "input.uploadDataInput == '2' || input.uploadDataInput == '3'",
					checkboxGroupInput("uploadGuessSep", NULL, c("detect delimiter" = TRUE), TRUE),
					conditionalPanel(condition = "input.uploadGuessSep == ''",
						radioButtons("uploadFileSep", "Delimiter:", list("Comma" = 1, "Tab" = 2, "Semicolon" = 3)),
						HTML('<p>Data in <a href = "http://en.wikipedia.org/wiki/Delimiter-separated_values">delimited text files </a> can be separated by comma, tab or semicolon. For example, Excel data can be exported in .csv (comma separated) or .tab (tab separated) format. </p>')
					),
					
					checkboxGroupInput("uploadGuessAnno", NULL, c("detect column and row annotations" = TRUE), TRUE),
					conditionalPanel(condition = "input.uploadGuessAnno == ''",
						numericInput("uploadNbrColAnnos", "Number of column annotations:", value = -1, min = -1, step = 1),
						numericInput("uploadNbrRowAnnos", "Number of row annotations:", value = -1, min = -1, step = 1)
					),
					
					selectInput("uploadQuotes", "Quotes:", choices = list("single or double" = "\"'", "single" = "'", "double" = "\"", "no quotes" = ""), selectize = useSelectize),
					textInput("uploadNaString", "Missing values:", "NA")
				),
        
				conditionalPanel(condition = "input.uploadDataInput == '5' || input.uploadDataInput == '6'",
				  h5("Gene subsetting or clustering:"),
				  radioButtons("uploadRowFiltering", NULL, list("Subset a pathway" = 1, "Subset a custom gene list" = 4, "Cluster genes" = 2, "Choose one cluster" = 3)),
				  conditionalPanel(condition = "input.uploadRowFiltering == '1'",
				    selectizeInput("uploadPbPathway", "Pathway:", choices = NULL, selected = NULL,
				      options = list(placeholder = 'search for a pathway')),
				    HTML('<p>You can type some keywords separated by space (e.g. kegg estrogen) and then select a pathway; use backspace to delete.</p>')
				  ),
          
				  conditionalPanel(condition = "input.uploadRowFiltering == '4'",
				    h5("Paste your gene list below:"),
				    tags$textarea(id = "uploadGeneList", rows = 10, cols = 5, ""),
				    actionButton('uploadClearGeneListButton', 'Clear data'),
				    HTML('<p>Genes should be separated by white space (spaces, tabs or newlines).</p>')
				  ),
          
				  conditionalPanel(condition = "input.uploadRowFiltering == '2' || input.uploadRowFiltering == '3'",
  				  numericInput("uploadNbrClusters", "Number of k-means clusters:", 
              value = 100, min = 2, max = 600),
  				  conditionalPanel(condition = "input.uploadRowFiltering == '3'",
  				    numericInput("uploadClusterId", "Cluster ID to choose:", value = 1, min = 1, max = 600)
  				  )
				  )
				),
        
				h5("Data matrix reshape:"),
				checkboxGroupInput("uploadColumnFilteringAnno", NULL, c("filter columns" = TRUE), FALSE),
				conditionalPanel(condition = "input.uploadColumnFilteringAnno != ''",
				                 uiOutput("uploadColumnFiltersAnno")
				),
				checkboxGroupInput("uploadRowFilteringAnno", NULL, c("filter rows" = TRUE), FALSE),
				conditionalPanel(condition = "input.uploadRowFilteringAnno != ''",
				                 uiOutput("uploadRowFiltersAnno")
				),
				checkboxGroupInput("uploadMatrixTranspose", NULL, c("transpose matrix" = TRUE), FALSE)
			),
			conditionalPanel(condition = "input.tabs1 == 'Data pre-processing'",
				h4("Pre-processing options"),
				selectInput("procTransformation", "Transformation:", choices = c("no transformation", "ln(x)", "ln(x + 1)"), selected = "no transformation", selectize = useSelectize),
				conditionalPanel(condition = str_c("input.procAnno != '", fakeAnno, "'"),
				  checkboxGroupInput("procAnnoGroups", "Column annotation groups to keep:", choices = fakeAnno, selected = fakeAnno),
				  checkboxGroupInput("procAnno", "Column annotations to keep:", choices = fakeAnno, selected = fakeAnno),
				  selectInput("procMethodAgg", "Collapse columns with similar annotations:", 
					  choices = procMethAgg, selectize = useSelectize)
        ),
				numericInput("procMaxNaPercRows", "Maximum percentage of NAs allowed in rows:", 
                     value = 99.99, min = 0, max = 100),
				numericInput("procMaxNaPercCols", "Maximum percentage of NAs allowed in columns:", 
                     value = 99.99, min = 0, max = 100),
				checkboxGroupInput("procRemConstCols", NULL, c("remove constant columns" = TRUE), FALSE),
				checkboxGroupInput("procCentering", NULL, c("row centering" = TRUE), TRUE),
				selectInput("procScaling", "Row scaling:", choices = procScalings, selected = "uv", selectize = useSelectize),
				selectInput("procMethod", "PCA method:", choices = procMeth, selected = "svdImpute", selectize = useSelectize)
			),
			conditionalPanel(condition = "input.tabs1 == 'PCA'",
			  conditionalPanel(condition = jitterPlotCondition("pca", negative = TRUE),
				h4("PCA options"),
				checkboxGroupInput("pcaChangeDataOptions", NULL, c("change data options" = TRUE), FALSE),
				conditionalPanel(condition = "input.pcaChangeDataOptions != ''",
				  h5("Data options"),
				  selectInput("pcaPcx", "Principal component on x-axis:", 
					  choices = "1", selected = "1", selectize = useSelectize),
				  selectInput("pcaPcy", "Principal component on y-axis:", 
					  choices = "1", selected = "1", selectize = useSelectize),
				  checkboxGroupInput("pcaSwitchDir", NULL, 
					  choices = c("switch direction of x-axis" = 1, "switch direction of y-axis" = 2),
					  selected = c(FALSE, FALSE)),
				  br()
				),
        
				checkboxGroupInput("pcaChangeDisplayOptions", NULL, c("change display options" = TRUE), FALSE),
				conditionalPanel(condition = "input.pcaChangeDisplayOptions != ''",
				  h5("Display options"),
				  conditionalPanel(condition = str_c("input.procAnno != '", fakeAnno, "' && input.procAnno != ''"),
				    checkboxGroupInput("pcaChangeColorOptions", NULL, c("change coloring options" = TRUE), FALSE),
				    conditionalPanel(condition = "input.pcaChangeColorOptions != ''",
				      checkboxGroupInput("pcaAnnoColor", "Color grouping:", choices = fakeAnno, selected = fakeAnno),
				      selectInput("pcaColor", "Color scheme:", schemeListPCA, "Set1", selectize = useSelectize),
				      checkboxGroupInput("pcaShowEllipses", NULL, c("show ellipses around groups" = TRUE), TRUE),
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
				    checkboxGroupInput("pcaChangeShapeOptions", NULL, c("change shape options" = TRUE), FALSE),
				    conditionalPanel(condition = "input.pcaChangeShapeOptions != ''",
				      checkboxGroupInput("pcaAnnoShape", "Shape grouping:", choices = fakeAnno, selected = fakeAnno),
				      selectInput("pcaShape", "Shape scheme:", shapeList, "various", selectize = useSelectize),
              br(), br()
				    )
				  ),
				  sliderInput("pcaPlotWidth", "Plot width:", value = 20, min = 5, max = 50, step = 0.1),
				  numericInput("pcaPlotRatio", "Plot ratio (height / width):", 
                       value = 0.8, min = 0.01, max = 100, step = 0.01),
				  numericInput("pcaMarginRatio", "Margin ratio:", 
				               value = 0.05, min = -0.1, max = 2, step = 0.001),
				  sliderInput("pcaPointSize", "Point size:", min = 1, max = 9, value = 5, step = 0.1),
				  conditionalPanel(condition = str_c("input.procAnno != '", fakeAnno, "' && input.procAnno != ''"),
				    selectInput("pcaLegendPosition", "Legend position:", c("none", "right", "bottom", "left", "top"), "right", selectize = useSelectize)
          ),
				  br(), br() #double br() needed after dropdown
				),
        
				checkboxGroupInput("pcaChangeLabelOptions", NULL, c("change plot labels" = TRUE), FALSE),
				conditionalPanel(condition = "input.pcaChangeLabelOptions != ''",
				  h5("Plot labels"),
				  sliderInput("pcaFontSize", "Font size:", min = 10, max = 30, value = 20, step = 1),
				  textInput("pcaAxisLabelPrefix", "Prefix for axes' labels:", "PC"), 
				  checkboxGroupInput("pcaShowVariance", NULL, c("show variance explained" = TRUE), TRUE),
				  conditionalPanel(condition = "input.pcaInteractivity == ''",
            checkboxGroupInput("pcaShowSampleIds", NULL, c("show sample IDs" = TRUE), FALSE)
				  ),
				  checkboxGroupInput("pcaInteractivity", NULL, c("add interactivity" = TRUE), FALSE)
				)
			  ),
        
				jitterPlotOptions("pca")
			),
      
			conditionalPanel(condition = "input.tabs1 == 'Heatmap'",
			  conditionalPanel(condition = jitterPlotCondition("hm", negative = TRUE), 
				h4("Heatmap options"),
				checkboxGroupInput("hmChangeDataOptions", NULL, c("change data options" = TRUE), FALSE),
				conditionalPanel(condition = "input.hmChangeDataOptions != ''",
				  h5("Data options"),
				  checkboxGroupInput("hmShowImputed", NULL, c("show imputed values" = TRUE), TRUE),
				  checkboxGroupInput("hmTransposeHeatmap", NULL, c("transpose heatmap" = TRUE), FALSE),
          selectInput("hmClustDistRows", "Clustering distance for rows:", 
                      clustDists, "correlation", selectize = useSelectize),
				  conditionalPanel(condition = "input.hmClustDistRows != 'no clustering'",
					  selectInput("hmClustMethodRows", "Clustering method for rows:", 
                        clustMethods, "average", selectize = useSelectize),
					  selectInput("hmTreeOrderingRows", "Tree ordering for rows:", treeOrderings, selectize = useSelectize),
					  numericInput("hmCutreeClustersRows", "Number of clusters in rows:", value = 1, min = 1, max = 5)
				  ),
				  selectInput("hmClustDistCols", "Clustering distance for columns:", 
                      clustDists, "correlation", selectize = useSelectize),
				  conditionalPanel(condition = "input.hmClustDistCols != 'no clustering'",
				    selectInput("hmClustMethodCols", "Clustering method for columns:", 
                        clustMethods, "average", selectize = useSelectize),
            selectInput("hmTreeOrderingCols", "Tree ordering for columns:", treeOrderings, selectize = useSelectize),
				    numericInput("hmCutreeClustersCols", "Number of clusters in columns:", value = 1, min = 1, max = 5)
				  ),
				  conditionalPanel(condition = "input.hmClustDistCols == 'no clustering'", div()), #to force one empty row
          br()
			  ),
        
			  checkboxGroupInput("hmChangeDisplayOptions", NULL, c("change display options" = TRUE), FALSE),
				conditionalPanel(condition = "input.hmChangeDisplayOptions != ''",
				  h5("Display options"),
				  conditionalPanel(condition = str_c("input.procAnno != '", fakeAnno, "' && input.procAnno != ''"),
				    checkboxGroupInput("hmAnnoCol", "Column annotations:", choices = fakeAnno, selected = fakeAnno)
          ),
				  conditionalPanel(condition = str_c("input.hmAnnoRow != '", fakeAnno, "'"),
				    checkboxGroupInput("hmAnnoRow", "Row annotations:", choices = fakeAnno, selected = fakeAnno)
          ),
				  conditionalPanel(condition = "input.hmAnnoCol != '' || input.hmAnnoRow != ''",
				    selectInput("hmLegendScheme", "Annotation color scheme:", choices = colQualitative, selected = "Set1", selectize = useSelectize)
				  ),
				  sliderInput("hmPlotWidth", "Plot width:", value = 25, min = 5, max = 50, step = 0.1),
				  numericInput("hmPlotRatio", "Plot ratio (height / width):", value = 0.8, min = 0.01, max = 100, step = 0.01),
				  numericInput("hmColorRangeMax", "Heatmap color range maximum:", value = 5, step = 0.001),
				  numericInput("hmColorRangeMin", "Heatmap color range minimum:", value = -5, step = 0.001),
				  selectInput("hmColorScheme", "Heatmap color scheme:", schemeListHM, selected = "RdBu", selectize = useSelectize),
				  checkboxGroupInput("hmRevScheme", NULL, c("reverse color scheme" = TRUE), TRUE),
				  selectInput("hmCellBorder", "Cell border:", c("no border", "grey60", "grey", "black"), "grey60", selectize = useSelectize),
          br(), br()
		    ),
        
				checkboxGroupInput("hmChangeLabelOptions", NULL, c("change plot labels" = TRUE), FALSE),
				conditionalPanel(condition = "input.hmChangeLabelOptions != ''",
				  h5("Plot labels"),
				  sliderInput("hmFontSizeGeneral", "General font size:", value = 10, min = 1, max = 25),
				  checkboxGroupInput("hmShowNumbers", NULL, c("show numbers" = TRUE), FALSE),
				  conditionalPanel(condition = "input.hmShowNumbers != ''",
					  sliderInput("hmFontSizeNumbers", "Font size of numbers:", 
                        value = 15 * 0.8, min = 1 * 0.8, max = 25 * 0.8, step = 1),
					  sliderInput("hmPrecisionNumbers", "Precision of numbers:", value = 2, min = 0, max = 4, step = 1)
				  ),
				  checkboxGroupInput("hmShowRownames", NULL, c("show row names" = TRUE), TRUE),
				  conditionalPanel(condition = "input.hmShowRownames != ''",
					  sliderInput("hmFontSizeRownames", "Font size of row names:", value = 15, min = 1, max = 25, step = 1)
				  ),
				  checkboxGroupInput("hmShowColnames", NULL, c("show column names" = TRUE), TRUE),
				  conditionalPanel(condition = "input.hmShowColnames != ''",
					  sliderInput("hmFontSizeColnames", "Font size of column names:", value = 15, min = 1, max = 25, step = 1)
				  ),
				  conditionalPanel(condition = str_c("input.procAnno != '", fakeAnno, "' && input.procAnno != ''"),
            checkboxGroupInput("hmShowAnnoTitlesCol", NULL, c("show column annotation titles" = TRUE), TRUE)
          ),
				  conditionalPanel(condition = str_c("input.hmAnnoRow != '", fakeAnno, "'"),
				    checkboxGroupInput("hmShowAnnoTitlesRow", NULL, c("show row annotation titles" = TRUE), TRUE)
				  ),
				  checkboxGroupInput("hmInteractivity", NULL, c("add interactivity" = TRUE), FALSE)
			  )
			  ),
        
				jitterPlotOptions("hm")
			),

			conditionalPanel(condition = "input.tabs1 == 'Export'",
				h4("Export options"),
				textInput("exportSaveDeleteSettingsId", "ID for settings:")
			),
			conditionalPanel(condition = "input.tabs1 == 'Help'",
				h4("Help"),
				a("General", href = "#general"), br(),
				a("Data import", href = "#upload"), br(),
				a("Public dataset from MEM", href = "#mem"), br(),
				a("List of datasets available", href = "#datasets"), br(),
				a("List of pathways available", href = "#pathways"), br(),
				a("Annotations based filtering", href = "#filtering"), br(),
				a("Data pre-processing", href = "#processing"), br(),
				a("PCA and heatmap", href = "#pca_heatmap"), br(),
				a("Interactivity of the plots", href = "#interactivity"), br(),
				a("Interpreting the output", href = "#interpretation"), br(),
				a("Mathematical basis", href = "#mathematics"), br(),
				a("ClustVis editions", href = "#editions")
			),
			conditionalPanel(condition = "input.tabs1 == 'News'",
				h4("News")
			),
			
			bsTooltip("uploadDataInput", "Which input to use: example dataset, upload a file, copy-paste from file, import from MEM (collection of public datasets from ArrayExpress), use saved settings or use a custom pre-saved dataset.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadGuessSep", "Whether delimiter (separator between the columns) is detected automatically or provided by the user.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadGuessAnno", "Whether number of column and row annotations is detected automatically or provided by the user. Annotations should be placed as the first rows and columns in the file. See help tab for more information.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadQuotes", "Which quotes are used around character values in the input data. You may need to change this setting if your data values naturally contain any quotes.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadNaString", "Which string is representing a missing value in the input data.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadOrganism", "Organism of the microarray platform.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadMinAnnoTracks", "You can filter out less informative datasets, e.g. those that have no annotations.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadMinAnnoLevels", "You can filter out less informative annotations, e.g. those that are constant.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadMaxAnnoLevels", "You can filter out less informative annotations, e.g. those that have different value for each sample.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadColumnFilteringAnno", "You can select a subset of columns using annotation levels.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadRowFilteringAnno", "You can select a subset of rows using annotation levels.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadMatrixTranspose", "You can transpose the data matrix. Row annotations will become column annotations and vice versa in this case.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadRowFiltering", "How to limit the number of rows shown - take one pathway or custom gene list, cluster the genes using k-means and show cluster centers or choose one specific cluster from k-means clustering.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadNbrClusters", "Number of clusters after appying k-means clustering.", tooltipPlace, options = tooltipOptions),
			bsTooltip("uploadClusterId", "Which cluster to visualize more closely. The previous option should be run first to identify a cluster of interest.", tooltipPlace, options = tooltipOptions),
			
			bsTooltip("procTransformation", "Whether to apply logarithmic transformation; recommended if data range covers multiple magnitudes. You may consider using ln(x) if smallest value in your data is 1, and ln(x + 1) if smallest value is 0.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procAnnoGroups", "You can select or deselect column annotations in batch by using groups.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procAnno", "Which column annotations to keep for further analysis, especially important if collapsing similar annotations.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procMethodAgg", "Whether to collapse columns with similar annotations before further analysis and if so, which function to use.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procMaxNaPercRows", "Rows that have higher percentage of missing values than this in the original dataset are removed.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procMaxNaPercCols", "Columns that have higher percentage of missing values than this in the original dataset are removed.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procRemConstCols", "Whether to remove constant columns. Constant rows are always removed.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procCentering", "Whether to subtract mean from each row or use original data. This only applies to the values on heatmap, rows are always centered before calculating principal components.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procScaling", "Which scaling to use for each row. See pcaMethods R package for more details.", tooltipPlace, options = tooltipOptions),
			bsTooltip("procMethod", "Which method to use for calculating principal components. Principal components are also used to impute missing values. See pcaMethods R package for more details.", tooltipPlace, options = tooltipOptions),
			
			bsTooltip("pcaChangeDataOptions", "Choose which principal components to show and whether to change the direction.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaChangeDisplayOptions", "Change the way how results are visualized, except textual labels.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaChangeColorOptions", "Change options related with color grouping.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaChangeShapeOptions", "Change options related with shape grouping.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaChangeLabelOptions", "Change the textual labels of the plot.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaSwitchDir", "Axes can be mirrored, e.g. to make the plot easier to visually compare with PCA plot made with another tool. This does not change the meaning of the plot.", tooltipPlace, options = tooltipOptions),
      #quote in the tooltip not working! (e.g. axes')
			bsTooltip("pcaShowVariance", "Whether to show percentage of variance explained added to the labels of the axes.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaAnnoColor", "Color grouping shown in the legend. Multiple annotations can be chosen; in this case, they are combined.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaAnnoShape", "Shape grouping shown in the legend. Multiple annotations can be chosen; in this case, they are combined.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaPlotWidth", "In arbitrary units.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaPlotRatio", "This applies to the plotting area, i.e. to the rectangle where points are situated (it does not take axis labels and legend into account).", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaMarginRatio", "How much white space is shown between data and plot borders, relative to y-axis data range. Margin is added after forcing the specified plot ratio.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaColor", "Coloring used for the groups. It is automatically turned off if there are more than 8 groups. See http://colorbrewer2.org/ for more details.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaShape", "Shapes used for the groups.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaShowSampleIds", "Whether to show sample IDs on the plot. This is mainly for finding points of interest from the plot, not necessarily publication quality.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaShowEllipses", "Whether to show prediction ellipses around groups. These are only shown for groups with at least 3 points. See FactoMineR R package for more details.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaEllipseConf", "Confidence level used for calculating prediction ellipses. This determines their size - a new observation will fall inside the ellipse with this probability. See FactoMineR R package for more details.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaLegendPosition", "Where legend is placed, relative to the plot.", tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaInteractivity", "Whether to make the plot interactive by adding tooltips and links to single points on the plot. This will make the plot more informative, but the plot generation time will increase (the response to changes in the input widgets will become slower).", tooltipPlace, options = tooltipOptions),
			
			bsTooltip("hmChangeDataOptions", "Change data presentation, clustering distance, linkage and tree ordering.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmChangeDisplayOptions", "Change the way how results are visualized, except textual labels.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmChangeLabelOptions", "Change the textual labels of the plot.", tooltipPlace, options = tooltipOptions),
      bsTooltip("hmShowImputed", "Whether to show imputed values on the heatmap or missing values with white color. Clustering is always applied to imputed matrix.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmTransposeHeatmap", "Transpose the heatmap so that rows become columns and vice versa.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmClustDistRows", "Which distance measure to use for calculating how far two rows (or clusters of rows) are from each other.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmClustDistCols", "Which distance measure to use for calculating how far two columns (or clusters of columns) are from each other.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmClustMethodRows", "Which linkage criterion to use for clustering rows.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmClustMethodCols", "Which linkage criterion to use for clustering columns.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmTreeOrderingRows", "How to order branches of the clustering tree of rows to make it visually more attractive.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmTreeOrderingCols", "How to order branches of the clustering tree of columns to make it visually more attractive.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmCutreeClustersRows", "Allows to add gaps for dividing rows into specified number of clusters.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmCutreeClustersCols", "Allows to add gaps for dividing columns into specified number of clusters.", tooltipPlace, options = tooltipOptions),
      bsTooltip("hmAnnoCol", "Which column annotations are shown above the heatmap.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmAnnoRow", "Which row annotations are shown left from the heatmap.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmLegendScheme", "Coloring scheme used for annotation colors.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmColorRangeMax", "Maximum of the color range. Values above that will appear the same as the maximum. If you have a range containing zero, you may consider changing minimum and maximum into opposite numbers to make sure that zero is exactly in the middle.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmColorRangeMin", "Minimum of the color range. Values below that will appear the same as the minimum. If you have a range containing zero, you may consider changing minimum and maximum into opposite numbers to make sure that zero is exactly in the middle.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmColorScheme", "Coloring used for the heatmap. Sequential palettes are better suitable for ordered data that progress from low to high (e.g. percentage between 0 and 100). Diverging palettes put equal emphasis on mid-range critical values and extremes at both ends of the data range, they are better suitable for data with both negative and positive values (e.g. difference from some reference). See http://colorbrewer2.org/ for more details.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmRevScheme", "Whether to switch the ends of the color scheme.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmCellBorder", "Color of the border around each cell.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmFontSizeGeneral", "Font size for texts other than row names, column names and numbers in the cells (if shown).", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmShowNumbers", "Whether to show numeric value in each cell.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmPrecisionNumbers", "How many decimal places to show for numbers in the cells.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmPlotWidth", "In arbitrary units.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmPlotRatio", "This applies to dimensions of the whole image file.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmShowAnnoTitlesRow", "Whether to show row annotation titles for the tracks left from the heatmap.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmShowAnnoTitlesCol", "Whether to show column annotation titles for the tracks above the heatmap.", tooltipPlace, options = tooltipOptions),
			bsTooltip("hmInteractivity", "Whether to make the plot interactive by adding tooltips and links to row and column names and to single cells on the plot. This will make the plot more informative, but the plot generation time will increase (the response to changes in the input widgets will become slower).", tooltipPlace, options = tooltipOptions),
      
      #repeated for PCA and heatmap
			bsTooltip("hmJitterPlotType", jitterPlotTypeTooltip, tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaJitterPlotType", jitterPlotTypeTooltip, tooltipPlace, options = tooltipOptions),
      bsTooltip("hmJitterSeparateOverlapping", jitterSeparateOverlappingTooltip, tooltipPlace, options = tooltipOptions),
			bsTooltip("pcaJitterSeparateOverlapping", jitterSeparateOverlappingTooltip, tooltipPlace, options = tooltipOptions),
			
			bsTooltip("exportSaveSettingsButton", "You can save settings to create a link with data and options pre-loaded.", tooltipPlace, options = tooltipOptions),
			bsTooltip("exportDeleteSettingsButton", "You can delete settings that you have saved before (for data privacy).", tooltipPlace, options = tooltipOptions),
			
			tags$head(
        #http://upgrade-bootstrap.bootply.com/
				tags$style(type = "text/css", ".jslider { max-width: 220px; }"),
				tags$style(type = "text/css", ".span4 { max-width: 265px; }"),
				tags$style(type = "text/css", ".shiny-image-output { position:absolute;}"),
				tags$style(type = "text/css", ".shiny-plot-output { position:absolute;}"),
				tags$style(type = "text/css", str_c(".rChart { height: ", h, "px; }")),
        #http://stackoverflow.com/questions/19855907/r-shiny-fix-sidebarpanel-width-to-a-specific-pixels-value
				tags$style(type = "text/css", ".col-sm-4 { max-width: 295px; }"),
				tags$style(type = "text/css", ".nav { margin-bottom: 20px; }"),
				tags$style(type = "text/css", "h1, h2, h3, h4, h5, h6 { font-weight: bold; padding: 0px 0px; }"),
				tags$style(type = "text/css", "h2 { font-size: 31.5px; }"),
				tags$style(type = "text/css", ".form-group { margin-bottom: 10px; }"),
        #http://stackoverflow.com/questions/8903313/using-regular-expression-in-css
        tags$style(type = "text/css", "div[id$='track'] { color: blue; }"),
				tags$style(type = "text/css", "div[id$='tracksub'] { color: green; }"),
				tags$style(type = "text/css", "div[id$='trackChangeAll'] { color: red; }"),
				tags$style(type = "text/css", "textarea { width: 220px; }"),
        #http://stackoverflow.com/questions/13217669/svg-image-with-a-border-stroke
				tags$style(type = "text/css", ".pcaCol:hover, .pcaCol:focus, .pcaCol:active { cursor: pointer; }"),
        tags$style(type = "text/css", ".hmCell:hover, .hmCell:focus, .hmCell:active, .pcaCell:hover, .pcaCell:focus, .pcaCell:active { cursor: pointer; stroke: red; stroke-width: 2px;}"),
				tags$style(type = "text/css", ".hmRow:hover, .hmRow:focus, .hmRow:active, .hmCol:hover, .hmCol:focus, .hmCol:active { cursor: pointer; font-weight: bold;}")
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
					"shiny, ggplot2, pheatmap, gridSVG, RColorBrewer, FactoMineR, pcaMethods, gProfileR, shinyBS, shinyjs and others. It is developed in ", 
					a("BIIT", href = "http://biit.cs.ut.ee/", target = "_blank"), 
					"Research Group. The source code of ClustVis is available in ",
					HTML("<a href='https://github.com/taunometsalu/ClustVis' target='_blank'>GitHub</a>."),
          "Please submit bug reports and feature requests to GitHub issues page or send to biit.support [at] ut.ee."),
					br(),
					img(src = "frontPage/frontPage.png", width = "100%"),
          #http://stackoverflow.com/questions/26058909/r-shiny-avoid-scrollbars-when-using-googlevis-charts-in-tabpanels
					footer,
          style = "overflow:hidden;"
				),

				tabPanel("Data import",
				  p(textOutput("uploadWarnings")),
					conditionalPanel(condition = "input.uploadDataInput == '5'",
						p("Number of annotation levels in the original data before filtering:"), 
						tableOutput("uploadAnnoLevelsTable")
					),
					uiOutput("uploadAnnoColInfo"),
					tableOutput("uploadAnnoColTable"),
					uiOutput("uploadDataInfo"),
					tableOutput("uploadDataTable"),
					uiOutput("uploadAnnoRowInfo"),
					tableOutput("uploadAnnoRowTable"),
          footer
				),
				tabPanel("Data pre-processing",
				  p(textOutput("procWarnings")),
				  p("Data matrix size:"),
				  p(tableOutput("procSizeTable")),
				  p("Missing values (NAs) in rows:"),
				  p(tableOutput("procNAsRows")),
				  p("Missing values (NAs) in columns:"),
				  p(tableOutput("procNAsCols")),
					uiOutput("procPcaVarInfo"),
					tableOutput("procPcaVarTable"),
					uiOutput("procMatPcaInfo"),
					tableOutput("procMatPca"),
					uiOutput("procPcaLoadingsInfo"),
					tableOutput("procPcaLoadings"),
          footer
				),
				tabPanel("PCA",
				  conditionalPanel(condition = jitterPlotCondition("pca", negative = TRUE),
				    p(textOutput("pcaWarnings")),
					  downloadButton("exportDownloadPdfPCA", "Download PDF-file"),
					  downloadButton("exportDownloadEpsPCA", "Download EPS-file"),
					  downloadButton("exportDownloadSvgPCA", "Download SVG-file"),
					  br(), br(),
					  htmlOutput("captionPCA"),
					  pcaPlot
				  ),
          
				  conditionalPanel(condition = jitterPlotCondition("pca"),
				    p(textOutput("pcaJitterWarnings")),
				    actionButton("pcaBackButton", "Back to PCA plot"),
				    actionButton("pcaBackPreviousButton", "Back to previous plot"), br(),
				    uiOutput("pcaJitterPlot", height = "100%", width = "100%"), br(),
				    dataTableOutput('pcaJitterTable')
				  )
				),
				tabPanel("Heatmap",
				  conditionalPanel(condition = jitterPlotCondition("hm", negative = TRUE),
				    p(textOutput("hmWarnings")),
					  downloadButton("exportDownloadPdfHeatmap", "Download PDF-file"),
					  downloadButton("exportDownloadEpsHeatmap", "Download EPS-file"),
					  downloadButton("exportDownloadSvgHeatmap", "Download SVG-file"),
					  br(), br(),
					  htmlOutput("captionHeatmap"),
            hmPlot
				  ),
          
				  conditionalPanel(condition = jitterPlotCondition("hm"),
				    p(textOutput("hmJitterWarnings")),
				    actionButton("hmBackButton", "Back to heatmap"),
				    actionButton("hmBackPreviousButton", "Back to previous plot"), br(),
				    uiOutput("hmJitterPlot", height = "100%", width = "100%"), br(),
				    dataTableOutput('hmJitterTable')
				  )
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
					downloadButton("exportDownloadPCAvartable", "Download PCA explained variance as CSV-file"),
          footer
				),
				tabPanel("Help",
					h5("General", id = "general"),
					p("You can move through the analysis steps by going to each of the tabs from left to right. All tabs work in a similar way: you can choose settings from the left panel, image or table on the right will automatically renew after that. Sometimes, it can take seconds to load. When moving from one tab to another, settings are saved automatically."),
					HTML("In general, different tabs provide the following options:<ul>
               <li>Data import - choose input dataset, option to filter rows/colums based on annotations and transpose matrix</li>
               <li>Data pre-processing - option to aggregate columns with similar annotations, remove rows/columns with missing values, row centering and scaling, method for calculating principal components and imputing missing values</li>
               <li>PCA - options related to PCA plot</li>
               <li>Heatmap - options related to heatmap</li>
               <li>Export - create link with current settings, download intermediate results</li>
          </ul>"),
          p("The idle timeout (the time when browser session ends if user is inactive) is set to 30 minutes from server side but this can be overridden by browser configuration. To save uploaded data and selected settings, you can use a button on the 'Export' tab, a link is given to recover the settings later. This can also be used to send a link to a collaborator to show the same view. There is no planned expiration time for the links, users can delete the settings if they are concerned about the privacy. Though, when version of ClustVis changes, old saved settings may not be fully compatible with the new version if e.g. there are some new features. "),
					h5("Data import", id = "upload"),
					p("We aimed for a simple input data format. The numeric data matrix is situated in the bottom right corner, dimensions presented in rows and points in columns. Row labels and annotations are left from the matrix, column labels and annotations are above the matrix. Annotation labels are in the first row and column, respectively. Format of the input file is shown on the image below. Annotations are optional, data sets without annotations can be uploaded as well (on the example image, omitting rows 2-4 and/or columns B and C). When taking data from spreadsheet program (e.g. MS Excel), you can copy-paste the data to 'Paste data' box or export the data as delimited text file (ending with .csv or .tab) and then upload this file to ClustVis. Uploading Excel native files directly (.xls or .xlsx) doesn't work."),
					img(src = "helpTab/uploadTableArrows.png", width = "100%"),
          p(),
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
              <li>Make sure all missing values are marked with NA or empty cell.</li>
              <li>If automatic detection of the delimiter is not working correctly, try to set it manually (uncheck the 'detect delimiter' checkbox).</li>
              <li>If automatic detection of the column and row annotations is not working correctly, try to set it manually (uncheck the 'detect column and row annotations' checkbox).</li></ul>"),
					p("For user-uploaded datasets, ClustVis automatically detects both delimiter and number of annotation rows from the data by default. To find delimiter, it counts for each possible delimiter (comma, tabulator, semicolon) how many times it appears on each row. We use the heuristic where minimum is taken over all rows and the delimiter with the greatest score is chosen as the right one."),
					p("When finding number of annotation rows and columns, the largest possible numeric matrix from bottom right corner is found. If the matrix contains non-integers (i.e. fractional numbers) and there are some left columns or top rows that contain integers only, these first rows and/or columns are considered integer-valued annotations (e.g. some grade coded with integers: 1, 2, 3)."),
          p("The situation is depicted on the following images where there are three annotation lines and the green and red line show automatic detection. On the left, the numeric matrix contains non-integer values and the last annotation row is detected correctly. On the right, the numeric matrix contains only integers and the last annotation row cannot be detected automatically."),
					img(src = "helpTab/helpMatrix.png", width = "100%"),
					h5("Public dataset from MEM", id = "mem"),
          p("If you select this option, you can choose one dataset at a time. There are four options to filter the number of rows shown:"),
          HTML("<ul><li>Select one KEGG or Reactome pathway or GO biological process. In case of some rare platforms, it can happen that gene IDs don't convert correctly and no data is shown.</li>
              <li>Select a custom gene list. IDs are automatically converted using <a href='http://biit.cs.ut.ee/gprofiler/gconvert.cgi'>g:Convert</a> tool.</li>
              <li>Cluster the genes using k-means. The number of clusters is provided by the user. Cluster ID and number of genes in each cluster is shown on the heatmap labels.</li>
              <li>Choose one of the k-means clusters. This options should be preceded by clustering with k-means and choosing a cluster of interest from the heatmap.</li></ul>"),
					p("If it happens that some names are very long and make the plot small, you can manually increase the width of the plot. We decided not to truncate the names automatically because sometimes the start of the name is important, sometimes the end and it is hard to decide it automatically. "),
					p("You can search for dataset and pathway by typing one or more keywords to the search box and then select from drop-down list that appears. The keywords can be about body part, dataset ID or any other word that appears in the experiment title. All available datasets and pathways are summarized in the tables below."),
					h5("List of datasets available", id = "datasets"),
          dataTableOutput('helpDatasetTable'),
					h5("List of pathways available", id = "pathways"),
					dataTableOutput('helpPathwayTable'),
					h5("Annotations based filtering", id = "filtering"),
          p("After choosing a dataset, it is possible to filter out rows or columns based on annotation levels. By default, all levels are included, you can uncheck them one by one or click 'change all levels' and then check some of the levels to be included. Each level has a number in brackets that shows the number of rows or columns in the full dataset having this level. If multiple annotations are used for filtering, a subset is retained that meets all filtering criteria."),
          h5("Data pre-processing", id = "processing"),
					p("On this tab, you can choose the method that is used for PCA. This method is also used for imputing missing values to the heatmap and it also determines, for example, whether values on the heatmap are centered or not. Number of components returned depends on the dimensions of the input data matrix. If there are more observations (n) than dimensions (d) then d principal components are calculated. Otherwise, the number of principal components is n."),
					p("In general, SVD (Singular Value Decomposition) is the standard PCA method that is most often used. Imputation means that if there are any missing values in the dataset, they are predicted and filled iteratively using other values in the dataset during SVD calculation. Other PCA methods are further strategies to cope with the missing value problem."),
					p("Unit variance is the most common scaling method. This means that all variables are scaled so that they will be equally important (variance = 1) when finding the components. As a result, a difference of 1 means that the values are one standard deviation away from each other, or from the average of the row if rows are centered."),
					p("If the variables already have a common scale and you want to keep their original variability (i.e. variables where the variability is larger should contribute more to the components), you can apply 'no scaling'."),
					p("When exactly to use some other PCA method (Nipals, probabilistic) or scaling (Pareto, vector scaling) is a matter of testing with each specific dataset. Interpreting the results of PCA and heatmap plots is quite subjective and needs further validation using other methods."),
					h5("PCA and heatmap", id = "pca_heatmap"),
					p("These are the main tabs, allowing you to generate and customize PCA plot and heatmap. Each individual setting is described more precisely with a tooltip that appears if you hover over with the mouse. To download an image, you can use one of the buttons above the plot. The following color palettes from ColorBrewer are available:"),
					img(src = "helpTab/colorBrewer.png"),
					p("Only up to eight color groups are allowed on the PCA plot because human eye cannot distinguish more colors easily. In this case, shapes should be enough for separating the groups. The following shapes are used:"),
					img(src = "helpTab/shapes.png"),
					p("If there are more groups than available shapes, some groups are not shown on the plot."),
					h5("Interactivity of the plots", id = "interactivity"),
          p("With the recent updates of the tool, interactive mode was added to both PCA plot and heatmap which allows to click and hover over specific areas of the plot. This mode is available when going to PCA or heatmap tab and choosing 'change plot labels', 'add interactivity'. Interactivity is still experimental, we are hoping to get feedback from users. It is not made the default option because plots are slower to render. It is recommended to first set other options in non-interactive mode and then switch to interactive mode as the last step. For larger datasets where an interactive plot would take too long to render, it is automatically switched to non-interactive mode and a warning message is shown."),
          HTML(str_c("Interactive mode includes the following additional options:<ul>
               <li>Hover over a point on the PCA plot to see additional information.</li>
               <li>Click on a point on the PCA plot to see values from one column on a separate jitter plot. If '", linkAnno, "' appears among column annotations, the points are linked to an external resource instead.</li>
               <li>Hover over a row name, column name or cell on the heatmap to see additional information.</li>
               <li>Click on a row name, column name or cell on the heatmap to see values from one row, column or cell on a separate jitter plot.</li>
               <li>Hover over a point on the jitter plot to see additional information.</li>
               <li>Click on a point on the jitter plot to go to an external resource. It is only available if there is a row or column annotation called '", linkAnno, "'.</li>
               <li>Click on a column name on the jitter plot to see a reduced jitter plot with values from only this column.</li>
          </ul>")),
          p(str_c("The default tooltips can be overridden by having a row or column annotation called '", tooltipAnno, "'. Below the jitter plot, a table is shown that includes all data on the plot and, in addition, information about missing values. Row and column IDs in the table are changed into links if '", linkAnno, "' appears among annotations.")),
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
					HTML("Available clustering distances:<ul>
               <li>correlation - Pearson correlation subtracted from 1</li><li>Euclidean - square root of sum of square distances</li>
               <li>maximum - greatest absolute difference between coordinates</li><li>Manhattan - sum of the absolute differences</li>
               <li>Canberra - weighted Manhattan distance</li>
               <li>binary - matrix is binarized (non-zero to 1, zero to 0), number of bits which are 1/0 or 0/1 divided by number of bits which are 0/1, 1/0 or 1/1</li>
          </ul>"),
					#http://support.minitab.com/en-us/minitab/17/topic-library/modeling-statistics/multivariate/item-and-cluster-analyses/linkage-methods/
					HTML("Available linkage methods:<ul><li>single linkage - using two closest objects from two clusters to be merged</li><li>complete linkage - using two farthest objects</li><li>average - average distance of all possible pairs</li><li>McQuitty - average distance of the two clusters (to be merged) to the cluster of interest</li><li>median - median distance of all possible pairs</li><li>centroid - distance between cluster means</li><li>Ward linkage - using sum of squared differences from points to centroids as the distance</li></ul>"),
					h5("Separate editions", id = "editions"),
					p("There is a", a("separate ClustVis edition", href = "http://biit.cs.ut.ee/clustvis_large/", target = "_blank"), "available which has higher limits for size of the uploaded data."),
					p("Also, a", a("Docker image", href = "https://hub.docker.com/r/taunometsalu/clustvis/", target = "_blank"), "is available in Docker Hub which makes it easier to run ClustVis locally. See", a("GitHub page", href = "https://github.com/taunometsalu/ClustVis", target = "_blank"), "for more information about the local installation."),
					p("If you want to automate ClustVis plot generation, the best way is to use ClustVis R package. See", a("GitHub page", href = "https://github.com/taunometsalu/ClustVis", target = "_blank"), "for more information."),
          footer
				),
				tabPanel("News",
					h5("Version history:"),
					p("20th December 2018 - fixed a bug when clustering is not applied on heatmap (thank you, Niladri Bhusan Pati, for letting us know about it!)."),
					p("17th December 2018 - added some text to help page."),
					p("10th December 2018 - added option to export data using ClustVis R package."),
					p("29th October 2018 - fixed a bug with transposing input matrix."),
					p("15th October 2018 - added option to change margin ratio for PCA plot."),
					p("8th October 2018 - added option to change quote and missing value type in the input data."),
					p("1st October 2018 - added option to apply logarithmic transformation to the original data."),
					p("21st May 2018 - ClustVis plots can now we generated using an R package, see ", a("GitHub page", href = "https://github.com/taunometsalu/ClustVis", target = "_blank"), " for instructions."),
					p("19th February 2018 - major code refactoring to separate main functions; fix a bug about setting PCA plot axis limits (thank you, Abby Benninghoff, for letting us know about it!); values outside color range on the heatmap are now appearing with the same color as the color range min or max (thank you, Abby Benninghoff, for the idea!); option to change heatmap legend colors added."),
					p("11th December 2017 - some bug fixes and optimizations."),
					p("23rd October 2017 - updated Shiny server version and several packages. Decreased minimum heatmap plot width."),
					p("4th April 2017 - added option to use grayscale colors for PCA plot groups (thank you, Julian R. Marchesi, for the idea!)."),
					p("13th February 2017 - fixed a bug when reading in a dataset with both integers and non-integers in the numeric matrix."),
					p("30th January 2017 - added ClustVis logo."),
					p("16th June 2016 - increased the maximum number of heatmap annotation levels for large data edition; a small bug fix."),
          p("3rd May 2016 - there is now a", a("separate edition", href = "http://biit.cs.ut.ee/clustvis_large/", target = "_blank"), "of ClustVis with higher limits. Also, a", a("Docker image", href = "https://hub.docker.com/r/taunometsalu/clustvis/", target = "_blank"), "is now available in Docker Hub which makes it easier to run ClustVis locally. See", a("GitHub page", href = "https://github.com/taunometsalu/ClustVis", target = "_blank"), "for more information about the local installation."),
					p("17th February 2016 - option 'change all levels' for annotations based filtering, jitter plot x-axis names link to reduced jitter plot, table is shown now below jitter plot, now always adding links to heatmap cells if row and column names have them, automatic calculation of jitter plot height improved, font of the interactive plots changed to Arial, fixed jitter plot tooltips with NA values and jitter plot with aggregated values, option to show violin plot or box plot without points, 'Data upload' tab renamed to 'Data import' to better reflect the meaning, slightly modified heatmap caption text, loading message added if calculation is in process, general logic of the tabs added to the help page, fixed showing filtering options after transposing, external links open on a new tab without necessarily holding down 'Ctrl' key, annotations with many levels removed automatically from the heatmap."),
					p("18th January 2016 - fix 'show imputed values' to show scaled heatmap when unchecked, option to use a custom gene list when subsetting ArrayExpress dataset, message about gene names that were not present in the dataset, limit for maximum number of components to be calculated (for performance reasons), warning message about maximum uploaded file size added, pathway genes sorted alphabetically, shiny package updated to 0.13."),
					p("8th January 2016 - major restructuring: interactive mode introduced (see help page), row annotations introduced (thank you, Sven-Eric Schelhorn, for the idea!), extended input file format and related changes to allow row annotations for the heatmap (including row filtering, different detection of annotation lines, modified help page), colored labels for annotation based filtering, annotation groups introduced, option to remove rows and/or columns with many missing values, option to remove constant columns, option to transpose heatmap, option to show missing values on heatmap, option to set general font size on heatmap, font of annotation legend automatically scaled down if it doesn't fit to the plot, option to add space between clusters on heatmap, corrected Ward linkage method added, some data format checks and warning messages, limit for maximum number of annotation levels shown on PCA plot and heatmap, footer changed into a link, Shiny Server updated to 1.4.1.759 and Shiny package updated to 0.12.2, some custom changes in the pheatmap R code merged to the main branch of pheatmap R package."),
					p("27th May 2015 - ClustVis is now running on ", HTML("<a href='https://www.docker.com/' target='_blank'>Docker</a>;"), " reference to published article added to the footer."),
					p("21st April 2015 - option to transpose the data matrix added under 'Data upload'; maximal heatmap dimension increased to 1200; a small bug fixed related with uploading a dataset without annotations."),
					p("17th April 2015 - second revision based on comments from reviewers: number of species increased to 17; more informative error messages for data upload; number of NAs in rows and columns is shown during upload; help page improved (including list of all datasets and pathways)."),
					p("6th April 2015 - small improvements related with option 'import prepared gene expression matrix'."),
					p("1st April 2015 - major revision based on comments from reviewers: some example datasets removed; it is possible to cluster whole gene expression dataset first using k-means or select one k-means cluster; some warning messages added; Bayesian PCA removed; PCA and heatmap options grouped; percentages shown together with axis labels; color and shape can be changed independently on PCA plot; help page improved a lot; example captions added for PCA plot and heatmap; new export options added; heatmap default color changed."),
					p("9th February 2015 - some optimization and help text added when importing dataset from ArrayExpress."),
					p("6th February 2015 - heatmap tree ordering options added; it is possible to choose different linkage method for rows and columns of heatmap; you can choose color range of the heatmap manually; heatmap annotation titles can be switched off; organism filtering added when importing public datasets; some bug fixes."),
					p("19th December 2014 - output interpretation added to the help page; better default size for row and column names on the heatmap."),
					p("18th December 2014 - you can aggregate similar annotations on 'pre-processing' tab; you can hide row and/or column names on heatmap; you can set minimum number of annotations when choosing from the list of public datasets."),
					p("15th December 2014 - column filtering for all import options added; number of annotations added to dataset name; more flexible plot width."),
					p("4th December 2014 - improved search for MEM datasets (you don't have to choose platform or pathway database first). Filtering option of less informative annotations and sample filtering added."),
					p("21st November 2014 - import from MEM added; some new example datasets; improved help page; number of rows and columns is now shown on upload tab."),
					p("14th November 2014 - option to download plot in different formats added; line type and width for ellipses added."),
					p("12th November 2014 - option to save settings added; and some other new features."),
					p("31st October 2014 - first version online.")
				),
				id = "tabs1"
			)
		)
	), title = titleFull
)
)
