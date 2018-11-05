#Author: Tauno Metsalu
#Copyright: 2018 University of Tartu

path = "/srv/shiny-server/" #path of this file
libPath = "/usr/local/lib/R/site-library/" #path of R libraries
sessPathLarge = "/srv/settings_large/" #where to save settings with large datasets
sessPath = "/srv/settings/" #where to save settings
sessPathExternal = "/srv/settings_external/" #where to save settings for externally saved datasets
pbPathPrefix = "/srv/data_pb/" #path of MEM files

source("/srv/shiny-server/Rpackage/R/clustvis.R")

.libPaths(libPath)
suppressPackageStartupMessages({
  library(stringr)
  library(RNetCDF)
  library(shiny)
  library(shinyBS)
  library(reshape2)
  library(Hmisc)
  library(RColorBrewer)
  library(FactoMineR)
  library(pcaMethods)
  library(gProfileR)
  library(plyr)
  library(gtable)
  library(ggplot2)
  library(Cairo) #nicer ggplot2 output
  library(XML)
  library(grid)
  library(gridSVG)
  library(shinyjs)
  library(svglite) #faster SVG generation
})

toolname = "clustvis"
fakeAnno = " " #placeholder annotation (if annotations are missing)
noClust = "no clustering"
changeAll = "change all levels" #label for row/column filtering menu
clustDists = c(noClust, "correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
names(clustDists) = convertIdsToNames(clustDists)
clustDists = clustDists[clustDists != "minkowski"] #it is already covered by special cases
clustMethods = c("single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2", "ward.D")
names(clustMethods) = convertIdsToNames(clustMethods)
treeOrderings = c("tightest cluster first", 
  "higher median value first", "higher mean value first", 
  "lower median value first", "lower mean value first",
  "original", "reverse original")
colortab = brewer.pal.info #all RColorBrewer colors
colSequential = rownames(colortab)[colortab$category == "seq"]
colDiverging = rownames(colortab)[colortab$category == "div"]
colQualitative = rownames(colortab)[colortab$category == "qual"]
schemeListHM = c(colSequential, colDiverging)
names(schemeListHM) = str_c(c(rep("Sequential", length(colSequential)), rep("Diverging", length(colDiverging))), ": ", schemeListHM)
schemeListPCA = c(colQualitative, "Grayscale")
procMeth = c("svdImpute", "nipals", "bpca", "ppca")
names(procMeth) = convertIdsToNames(procMeth)
procMeth = procMeth[procMeth != "bpca"] #gives too many errors
procScalings = c("none", "uv", "pareto", "vector")
names(procScalings) = convertIdsToNames(procScalings)
procMethAgg = c("no collapse", "median", "mean")
lineTypeList = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
tooltipPlace = "right"
tooltipOptions = list(container = "body") #https://github.com/ebailey78/shinyBS/issues/15
useSelectize = FALSE #https://github.com/ebailey78/shinyBS/issues/7
shapeList = c("letters", "various")
#reserved annotation names:
tooltipAnno = "plot_tooltip"
linkAnno = "plot_link"
interactivityAnnos = c(tooltipAnno, linkAnno)
titleSufix = "" #sufix in the title for other editions

maxCharactersAnnotations = 20 #how many characters to show for long annotations - to cut too long names
maxDimensionHeatmap = 1200 #how large matrix we allow for heatmap (clustering and plotting a large matrix will be slow)

#maximum number of tooltips allowed on different types of plot, to avoid very slow rendering
#if greater than that, falls back to static plot and gives warning message
maxTooltipsPCA = 400
maxTooltipsHm = 80
maxTooltipsJitterPlot = 100

maxLabelLengthJitterplot = 35 #maximum length of the label on jitterplot (in characters), to avoid very long names when columns are aggregated
maxUploadMB = 2 #maximum uploaded file size in MB

logUsage = TRUE #whether to log size of datasets
logFile = str_c(sessPath, "shiny-server-matrix-size.log")
if(logUsage && !file.exists(logFile)) cat(str_c(c("time", "input", "nrow", "ncol", "edition"), collapse = ";"), "\n", append = TRUE, file = logFile)

#R package parameters:
maxComponents = 100 #maximum number of principal components to calculate (more will make it too slow)
#maximum number of levels allowed for color and shape on PCA plot and heatmap (too many levels can make rendering slow and output ugly):
maxColorLevels = 8 #for PCA plot
maxShapeLevels = 62 #for PCA plot
maxAnnoLevels = 30 #for heatmap

#override parameters if running different edition of ClustVis
find = c("UA-63396304-1", "UA-63396304-2", "UA-63396304-3")
replace = c("original", "test", "large data")
clustvisEdition = mapvalues(Sys.getenv("SHINY_GAID"), find, replace, warn_missing = FALSE)
if(!(clustvisEdition %in% replace)) clustvisEdition = "custom"
if(clustvisEdition != "original"){
  maxDimensionHeatmap = 2400
  maxTooltipsPCA = 600
  maxTooltipsHm = 120
  maxTooltipsJitterPlot = 150
  maxUploadMB = 15
  maxAnnoLevels = 50
  titleSufix = str_c(" - ", clustvisEdition, " edition")
}

#http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
options(shiny.maxRequestSize = maxUploadMB * 1024 ^ 2)
options(shiny.sanitize.errors = FALSE) #avoid generic error messages
options(stringsAsFactors = FALSE)
setwd(sessPath)

gprofDate = "20150416" #"20150205" #gprofOntos file
pwDate = "20150416" #"20150205" #clustvisInput file
pwPath = str_c(path, "datasets/")
load(file = str_c(pwPath, "clustvisInput_", pwDate, ".RData"))
str = strsplit(projectBrowserPath, "/")[[1]]
projectBrowserPath = str_c(pbPathPrefix, str[length(str)], "/")
load(file = str_c(pwPath, "clustvisInput_", pwDate, "_helpTables.RData"))
helpTablesOptions = list(rownames = FALSE, lengthMenu = c(5, 10, 25, 50), pageLength = 5) #options for help page tables and table below jitterplot
uploadInputOptions = list("Load sample data" = 1, "Upload file" = 2,
  "Paste data" = 3, "Import public dataset from ArrayExpress" = 5, 
  "Load saved settings" = 4, "Import prepared gene expression matrix" = 6)
if(clustvisEdition == "custom"){
  uploadInputOptions[[which(uploadInputOptions == 5)]] = NULL
}

allAnno = "All annotations" #name for the default annotation group
#change font to Arial:
fonts = getSVGFonts()
fonts$sans = "Arial"
setSVGFonts(fonts)

#javascript functions for tooltips
#http://stackoverflow.com/questions/10687131/jquery-select-by-attribute-using-and-and-or-operators
jsCode = "
shinyjs.activateTooltips = function(){
  $('[data-toggle=\"tooltip\"]').tooltip({container: 'body', html: true, placement: right});
};
shinyjs.hideTooltips = function(){
  $('[data-toggle=\"tooltip\"]').tooltip('hide');
};
shinyjs.addHandlers = function(){
  $('[data-toggle=\"tooltip\"][class=\"hmRow\"],[data-toggle=\"tooltip\"][class=\"hmCol\"],[data-toggle=\"tooltip\"][class=\"hmCell\"]').click(function() {
    Shiny.onInputChange('hmClickedObject', this.id);
    Shiny.onInputChange('hmClickedType', $(this).attr('class'));
  });
  $('[data-toggle=\"tooltip\"][class=\"pcaRow\"],[data-toggle=\"tooltip\"][class=\"pcaCol\"],[data-toggle=\"tooltip\"][class=\"pcaCell\"]').click(function() {
    Shiny.onInputChange('pcaClickedObject', this.id);
    Shiny.onInputChange('pcaClickedType', $(this).attr('class'));
  });
}
shinyjs.changeClicked = function(message){
  Shiny.onInputChange(message.prefix + 'ClickedObject', message.newObject);
  Shiny.onInputChange(message.prefix + 'ClickedType', message.newType);
}
"

#convert checkboxGroup to boolean
#since tooltips for checkbox() don't show nicely (only appear if you hover over checkbox itself, not label)
toBoolean = function(x){
	!is.null(x)
}

#find number of annotations
nAnno = function(anno){
  if(is.null(anno)){
    n = 0
  } else {
    n = ncol(anno)
  }
  n
}

#find prefix based on the tab
findPrefix = function(tab){
  if(tab == "Heatmap"){
    prefix = "hm"
  } else if(tab == "PCA"){
    prefix = "pca"
  } else {
    prefix = "othertab"
  }
  prefix
}

#cut long strings
#s - vector of strings
#len - maximum length allowed remaining (after adding three dots)
cutLong = function(s, len){
	lens = str_length(s)
	w = which(lens > len)
	s[w] = str_c(str_sub(s[w], 1, len - 3), "...")
	s
}

#cut long labels
#split by comma first, then keep the beginning of each part
#three dots account for one character
#only for y-axis label because x-labels could be made non-unique accidentally
cutLabels = function(s, len){
  sep = ", "
  replacement = "."
  s = as.vector(s)
  spl = strsplit(s, sep)
  lenSeps = (max(sapply(spl, length)) - 1) * nchar(sep) #total length of separators
  lenPieces = len - lenSeps
  lenEachPiece = floor(min(sapply(spl, function(x) lenPieces / length(x))))
  if(lenEachPiece < 2){
    res = str_c(str_sub(s, 1, len - 1), "...")
  } else {
    res = sapply(spl, function(x){
      add = ifelse(nchar(x) > lenEachPiece - 1, replacement, "")
      res = str_c(str_c(str_sub(x, 1, lenEachPiece - 1), add), collapse = sep)
    })
  }
  res
}

#remove technical annotations that are used for tooltips and links
removeTechnical = function(anno){
  anno[, !(colnames(anno) %in% interactivityAnnos), drop = FALSE]
}

#add a line to log with dataset details
sendToLog = function(data){
  inp = data$inputSaved$uploadDataInput
  v = c(format(Sys.time()), inp, nrow(data$mat), ncol(data$mat), clustvisEdition)
  cat(str_c(v, collapse = ";"), "\n", append = TRUE, file = logFile, sep = "")
}

#empty if group changed, otherwise keep previous settings
#type - "Col" or "Row"
findSaved = function(data, values, type){
  oppositeType = ifelse(type == "Col", "Row", "Col")
  oldType = ifelse(toBoolean(data$inputSaved$uploadMatrixTranspose), oppositeType, type)
  if(!identical(data[[str_c("annoGroups", type)]], values[[str_c("annoGroups", oldType, "Old")]])){
    saved = list()
  } else {
    saved = data$inputSaved
  }
  saved
}

#find which columns or rows are retained after filtering
#boolean vector
findRetainedAfterFiltering = function(anno, input, type, mat){
  anno = removeTechnical(anno)
  if(is.null(anno)){
    if(is.null(mat)){
      return(NULL)
    } else if(type == "Column"){
      return(rep(TRUE, ncol(mat)))
    } else {
      return(rep(TRUE, nrow(mat)))
    }
  }
	cn = colnames(anno)
	wRetain = rep(TRUE, length(rownames(anno))) #by default, all columns or rows are retained
	if(toBoolean(input[[str_c("upload", type, "FilteringAnno")]])){
		for(i in 1:length(cn)){
			name = str_c("upload", type, "FiltersAnno", i, "track")
			if(toBoolean(input[[name]])){
				fSub = input[[str_c(name, "sub")]]
        fSub[fSub == ""] = NA #to account correctly for missing values
				w = (anno[[i]] %in% fSub)
				wRetain = wRetain & w
			}
		}
	}
	wRetain
}

#values of checkboxes "change all levels"
getChangeAllCheckboxes = function(inputSaved, annoCol, annoRow){
  if(!is.null(annoCol)){
    labelsCol = str_c("uploadColumnFiltersAnno", 1:ncol(annoCol))
  } else {
    labelsCol = NULL
  }
  if(!is.null(annoRow)){
    labelsRow = str_c("uploadRowFiltersAnno", 1:ncol(annoRow))
  } else {
    labelsRow = NULL
  }
  labels = c(labelsCol, labelsRow)
  if(is.null(labels)) return(NULL)
  bool = sapply(labels, function(x) toBoolean(inputSaved[[str_c(x, "trackChangeAll")]]))
  names(bool) = labels
  bool
}

#show only nr first rows and nc first columns of a matrix
#replace other rows/columns with three dots (...)
cutMatrix = function(mat, nr = 50, nc = 50, digits = 2){
	if(is.null(mat)) return(NULL)
	mat = as.matrix(mat)
	mat = formatC(mat, format = "f", digits = digits)
	dots = "..."
	if(nrow(mat) > nr){
		mat = mat[1:nr, , drop = FALSE]
		mat = rbind(mat, rep(dots, ncol(mat)))
		rownames(mat)[nrow(mat)] = dots
	}
	if(ncol(mat) > nc){
		mat = mat[, 1:nc, drop = FALSE]
		mat = cbind(mat, rep(dots, nrow(mat)))
		colnames(mat)[ncol(mat)] = dots
	}
	mat
}

#convert ID to safe one
convert2safe = function(id){
	#http://stackoverflow.com/questions/3210393/how-do-i-remove-all-non-alphanumeric-characters-from-a-string-except-dash
  if(id == "") id = "empty"
	str_replace_all(id, "[^a-zA-Z0-9_]+", "_")
}

writeOutput = function(d, file){
  write.csv(d, file)
}

#indices where the string has the specified prefix
searchPrefix = function(prefix, names){
  which(str_sub(names, 1, str_length(prefix)) %in% prefix)
}

#checks whether there is an annotation with a given name
hasAnno = function(anno, name){
  !is.null(anno) && (name %in% colnames(anno))
}

#add links to the table below jitterplot if available among annotations
addLinks = function(v, anno){
  if(hasAnno(anno, linkAnno)){
    m = match(v, rownames(anno))
    v2 = str_c(str_c(str_c(str_c("<a href=\"", anno[m, linkAnno]), "\" target=\"_blank\">"), v), "</a>")
  } else {
    v2 = v
  }
  v2
}


dataProcess = function(data){
	if(is.null(data$inputSaved) | is.null(data$mat)) return(NULL)
	set.seed(124987234)
	inputSaved = data$inputSaved
	
	if(inputSaved$procTransformation == "no transformation"){
	  transformation = NA
	} else {
	  transformation = inputSaved$procTransformation
	}
	annoColKeep = inputSaved$procAnno
	if(inputSaved$procMethodAgg == "no collapse"){
	  annoColMethodAgg = NA
	} else {
	  annoColMethodAgg = inputSaved$procMethodAgg
	}
	maxNaRows = inputSaved$procMaxNaPercRows / 100
	maxNaCols = inputSaved$procMaxNaPercCols / 100
	remConstCols = toBoolean(inputSaved$procRemConstCols)
	rowCentering = toBoolean(inputSaved$procCentering)
	rowScaling = inputSaved$procScaling
	pcaMethod = inputSaved$procMethod
	
	if(is.na(annoColMethodAgg) || length(annoColKeep) == 0){
	  #not aggregating
	  annoColMethodAgg = NA
	  annoColKeep = c(annoColKeep, intersect(colnames(data$annoCol), interactivityAnnos))
	}
	
	l = processData(data, transformation = transformation, annoColKeep = annoColKeep, annoColMethodAgg = annoColMethodAgg, maxNaRows = maxNaRows, maxNaCols = maxNaCols, remConstCols = remConstCols, rowCentering = rowCentering, rowScaling = rowScaling, pcaMethod = pcaMethod, maxComponents = maxComponents)
	l$inputSaved = inputSaved
	l
}

convertAnno = function(cn){
  cnFiltered = cn
  if(is.null(cnFiltered)){
    cnFiltered = fakeAnno
  }
  names(cnFiltered) = cutLong(cnFiltered, maxCharactersAnnotations)
  cnFiltered
}

updateProcOptions = function(session, annoCol, annoGroupsCol){
  annoCol = removeTechnical(annoCol)
  if(!is.null(annoCol)){
    cn = colnames(annoCol)
  } else {
    cn = fakeAnno
  }
  gr = names(annoGroupsCol)
  grSel = gr[1]
  cnSel = unlist(annoGroupsCol[grSel])
  names(cnSel) = NULL
  updateCheckboxGroupInput(session, "procAnnoGroups", choices = gr, selected = grSel)
  updateCheckboxGroupInput(session, "procAnno", choices = cn, selected = cnSel)
}

updatePcaOptions = function(session, mat, input){
  cnFiltered = convertAnno(input$procAnno)
  if(length(cnFiltered) > 1 || cnFiltered != fakeAnno){
    colorSel = cnFiltered[1]
  } else {
    colorSel = NULL
  }
  if(length(cnFiltered) > 1){
    shapeSel = cnFiltered[2]
  } else {
    shapeSel = NULL
  }
  if(!is.null(mat)){
    npc = min(c(dim(mat), maxComponents))
  } else {
    npc = 1
  }
  updateSelectInput(session, "pcaPcx", choices = as.character(1:npc), selected = "1")
  updateSelectInput(session, "pcaPcy", choices = as.character(1:npc), selected = "2")
  updateCheckboxGroupInput(session, "pcaAnnoColor", choices = cnFiltered, selected = colorSel)
  updateCheckboxGroupInput(session, "pcaAnnoShape", choices = cnFiltered, selected = shapeSel)
}

updateHmOptions = function(session, mat, cnr, cnc){
  cnRow = convertAnno(cnr)
  cnCol = convertAnno(cnc)
  cnRowSel = cnRow
  if(cnCol[1] == fakeAnno){
    cnColSel = NULL
  } else {
    cnColSel = cnCol
  }
  updateNumericInput(session, "hmCutreeClustersRows", max = ifelse(!is.null(mat), nrow(mat), 1))
  updateNumericInput(session, "hmCutreeClustersCols", max = ifelse(!is.null(mat), ncol(mat), 1))
  updateCheckboxGroupInput(session, "hmAnnoRow", choices = cnRow, selected = cnRowSel)
  updateCheckboxGroupInput(session, "hmAnnoCol", choices = cnCol, selected = cnColSel)
  if(!is.null(mat)){
    hmOptions = calculateHmOptions(mat)
    updateNumericInput(session, "hmColorRangeMax", value = hmOptions$colorRangeMax)
    updateNumericInput(session, "hmColorRangeMin", value = hmOptions$colorRangeMin)
    updateSliderInput(session, "hmFontSizeRownames", value = hmOptions$fontSizeRownames)
    updateSliderInput(session, "hmFontSizeColnames", value = hmOptions$fontSizeColnames)
  }
}

filterRows = function(session, mat, input, organism, annoGroupsCol){
  if(input$uploadRowFiltering == 1 & input$uploadPbPathway != ""){
    pwDb = strsplit(input$uploadPbPathway, ":")[[1]][1]
    pwFile = str_c(pwPath, "gprofOntos_", gprofDate, "_", organism, "_", pwDb, ".RData")
    load(pwFile)
    rlist2 = rlist[rlist$term == input$uploadPbPathway, ]
    glist = rlist2$gene
  } else if(input$uploadRowFiltering == 4){
    glist = strsplit(input$uploadGeneList, "\\s+")[[1]]
  } else if(input$uploadRowFiltering %in% 2:3 & input$uploadNbrClusters >= 2 & input$uploadNbrClusters <= maxDimensionHeatmap){
    set.seed(52710953)
    km = kmeans(mat, centers = input$uploadNbrClusters)
    if(input$uploadRowFiltering == 2){
      mat = km$centers
      rownames(mat) = str_c("Cluster ", 1:nrow(mat), " (", km$size, " genes)")
    } else if(input$uploadRowFiltering == 3){
      mat = mat[km$cluster == input$uploadClusterId, , drop = FALSE]
      glist = rownames(mat)
    }
  } else {
    mat = NULL
  }
  message = NULL
  if((input$uploadRowFiltering == 1 & input$uploadPbPathway != "") | 
       (input$uploadRowFiltering == 4) |
       (input$uploadRowFiltering == 3 & input$uploadNbrClusters >= 2 & input$uploadNbrClusters <= 600)){
    if(input$uploadDataInput == 5){
      platf = strsplit(input$uploadPbDataset, "/")[[1]][1]
      targetPlatform = uploadPlatformTable$name[uploadPlatformTable$id == platf]
      safegconvert = failwith(data.frame(), gconvert, TRUE)
      gcon = safegconvert(glist, organism = organism, target = targetPlatform)
      glist2 = toupper(glist)
      if(nrow(gcon) > 0){
        gcon = gcon[order(gcon$alias.number, gcon$target.number), ] #keep original order
        if(all.is.numeric(rownames(mat))){
          rownames(mat) = str_c(targetPlatform, ":", rownames(mat))
          glist2 = str_c(targetPlatform, ":", glist2)
        }
        m = match(toupper(rownames(mat)), toupper(gcon$target)) #different in newer gProfileR version
        w = which(!is.na(m))
        w = w[order(m[!is.na(m)])] #sort based on original order
        if(length(w) > 0){
          mat = mat[w, , drop = FALSE]
          rownames(mat) = str_c(gcon[m[w], "name"], " (", rownames(mat), ")")
          if(input$uploadRowFiltering == 1){
            mat = mat[order(rownames(mat)), , drop = FALSE] #sort pathway genes alphabetically
          }
        } else {
          mat = NULL
        }
      } else {
        mat = NULL
      }
      glistRm = glist[which(!(glist2 %in% gcon$alias))]
    } else {
      mat = mat[rownames(mat) %in% glist, , drop = FALSE]
      glistRm = glist[!(glist %in% rownames(mat))]
    }
    if(length(glistRm) > 0){
      message = str_c("The following genes are not present in the dataset and were removed: ", 
                      str_c(glistRm, collapse = ", "), ".")
    }
  }
  list(mat = mat, message = message)
}

findCounts = function(v){
  if(is.null(v)) return(v)
  if(class(v) == "factor"){
    #assume there are no NAs if factor
    tab = table(v)
  } else {
    v2 = sort(as.vector(v), na.last = FALSE)
    v2[is.na(v2)] = ""
    tab = table(factor(v2, levels = unique(v2)))
  }
  tab
}

annotationsFilters = function(anno, inputSaved, type, groups){
  anno = removeTechnical(anno)
  taglist = list()
  if(!is.null(anno)){
    grnames = names(groups)
    names(grnames) = str_c("from '", grnames, "'")
    lens = sapply(groups, length)
    id = str_c("upload", type, "FilterGroupsAnno")
    rb = radioButtons(id, NULL, choices = grnames, selected = inputSaved[[id]])
    if(length(grnames) > 1){
      taglist[[1]] = rb
    } else {
      taglist[[1]] = conditionalPanel(condition = "false", rb) #hidden if only one group
    }
    idgr = id
    for(j in 1:length(grnames)){
      if(grnames[j] %in% inputSaved[[idgr]]){
        cn = groups[[j]]
        cnCut = cutLong(cn, maxCharactersAnnotations)
        names(cn) = str_c("by '", cnCut, "'")
        for(i in 1:length(cn)){
          m = match(cn[i], colnames(anno))
          idtrack = str_c("upload", type, "FiltersAnno", m, "track")
          cnts = findCounts(v = anno[, cn[i]])
          uni = names(cnts)
          uniCut = cutLong(uni, maxCharactersAnnotations)
          names(uni) = str_c(str_c(str_c(str_c("- ", uniCut), " ("), cnts), ")")
          isolate({
            sel = inputSaved[[idtrack]]
            if(is.null(sel) || (sel != cn[i])){
              selSub = uni
              selChange = changeAll
            } else {
              selSub = inputSaved[[str_c(idtrack, "sub")]]
              selChange = inputSaved[[str_c(idtrack, "ChangeAll")]]
            }
          })
          taglist[[2 * m]] = checkboxGroupInput(idtrack, NULL, choices = cn[i], selected = sel)
          taglist[[2 * m + 1]] = conditionalPanel(condition = str_c("input.", idtrack, " != ''"),
            checkboxGroupInput(str_c(idtrack, "ChangeAll"), 
              NULL, choices = changeAll, selected = selChange),
            checkboxGroupInput(str_c(idtrack, "sub"), NULL, choices = uni, selected = selSub)
          )
        }
      }
    }
  }
  taglist
}

#create titles for the tooltips
#Column ID: ... if there is only one column
#Number of columns: ... if multiple columns are aggregated
#or "columns" is changed to "rows" if rows == TRUE
getTooltipTitles = function(mapping, names, rows = FALSE){
  if(is.null(mapping) || all(mapping$orig == mapping$agg)){
    titles = str_c(changeRowsCols("Column", rows), " ID: ", names)
  } else {
    tab = table(factor(mapping$agg, levels = unique(mapping$agg)))
    titles = str_c("Number of ", changeRowsCols("columns", rows), ": ", as.vector(tab))
  }
  titles
}

#generate text for tooltips
getTooltipTexts = function(anno, titles){
  linebr = "<br />"
  if(is.null(anno)){
    tooltips = titles
  } else if(tooltipAnno %in% colnames(anno)){
    tooltips = anno[[tooltipAnno]]
  } else if(all(colnames(anno) %in% interactivityAnnos)){
    tooltips = titles
  } else {
    anno2 = removeTechnical(anno)
    anno3 = apply(anno2, 1, function(x) str_c(str_c(str_c(colnames(anno2), ": "), x), collapse = linebr))
    tooltips = str_c(str_c(titles, linebr), unname(anno3))
  }
  tooltips
}

plotPCA = function(data){
	if(is.null(data)) return(list(NULL, 0, 0, message = NULL))
  inputSaved = data$inputSaved
  pcx = as.numeric(inputSaved$pcaPcx)
  pcy = as.numeric(inputSaved$pcaPcy)
  switchDirX = as.numeric(inputSaved$pcaSwitchDir)[1]
  switchDirY = as.numeric(inputSaved$pcaSwitchDir)[2]
  colorAnno = inputSaved$pcaAnnoColor
  colorScheme = inputSaved$pcaColor
  showEllipses = toBoolean(inputSaved$pcaShowEllipses)
  ellipseConf = inputSaved$pcaEllipseConf
  ellipseLineWidth = inputSaved$pcaEllipseLineWidth
  ellipseLineType = inputSaved$pcaEllipseLineType
  shapeAnno = inputSaved$pcaAnnoShape
  shapeScheme = inputSaved$pcaShape
  plotWidth = inputSaved$pcaPlotWidth
  plotRatio = inputSaved$pcaPlotRatio
  marginRatio = inputSaved$pcaMarginRatio
  pointSize = inputSaved$pcaPointSize
  legendPosition = inputSaved$pcaLegendPosition
  fontSize = inputSaved$pcaFontSize
  axisLabelPrefix = inputSaved$pcaAxisLabelPrefix
  showVariance = toBoolean(inputSaved$pcaShowVariance)
  interactivity = toBoolean(inputSaved$pcaInteractivity)
  if(interactivity){
    showSampleIds = FALSE #no IDs shown in interactive mode
  } else {
    showSampleIds = toBoolean(inputSaved$pcaShowSampleIds)
  }
  
  generatePCA(proc = data, pcx = pcx, pcy = pcy, switchDirX = switchDirX, switchDirY = switchDirY, colorAnno = colorAnno, colorScheme = colorScheme, showEllipses = showEllipses, ellipseConf = ellipseConf, ellipseLineWidth = ellipseLineWidth, ellipseLineType = ellipseLineType, shapeAnno = shapeAnno, shapeScheme = shapeScheme, plotWidth = plotWidth, plotRatio = plotRatio, marginRatio = marginRatio, pointSize = pointSize, legendPosition = legendPosition, fontSize = fontSize, axisLabelPrefix = axisLabelPrefix, showVariance = showVariance, showSampleIds = showSampleIds, maxColorLevels = maxColorLevels, maxShapeLevels = maxShapeLevels)
}

plotHeatmap = function(data){
	if(is.null(data)) return(frame())
  inputSaved = data$inputSaved
	nbrClustersRows = inputSaved$hmCutreeClustersRows
	nbrClustersCols = inputSaved$hmCutreeClustersCols
	colorAnnoRow = inputSaved$hmAnnoRow
	colorAnnoCol = inputSaved$hmAnnoCol
	legendColorScheme = inputSaved$hmLegendScheme
	plotWidth = inputSaved$hmPlotWidth
	plotRatio = inputSaved$hmPlotRatio
	colorRangeMin = inputSaved$hmColorRangeMin
	colorRangeMax = inputSaved$hmColorRangeMax
	matrixColorScheme = inputSaved$hmColorScheme
	revScheme = toBoolean(inputSaved$hmRevScheme)
	cellBorder = inputSaved$hmCellBorder
	if(cellBorder == "no border") cellBorder = NA
	fontSizeGeneral = inputSaved$hmFontSizeGeneral
	showNumbers = toBoolean(inputSaved$hmShowNumbers)
	fontSizeNumbers = inputSaved$hmFontSizeNumbers
	precisionNumbers = inputSaved$hmPrecisionNumbers
	showRownames = toBoolean(inputSaved$hmShowRownames)
	fontSizeRownames = inputSaved$hmFontSizeRownames
	showColnames = toBoolean(inputSaved$hmShowColnames)
	fontSizeColnames = inputSaved$hmFontSizeColnames
	showAnnoTitlesRow = toBoolean(inputSaved$hmShowAnnoTitlesRow)
	showAnnoTitlesCol = toBoolean(inputSaved$hmShowAnnoTitlesCol)
  
	createHeatmap(clust = data, nbrClustersRows = nbrClustersRows, nbrClustersCols = nbrClustersCols, colorAnnoRow = colorAnnoRow, colorAnnoCol = colorAnnoCol, legendColorScheme = legendColorScheme, plotWidth = plotWidth, plotRatio = plotRatio, colorRangeMin = colorRangeMin, colorRangeMax = colorRangeMax, matrixColorScheme = matrixColorScheme, revScheme = revScheme, cellBorder = cellBorder, fontSizeGeneral = fontSizeGeneral, showNumbers = showNumbers, fontSizeNumbers = fontSizeNumbers, precisionNumbers = precisionNumbers, showRownames = showRownames, fontSizeRownames = fontSizeRownames, showColnames = showColnames, fontSizeColnames = fontSizeColnames, showAnnoTitlesRow = showAnnoTitlesRow, showAnnoTitlesCol = showAnnoTitlesCol, maxAnnoLevels = maxAnnoLevels)
}

#table below the plot when clicked on heatmap row or column or cell or PCA point
tableJitter = function(data, original, plotType){
  set.seed(123)
  if(is.null(data$matFinal)) return(NULL)
  inputSaved = data$inputSaved
  object = inputSaved[[str_c(plotType, "ClickedObject")]]
  clickedType = inputSaved[[str_c(plotType, "ClickedType")]]
  matFinal = data$matFinal
  if(!(clickedType %in% c("hmRow", "hmCol", "hmCell", "pcaCol", "pcaCell"))) return(frame())
  
  str = strsplit(object, "-")[[1]]
  if(clickedType == "hmRow"){
    rowIds = as.numeric(str[2])
    colIds = 1:ncol(matFinal)
    nextLinks = str_c(str_c(str_c("hmCell-", rowIds), "-"), colIds)
  } else if(clickedType %in% c("hmCol", "pcaCol")){
    if((clickedType == "pcaCol") & toBoolean(inputSaved$hmTransposeHeatmap)){
      rowIds = 1:ncol(matFinal)
    } else {
      rowIds = 1:nrow(matFinal)
    }
    colIds = as.numeric(str[2])
    nextLinks = str_c(str_c(str_c(str_c(plotType, "Cell-"), rowIds), "-"), colIds)
  } else if(clickedType %in% c("hmCell", "pcaCell")){
    rowIds = as.numeric(str[2])
    colIds = as.numeric(str[3])
    nextLinks = NULL
  }
  mapping = data$mappingCol
  
  if(clickedType %in% c("hmRow", "hmCol", "hmCell")){
    if(!is.na(data$hcRows)){
      rowIds = data$hcRows$order[rowIds]
    }
    if(!is.na(data$hcCols)){
      colIds = data$hcCols$order[colIds]
    }
    if(toBoolean(inputSaved$hmTransposeHeatmap)){
      temp = colIds; colIds = rowIds; rowIds = temp
    }
  }
  
  if(toBoolean(inputSaved$hmTransposeHeatmap)){
    mapping = data$mappingRow
    matFinal = t(matFinal)
  }
  
  xlab = rownames(matFinal)[rowIds]
  ylab = colnames(matFinal)[colIds]
  ylabOriginal = mapping$origName[mapping$aggName %in% ylab]
  sub = original[xlab, ylabOriginal, drop = FALSE]
  
  rowClicked = (length(ylab) >= length(xlab)) #one cell is also like a row
  if(rowClicked){
    temp = ylab; ylab = xlab; xlab = temp
    sub = t(sub)
  }
  points = melt(sub, varnames = c("rn", "cn"))
  if(rowClicked){
    points$gr = mapping$aggName[match(ylabOriginal, mapping$origName)]
    points$gr = factor(points$gr, levels = xlab)
  } else {
    points$gr = points$rn
  }  
  
  if(all(points$rn %in% colnames(original)) && all(points$cn %in% rownames(original))){
    points$rnOrig = points$cn
    points$cnOrig = points$rn
  } else if(all(points$rn %in% rownames(original)) && all(points$cn %in% colnames(original))){
    points$rnOrig = points$rn
    points$cnOrig = points$cn
  } else {
    return(NULL)
  }
  list(points = points, xlab = xlab, ylab = ylab, 
       plotType = plotType, nextLinks = nextLinks, inputSaved = inputSaved, test = list())
}

#plot when clicked on heatmap row or column or cell or PCA point
plotJitter = function(data){
  points = data$points
  xlab = data$xlab
  ylab = data$ylab
  plotType = data$plotType
  nextLinks = data$nextLinks
  inputSaved = data$inputSaved
  if(is.null(points)) return(frame())
  xLev = unique(points$gr)
  n = length(xLev)
  baseSize = 20
  constDecrease = 0.175 #how fast font size decreases if more than 50 groups
  xSize = ifelse(n <= 50, baseSize, max(0.1, baseSize - (n - 50) * constDecrease))
  if(n == 1){
    xProp = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = xSize)
  } else {
    xProp = element_text(angle = 90, vjust = 0.5, hjust = 1, size = xSize)
  }
  ylab = cutLabels(ylab, maxLabelLengthJitterplot)
  
  #dimensions:
  pdf("Rplots.pdf")
  maxw = max(strwidth(xLev, units = 'in', cex = xSize / 12), na.rm = TRUE) #longest x-axis name
  dev.off(); unlink("Rplots.pdf")
  if(length(xLev) == 1) maxw = strheight(xLev, units = 'in', cex = xSize / 12) / 2.54 #horizontal x-label
  pichIn = 15 / 2.54 + maxw
  picwIn = 30 / 2.54
  roundingDigits = 2
  picw = round(picwIn * 72, roundingDigits)
  pich = round(pichIn * 72, roundingDigits)
  
  q = ggplot(points, aes(gr, value)) + theme_bw(base_size = baseSize) +
  labs(x = NULL, y = ylab) + theme(axis.text.x = xProp,
    plot.margin = unit(c(1, 0, 0.5, 0), "cm"))
  if(inputSaved[[str_c(plotType, "JitterPlotType")]] %in% c("violin", "violin dots")){
    q = q + geom_violin()
  } else if(inputSaved[[str_c(plotType, "JitterPlotType")]] %in% c("box", "box dots")){
    q = q + geom_boxplot()
  }
  jitterWidth = ifelse(inputSaved[[str_c(plotType, "JitterSeparateOverlapping")]] == "jittering", 
                       inputSaved[[str_c(plotType, "JitterJitteringWidth")]], 0)
  if(inputSaved[[str_c(plotType, "JitterPlotType")]] %in% c("violin", "box")){
    alpha = 0 #points not shown
  } else if(inputSaved[[str_c(plotType, "JitterSeparateOverlapping")]] == "transparency"){
    alpha = 1 - inputSaved[[str_c(plotType, "JitterTransparency")]]
  } else {
    alpha = 1 #black points
  }
  q = q + geom_jitter(position = position_jitter(height = 0, width = jitterWidth), size = 5, alpha = alpha)
  list(q = q, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn, 
              points = points, plotType = plotType, nextLinks = nextLinks)
}


