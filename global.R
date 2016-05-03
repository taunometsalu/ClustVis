#Author: Tauno Metsalu
#Copyright: 2016 University of Tartu

path = "/srv/shiny-server/" #path of this file
libPath = "/usr/local/lib/R/site-library/" #path of R libraries
sessPathLarge = "/srv/settings_large/" #where to save settings with large datasets
sessPath = "/srv/settings/" #where to save settings
pbPathPrefix = "/srv/data_pb/" #path of MEM files

.libPaths(libPath)
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
library(Hmisc)
library(gtable)
library(ggplot2)
library(Cairo) #nicer ggplot2 output
library(XML)
library(grid)
library(gridSVG)
library(shinyjs)
library(svglite) #faster SVG generation
library(pheatmap)

toolname = "clustvis"
fakeAnno = " " #placeholder annotation (if annotations are missing)
#http://stackoverflow.com/questions/2129952/creating-a-plot-window-of-a-particular-size
#"Mointors usually display 72 or 96 pixels per inch"
coef = 96 / 72
dotsPerCm = coef * 72 / 2.54 #how many points per cm
noClust = "no clustering"
changeAll = "change all levels" #label for row/column filtering menu
clustDists = c(noClust, "correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
names(clustDists) = c(noClust, "correlation", "Euclidean", "maximum", "Manhattan", "Canberra", "binary", "Minkowski")
clustDists = clustDists[clustDists != "minkowski"] #it is already covered by special cases
clustMethods = c("single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2", "ward.D")
names(clustMethods) = c("single", "complete", "average", "McQuitty", "median", "centroid", "Ward", "Ward (unsquared distances)")
treeOrderings = c("tightest cluster first", 
  "higher median value first", "higher mean value first", 
  "lower median value first", "lower mean value first",
  "original", "reverse original")
charlist = c(LETTERS, letters, 0:9)
#http://stackoverflow.com/questions/1478532/changing-shapes-used-for-scale-shape-in-ggplot2
variouslist = c(16, 15, 17:18, 1, 0, 2:14)
colortab = brewer.pal.info #all RColorBrewer colors
colSequential = rownames(colortab)[colortab$category == "seq"]
colDiverging = rownames(colortab)[colortab$category == "div"]
colQualitative = rownames(colortab)[colortab$category == "qual"]
annoLegendColors = brewer.pal(9, "Set1")[-6] #without yellow
schemeListHM = c(colSequential, colDiverging)
names(schemeListHM) = str_c(c(rep("Sequential", length(colSequential)), rep("Diverging", length(colDiverging))), ": ", schemeListHM)
procMeth = c("svdImpute", "nipals", "bpca", "ppca")
names(procMeth) = c("SVD with imputation", "Nipals PCA", "Bayesian PCA", "Probabilistic PCA")
procMeth = procMeth[procMeth != "bpca"] #gives too many errors
procScalings = c("none", "uv", "pareto", "vector")
names(procScalings) = c("no scaling", "unit variance scaling", "Pareto scaling", "vector scaling")
procMethAgg = c("no collapse", "median", "mean")
lineTypeList = c("solid", "dashed", "dotted", "dotdash", "longdash","twodash")
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
maxComponents = 100 #maximum number of principal components to calculate (more will make it too slow)
#maximum number of tooltips allowed on different types of plot, to avoid very slow rendering
#if greater than that, falls back to static plot and gives warning message
maxTooltipsPCA = 400
maxTooltipsHm = 80
maxTooltipsJitterPlot = 100

#maximum number of levels allowed for color and shape on PCA plot and heatmap (too many levels can make rendering slow and output ugly):
maxColorLevels = 8 #for PCA plot
maxShapeLevels = 62 #for PCA plot
maxAnnoLevels = 30 #for heatmap
maxLabelLengthJitterplot = 35 #maximum length of the label on jitterplot (in characters), to avoid very long names when columns are aggregated
maxUploadMB = 2 #maximum uploaded file size in MB

logUsage = TRUE #whether to log size of datasets
logFile = str_c(sessPath, "shiny-server-matrix-size.log")
if(logUsage && !file.exists(logFile)) cat(str_c(c("time", "input", "nrow", "ncol", "edition"), collapse = ";"), "\n", append = TRUE, file = logFile)

#override parameters if running different edition of ClustVis
find = c("UA-63396304-1", "UA-63396304-2", "UA-63396304-3")
replace = c("original", "test", "large data")
clustvisEdition = mapvalues(Sys.getenv("SHINY_GAID"), find, replace , warn_missing = FALSE)
if(!(clustvisEdition %in% replace)) clustvisEdition = "custom"
if(clustvisEdition != "original"){
  maxDimensionHeatmap = 2400
  maxTooltipsPCA = 600
  maxTooltipsHm = 120
  maxTooltipsJitterPlot = 150
  maxUploadMB = 15
  titleSufix = str_c(" - ", clustvisEdition, " edition")
}

#http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
options(shiny.maxRequestSize = maxUploadMB * 1024 ^ 2)
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
  cat(str_c(v, collapse = ";"), "\n", append = TRUE, file = logFile)
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
findRetainedAfterFiltering = function(anno, input, type, mat){
  anno = removeTechnical(anno)
  if(is.null(anno)){
    if(is.null(mat)){
      return(NULL)
    } else if(type == "Column"){
      return(1:ncol(mat))
    } else {
      return(1:nrow(mat))
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
	which(wRetain)
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

calcEllipses = function(x2, conf){
	tab = table(x2$groupingColor)
	grs = names(tab[tab > 2])
	x3 = x2[which(x2$groupingColor %in% grs), c("groupingColor", "pcx", "pcy")]
	if(nrow(x3) > 0){
		x3$groupingColor = factor(x3$groupingColor) #coord.ellipse needs that
		#bary - confidence interval for the mean (TRUE) or prediction interval for the new value (FALSE)
		coord = coord.ellipse(x3, bary = FALSE, npoint = 200, level.conf = conf)$res
		coord$sample = "sampleX" #dummy to make ggplot work
	} else {
		coord = NULL
	}
	coord
}

collapseSimilarAnnoMat = function(anno, mat, fun = median){
	#http://stackoverflow.com/questions/8139301/aggregate-rows-in-a-large-matrix-by-rowname
	fun2 = function(x) apply(x, 1, fun, na.rm = TRUE)
	anno$gr = apply(anno, 1, function(x) str_c(x, collapse = ", "))
	anno2 = unique(anno)
	rownames(anno2) = NULL
	res = NULL
  mapping = NULL
	for(i in 1:length(anno2$gr)){
		w = which(anno$gr == anno2$gr[i])
		res = cbind(res, fun2(mat[, w, drop = FALSE]))
		mapping = rbind(mapping, data.frame(orig = w, agg = i, origName = colnames(mat)[w], aggName = anno2$gr[i]))
	}
	colnames(res) = anno2$gr
	rownames(anno2) = anno2$gr
	anno3 = anno2[, !(colnames(anno2) == "gr"), drop = FALSE]
	res[is.nan(res)] = NA #otherwise pcaMethods will give error
	list(anno = anno3, mat = res, mapping = mapping)
}

findNAs = function(mat, dim){
  nas = apply(mat, dim, function(x) sum(is.na(x)))
  names(nas) = dimnames(mat)[[dim]]
  w = which(nas != 0)
  if(length(w) > 0){
    return(sort(nas[w], decreasing = TRUE))
  } else {
    return(NULL)
  }
}

findSD0 = function(mat, dim){
  sds = apply(mat, dim, function(x) sd(x, na.rm = TRUE))
  dimnames(mat)[[dim]][which(is.na(sds) | (sds == 0))]
}

writeOutput = function(d, file){
  write.csv(d, file)
}

changeRowsCols = function(s, change){
  rows = c("rows", "Rows", "row", "Row")
  cols = c("columns", "Columns", "column", "Column")
  from = c(rows, cols)
  to = c(cols, rows)
  if(change){
    s = mapvalues(s, from, to, warn_missing = FALSE)
  }
  s
}

changeIfTransposed = function(s, input){
  transp = toBoolean(input$hmTransposeHeatmap)
  changeRowsCols(s, transp)
}

#calculate colors for annotation legend
calcAnnoLegendColors = function(x){
  cls = class(x)
  if(cls == "factor"){
    levs = levels(x)
  } else if(cls == "character"){
    levs = sort(unique(x))
  } else {
    return(NULL)
  }
  n = length(levs)
  
  if(n <= 8){
    cols = annoLegendColors[1:n]
  } else {
    cols = rainbow(n)
  }
  names(cols) = levs
  cols
}

#number of rows and columns of the matrix
calcSize = function(mat){
  if(is.null(mat)){
    size = data.frame(Rows = 0L, Columns = 0L)
  } else {
    size = data.frame(Rows = nrow(mat), Columns = ncol(mat))
  }
  size
}

#convert NAs to table
calcNaTable = function(mat, na, naRem){
  if(length(na) > 0){
    perc = na / ncol(mat) * 100
    perc2 = str_c(formatC(perc, format = "f", digits = 1), "%")
    rem = mapvalues(names(na) %in% naRem, c(TRUE, FALSE), c("yes", "no"), warn_missing = FALSE)
    tab = rbind(Count = na, Percentage = perc2, Removed = rem)
  } else {
    tab = NULL
  }
  tab
}

#remove factor levels that are not used
recalcFactorLevels = function(anno){
  if(!is.null(anno)){
    w = which(sapply(anno, class) == "factor")
    if(length(w) > 0){
      for(i in w){
        lev = levels(anno[, i])
        vec = as.vector(anno[, i])
        anno[, i] = factor(vec, levels = lev[lev %in% vec])
      }
    }
  }
  anno
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

#trivial mapping
defaultMapping = function(mat){
  data.frame(orig = 1:ncol(mat), agg = 1:ncol(mat), origName = colnames(mat), aggName = colnames(mat))
}


dataProcess = function(data){
	if(is.null(data$inputSaved)) return(NULL)
	set.seed(124987234)
	inputSaved = data$inputSaved
	procCentering = toBoolean(inputSaved$procCentering)
	procRemConstCols = toBoolean(inputSaved$procRemConstCols)
	annoRow = data$annoRow
  sizeTable = calcSize(data$mat)
	keep = c(inputSaved$procAnno, intersect(colnames(data$annoCol), interactivityAnnos))
  if(length(inputSaved$procAnno) > 0){
    if(inputSaved$procMethodAgg != "no collapse"){
      annoFiltered = data$annoCol[, inputSaved$procAnno, drop = FALSE]
      coll = collapseSimilarAnnoMat(annoFiltered, data$mat, get(inputSaved$procMethodAgg))
      annoCol = coll$anno
      mat = coll$mat
      mappingCol = coll$mapping
    } else {
      annoCol = data$annoCol[, keep, drop = FALSE]
      mat = data$mat
      if(!is.null(mat)){
        mappingCol = defaultMapping(mat)
      } else {
        mappingCol = NULL
      }
    }
  } else {
    mat = data$mat
    if(length(keep) > 0){
      annoCol = data$annoCol[, keep, drop = FALSE]
    } else {
      annoCol = NULL
    }
    if(!is.null(mat)){
      mappingCol = defaultMapping(mat)
    } else {
      mappingCol = NULL
    }
  }
	if(!is.null(mat)){
	  mappingRow = defaultMapping(t(mat))
	} else {
	  mappingRow = NULL
	}
	sizeTable = rbind(sizeTable, calcSize(mat))
  
	#missing values:
	#http://stackoverflow.com/questions/20669150/exclude-row-names-from-r-shiny-rendertable
	if(!is.null(mat)){
	  naRows = findNAs(mat, 1)
	  naCols = findNAs(mat, 2)
	  naRowsRem = names(naRows[naRows / ncol(mat) * 100 > inputSaved$procMaxNaPercRows])
	  naColsRem = names(naCols[naCols / nrow(mat) * 100 > inputSaved$procMaxNaPercCols])
	  naTableRows = calcNaTable(mat, naRows, naRowsRem)
	  naTableCols = calcNaTable(t(mat), naCols, naColsRem)
	  wr = which(!(rownames(mat) %in% naRowsRem))
	  wc = which(!(colnames(mat) %in% naColsRem))
    if((length(wr) == 0) | (length(wc) == 0)){
      mat = annoCol = annoRow = NULL
    } else {
      mat = mat[wr, wc, drop = FALSE]
      annoCol = annoCol[wc, , drop = FALSE]
      annoRow = annoRow[wr, , drop = FALSE]
    }
	} else {
	  naTableRows = naTableCols = naRowsRem = naColsRem = NULL
	}
	sizeTable = rbind(sizeTable, calcSize(mat))
  
	#remove constant rows and optionally columns:
	if(!is.null(mat)){
	  constRows = findSD0(mat, 1)
	  constCols = findSD0(mat, 2)
	  wr = which(!(rownames(mat) %in% constRows))
	  if(procRemConstCols){
	    wc = which(!(colnames(mat) %in% constCols))
	  } else {
	    wc = 1:ncol(mat)
	  }
	  if((length(wr) == 0) | (length(wc) == 0)){
	    mat = annoCol = annoRow = NULL
	  } else {
	    mat = mat[wr, wc, drop = FALSE]
	    annoCol = annoCol[wc, , drop = FALSE]
	    annoRow = annoRow[wr, , drop = FALSE]
	  }
	} else {
	  constRows = constCols = NULL
	}
	sizeTable = rbind(sizeTable, calcSize(mat))
	rownames(sizeTable) = c("Before processing", "After collapsing similar columns (if applied)", 
	  "After removing rows and columns with NAs", "After removing constant rows and optionally columns")
	sizeTable = t(sizeTable)
  
	if(!is.null(mat)){
	  prep = prep(t(mat), scale = inputSaved$procScaling, center = procCentering)
	  pca = pca(prep, method = inputSaved$procMethod, nPcs = min(c(dim(mat), maxComponents)))
	  matPca = scores(pca)
	  pcaLoadings = loadings(pca)
	  matImputed = t(completeObs(pca))
	  matScaled = t(prep)
	  varTable = rbind(Individual = pca@R2, Cumulative = pca@R2cum)
	  colnames(varTable) = str_c("PC", 1:ncol(varTable))
	  annoCol = recalcFactorLevels(annoCol)
	  annoRow = recalcFactorLevels(annoRow)
	  l = list(annoCol = annoCol, annoRow = annoRow, 
	           mat = mat, matPca = matPca, matScaled = matScaled, matImputed = matImputed, 
	           varTable = varTable, sizeTable = sizeTable,
	           pcaLoadings = pcaLoadings, inputSaved = inputSaved,
	           naTableRows = naTableRows, naTableCols = naTableCols, 
	           naRowsRem = naRowsRem, naColsRem = naColsRem,
	           constRows = constRows, constCols = constCols,
	           mappingCol = mappingCol, mappingRow = mappingRow)
	} else {
	  l = list(sizeTable = sizeTable, inputSaved = inputSaved,
	           naTableRows = naTableRows, naTableCols = naTableCols, 
	           naRowsRem = naRowsRem, naColsRem = naColsRem,
	           constRows = constRows, constCols = constCols)
	}
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
  if(cnFiltered != fakeAnno){
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
  digits = 3
  colRangeMax = round(max(mat, na.rm = TRUE), digits) + 10 ** (-digits)
  colRangeMin = round(min(mat, na.rm = TRUE), digits) - 10 ** (-digits)
  rowNamesSize = min(16, max(1, floor(450 / nrow(mat))))
  colNamesSize = min(16, max(1, floor(350 / ncol(mat))))
  
  updateNumericInput(session, "hmCutreeClustersRows", max = ifelse(!is.null(mat), nrow(mat), 1))
  updateNumericInput(session, "hmCutreeClustersCols", max = ifelse(!is.null(mat), ncol(mat), 1))
  updateCheckboxGroupInput(session, "hmAnnoRow", choices = cnRow, selected = cnRowSel)
  updateCheckboxGroupInput(session, "hmAnnoCol", choices = cnCol, selected = cnColSel)
  updateNumericInput(session, "hmColorRangeMax", value = colRangeMax)
  updateNumericInput(session, "hmColorRangeMin", value = colRangeMin)
  updateSliderInput(session, "hmFontSizeRownames", value = rowNamesSize)
  updateSliderInput(session, "hmFontSizeColnames", value = colNamesSize)
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
	annoCol = data$annoCol
  annoRow = data$annoRow
  matPca = data$matPca
	varTable = data$varTable
  inputSaved = data$inputSaved
  
  pcaInteractivity = toBoolean(inputSaved$pcaInteractivity)
	pcaShowSampleIds = toBoolean(inputSaved$pcaShowSampleIds)
	pcaShowEllipses = toBoolean(inputSaved$pcaShowEllipses)
	pcaShowVariance = toBoolean(inputSaved$pcaShowVariance)
	pcs = c(as.numeric(inputSaved$pcaPcx), as.numeric(inputSaved$pcaPcy))
	grColor = inputSaved$pcaAnnoColor
	grShape = inputSaved$pcaAnnoShape
	psize = inputSaved$pcaPointSize
	
	#flip axes in a unified way - to make sure that similar results show similar plot, not mirrored
	for(i in 1:2){
		if(median(matPca[, pcs[i]]) < 0){
		  matPca[, pcs[i]] = -matPca[, pcs[i]]
		}
		if(i %in% as.numeric(inputSaved$pcaSwitchDir)){
		  matPca[, pcs[i]] = -matPca[, pcs[i]]
		}
	}
	
	x2 = data.frame(pcx = matPca[, pcs[1]], pcy = matPca[, pcs[2]], sample = rownames(matPca))
	if(!is.null(annoCol)){
		m = match(rownames(matPca), rownames(annoCol))
		x2 = data.frame(x2, annoCol[m, , drop = FALSE], check.names = FALSE)
	} else {
		grColor = grShape = NULL
	}
	#if from previous dataset:
	if(!is.null(grColor) && !(grColor %in% colnames(x2))) return(list(NULL, 0, 0, message = NULL))
	if(!is.null(grShape) && !(grShape %in% colnames(x2))) return(list(NULL, 0, 0, message = NULL))
  grSep = ", "
  if(!is.null(grColor)){
    x2$groupingColor = apply(x2[grColor], 1, function(x) str_c(x, collapse = grSep))
    if((length(grColor) == 1) & (class(x2[, grColor]) == "factor")){
      x2$groupingColor = factor(x2$groupingColor, levels = levels(x2[, grColor]))
    }
    groupingTitleColor = str_c(grColor, collapse = grSep)
  } else {
    x2$groupingColor = ""
    groupingTitleColor = ""
  }
	if(!is.null(grShape)){
	  x2$groupingShape = apply(x2[grShape], 1, function(x) str_c(x, collapse = grSep))
	  if((length(grShape) == 1) & (class(x2[, grShape]) == "factor")){
	    x2$groupingShape = factor(x2$groupingShape, levels = levels(x2[, grShape]))
	  }
	  groupingTitleShape = str_c(grShape, collapse = grSep)
	} else {
	  x2$groupingShape = ""
	  groupingTitleShape = ""
	}
	nColor = length(unique(x2$groupingColor)) #number of different groups for color
	nShape = length(unique(x2$groupingShape)) #number of different groups for shape
	if(nColor > maxColorLevels){
	  return(list(NULL, 0, 0, message = str_c("You have ", nColor, " different groups for color, only up to ", maxColorLevels, " are allowed. Please change color grouping!")))
	} else if(nShape > maxShapeLevels){
	  return(list(NULL, 0, 0, message = str_c("You have ", nShape, " different groups for shape, only up to ", maxShapeLevels, " are allowed. Please change shape grouping!")))
	}
	ellCoord = calcEllipses(x2, inputSaved$pcaEllipseConf)
	showEllipses = (pcaShowEllipses & (length(grColor) > 0) & (!is.null(ellCoord)))
	
	wantedRatio = inputSaved$pcaPlotRatio
	picw = dotsPerCm * inputSaved$pcaPlotWidth #width of whole image in pixels
	margins = c(0, 0, 0, 0)
	plotw = picw - margins[2] - margins[4] #width of internal area
	ploth = wantedRatio * plotw #height of internal area
	pich = ploth + margins[1] + margins[3] #height of whole image in pixels
  picwIn = picw / 72
	pichIn = pich / 72
  
	if(showEllipses){
		xr = c(x2$pcx, ellCoord$pcx)
		yr = c(x2$pcy, ellCoord$pcy)
	} else {
		xr = x2$pcx
		yr = x2$pcy
	}
	picAdd = 5 #how many percent white space from height (in units of data) to add
	xrange = range(xr, na.rm = TRUE)
	yrange = range(yr, na.rm = TRUE)
	xdiff = xrange[2] - xrange[1]
	ydiff = yrange[2] - yrange[1]
	realRatio = ydiff / xdiff
	if(realRatio > wantedRatio){
		#increase towards x-axis
		xDiffWanted = ydiff / wantedRatio
		xAddition = (xDiffWanted - xdiff) / 2
		xrange = c(xrange[1] - xAddition, xrange[2] + xAddition)
	} else {
		#increase towards y-axis
		yDiffWanted = xdiff * wantedRatio
		yAddition = (yDiffWanted - ydiff) / 2
		yrange = c(yrange[1] - yAddition, yrange[2] + yAddition)
	}
	add = c(-1, 1) * picAdd / 100 * (yrange[2] - yrange[1])
	xrange = xrange + add
	yrange = yrange + add

	sh = inputSaved$pcaShape
	xl = str_c(inputSaved$pcaAxisLabelPrefix, pcs[1])
	yl = str_c(inputSaved$pcaAxisLabelPrefix, pcs[2])
	if(pcaShowVariance){
	  xl = str_c(xl, " (", round(varTable[1, pcs[1]] * 100, 1), "%)")
	  yl = str_c(yl, " (", round(varTable[1, pcs[2]] * 100, 1), "%)")
	}
	
	#http://stackoverflow.com/questions/11393123/controlling-ggplot2-legend-display-order
	q = ggplot(x2, aes(x = pcx, y = pcy, shape = groupingShape, colour = groupingColor, label = sample)) + 
	  xlab(xl) + ylab(yl) + xlim(xrange) + ylim(yrange) + 
	  geom_point(size = psize) + coord_fixed() + ggtitle("") + 
	  theme_bw(base_size = inputSaved$pcaFontSize) + 
	  theme(legend.position = inputSaved$pcaLegendPosition, plot.margin = unit(margins, "bigpts"))
	
	#set shape
	if(sh == "letters"){
	  q = q + scale_shape_manual(values = charlist[1:nShape])
	} else if(sh == "various"){
	  q = q + scale_shape_manual(values = variouslist[1:nShape])
	} else {
	  q = q + scale_shape_manual(values = rep(16, nShape))
	}
	
	#set color
	if(nColor <= 8 & length(grColor) > 0 & inputSaved$pcaColor != "Black"){
	  q = q + scale_colour_brewer(palette = inputSaved$pcaColor)
	} else {
	  q = q + scale_colour_manual(values = rep("black", nColor))
	}
	
	#set legend options
	q = q + labs(shape = groupingTitleShape, colour = groupingTitleColor)
	#http://stackoverflow.com/questions/11393123/controlling-ggplot2-legend-display-order
	if(groupingTitleShape != groupingTitleColor){
	  q = q + guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))
	}
	#http://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot
	if(length(grColor) == 0){
	  q = q + guides(colour = FALSE)
	}
	if(length(grShape) == 0){
	  q = q + guides(shape = FALSE)
	}
	
	#write text if no tooltips
	if(pcaShowSampleIds & !pcaInteractivity){
	  #http://stackoverflow.com/questions/18337653/remove-a-from-legend-when-using-aesthetics-and-geom-text
	  q = q + geom_text(hjust = 0.5, vjust = -1, show_guide = FALSE)
	}
	
	#add ellipses
	if(showEllipses){
	  #http://stackoverflow.com/questions/5415132/object-not-found-error-with-ggplot2-when-adding-shape-aesthetic
	  q = q + geom_path(aes(shape = NULL), data = ellCoord, size = inputSaved$pcaEllipseLineWidth, linetype = inputSaved$pcaEllipseLineType, show_guide = FALSE)
	}
	
  list(q = q, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn, 
              message = NULL, showEllipses = showEllipses)
}

calcClustering = function(mat, distance, linkage){
	#calculate distances between rows of mat and clustering
	if(distance == "no clustering"){
		return(NA)
	} else if(distance == "correlation"){
    sds = apply(mat, 1, sd, na.rm = TRUE)
    if(any(is.na(sds) | (sds == 0))){
      return("some objects have zero standard deviation, please choose a distance other than correlation!")
    }
		d = as.dist(1 - cor(t(mat)))
	} else {
		d = dist(mat, method = distance)
	}
	hclust(d, method = linkage)
}

calcOrdering = function(mat, distance, linkage, ordering){
  hc = calcClustering(mat, distance, linkage)
  if(class(hc) != "hclust") return(hc)
  if((length(unique(hc$height)) < length(hc$height)) && (ordering != "tightest cluster first")){
    return("multiple objects have same distance, only tree ordering 'tightest cluster first' is supported!")
  }
  if(!(all(hc$height == sort(hc$height)))){
    return("some clusters have distance lower than its subclusters, please choose a method other than median or centroid!")
  }
  if(ordering == "tightest cluster first"){
    return(hc) #default hclust() output
  } else if(ordering == "higher median value first"){
    wts = rank(-apply(mat, 1, median, na.rm = TRUE))
  } else if(ordering == "higher mean value first"){
    wts = rank(-rowMeans(mat, na.rm = TRUE)) #faster than apply
  } else if(ordering == "lower median value first"){
    wts = rank(apply(mat, 1, median, na.rm = TRUE))
  } else if(ordering == "lower mean value first"){
    wts = rank(rowMeans(mat, na.rm = TRUE)) #faster than apply
  } else if(ordering == "original"){
    wts = 1:nrow(mat)
  } else if(ordering == "reverse original"){
    wts = nrow(mat):1
  } else {
    return(NA)
  }
	hc2 = as.hclust(reorder(as.dendrogram(hc), wts, agglo.FUN = mean))
  hc2
}

annoLevels = function(anno){
  if(is.na(anno)){
    res = list(anno = anno, removed = NULL)
  } else {
    tab = apply(anno, 2, function(x) length(unique(x)))
    rm = names(tab[tab > maxAnnoLevels])
    if(length(rm) == 0) rm = NULL
    res = list(anno = anno[, !(colnames(anno) %in% rm), drop = FALSE], removed = rm)
  }
  res
}

plotHeatmap = function(data, filename = NA){
	if(is.null(data)) return(frame())
	matFinal = data$matFinal
  annoCol = data$annoCol
  annoRow = data$annoRow
  inputSaved = data$inputSaved
	hcRows = data$hcRows
	hcCols = data$hcCols
  
	revScheme = toBoolean(inputSaved$hmRevScheme)
	showNumbers = toBoolean(inputSaved$hmShowNumbers)
	showRownames = toBoolean(inputSaved$hmShowRownames)
	showColnames = toBoolean(inputSaved$hmShowColnames)
	showAnnoTitlesRow = toBoolean(inputSaved$hmShowAnnoTitlesRow)
	showAnnoTitlesCol = toBoolean(inputSaved$hmShowAnnoTitlesCol)
  
	if(!is.null(annoRow) && length(inputSaved$hmAnnoRow) > 0){
	  if(!all(inputSaved$hmAnnoRow %in% colnames(annoRow))) return(frame())
	  annoRow2 = annoRow[, inputSaved$hmAnnoRow, drop = FALSE]
	} else {
	  annoRow2 = NA
	}
	if(!is.null(annoCol) && length(inputSaved$hmAnnoCol) > 0){
		if(!all(inputSaved$hmAnnoCol %in% colnames(annoCol))) return(frame())
		annoCol2 = annoCol[, inputSaved$hmAnnoCol, drop = FALSE]
	} else {
		annoCol2 = NA
	}
  
	#remove annotations with large number of levels:
  alr = annoLevels(annoRow2)
  alc = annoLevels(annoCol2)
  annoRow2 = alr$anno
	annoCol2 = alc$anno
  removed = c(alr$removed, alc$removed)
  if(!is.null(removed)){
    message = str_c("The following annotations have more than ", maxAnnoLevels, " levels and were removed from the plot: '", str_c(removed, collapse = "', '"), "'.")
  } else {
    message = NULL
  }
  
	colScheme = brewer.pal(n = 7, name = inputSaved$hmColorScheme)
	if(revScheme) colScheme = rev(colScheme)
	
	picwIn = inputSaved$hmPlotWidth / 2.54
	pichIn = picwIn * inputSaved$hmPlotRatio
	picw = picwIn * 2.54 * dotsPerCm
	pich = pichIn * 2.54 * dotsPerCm
	
  nbrColors = 100
  colBreaks = seq(inputSaved$hmColorRangeMin, inputSaved$hmColorRangeMax, length.out = nbrColors + 1)
  
  if(inputSaved$hmClustDistRows != noClust){
    clRows = hcRows
  } else {
    clRows = FALSE
  }
	if(inputSaved$hmClustDistCols != noClust){
	  clCols = hcCols
	} else {
	  clCols = FALSE
	}
  
  #current implementation of pheatmap reverses the annotations:
  annoRow2 = rev(annoRow2)
  annoCol2 = rev(annoCol2)
  
  #calculate annotation colors, default ones are sometimes strange
	legendColors = c(lapply(annoRow2, calcAnnoLegendColors), lapply(annoCol2, calcAnnoLegendColors))
	legendColors = legendColors[sapply(legendColors, length) > 0] #default colors if not factor or character
  
	ph = pheatmap(matFinal,
	  annotation_row = annoRow2, annotation_col = annoCol2, annotation_colors = legendColors,
	  cluster_rows = clRows, cluster_cols = clCols,
    cutree_rows = inputSaved$hmCutreeClustersRows, cutree_cols = inputSaved$hmCutreeClustersCols,
		color = colorRampPalette(colScheme)(nbrColors), breaks = colBreaks,
		border_color = ifelse(inputSaved$hmCellBorder == "no border", NA, inputSaved$hmCellBorder),
		show_rownames = showRownames, fontsize_row = inputSaved$hmFontSizeRownames, 
		show_colnames = showColnames, fontsize_col = inputSaved$hmFontSizeColnames, 
    annotation_names_row = showAnnoTitlesRow, annotation_names_col = showAnnoTitlesCol, 
		display_numbers = showNumbers, number_format = str_c("%.", inputSaved$hmPrecisionNumbers, "f"), 
		fontsize = inputSaved$hmFontSizeGeneral, fontsize_number = inputSaved$hmFontSizeNumbers, 
    filename = filename, width = picwIn, height = pichIn, silent = is.na(filename)
	)
  
	list(ph = ph, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn, message = message)
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


