path = "/home/metsalu/ShinyApps/upload_webtool/"
#path = "/home/metsalu/ShinyApps/upload_webtool_test/"

lib = "/home/metsalu/ShinyApps/pca_431095613456015691/R/"
.libPaths(lib)
sessPath = "/home/metsalu/ShinyApps/upload_webtool_settings/" #where to save settings
sessPathLarge = "/home/metsalu/ShinyApps/upload_webtool_settings_large/" #where to save settings with large datasets

library(stringr)
options(stringsAsFactors = FALSE)
library(rCharts)
library(shiny)
library(shinyBS)
library(reshape)
library(Hmisc)

toolname = "clustvis"
#http://stackoverflow.com/questions/2129952/creating-a-plot-window-of-a-particular-size
#"Mointors usually display 72 or 96 pixels per inch"
coef = 96 / 72
dotsPerCm = coef * 72 / 2.54 #how many points per cm
noClust = "no clustering"
clustDists = c(noClust, "correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
names(clustDists) = c(noClust, "correlation", "Euclidean", "maximum", "Manhattan", "Canberra", "binary", "Minkowski")
clustDists = clustDists[clustDists != "minkowski"] #it is already covered by special cases
clustMethods = c("single", "complete", "average", "mcquitty", "median", "centroid", "ward")
names(clustMethods) = c("single", "complete", "average", "McQuitty", "median", "centroid", "Ward")
treeOrderings = c("tightest cluster first", 
  "higher median value first", "higher mean value first", 
  "lower median value first", "lower mean value first",
  "original", "reverse original")
charlist = c(LETTERS, letters, 0:9)
#http://stackoverflow.com/questions/1478532/changing-shapes-used-for-scale-shape-in-ggplot2
variouslist = c(16, 15, 17:18, 1, 0, 2:14)
library(RColorBrewer)
colortab = brewer.pal.info #all RColorBrewer colors
colSequential = rownames(colortab)[colortab$category == "seq"]
colDiverging = rownames(colortab)[colortab$category == "div"]
colQualitative = rownames(colortab)[colortab$category == "qual"]
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
useSelectize = FALSE #https://github.com/ebailey78/shinyBS/issues/7
shapeList = c("letters", "various")
maxCharactersAnnotations = 20 #how many characters to show for long annotations - to cut too long names
maxDimensionHeatmap = 600 #how large matrix we allow for heatmap (clustering a large matrix will be slow)
pwPath = "/home/metsalu/predect/results/cache/clustvis/"
load(file = "/home/metsalu/predect/results/cache/clustvis/clustvisInput_20150205.RData")
gprofDate = "20150205" #gprofOntos file
pwDate = "20150205" #clustvisInput file
setwd(path)
options(shiny.maxRequestSize = 2 * 1024 ^ 2) #http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user

#convert checkboxGroup to boolean
#since tooltips for checkbox() don't show nicely (only appear if you hover over checkbox itself, not label)
toBoolean = function(x){
	!is.null(x)
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

#find which columns are retained after filtering
findRetainedColumns = function(anno, input){
	cn = colnames(anno)
	wRetain = rep(TRUE, length(rownames(anno))) #by default, all columns are retained
	if(toBoolean(input[["uploadColumnFiltering"]])){
		for(i in 1:length(cn)){
			name = str_c("uploadColumnFilters", i)
			if(toBoolean(input[[name]])){
				fSub = input[[str_c(name, "sub")]]
				w = (anno[[i]] %in% fSub)
				wRetain = wRetain & w
			}
		}
	}
	which(wRetain)
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
	str_replace_all(id, "[^a-zA-Z0-9_]+", "_")
}

calcEllipses = function(x2, conf){
	library(FactoMineR)
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
	for(i in 1:length(anno2$gr)){
		w = which(anno$gr == anno2$gr[i])
		res = cbind(res, fun2(mat[, w, drop = FALSE]))
	}
	colnames(res) = anno2$gr
	rownames(anno2) = anno2$gr
	anno3 = anno2[, !(colnames(anno2) == "gr"), drop = FALSE]
	res[is.nan(res)] = NA #otherwise pcaMethods will give error
	list(anno = anno3, mat = res)
}

dataProcess = function(data){
	if(is.null(data)) return(NULL)
	set.seed(124987234)
	attach(data, warn.conflicts = FALSE)
	procCentering = toBoolean(inputSaved$procCentering)
	
	annoFiltered = anno[, inputSaved$procAnno, drop = FALSE]
	if(inputSaved$procMethodAgg != "no collapse"){
		coll = collapseSimilarAnnoMat(annoFiltered, mat, get(inputSaved$procMethodAgg))
		anno = coll$anno
		mat = coll$mat
	}
	
	#missing values in PCA:
	library(pcaMethods)
	nbrNA = apply(mat, 1, function(x) sum(is.na(x)))
	mat = mat[which(nbrNA != ncol(mat)), , drop = FALSE]
	prep = prep(t(mat), scale = inputSaved$procScaling, center = procCentering)
	pca = pca(prep, method = inputSaved$procMethod, nPcs = min(dim(mat)))
	matPca = scores(pca)
  pcaLoadings = loadings(pca)
	matImputed = t(completeObs(pca))
	varTable = rbind(Individual = pca@R2, Cumulative = pca@R2cum)
	colnames(varTable) = str_c("PC", 1:ncol(varTable))
	l = list(anno = anno, mat = mat, matPca = matPca, matImputed = matImputed, 
           varTable = varTable, pcaLoadings = pcaLoadings, inputSaved = inputSaved)
	detach(data)
	l
}

updatePcsAnnos = function(session, anno, mat, input){
	npc = min(dim(mat))
	if(!is.null(anno)){
		cn = colnames(anno)
	} else {
		cn = "Annotation"
	}
	names(cn) = cutLong(cn, maxCharactersAnnotations)
	cnFiltered = input$procAnno
	if(is.null(cnFiltered)){
    cnFiltered = cn
	}
	names(cnFiltered) = cutLong(cnFiltered, maxCharactersAnnotations)
  cnSel = cnFiltered
	updateCheckboxGroupInput(session, "procAnno", choices = cn, selected = cnSel)
	updateSelectInput(session, "pcaPcx", choices = as.character(1:npc), selected = "1")
	updateSelectInput(session, "pcaPcy", choices = as.character(1:npc), selected = "2")
	updateCheckboxGroupInput(session, "pcaAnnoColor", choices = cnFiltered, selected = cnFiltered[1])
  if(length(cnFiltered) > 1){
    shapeSel = cnFiltered[2]
  } else {
    shapeSel = NULL
  }
	updateCheckboxGroupInput(session, "pcaAnnoShape", choices = cnFiltered, selected = shapeSel)
	updateCheckboxGroupInput(session, "hmAnno", choices = cnFiltered, selected = cnFiltered)
}

updateNbrs = function(session, mat){
  digits = 3
  colRangeMax = round(max(mat, na.rm = TRUE), digits) + 10 ** (-digits)
  colRangeMin = round(min(mat, na.rm = TRUE), digits) - 10 ** (-digits)
  rowNbrSize = min(16, max(1, floor(450 / nrow(mat))))
  colNbrSize = min(16, max(1, floor(350 / ncol(mat))))
  updateNumericInput(session, "hmColorRangeMax", value = colRangeMax)
  updateNumericInput(session, "hmColorRangeMin", value = colRangeMin)
  updateSliderInput(session, "hmFontSizeRownames", value = rowNbrSize)
  updateSliderInput(session, "hmFontSizeColnames", value = colNbrSize)
}

filterRows = function(session, anno, mat, input, organism){
  #row filtering
  if(input$uploadRowFiltering == 1 & input$uploadPbPathway != ""){
    pwDb = strsplit(input$uploadPbPathway, ":")[[1]][1]
    pwFile = str_c(pwPath, "gprofOntos_", gprofDate, "_", organism, "_", pwDb, ".RData")
    load(pwFile)
    rlist2 = rlist[rlist$term == input$uploadPbPathway, ]
    glist = rlist2$gene
  } else if(input$uploadRowFiltering %in% 2:3 & input$uploadNbrClusters >= 2 & input$uploadNbrClusters <= 600){
    set.seed(52710953)
    km = kmeans(mat, centers = input$uploadNbrClusters)
    if(input$uploadRowFiltering == 2){
      mat = km$centers
      rownames(mat) = str_c("Cluster ", 1:nrow(mat), " (", km$size, " genes)")
      updatePcsAnnos(session, anno, mat, input)
    } else if(input$uploadRowFiltering == 3){
      mat = mat[km$cluster == input$uploadClusterId, , drop = FALSE]
      glist = rownames(mat)
    }
  } else {
    mat = NULL
  }
  if((input$uploadRowFiltering == 1 & input$uploadPbPathway != "") | 
       (input$uploadRowFiltering == 3 & input$uploadNbrClusters >= 2 & input$uploadNbrClusters <= 600)){
    if(input$uploadDataInput == 5){
      platf = strsplit(input$uploadPbDataset, "/")[[1]][1]
      targetPlatform = uploadPlatformTable$name[uploadPlatformTable$id == platf]
      library(gProfileR)
      library(plyr)
      safegconvert = failwith(data.frame(), gconvert, TRUE)
      gcon = safegconvert(glist, organism = organism, target = targetPlatform)
      if(nrow(gcon) > 0){
        library(Hmisc)
        if(all.is.numeric(rownames(mat))){
          rownames(mat) = str_c(targetPlatform, ":", rownames(mat))
        }
        m = match(toupper(rownames(mat)), toupper(gcon$target)) #different in newer gProfileR version
        w = which(!is.na(m))
        if(length(w) > 0){
          mat = mat[w, , drop = FALSE]
          rownames(mat) = str_c(gcon[m[w], "name"], " (", rownames(mat), ")")
          updatePcsAnnos(session, anno, mat, input)
        } else {
          mat = NULL
        }
      } else {
        mat = NULL
      }
    } else {
      mat = mat[rownames(mat) %in% glist, , drop = FALSE]
      updatePcsAnnos(session, anno, mat, input) #to change if "annotations to keep" is changed or size of row and column names if you change pathway
    }
  }
  mat
}

plotPCA = function(data){
	if(is.null(data)) return(list(NULL, 0, 0))
	attach(data)
	pcaShowSampleIds = toBoolean(inputSaved$pcaShowSampleIds)
	pcaShowEllipses = toBoolean(inputSaved$pcaShowEllipses)
	pcaShowVariance = toBoolean(inputSaved$pcaShowVariance)
	pcs = c(as.numeric(inputSaved$pcaPcx), as.numeric(inputSaved$pcaPcy))
	grColor = inputSaved$pcaAnnoColor
	grShape = inputSaved$pcaAnnoShape
	psize = inputSaved$pcaPointSize
	x = matPca
	
	#flip axes in a unified way - to make sure that similar results show similar plot, not mirrored
	for(i in 1:2){
		if(median(x[, pcs[i]]) < 0){
			x[, pcs[i]] = -x[, pcs[i]]
		}
		if(i %in% as.numeric(inputSaved$pcaSwitchDir)){
			x[, pcs[i]] = -x[, pcs[i]]
		}
	}
	
	x2 = data.frame(pcx = x[, pcs[1]], pcy = x[, pcs[2]], sample = rownames(x))
	if(!is.null(anno)){
		m = match(rownames(x), rownames(anno))
		x2 = data.frame(x2, anno[m, , drop = FALSE], check.names = FALSE)
	} else {
		grColor = grShape = NULL
	}
	if(!is.null(grColor) && !(grColor %in% colnames(x2))) return(list(NULL, 0, 0)) #if from previous dataset
	if(!is.null(grShape) && !(grShape %in% colnames(x2))) return(list(NULL, 0, 0))
	x2$groupingColor = apply(x2[grColor], 1, function(x) str_c(x, collapse = ", "))
	x2$groupingShape = apply(x2[grShape], 1, function(x) str_c(x, collapse = ", "))
	groupingTitleColor = str_c(grColor, collapse = ", ")
	groupingTitleShape = str_c(grShape, collapse = ", ")
	ellCoord = calcEllipses(x2, inputSaved$pcaEllipseConf)
	showEllipses = (pcaShowEllipses & (length(grColor) > 0) & (!is.null(ellCoord)))
	
	wantedRatio = inputSaved$pcaPlotRatio
	picw = dotsPerCm * inputSaved$pcaPlotWidth #width of whole image in pixels
	margins = c(0, 0, 0, 0)
	plotw = picw - margins[2] - margins[4] #width of internal area
	ploth = wantedRatio * plotw #height of internal area
	pich = ploth + margins[1] + margins[3] #height of whole image in pixels

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

	library(ggplot2)
	library(grid)
	sh = inputSaved$pcaShape
	nColor = length(unique(x2$groupingColor)) #number of different groups for color
	nShape = length(unique(x2$groupingShape)) #number of different groups for shape
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
	
	#write text
	if(pcaShowSampleIds){
	  #http://stackoverflow.com/questions/18337653/remove-a-from-legend-when-using-aesthetics-and-geom-text
	  q = q + geom_text(hjust = 0.5, vjust = -1, show_guide = FALSE)
	}
	
	#add ellipses
	if(showEllipses){
	  #http://stackoverflow.com/questions/5415132/object-not-found-error-with-ggplot2-when-adding-shape-aesthetic
	  q = q + geom_path(aes(shape = NULL), data = ellCoord, size = inputSaved$pcaEllipseLineWidth, linetype = inputSaved$pcaEllipseLineType, show_guide = FALSE)
	}
	
	detach(data)
	return(list(q, pich, picw))
}

calcClustering = function(mat, distance, linkage, ordering){
	#calculate distances between rows of mat and clustering
	if(distance == "no clustering"){
		return(NA)
	} else if(distance == "correlation"){
		d = as.dist(1 - cor(t(mat)))
	} else {
		d = dist(mat, method = distance)
	}
	hc = hclust(d, method = linkage)
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
    return(NULL)
  }
	hc2 = as.hclust(reorder(as.dendrogram(hc), wts, agglo.FUN = mean))
  hc2
}

plotHeatmap = function(data, filename = NA){
	if(is.null(data)) return(frame())
	attach(data)
	revScheme = toBoolean(inputSaved$hmRevScheme)
	showNumbers = toBoolean(inputSaved$hmShowNumbers)
	showRownames = toBoolean(inputSaved$hmShowRownames)
	showColnames = toBoolean(inputSaved$hmShowColnames)
	showAnnoTitles = toBoolean(inputSaved$hmShowAnnoTitles)
	library(grid); source("/home/metsalu/Dokumendid/Ülikool/BIIT/Projektid/predect/homepage/gen/R/pheatmap.r")
	matFinal = matImputed
	if(!is.null(anno) && length(inputSaved$hmAnno) > 0){
		if(!all(inputSaved$hmAnno %in% colnames(anno))) return(frame())
		anno2 = anno[, inputSaved$hmAnno, drop = FALSE]
	} else {
		anno2 = NA
	}
	colScheme = brewer.pal(n = 7, name = inputSaved$hmColorScheme)
	if(revScheme) colScheme = rev(colScheme)
	
	picwIn = inputSaved$hmPlotWidth / 2.54
	pichIn = picwIn * inputSaved$hmPlotRatio
	picw = picwIn * 2.54 * dotsPerCm
	pich = pichIn * 2.54 * dotsPerCm
	
  nbrColors = 100
  colBreaks = seq(inputSaved$hmColorRangeMin, inputSaved$hmColorRangeMax, length.out = nbrColors + 1)
  
	ph = pheatmap(matFinal, 
    annotation = anno2, annotation_titles = showAnnoTitles,
		cluster_rows = (inputSaved$hmClustDistRows != noClust), cluster_cols = (inputSaved$hmClustDistCols != noClust), 
		clustering_distance_rows = hcRows, clustering_distance_cols = hcCols,
		color = colorRampPalette(colScheme)(nbrColors),
    breaks = colBreaks,
		border_color = ifelse(inputSaved$hmCellBorder == "no border", NA, inputSaved$hmCellBorder),
		show_rownames = showRownames, fontsize_row = inputSaved$hmFontSizeRownames, 
		show_colnames = showColnames, fontsize_col = inputSaved$hmFontSizeColnames, 
		display_numbers = showNumbers, number_format = str_c("%.", inputSaved$hmPrecisionNumbers, "f"), 
		fontsize_number = inputSaved$hmFontSizeNumbers, filename = filename,
		width = picwIn, height = pichIn
	)
	detach(data)
	return(list(ph = ph, pich = pich, picw = picw))
}
