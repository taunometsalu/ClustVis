suppressPackageStartupMessages({
  library(stringr)
  library(plyr)
  library(pcaMethods)
  library(FactoMineR)
  library(ggplot2)
  library(RColorBrewer)
  library(grid)
  library(pheatmap)
})

readFile = function(file, sep, nbrRowAnnos, nbrColAnnos){
  #guess delimiter if needed:
  sepList = c(",", "\t", ";")
  if(is.na(sep)){
    rl = readLines(file, warn = FALSE)
    rl = rl[rl != ""] #last line can be empty
    sepNbrsMin = sapply(sepList, function(x) min(str_count(rl, x))) #minimal number of separators on a line
    sep = sepList[which.max(sepNbrsMin)]
    f2 = textConnection(rl) #create new connection
  } else {
    f2 = file
  }
  
  readText = function(f, sep){
    read.table(f, sep = sep, header = TRUE, fill = TRUE, colClasses = "character", 
               check.names = FALSE, comment.char = "")
  }
  safeRead = failwith(NULL, readText, quiet = TRUE)
  message = NULL
  data = safeRead(f2, sep)
  if(!is.null(data)){
    rn = data[, 1]
    if(any(duplicated(rn))){
      #make row names unique
      rn = make.unique(rn)
      message = str_c(message, "Row names were converted because they were not unique!")
    }
    cn = colnames(data)[-1]
    if(any(duplicated(cn))){
      #make column names unique
      cn = make.unique(cn)
      message = str_c(message, "Column names were converted because they were not unique!", sep = "\n")
    }
    data = data[, -1, drop = FALSE]
    rownames(data) = rn
    colnames(data) = cn
  }
  if(is.null(data) || nrow(data) < 1 || ncol(data) < 1) return(NULL)
  data2 = apply(data, 1:2, function(x) gsub(",", ".", x))
  data2[is.na(data2)] = ""
  
  #guess border between annotations and numeric data if needed:
  if(is.na(nbrColAnnos) || is.na(nbrRowAnnos) || !is.numeric(nbrRowAnnos) || !is.numeric(nbrColAnnos) ||
     nbrRowAnnos < 0 || nbrColAnnos < 0 || nbrRowAnnos >= ncol(data) || nbrColAnnos >= nrow(data)){
    data2num = data2; suppressWarnings(class(data2num) <- "numeric")
    numericCells = !is.na(data2num) | (data2 == "")
    numericRows = rowSums(numericCells)
    numericCols = colSums(numericCells)
    nbrRowAnnos = max(c(0, which(numericCols < numericCols[ncol(data2)])))
    nbrColAnnos = max(c(0, which(numericRows < numericRows[nrow(data2)])))
    if((nbrColAnnos < nrow(data)) & (nbrRowAnnos < ncol(data))){
      data3 = data2[(nbrColAnnos + 1):nrow(data2), (nbrRowAnnos + 1):ncol(data2), drop = FALSE]
      data3int = data3; suppressWarnings(class(data3int) <- "integer") #large integers may convert to NA
      data3num = data3; suppressWarnings(class(data3num) <- "numeric")
      intCells = is.na(data3num) | ((data3int - round(data3num, 10)) == 0)
      if(!all(intCells, na.rm = TRUE)){
        #find how far the rows with integers go (which presumably belong to numeric annotations)
        #if they go till the end of the matrix, consider them as part of numeric matrix, not annotations
        intRowLast = as.vector(intCells[nrow(intCells), ])
        nbrRowAnnos = nbrRowAnnos + ifelse(all(intRowLast), 0, which(!intRowLast)[1] - 1)
        intColLast = as.vector(intCells[, ncol(intCells)])
        nbrColAnnos = nbrColAnnos + ifelse(all(intColLast), 0, which(!intColLast)[1] - 1)
      }
    }
  }
  if(nbrColAnnos == 0 | nbrRowAnnos == ncol(data)){
    annoCol = NULL
  } else {
    annoCol = as.data.frame(t(data[1:nbrColAnnos, (nbrRowAnnos + 1):ncol(data), drop = FALSE]), stringsAsFactors = FALSE)
    annoCol[is.na(annoCol)] = "NA" #to make filtering and heatmap annotations work correctly
  }
  if(nbrRowAnnos == 0 | nbrColAnnos == nrow(data)){
    annoRow = NULL
  } else {
    annoRow = as.data.frame(data[(nbrColAnnos + 1):nrow(data), 1:nbrRowAnnos, drop = FALSE], stringsAsFactors = FALSE)
    annoRow[is.na(annoRow)] = "NA"
  }
  if(nbrColAnnos == nrow(data) | nbrRowAnnos == ncol(data)){
    mat = NULL
  } else {
    mat = data2[(nbrColAnnos + 1):nrow(data2), (nbrRowAnnos + 1):ncol(data2), drop = FALSE]
    mat = apply(mat, 1:2, as.numeric)
  }
  
  list(annoCol = annoCol, annoRow = annoRow, mat = mat, sep = sep, message = message)
}

filterData = function(data, filteringRows, filteringCols, transpose){
  if(is.null(data$mat)) return(data)
  if(is.null(filteringCols)){
    wc = 1:ncol(data$mat)
  } else {
    wc = which(filteringCols)
  }
  if(is.null(filteringRows)){
    wr = 1:nrow(data$mat)
  } else {
    wr = which(filteringRows)
  }
  if(length(wc) > 0){
    data$annoCol = data$annoCol[wc, , drop = FALSE]
    if(length(wr) > 0){
      data$annoRow = data$annoRow[wr, , drop = FALSE]
      data$mat = data$mat[wr, wc, drop = FALSE]
    } else {
      data$annoRow = data$mat = NULL
    }
  } else {
    data$annoCol = data$mat = NULL
    if(length(wr) > 0){
      data$annoRow = data$annoRow[wr, , drop = FALSE]
    } else {
      data$annoRow = NULL
    }
  }
  
  if(transpose){
    data$mat = t(data$mat)
    
    #switch annotations
    temp = data$annoCol
    data['annoCol'] = list(data$annoRow)
    data['annoRow'] = list(temp)
  }
  
  data
}

importData = function(file, sep = NA, nbrRowAnnos = NA, nbrColAnnos = NA, filteringRows = NULL, filteringCols = NULL, transpose = FALSE){
  data = readFile(file = file, sep = sep, nbrRowAnnos = nbrRowAnnos, nbrColAnnos = nbrColAnnos)
  filterData(data, filteringRows = filteringRows, filteringCols = filteringCols, transpose = transpose)
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
    mapping = rbind(mapping, data.frame(orig = w, agg = i, origName = colnames(mat)[w], aggName = anno2$gr[i]), stringsAsFactors = FALSE)
  }
  colnames(res) = anno2$gr
  rownames(anno2) = anno2$gr
  anno3 = anno2[, !(colnames(anno2) == "gr"), drop = FALSE]
  res[is.nan(res)] = NA #otherwise pcaMethods will give error
  list(anno = anno3, mat = res, mapping = mapping)
}

#trivial mapping
defaultMapping = function(mat){
  data.frame(orig = 1:ncol(mat), agg = 1:ncol(mat), origName = colnames(mat), aggName = colnames(mat), stringsAsFactors = FALSE)
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

findSD0 = function(mat, dim){
  sds = apply(mat, dim, function(x) sd(x, na.rm = TRUE))
  names = dimnames(mat)[[dim]][which(is.na(sds) | (sds == 0))]
  if(length(names) == 0) names = NULL
  names
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

processData = function(data, annoColKeep = NULL, annoColMethodAgg = NA, maxNaRows = 0.9999, maxNaCols = 0.9999, remConstCols = FALSE, rowCentering = TRUE, rowScaling = "uv", pcaMethod = "svdImpute", maxComponents = 100){
  annoRow = data$annoRow
  if(is.null(annoColKeep) && !is.null(data$annoCol)){
    annoColKeep = colnames(data$annoCol)
  }
  sizeTable = calcSize(data$mat)
  if(!is.na(annoColMethodAgg)){
    #aggregate columns
    annoFiltered = data$annoCol[, annoColKeep, drop = FALSE]
    coll = collapseSimilarAnnoMat(annoFiltered, data$mat, get(annoColMethodAgg))
    annoCol = coll$anno
    mat = coll$mat
    mappingCol = coll$mapping
  } else {
    mat = data$mat
    if(!is.null(data$annoCol) && length(annoColKeep) > 0 && all(annoColKeep %in% colnames(data$annoCol))){
      annoCol = data$annoCol[, annoColKeep, drop = FALSE]
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
    naRowsRem = names(naRows[naRows / ncol(mat) > maxNaRows])
    if(length(naRowsRem) == 0) naRowsRem = NULL
    naColsRem = names(naCols[naCols / nrow(mat) > maxNaCols])
    if(length(naColsRem) == 0) naColsRem = NULL
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
    if(remConstCols){
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
  rownames(sizeTable) = c("Before processing", "After collapsing similar columns (if applied)", "After removing rows and columns with NAs", "After removing constant rows and optionally columns")
  sizeTable = t(sizeTable)
  
  if(!is.null(mat)){
    prep = prep(t(mat), scale = rowScaling, center = rowCentering)
    messages = capture.output(pca <- pca(prep, method = pcaMethod, nPcs = min(c(dim(mat), maxComponents)))) #avoid message printout
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
             varTable = varTable, pcaLoadings = pcaLoadings,
             mappingCol = mappingCol, mappingRow = mappingRow,
             sizeTable = sizeTable,
             naTableRows = naTableRows, naTableCols = naTableCols, 
             naRowsRem = naRowsRem, naColsRem = naColsRem,
             constRows = constRows, constCols = constCols)
  } else {
    l = list(sizeTable = sizeTable,
             naTableRows = naTableRows, naTableCols = naTableCols, 
             naRowsRem = naRowsRem, naColsRem = naColsRem,
             constRows = constRows, constCols = constCols)
  }
  l
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

generatePCA = function(proc, pcx = 1, pcy = 2, switchDirX = FALSE, switchDirY = FALSE, colorAnno = 1, colorScheme = "Set1", showEllipses = TRUE, ellipseConf = 0.95, ellipseLineWidth = 1, ellipseLineType = "solid", shapeAnno = 2, shapeScheme = "various", plotWidth = 20, plotRatio = 0.8, pointSize = 5, legendPosition = "right", fontSize = 20, axisLabelPrefix = "PC", showVariance = TRUE, showSampleIds = FALSE, maxColorLevels = 8, maxShapeLevels = 62, characterList = c(LETTERS, letters, 0:9), shapeList = c(16, 15, 17:18, 1, 0, 2:14)){
  pcs = c(pcx, pcy)
  switchDirs = c(switchDirX, switchDirY)
  annoCol = proc$annoCol
  annoRow = proc$annoRow
  matPca = proc$matPca
  varTable = proc$varTable
  if(is.numeric(colorAnno)) colorAnno = colnames(annoCol)[colorAnno]
  if(is.numeric(shapeAnno)) shapeAnno = colnames(annoCol)[shapeAnno]
  
  #flip axes in a unified way - to make sure that similar results show similar plot, not mirrored
  for(i in 1:2){
    if(median(matPca[, pcs[i]]) < 0){
      matPca[, pcs[i]] = -matPca[, pcs[i]]
    }
    if(i %in% switchDirs){
      matPca[, pcs[i]] = -matPca[, pcs[i]]
    }
  }
  
  x2 = data.frame(pcx = matPca[, pcs[1]], pcy = matPca[, pcs[2]], sample = rownames(matPca), stringsAsFactors = FALSE)
  if(!is.null(annoCol)){
    m = match(rownames(matPca), rownames(annoCol))
    x2 = data.frame(x2, annoCol[m, , drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    colorAnno = shapeAnno = NULL
  }
  #if from previous dataset:
  if(!is.null(colorAnno) && !(colorAnno %in% colnames(x2))) return(list(NULL, 0, 0, message = NULL))
  if(!is.null(shapeAnno) && !(shapeAnno %in% colnames(x2))) return(list(NULL, 0, 0, message = NULL))
  grSep = ", "
  if(!is.null(colorAnno)){
    x2$groupingColor = apply(x2[colorAnno], 1, function(x) str_c(x, collapse = grSep))
    if((length(colorAnno) == 1) & (class(x2[, colorAnno]) == "factor")){
      x2$groupingColor = factor(x2$groupingColor, levels = levels(x2[, colorAnno]))
    }
    groupingTitleColor = str_c(colorAnno, collapse = grSep)
  } else {
    x2$groupingColor = ""
    groupingTitleColor = ""
  }
  if(!is.null(shapeAnno)){
    x2$groupingShape = apply(x2[shapeAnno], 1, function(x) str_c(x, collapse = grSep))
    if((length(shapeAnno) == 1) & (class(x2[, shapeAnno]) == "factor")){
      x2$groupingShape = factor(x2$groupingShape, levels = levels(x2[, shapeAnno]))
    }
    groupingTitleShape = str_c(shapeAnno, collapse = grSep)
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
  ellCoord = calcEllipses(x2, ellipseConf)
  ellipses = (showEllipses & (length(colorAnno) > 0) & (!is.null(ellCoord)))
  
  dotsPerCm = 96 / 2.54 #how many points per cm
  picw = dotsPerCm * plotWidth #width of whole image in pixels
  margins = c(0, 0, 0, 0)
  plotw = picw - margins[2] - margins[4] #width of internal area
  ploth = plotRatio * plotw #height of internal area
  pich = ploth + margins[1] + margins[3] #height of whole image in pixels
  picwIn = picw / 72
  pichIn = pich / 72
  
  if(ellipses){
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
  if(realRatio > plotRatio){
    #increase towards x-axis
    xDiffWanted = ydiff / plotRatio
    xAddition = (xDiffWanted - xdiff) / 2
    xrange = c(xrange[1] - xAddition, xrange[2] + xAddition)
  } else {
    #increase towards y-axis
    yDiffWanted = xdiff * plotRatio
    yAddition = (yDiffWanted - ydiff) / 2
    yrange = c(yrange[1] - yAddition, yrange[2] + yAddition)
  }
  add = c(-1, 1) * picAdd / 100 * (yrange[2] - yrange[1])
  xrange = xrange + add
  yrange = yrange + add
  
  xl = str_c(axisLabelPrefix, pcs[1])
  yl = str_c(axisLabelPrefix, pcs[2])
  if(showVariance){
    xl = str_c(xl, " (", round(varTable[1, pcs[1]] * 100, 1), "%)")
    yl = str_c(yl, " (", round(varTable[1, pcs[2]] * 100, 1), "%)")
  }
  
  #http://stackoverflow.com/questions/11393123/controlling-ggplot2-legend-display-order
  q = ggplot(x2, aes(x = pcx, y = pcy, shape = groupingShape, colour = groupingColor, label = sample)) +
    xlab(xl) + ylab(yl) + geom_point(size = pointSize) +
    coord_fixed(xlim = xrange, ylim = yrange) + ggtitle("") +
    theme_bw(base_size = fontSize) +
    theme(legend.position = legendPosition, plot.margin = unit(margins, "bigpts"))
  
  #set shape
  if(shapeScheme == "letters"){
    q = q + scale_shape_manual(values = characterList[1:nShape])
  } else if(shapeScheme == "various"){
    q = q + scale_shape_manual(values = shapeList[1:nShape])
  } else {
    q = q + scale_shape_manual(values = rep(16, nShape))
  }
  
  #set color
  if(nColor <= 8 & length(colorAnno) > 0 & colorScheme != "Black"){
    if(colorScheme == "Grayscale"){
      #http://stackoverflow.com/questions/20125253/ggplot-2-barplot-with-a-diverging-colour-palette
      q = q + scale_colour_manual(values = rev(brewer.pal(nColor, "Greys")))
    } else {
      q = q + scale_colour_brewer(palette = colorScheme)
    }
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
  if(length(colorAnno) == 0){
    q = q + guides(colour = FALSE)
  }
  if(length(shapeAnno) == 0){
    q = q + guides(shape = FALSE)
  }
  
  #write text if no tooltips
  if(showSampleIds){
    #http://stackoverflow.com/questions/18337653/remove-a-from-legend-when-using-aesthetics-and-geom-text
    q = q + geom_text(hjust = 0.5, vjust = -1, show.legend = FALSE)
  }
  
  #add ellipses
  if(ellipses){
    #http://stackoverflow.com/questions/5415132/object-not-found-error-with-ggplot2-when-adding-shape-aesthetic
    q = q + geom_path(aes(x = pcx, y = pcy, colour = groupingColor), data = ellCoord, size = ellipseLineWidth, linetype = ellipseLineType, show.legend = FALSE, inherit.aes = FALSE)
  }
  
  list(q = q, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn, 
       message = NULL, showEllipses = ellipses)
}

savePCA = function(pca, file){
  pdf(file, width = pca$picwIn, height = pca$pichIn)
  print(pca$q)
  dev.off()
}

transposeMatrix = function(proc, showImputed, transpose){
  matImputed = proc$matImputed
  matScaled = proc$matScaled
  if(showImputed){
    matFinal = matImputed
  } else {
    matFinal = matScaled
  }
  annoCol = proc$annoCol
  annoRow = proc$annoRow
  mappingRow = proc$mappingRow
  mappingCol = proc$mappingCol
  
  if(transpose){
    if(!is.null(matImputed)){
      matImputed = t(matImputed)
      matScaled = t(matScaled)
      matFinal = t(matFinal)
    }
    temp = annoCol
    annoCol = annoRow
    annoRow = temp
    
    temp = mappingCol
    mappingCol = mappingRow
    mappingRow = temp
  }
  
  list(matFinal = matFinal, matImputed = matImputed,
       annoCol = annoCol, annoRow = annoRow,
       mappingCol = mappingCol, mappingRow = mappingRow)
}

calcClustering = function(mat, distance, linkage){
  #calculate distances between rows of mat and clustering
  if(is.na(distance)){
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
  if((length(unique(hc$height)) < length(hc$height)) && !is.na(ordering)){
    return("multiple objects have same distance, only tree ordering 'tightest cluster first' is supported!")
  }
  if(!(all(hc$height == sort(hc$height)))){
    return("some clusters have distance lower than its subclusters, please choose a method other than median or centroid!")
  }
  if(is.na(ordering)){
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

clusterMatrix = function(trans, clustDistRows, clustMethodRows, treeOrderingRows, clustDistCols, clustMethodCols, treeOrderingCols){
  if(!is.na(clustDistRows)){
    trans$hcRows = calcOrdering(trans$matImputed, clustDistRows, clustMethodRows, treeOrderingRows)
  } else {
    trans$hcRows = FALSE
  }
  if(!is.na(clustDistCols)){
    trans$hcCols = calcOrdering(t(trans$matImputed), clustDistCols, clustMethodCols, treeOrderingCols)
  } else {
    trans$hcCols = FALSE
  }
  trans
}

calculateHmOptions = function(mat, digits = 3){
  colorRangeMin = round(min(mat, na.rm = TRUE), digits) - 10 ** (-digits)
  colorRangeMax = round(max(mat, na.rm = TRUE), digits) + 10 ** (-digits)
  fontSizeRownames = min(16, max(1, floor(450 / nrow(mat))))
  fontSizeColnames = min(16, max(1, floor(350 / ncol(mat))))
  list(colorRangeMin = colorRangeMin, colorRangeMax = colorRangeMax,
       fontSizeRownames = fontSizeRownames, fontSizeColnames = fontSizeColnames)
}

annoLevels = function(anno, maxAnnoLevels){
  if(is.vector(anno) && length(anno) == 1 && is.na(anno)){
    res = list(anno = anno, removed = NULL)
  } else {
    tab = apply(anno, 2, function(x) length(unique(x)))
    rm = names(tab[tab > maxAnnoLevels])
    if(length(rm) == 0) rm = NULL
    keepCols = !(colnames(anno) %in% rm)
    if(any(keepCols)){
      annoKeep = anno[, keepCols, drop = FALSE]
    } else {
      annoKeep = NULL
    }
    res = list(anno = annoKeep, removed = rm)
  }
  res
}

#calculate colors for annotation legend
calcAnnoLegendColors = function(x, legendColorScheme){
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
    if(legendColorScheme %in% c("Set1", "Pastel1")){
      cols = brewer.pal(9, legendColorScheme)[-6] #without yellow
    } else {
      cols = brewer.pal(8, legendColorScheme)
    }
    cols = cols[1:n]
  } else {
    cols = rainbow(n)
  }
  names(cols) = levs
  cols
}

createHeatmap = function(clust, nbrClustersRows, nbrClustersCols, colorAnnoRow, colorAnnoCol, legendColorScheme, plotWidth, plotRatio, colorRangeMin, colorRangeMax, matrixColorScheme, revScheme, cellBorder, fontSizeGeneral, showNumbers, fontSizeNumbers, precisionNumbers, showRownames, fontSizeRownames, showColnames, fontSizeColnames, showAnnoTitlesRow, showAnnoTitlesCol, maxAnnoLevels){
  matFinal = clust$matFinal
  annoCol = clust$annoCol
  annoRow = clust$annoRow
  hcRows = clust$hcRows
  hcCols = clust$hcCols
  
  #filter row annotations:
  if(!is.null(annoRow) && (length(colorAnnoRow) == 1) && all(is.na(colorAnnoRow))){
    colorAnnoRow = 1:ncol(annoRow) #keep all
  }
  if(!is.null(annoRow) && length(colorAnnoRow) > 0){
    if(is.numeric(colorAnnoRow)) colorAnnoRow = colnames(annoRow)[colorAnnoRow]
    if(!all(colorAnnoRow %in% colnames(annoRow))) return(frame())
    annoRow2 = annoRow[, colorAnnoRow, drop = FALSE]
  } else {
    annoRow2 = NA
  }
  
  #filter column annotations:
  if(!is.null(annoCol) && (length(colorAnnoCol) == 1) && is.na(colorAnnoCol)){
    colorAnnoCol = 1:ncol(annoCol) #keep all
  }
  if(!is.null(annoCol) && length(colorAnnoCol) > 0){
    if(is.numeric(colorAnnoCol)) colorAnnoCol = colnames(annoCol)[colorAnnoCol]
    if(!all(colorAnnoCol %in% colnames(annoCol))) return(frame())
    annoCol2 = annoCol[, colorAnnoCol, drop = FALSE]
  } else {
    annoCol2 = NA
  }
  
  #calculate default values if needed:
  hmOptions = calculateHmOptions(matFinal)
  if(is.na(colorRangeMin)) colorRangeMin = hmOptions$colorRangeMin
  if(is.na(colorRangeMax)) colorRangeMax = hmOptions$colorRangeMax
  if(is.na(fontSizeRownames)) fontSizeRownames = hmOptions$fontSizeRownames
  if(is.na(fontSizeColnames)) fontSizeColnames = hmOptions$fontSizeColnames
  
  #remove annotations with large number of levels:
  alr = annoLevels(annoRow2, maxAnnoLevels)
  alc = annoLevels(annoCol2, maxAnnoLevels)
  annoRow2 = alr$anno
  annoCol2 = alc$anno
  removed = c(alr$removed, alc$removed)
  if(!is.null(removed)){
    message = str_c("The following annotations have more than ", maxAnnoLevels, " levels and were removed from the plot: '", str_c(removed, collapse = "', '"), "'.")
  } else {
    message = NULL
  }
  
  #image dimensions:
  picwIn = plotWidth / 2.54
  pichIn = picwIn * plotRatio
  dotsPerCm = 96 / 2.54 #how many points per cm
  picw = picwIn * 2.54 * dotsPerCm
  pich = pichIn * 2.54 * dotsPerCm
  
  #color scheme:
  colScheme = brewer.pal(n = 7, name = matrixColorScheme)
  if(revScheme) colScheme = rev(colScheme)
  nbrColors = 100
  colVec = colorRampPalette(colScheme)(nbrColors)
  colBreaks = seq(colorRangeMin, colorRangeMax, length.out = nbrColors + 1)
  
  #outside the specified range, set color to respective min/max color:
  if(colorRangeMin != hmOptions$colorRangeMin){
    colVec = c(head(colVec, 1), colVec)
    colBreaks = c(hmOptions$colorRangeMin, colBreaks)
  }
  if(colorRangeMax != hmOptions$colorRangeMax){
    colVec = c(colVec, tail(colVec, 1))
    colBreaks = c(colBreaks, hmOptions$colorRangeMax)
  }
  
  #current implementation of pheatmap reverses the annotations:
  annoRow2 = rev(annoRow2)
  annoCol2 = rev(annoCol2)
  
  #calculate annotation colors, default ones are sometimes strange
  legendColors = c(lapply(annoRow2, calcAnnoLegendColors, legendColorScheme), lapply(annoCol2, calcAnnoLegendColors, legendColorScheme))
  legendColors = legendColors[sapply(legendColors, length) > 0] #default colors if not factor or character
  
  q = pheatmap(matFinal,
               annotation_row = annoRow2, annotation_col = annoCol2, annotation_colors = legendColors,
               cluster_rows = hcRows, cluster_cols = hcCols,
               cutree_rows = nbrClustersRows, cutree_cols = nbrClustersCols,
               color = colVec, breaks = colBreaks,
               border_color = cellBorder,
               show_rownames = showRownames, fontsize_row = fontSizeRownames,
               show_colnames = showColnames, fontsize_col = fontSizeColnames,
               annotation_names_row = showAnnoTitlesRow, annotation_names_col = showAnnoTitlesCol,
               display_numbers = showNumbers, number_format = str_c("%.", precisionNumbers, "f"),
               fontsize = fontSizeGeneral, fontsize_number = fontSizeNumbers,
               width = picwIn, height = pichIn, silent = TRUE
  )
  graphics.off()
  
  list(q = q, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn, message = message)
}

generateHeatmap = function(proc, showImputed = TRUE, transpose = FALSE, clustDistRows = "correlation", clustMethodRows = "average", treeOrderingRows = NA, nbrClustersRows = 1, clustDistCols = "correlation", clustMethodCols = "average", treeOrderingCols = NA, nbrClustersCols = 1, colorAnnoRow = NA, colorAnnoCol = NA, legendColorScheme = "Set1", plotWidth = 25, plotRatio = 0.8, colorRangeMin = NA, colorRangeMax = NA, matrixColorScheme = "RdBu", revScheme = TRUE, cellBorder = "grey60", fontSizeGeneral = 10, showNumbers = FALSE, fontSizeNumbers = 12, precisionNumbers = 2, showRownames = TRUE, fontSizeRownames = NA, showColnames = TRUE, fontSizeColnames = NA, showAnnoTitlesRow = TRUE, showAnnoTitlesCol = TRUE, maxAnnoLevels = 30){
  trans = transposeMatrix(proc, showImputed = showImputed, transpose = transpose)
  clust = clusterMatrix(trans, clustDistRows = clustDistRows, clustMethodRows = clustMethodRows, treeOrderingRows = treeOrderingRows, clustDistCols = clustDistCols, clustMethodCols = clustMethodCols, treeOrderingCols = treeOrderingCols)
  createHeatmap(clust = clust, nbrClustersRows = nbrClustersRows, nbrClustersCols = nbrClustersCols, colorAnnoRow = colorAnnoRow, colorAnnoCol = colorAnnoCol, legendColorScheme = legendColorScheme, plotWidth = plotWidth, plotRatio = plotRatio, colorRangeMin = colorRangeMin, colorRangeMax = colorRangeMax, matrixColorScheme = matrixColorScheme, revScheme = revScheme, cellBorder = cellBorder, fontSizeGeneral = fontSizeGeneral, showNumbers = showNumbers, fontSizeNumbers = fontSizeNumbers, precisionNumbers = precisionNumbers, showRownames = showRownames, fontSizeRownames = fontSizeRownames, showColnames = showColnames, fontSizeColnames = fontSizeColnames, showAnnoTitlesRow = showAnnoTitlesRow, showAnnoTitlesCol = showAnnoTitlesCol, maxAnnoLevels = maxAnnoLevels)
}

saveHeatmap = function(hm, file){
  pdf(file, width = hm$picwIn, height = hm$pichIn)
  grid.draw(hm$q$gtable)
  dev.off()
}

