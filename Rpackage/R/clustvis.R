convertIdsToNames = function(s){
  map = c(
    "euclidean" = "Euclidean",
    "manhattan" = "Manhattan",
    "canberra" = "Canberra",
    "minkowski" = "Minkowski",
    "mcquitty" = "McQuitty",
    "ward.D2" = "Ward",
    "ward.D" = "Ward (unsquared distances)",
    "svdImpute" = "SVD with imputation",
    "nipals" = "Nipals PCA",
    "bpca" = "Bayesian PCA",
    "ppca" = "Probabilistic PCA",
    "none" = "no scaling",
    "uv" = "unit variance scaling",
    "pareto" = "Pareto scaling",
    "vector" = "vector scaling"
  )
  plyr::mapvalues(s, names(map), unname(map), warn_missing = FALSE)
}

changeRowsCols = function(s, change){
  rows = c("rows", "Rows", "row", "Row")
  cols = c("columns", "Columns", "column", "Column")
  from = c(rows, cols)
  to = c(cols, rows)
  if(change){
    s = plyr::mapvalues(s, from, to, warn_missing = FALSE)
  }
  s
}

#create example caption for PCA and heatmap
createCaption = function(type, info){
  leg = c()
  transp = ifelse(type == "hm", info$transpose, FALSE) #no transpose for PCA
  if(!is.na(info$transformation)){
    leg = append(leg, c("Original values are ", info$transformation, "-transformed. "))
  }
  if(!is.na(info$annoColMethodAgg)){
    leg = append(leg, c(changeRowsCols("Columns", transp), " with similar annotations are collapsed by taking ", info$annoColMethodAgg, " inside each group. "))
  }
  
  if(type == "pca"){
    sc = convertIdsToNames(info$rowScaling)
    meth = convertIdsToNames(info$pcaMethod)
    leg = append(leg, c(Hmisc::capitalize(sc), " is applied to rows; ", meth, " is used to calculate principal components. X and Y axis show principal component ", info$pcs[1], 
                        " and principal component ", info$pcs[2], " that explain ", info$variance[1], " and ", info$variance[2], " of the total variance, respectively. "))
    if(info$showEllipses){
      leg = append(leg, c("Prediction ellipses are such that with probability ", 
                          info$ellipseConf, 
                          ", a new observation from the same group will fall inside the ellipse. "))
    }
    leg = append(leg, c("N = ", info$n[2], " data points."))
  } else if(type == "hm"){
    sc = convertIdsToNames(info$rowScaling)
    scaling = paste0(sc, " is applied to ", changeRowsCols("rows", transp), ". ")
    if(info$rowCentering){
      leg = append(leg, c(changeRowsCols("Rows", transp), " are centered; ", scaling))
    } else {
      leg = append(leg, Hmisc::capitalize(scaling))
    }
    meth = convertIdsToNames(info$pcaMethod)
    if(meth == "SVD with imputation") meth = "Imputation"
    if(info$anyMissingValue){
      leg = append(leg, c(meth, " is used for missing value estimation. "))
    }
    distLabelRows = convertIdsToNames(info$clustDistRows)
    distLabelCols = convertIdsToNames(info$clustDistCols)
    linkLabelRows = convertIdsToNames(info$clustMethodRows)
    linkLabelCols = convertIdsToNames(info$clustMethodCols)
    if(!is.na(distLabelRows) && !is.na(distLabelCols) && distLabelRows == distLabelCols && linkLabelRows == linkLabelCols){
      leg = append(leg, c("Both rows and columns are clustered using ", distLabelRows, " distance and ", linkLabelRows, " linkage. "))
    } else {
      if(!is.na(distLabelRows)){
        leg = append(leg, c("Rows are clustered using ", distLabelRows, " distance and ", linkLabelRows, " linkage. "))
      }
      if(!is.na(distLabelCols)){
        leg = append(leg, c("Columns are clustered using ", distLabelCols, " distance and ", linkLabelCols, " linkage. "))
      }
    }
    n = info$n
    if(transp) n = rev(n)
    leg = append(leg, paste(paste(n, c("rows", "columns.")), collapse = ", "))
  } else {
    stop("caption type is not supported!")
  }
  paste(leg, collapse = "")
}

#read file and extract annotations
readFile = function(file, sep, nbrRowAnnos, nbrColAnnos, quotes, naString){
  #guess delimiter if needed:
  sepList = c(",", "\t", ";")
  if(is.na(sep)){
    rl = readLines(file, warn = FALSE)
    rl = rl[rl != ""] #last line can be empty
    sepNbrsMin = sapply(sepList, function(x) min(stringr::str_count(rl, x))) #minimal number of separators on a line
    sep = sepList[which.max(sepNbrsMin)]
    f2 = textConnection(rl) #create new connection
  } else {
    f2 = file
  }
  
  readText = function(f, sep){
    read.table(f, sep = sep, header = TRUE, fill = TRUE, colClasses = "character", 
               check.names = FALSE, comment.char = "", quote = quotes, na.strings = naString)
  }
  safeRead = plyr::failwith(NULL, readText, quiet = TRUE)
  message = NULL
  data = safeRead(f2, sep)
  if(!is.null(data)){
    rn = data[, 1]
    if(any(duplicated(rn))){
      #make row names unique
      rn = make.unique(rn)
      message = paste0(message, "Row names were converted because they were not unique!")
    }
    cn = colnames(data)[-1]
    if(any(duplicated(cn))){
      #make column names unique
      cn = make.unique(cn)
      message = paste0(message, "Column names were converted because they were not unique!", sep = "\n")
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

#filter data and transpose if needed
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
  structure(data, class = "imp")
}

#' Import data into ClustVis.
#' 
#' This function performs the steps shown on the 'Data import' tab of the online ClustVis.
#' 
#' @param file file name containing the input data.
#' @param sep field separator between columns. If \code{NA} (default), the separator is detected automatically.
#' @param nbrRowAnnos number of row annotations placed as the first columns in the input file. If \code{nbrRowAnnos} or \code{nbrColAnnos} is \code{NA} (default), number of row annotations is detected automatically.
#' @param nbrColAnnos number of column annotations placed as the first rows in the input file. If \code{nbrRowAnnos} or \code{nbrColAnnos} is \code{NA} (default), number of column annotations is detected automatically.
#' @param quotes quoting characters. See \code{quote} parameter from \code{read.table} function for more details.
#' @param naString string for representing a missing value. See \code{na.srings} parameter from \code{read.table} function for more details.
#' @param filteringRows logical vector showing which rows to keep. Should have the same length as the number of rows in the data matrix. \code{NULL} (default) keeps all rows.
#' @param filteringCols logical vector showing which columns to keep. Should have the same length as the number of columns in the data matrix. \code{NULL} (default) keeps all columns.
#' @param transpose whether to transpose the data matrix.
#' @return a structure to be used as input for the function \code{processData}
#' @export
importData = function(file, sep = NA, nbrRowAnnos = NA, nbrColAnnos = NA, quotes = "\"'", naString = "NA", filteringRows = NULL, filteringCols = NULL, transpose = FALSE){
  data = readFile(file = file, sep = sep, nbrRowAnnos = nbrRowAnnos, nbrColAnnos = nbrColAnnos, quotes = quotes, naString = naString)
  filt = filterData(data, filteringRows = filteringRows, filteringCols = filteringCols, transpose = transpose)
  filt
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

#collapse similar annotations
collapseSimilarAnnoMat = function(anno, mat, fun = median){
  #http://stackoverflow.com/questions/8139301/aggregate-rows-in-a-large-matrix-by-rowname
  fun2 = function(x) apply(x, 1, fun, na.rm = TRUE)
  anno$gr = apply(anno, 1, function(x) paste0(x, collapse = ", "))
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

#find rows or columns with missing values
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
    perc2 = paste0(formatC(perc, format = "f", digits = 1), "%")
    rem = plyr::mapvalues(names(na) %in% naRem, c(TRUE, FALSE), c("yes", "no"), warn_missing = FALSE)
    tab = rbind(Count = na, Percentage = perc2, Removed = rem)
  } else {
    tab = NULL
  }
  tab
}

#find rows or columns with zero standard deviation
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

#' Pre-process ClustVis data.
#' 
#' This function performs the steps shown on the 'Data pre-processing' tab of the online ClustVis.
#' 
#' @param data data structure returned by the \code{importData} function.
#' @param transformation \code{"ln(x)"} or \code{"ln(x + 1)"} to apply logarithmic tranformation, or \code{NA} (default) to use original values.
#' @param annoColKeep column names to keep or \code{NULL} (default) to keep all columns.
#' @param annoColMethodAgg column aggregation method (function name like \code{"mean"} or \code{"median"} as a string) or \code{NA} (default) to keep original columns without aggregation.
#' @param maxNaRows maximum proportion of missing values allowed in rows. Rows having a higher proportion of missing values are removed from further analysis.
#' @param maxNaCols maximum proportion of missing values allowed in columns. Columns having a higher proportion of missing values are removed from further analysis.
#' @param remConstCols whether to remove constant columns.
#' @param rowCentering whether to center rows to have zero mean.
#' @param rowScaling which method to use for scaling the rows. One from \code{"uv"} (default), \code{"pareto"}, \code{"vector"} or \code{"none"} for keeping the rows without scaling.
#' @param pcaMethod which method to use for calculating principal components. One from \code{"svdImpute"} (default), \code{"nipals"} or \code{"ppca"}.
#' @param maxComponents maximum number of PCA components to calculate.
#' @return a structure to be used as input for the functions \code{generatePCA} and \code{generateHeatmap}.
#' @export
processData = function(data, transformation = NA, annoColKeep = NULL, annoColMethodAgg = NA, maxNaRows = 0.9999, maxNaCols = 0.9999, remConstCols = FALSE, rowCentering = TRUE, rowScaling = "uv", pcaMethod = "svdImpute", maxComponents = 100){
  if(!(class(data) %in% c("imp", "NULL"))){
    stop("class of the data parameter is incorrect!")
  }
  mat = data$mat
  sizeTable = calcSize(mat)
  if(!is.na(transformation)){
    if(transformation == "ln(x)"){
      mat = log(pmax(mat, 1))
    } else if(transformation == "ln(x + 1)"){
      mat = log(pmax(mat + 1, 1))
    } else {
      stop("transformation is not supported!")
    }
  }
  annoRow = data$annoRow
  if(is.null(annoColKeep) && !is.null(data$annoCol)){
    annoColKeep = colnames(data$annoCol)
  }
  if(!is.na(annoColMethodAgg)){
    #aggregate columns
    annoFiltered = data$annoCol[, annoColKeep, drop = FALSE]
    coll = collapseSimilarAnnoMat(annoFiltered, mat, get(annoColMethodAgg))
    annoCol = coll$anno
    mat = coll$mat
    mappingCol = coll$mapping
  } else {
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
    prep = pcaMethods::prep(t(mat), scale = rowScaling, center = rowCentering)
    messages = capture.output(pca <- pcaMethods::pca(prep, method = pcaMethod, nPcs = min(c(dim(mat), maxComponents)))) #avoid message printout
    matPca = pcaMethods::scores(pca)
    pcaLoadings = pcaMethods::loadings(pca)
    matImputed = t(pcaMethods::completeObs(pca))
    matScaled = t(prep)
    varTable = rbind(Individual = pca@R2, Cumulative = pca@R2cum)
    colnames(varTable) = paste0("PC", 1:ncol(varTable))
    annoCol = recalcFactorLevels(annoCol)
    annoRow = recalcFactorLevels(annoRow)
    captionInfo = list(
      transformation = transformation,
      annoColMethodAgg = annoColMethodAgg,
      rowCentering = rowCentering,
      rowScaling = rowScaling,
      pcaMethod = pcaMethod,
      anyMissingValue = (sum(is.na(mat)) > 0),
      n = dim(matImputed)
    )
    l = list(annoCol = annoCol, annoRow = annoRow,
             mat = mat, matPca = matPca, matScaled = matScaled, matImputed = matImputed,
             varTable = varTable, pcaLoadings = pcaLoadings,
             mappingCol = mappingCol, mappingRow = mappingRow,
             sizeTable = sizeTable, captionInfo = captionInfo,
             naTableRows = naTableRows, naTableCols = naTableCols,
             naRowsRem = naRowsRem, naColsRem = naColsRem,
             constRows = constRows, constCols = constCols)
  } else {
    l = list(sizeTable = sizeTable,
             naTableRows = naTableRows, naTableCols = naTableCols,
             naRowsRem = naRowsRem, naColsRem = naColsRem,
             constRows = constRows, constCols = constCols)
  }
  structure(l, class = "proc")
}

#calculate ellipse coordinates
calcEllipses = function(x2, conf){
  tab = table(x2$groupingColor)
  grs = names(tab[tab > 2])
  x3 = x2[which(x2$groupingColor %in% grs), c("groupingColor", "pcx", "pcy")]
  if(nrow(x3) > 0){
    x3$groupingColor = factor(x3$groupingColor) #coord.ellipse needs that
    #bary - confidence interval for the mean (TRUE) or prediction interval for the new value (FALSE)
    coord = FactoMineR::coord.ellipse(x3, bary = FALSE, npoint = 200, level.conf = conf)$res
    coord$sample = "sampleX" #dummy to make ggplot work
  } else {
    coord = NULL
  }
  coord
}

#' Generate ClustVis PCA plot.
#' 
#' This function performs the steps shown on the 'PCA' tab of the online ClustVis.
#' 
#' @param proc structure returned by the \code{processData} function.
#' @param pcx principal component shown on the x-axis.
#' @param pcy principal component shown on the y-axis.
#' @param switchDirX whether to switch the direction of x-axis (reverse the sign).
#' @param switchDirY whether to switch the direction of y-axis (reverse the sign).
#' @param colorAnno the annotation(s) used for coloring and ellipses, as column numbers (if numeric) or column names (if character). If length is greater than one, unique combinations of the levels are found. If \code{NULL}, all points are black.
#' @param colorScheme coloring scheme used for the annotation groups. One from \code{"Accent"}, \code{"Dark2"}, \code{"Paired"}, \code{"Pastel1"}, \code{"Pastel2"}, \code{"Set1"} (default), \code{"Set2"}, \code{"Set3"} or \code{"Grayscale"}.
#' @param showEllipses whether to show prediction ellipses around groups.
#' @param ellipseConf confidence level used for calculating prediction ellipses.
#' @param ellipseLineWidth line width for the ellipses.
#' @param ellipseLineType line type for the ellipses. One from \code{"solid"} (default), \code{"dashed"}, \code{"dotted"}, \code{"dotdash"}, \code{"longdash"} or \code{"twodash"}.
#' @param shapeAnno the annotation(s) used for shape, as column numbers (if numeric) or column names (if character). If length is greater than one, unique combinations of the levels are found. If \code{NULL}, all points are round.
#' @param shapeScheme shape scheme used for annotation groups. One from \code{"various"} (default), \code{"letters"}, or a list of point symbol codes or characters.
#' @param plotWidth plot width in cm.
#' @param plotRatio ratio of height and width of the plotting area.
#' @param marginRatio how much white space is shown between data and plot borders, relative to y-axis data range. Margin is added after forcing the specified plot ratio.
#' @param pointSize point size.
#' @param legendPosition legend position relative to the plot.
#' @param fontSize font size.
#' @param axisLabelPrefix prefix shown for the axis labels.
#' @param showVariance whether to show percentage of variance explained by the component in the axis label.
#' @param showSampleIds whether to show sample IDs for the points shown on the plot.
#' @param maxColorLevels maximum number of levels allowed for colors.
#' @param maxShapeLevels maximum number of levels allowed for shapes.
#' @return a structure to be used as input for the function \code{savePCA}.
#' @import ggplot2
#' @export
generatePCA = function(proc, pcx = 1, pcy = 2, switchDirX = FALSE, switchDirY = FALSE, colorAnno = 1, colorScheme = "Set1", showEllipses = TRUE, ellipseConf = 0.95, ellipseLineWidth = 1, ellipseLineType = "solid", shapeAnno = 2, shapeScheme = "various", plotWidth = 20, plotRatio = 0.8, marginRatio = 0.05, pointSize = 5, legendPosition = "right", fontSize = 20, axisLabelPrefix = "PC", showVariance = TRUE, showSampleIds = FALSE, maxColorLevels = 8, maxShapeLevels = 62){
  if(!(class(proc) %in% c("proc", "NULL"))){
    stop("class of the proc parameter is incorrect!")
  }
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
  if((!is.null(colorAnno) && !(colorAnno %in% colnames(x2))) || (!is.null(shapeAnno) && !(shapeAnno %in% colnames(x2)))){
    l = list(NULL, 0, 0, message = NULL)
    return(structure(l, class = "pca"))
  }
  grSep = ", "
  if(!is.null(colorAnno)){
    x2$groupingColor = apply(x2[colorAnno], 1, function(x) paste0(x, collapse = grSep))
    if((length(colorAnno) == 1) & (class(x2[, colorAnno]) == "factor")){
      x2$groupingColor = factor(x2$groupingColor, levels = levels(x2[, colorAnno]))
    }
    groupingTitleColor = paste0(colorAnno, collapse = grSep)
  } else {
    x2$groupingColor = ""
    groupingTitleColor = ""
  }
  if(!is.null(shapeAnno)){
    x2$groupingShape = apply(x2[shapeAnno], 1, function(x) paste0(x, collapse = grSep))
    if((length(shapeAnno) == 1) & (class(x2[, shapeAnno]) == "factor")){
      x2$groupingShape = factor(x2$groupingShape, levels = levels(x2[, shapeAnno]))
    }
    groupingTitleShape = paste0(shapeAnno, collapse = grSep)
  } else {
    x2$groupingShape = ""
    groupingTitleShape = ""
  }
  nColor = length(unique(x2$groupingColor)) #number of different groups for color
  nShape = length(unique(x2$groupingShape)) #number of different groups for shape
  if(nColor > maxColorLevels){
    l = list(NULL, 0, 0, message = paste0("You have ", nColor, " different groups for color, only up to ", maxColorLevels, " are allowed. Please change color grouping!"))
    return(structure(l, class = "pca"))
  } else if(nShape > maxShapeLevels){
    l = list(NULL, 0, 0, message = paste0("You have ", nShape, " different groups for shape, only up to ", maxShapeLevels, " are allowed. Please change shape grouping!"))
    return(structure(l, class = "pca"))
  }
  ellCoord = calcEllipses(x2, ellipseConf)
  ellipses = (showEllipses & (length(colorAnno) > 0) & (!is.null(ellCoord)))
  
  #collect information for the caption
  captionInfoAdded = list(
    pcs = pcs,
    variance = paste0(round(varTable[1, pcs] * 100, 1), "%"),
    showEllipses = ellipses,
    ellipseConf = ellipseConf
  )
  captionInfo = c(proc$captionInfo, captionInfoAdded)
  
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
  add = c(-1, 1) * marginRatio * (yrange[2] - yrange[1])
  xrange = xrange + add
  yrange = yrange + add
  
  xl = paste0(axisLabelPrefix, pcs[1])
  yl = paste0(axisLabelPrefix, pcs[2])
  if(showVariance){
    xl = paste0(xl, " (", captionInfo$variance[1], ")")
    yl = paste0(yl, " (", captionInfo$variance[2], ")")
  }
  
  #http://stackoverflow.com/questions/11393123/controlling-ggplot2-legend-display-order
  q = ggplot(x2, aes(x = pcx, y = pcy, shape = groupingShape, colour = groupingColor, label = sample)) +
    xlab(xl) + ylab(yl) + geom_point(size = pointSize) +
    coord_fixed(xlim = xrange, ylim = yrange) + ggtitle("") +
    theme_bw(base_size = fontSize) +
    theme(legend.position = legendPosition, plot.margin = unit(margins, "bigpts"))
  
  #set shape
  shapeListLetters = c(LETTERS, letters, 0:9)
  shapeListVarious = c(16, 15, 17:18, 1, 0, 2:14)
  if(length(shapeScheme) > 1 || (length(shapeScheme == 1) && !(shapeScheme %in% c("letters", "various")))){
    if(length(shapeScheme) < nShape) stop("shapeScheme is too short!")
    shapeValues = shapeScheme[1:nShape]
  } else if(shapeScheme == "letters"){
    shapeValues = shapeListLetters[1:nShape]
  } else if(shapeScheme == "various"){
    shapeValues = shapeListVarious[1:nShape]
  } else {
    shapeValues = rep(16, nShape)
  }
  q = q + scale_shape_manual(values = shapeValues)
  
  #set color
  if(nColor <= 8 & length(colorAnno) > 0 & colorScheme != "Black"){
    if(colorScheme == "Grayscale"){
      #http://stackoverflow.com/questions/20125253/ggplot-2-barplot-with-a-diverging-colour-palette
      q = q + scale_colour_manual(values = rev(RColorBrewer::brewer.pal(nColor, "Greys")))
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
  
  caption = createCaption(type = "pca", info = captionInfo)
  points = data.frame(
    pcx = x2$pcx,
    pcy = x2$pcy,
    color = x2$groupingColor,
    shape = x2$groupingShape,
    label = x2$sample,
    stringsAsFactors = FALSE
  )
  l = list(q = q, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn,
           message = NULL, caption = caption, points = points)
  structure(l, class = "pca")
}

#' Save ClustVis PCA plot.
#' 
#' This function saves the PCA output.
#' 
#' @param pca structure returned by the \code{generatePCA} function.
#' @param file relative or absolute path of the output file, or \code{NA} (default) for current output device.
#' @export
savePCA = function(pca, file = NA){
  if(class(pca) != "pca"){
    stop("class of the pca parameter is incorrect!")
  }
  if(!is.null(pca$message)){
    warning(pca$message)
  }
  if(is.na(file)){
    print(pca$q)
  } else {
    pdf(file, width = pca$picwIn, height = pca$pichIn)
    print(pca$q)
    dev.off()
  }
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
  
  captionInfoAdded = list(
    transpose = transpose
  )
  captionInfo = c(proc$captionInfo, captionInfoAdded)
  
  list(matFinal = matFinal, matImputed = matImputed,
       annoCol = annoCol, annoRow = annoRow,
       mappingCol = mappingCol, mappingRow = mappingRow,
       captionInfo = captionInfo)
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
  trans$captionInfo$clustDistRows = clustDistRows
  trans$captionInfo$clustMethodRows = clustMethodRows
  trans$captionInfo$clustDistCols = clustDistCols
  trans$captionInfo$clustMethodCols = clustMethodCols
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
      cols = RColorBrewer::brewer.pal(9, legendColorScheme)[-6] #without yellow
    } else {
      cols = RColorBrewer::brewer.pal(8, legendColorScheme)
    }
    cols = cols[1:n]
  } else {
    cols = rainbow(n)
  }
  names(cols) = levs
  cols
}

createHeatmap = function(clust, nbrClustersRows, nbrClustersCols, colorAnnoRow, colorAnnoCol, legendColorScheme, plotWidth, plotRatio, colorRangeMin, colorRangeMax, matrixColorScheme, revScheme, cellBorder, fontSizeGeneral, showNumbers, fontSizeNumbers, precisionNumbers, showRownames, fontSizeRownames, showColnames, fontSizeColnames, showAnnoTitlesRow, showAnnoTitlesCol, maxAnnoLevels, ...){
  matFinal = clust$matFinal
  annoCol = clust$annoCol
  annoRow = clust$annoRow
  hcRows = clust$hcRows
  hcCols = clust$hcCols
  captionInfo = clust$captionInfo
  
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
    message = paste0("The following annotations have more than ", maxAnnoLevels, " levels and were removed from the plot: '", paste0(removed, collapse = "', '"), "'.")
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
  colScheme = RColorBrewer::brewer.pal(n = 7, name = matrixColorScheme)
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
  
  q = pheatmap::pheatmap(matFinal,
                         annotation_row = annoRow2, annotation_col = annoCol2, annotation_colors = legendColors,
                         cluster_rows = hcRows, cluster_cols = hcCols,
                         cutree_rows = nbrClustersRows, cutree_cols = nbrClustersCols,
                         color = colVec, breaks = colBreaks,
                         border_color = cellBorder,
                         show_rownames = showRownames, fontsize_row = fontSizeRownames,
                         show_colnames = showColnames, fontsize_col = fontSizeColnames,
                         annotation_names_row = showAnnoTitlesRow, annotation_names_col = showAnnoTitlesCol,
                         display_numbers = showNumbers, number_format = paste0("%.", precisionNumbers, "f"),
                         fontsize = fontSizeGeneral, fontsize_number = fontSizeNumbers,
                         width = picwIn, height = pichIn, silent = TRUE,
                         ...
  )
  graphics.off()
  
  caption = createCaption(type = "hm", info = captionInfo)
  if(class(hcRows) != "hclust"){
    wr = 1:nrow(matFinal)
  } else {
    wr = q$tree_row$order
  }
  if(class(hcCols) != "hclust"){
    wc = 1:ncol(matFinal)
  } else {
    wc = q$tree_col$order
  }
  cells = matFinal[wr, wc, drop = FALSE]
  list(q = q, pich = pich, picw = picw, pichIn = pichIn, picwIn = picwIn, message = message, caption = caption, cells = cells)
}

#' Generate ClustVis heatmap.
#' 
#' This function performs the steps shown on the 'Heatmap' tab of the online ClustVis.
#' 
#' @param proc structure returned by the \code{processData} function.
#' @param showImputed whether to show imputed values (default) or missing values on the heatmap.
#' @param transpose whether to transpose the heatmap before plotting or not (default).
#' @param clustDistRows clustering distance measure used for rows. One from \code{"correlation"} (default), \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"}, \code{"minkowski"} or \code{NA} for no clustering.
#' @param clustMethodRows linkage criterion used for rows. One from \code{"single"}, \code{"complete"}, \code{"average"}  (default), \code{"mcquitty"}, \code{"median"}, \code{"centroid"}, \code{"ward.D2"} or \code{"ward.D"}.
#' @param treeOrderingRows how to order branches of the clustering tree of rows to make it visually more attractive. One from \code{"higher median value first"}, \code{"higher mean value first"}, \code{"lower median value first"}, \code{"lower mean value first"}, \code{"original"}, \code{"reverse original"}, or \code{NA} (default) to leave the ordering unchanged.
#' @param nbrClustersRows number of clusters in rows separated by gaps.
#' @param clustDistCols clustering distance measure used for columns. One from \code{"correlation"} (default), \code{"correlation"}, \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"}, \code{"minkowski"} or \code{NA} for no clustering.
#' @param clustMethodCols linkage criterion used for columns. One from \code{"single"}, \code{"complete"}, \code{"average"}  (default), \code{"mcquitty"}, \code{"median"}, \code{"centroid"}, \code{"ward.D2"} or \code{"ward.D"}.
#' @param treeOrderingCols how to order branches of the clustering tree of columns to make it visually more attractive. One from \code{"higher median value first"}, \code{"higher mean value first"}, \code{"lower median value first"}, \code{"lower mean value first"}, \code{"original"}, \code{"reverse original"}, or \code{NA} (default) to leave the ordering unchanged.
#' @param nbrClustersCols number of clusters in columns separated by gaps.
#' @param colorAnnoRow row annotations shown left from the heatmap, as a character vector. If \code{NA} (default), all annotations are shown.
#' @param colorAnnoCol column annotations shown above the heatmap, as a character vector. If \code{NA} (default), all annotations are shown.
#' @param legendColorScheme coloring scheme used for the annotations. One from \code{"Accent"}, \code{"Dark2"}, \code{"Paired"}, \code{"Pastel1"}, \code{"Pastel2"}, \code{"Set1"} (default), \code{"Set2"} or \code{"Set3"}.
#' @param plotWidth width of the plot.
#' @param plotRatio ratio of height and width of the image.
#' @param colorRangeMin minimum of the color range or \code{NA} (default) to calculate it automatically.
#' @param colorRangeMax maximum of the color range or \code{NA} (default) to calculate it automatically.
#' @param matrixColorScheme coloring scheme used for the heatmap. One of the sequential or diverging color schemes from \code{"RColocBrewer"} package, see row names of \code{"RColorBrewer::brewer.pal.info"} for the list.
#' @param revScheme whether to reverse the coloring scheme.
#' @param cellBorder color of the cell border or \code{NA} to omit border.
#' @param fontSizeGeneral general font size.
#' @param showNumbers whether to show numbers in the cells.
#' @param fontSizeNumbers font size of the numbers.
#' @param precisionNumbers number of decimal places shown.
#' @param showRownames whether to show row names.
#' @param fontSizeRownames font size of the row names.
#' @param showColnames whether to show column names.
#' @param fontSizeColnames font size of the column names.
#' @param showAnnoTitlesRow whether to show row annotation titles.
#' @param showAnnoTitlesCol whether to show column annotation titles.
#' @param maxAnnoLevels maximum number of annotation levels.
#' @param ... further parameters passed to \code{pheatmap} function
#' @return a structure to be used as input for the function \code{saveHeatmap}.
#' @export
generateHeatmap = function(proc, showImputed = TRUE, transpose = FALSE, clustDistRows = "correlation", clustMethodRows = "average", treeOrderingRows = NA, nbrClustersRows = 1, clustDistCols = "correlation", clustMethodCols = "average", treeOrderingCols = NA, nbrClustersCols = 1, colorAnnoRow = NA, colorAnnoCol = NA, legendColorScheme = "Set1", plotWidth = 25, plotRatio = 0.8, colorRangeMin = NA, colorRangeMax = NA, matrixColorScheme = "RdBu", revScheme = TRUE, cellBorder = "grey60", fontSizeGeneral = 10, showNumbers = FALSE, fontSizeNumbers = 12, precisionNumbers = 2, showRownames = TRUE, fontSizeRownames = NA, showColnames = TRUE, fontSizeColnames = NA, showAnnoTitlesRow = TRUE, showAnnoTitlesCol = TRUE, maxAnnoLevels = 50, ...){
  if(!(class(proc) %in% c("proc", "NULL"))){
    stop("class of the proc parameter is incorrect!")
  }
  trans = transposeMatrix(proc, showImputed = showImputed, transpose = transpose)
  clust = clusterMatrix(trans, clustDistRows = clustDistRows, clustMethodRows = clustMethodRows, treeOrderingRows = treeOrderingRows, clustDistCols = clustDistCols, clustMethodCols = clustMethodCols, treeOrderingCols = treeOrderingCols)
  l = createHeatmap(clust = clust, nbrClustersRows = nbrClustersRows, nbrClustersCols = nbrClustersCols, colorAnnoRow = colorAnnoRow, colorAnnoCol = colorAnnoCol, legendColorScheme = legendColorScheme, plotWidth = plotWidth, plotRatio = plotRatio, colorRangeMin = colorRangeMin, colorRangeMax = colorRangeMax, matrixColorScheme = matrixColorScheme, revScheme = revScheme, cellBorder = cellBorder, fontSizeGeneral = fontSizeGeneral, showNumbers = showNumbers, fontSizeNumbers = fontSizeNumbers, precisionNumbers = precisionNumbers, showRownames = showRownames, fontSizeRownames = fontSizeRownames, showColnames = showColnames, fontSizeColnames = fontSizeColnames, showAnnoTitlesRow = showAnnoTitlesRow, showAnnoTitlesCol = showAnnoTitlesCol, maxAnnoLevels = maxAnnoLevels, ...)
  structure(l, class = "hm")
}

#' Save ClustVis heatmap plot.
#' 
#' This function saves the heatmap output.
#' 
#' @param hm structure returned by the \code{generateHeatmap} function.
#' @param file relative or absolute path of the output file, or \code{NA} (default) for current output device.
#' @export
saveHeatmap = function(hm, file = NA){
  if(class(hm) != "hm"){
    stop("class of the hm parameter is incorrect!")
  }
  if(!is.null(hm$message)){
    warning(hm$message)
  }
  if(is.na(file)){
    grid::grid.draw(hm$q$gtable)
  } else {
    pdf(file, width = hm$picwIn, height = hm$pichIn)
    grid::grid.draw(hm$q$gtable)
    dev.off()
  }
}

#' Export data from ClustVis.
#' 
#' This function allows to export textual data, similar to but not limited to the options on the 'Export' tab of the online ClustVis.
#' 
#' @param x structure returned by the function \code{importData}, \code{processData}, \code{generatePCA} or \code{generateHeatmap}.
#' @return a structure depending on the class of the input object
#' @export
exportData = function(x){
  cl = class(x)
  if(!(cl %in% c("imp", "proc", "pca", "hm"))){
    stop("class of the x parameter is incorrect!")
  }
  l = list()
  if(cl == "imp"){
    l$initialMessage = x$message
    l$initialMatrix = x$mat
    l$initialAnnoRow = x$annoRow
    l$initialAnnoCol = x$annoCol
  }
  if(cl == "proc"){
    l$processedMatrix = x$matImputed
    l$processedAnnoRow = x$annoRow
    l$processedAnnoCol = x$annoCol
    l$processedSize = x$sizeTable
    l$processedNaRows = x$naTableRows
    l$processedNaCols = x$naTableCols
    l$processedNaRowsRem = x$naRowsRem
    l$processedNaColsRem = x$naColsRem
    l$processedConstRows = x$constRows
    l$processedConstCols = x$constCols
    l$pcaScores = x$matPca
    l$pcaLoadings = x$pcaLoadings
    l$pcaVariance = x$varTable
  }
  if(cl == "pca"){
    l$pcaMessage = x$message
    l$pcaCaption = x$caption
    l$pcaMatrix = x$points
  }
  if(cl == "hm"){
    l$hmMessage = x$message
    l$hmCaption = x$caption
    l$hmMatrix = x$cells
  }
  l
}

