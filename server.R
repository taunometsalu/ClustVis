source("/srv/shiny-server/global.R")

shinyServer(function(input, output, session) {
	readData = reactive({
		gSep = toBoolean(input$uploadGuessSep)
		gAnno = toBoolean(input$uploadGuessAnno)
		
		if(input$uploadDataInput == 1){
			f = str_c(path, "datasets/", input$uploadSampleData)
		} else if(input$uploadDataInput == 2){
			inFile = input$uploadFile
			if (is.null(input$uploadFile))  {return(NULL)}
			f = inFile$datapath
		} else if(input$uploadDataInput == 3){
			if(is.null(input$uploadCopyPaste) || input$uploadCopyPaste == "") return(NULL)
			f = textConnection(input$uploadCopyPaste)
		} else if(input$uploadDataInput %in% c(4, 6)){
			fname = str_c("settings_", input$uploadSettingsId, ".RData")
			settingsSmall = (input$uploadDataInput == 4)
			settingsLarge = (input$uploadDataInput == 6)
			if(settingsSmall || settingsLarge){
				if(settingsSmall){
					sessPathUsed = sessPath
				} else {
					sessPathUsed = sessPathLarge
				}
				if(!(fname %in% list.files(sessPathUsed))) return(NULL)
				load(file = str_c(sessPathUsed, fname))
        inputLoaded = data$inputSaved
        mat = data$mat
        sep = data$sep
        if("annoCol" %in% names(data)){
          annoCol = data$annoCol
          annoRow = data$annoRow
          nAnnoCol = data$nAnnoCol
          nAnnoRow = data$nAnnoRow
        } else { #backward compatibility
          annoCol = data$anno
          annoRow = NULL
          nAnnoCol = data$n
          nAnnoRow = 0
        }
				annoGroupsCol = data$annoGroupsCol
				annoGroupsRow = data$annoGroupsRow
				
				#hardcoded manually - found no better way ...
				updates = list(
					updateSelectInput = c("procMethodAgg", "procScaling", "procMethod", 
						"pcaPcx", "pcaPcy", "pcaColor", "pcaShape", "pcaEllipseLineType", "pcaLegendPosition", 
						"hmClustDistRows", "hmClustDistCols", "hmClustMethodRows", "hmClustMethodCols", 
						"hmTreeOrderingRows", "hmTreeOrderingCols", "hmColorScheme", "hmCellBorder"),
					updateCheckboxGroupInput = c(
            "procAnno", "procCentering", "pcaSwitchDir", "pcaAnnoColor", "pcaAnnoShape", 
            "pcaShowVariance", "pcaShowSampleIds", "pcaShowEllipses", 
            "hmAnnoCol", "hmAnnoRow", "hmRevScheme", "hmShowNumbers", 
						"hmShowRownames", "hmShowColnames", "hmShowAnnoTitlesCol", "hmShowAnnoTitlesRow", 
            "pcaChangeDataOptions", "pcaChangeDisplayOptions", "pcaChangeColorOptions",
            "pcaChangeShapeOptions", "pcaChangeLabelOptions", "hmChangeDataOptions",
            "hmChangeDisplayOptions", "hmChangeLabelOptions", 
            "hmTransposeHeatmap", "hmShowImputed",
            "pcaInteractivity", "hmInteractivity"),
					updateRadioButtons = c(),
					updateTabsetPanel = c(),
					
					updateCheckboxInput = c(),
					updateNumericInput = c("procMaxNaPercRows", "procMaxNaPercCols", 
            "pcaPlotRatio", "pcaEllipseConf", "hmPlotRatio", 
            "hmColorRangeMax", "hmColorRangeMin", "hmCutreeClustersRows", "hmCutreeClustersCols"),
					updateSliderInput = c("pcaPointSize", "pcaPlotWidth", "pcaFontSize", "pcaEllipseLineWidth", 
					  "hmFontSizeGeneral", "hmFontSizeNumbers", "hmPrecisionNumbers", 
            "hmFontSizeRownames", "hmFontSizeColnames", "hmPlotWidth"), 
					updateTextInput = c("pcaAxisLabelPrefix"),
					updateDateInput = c(),
					
					updateDateRangeInput = c()
				)
				lens = sapply(updates, length)
				updates2 = data.frame(fun = rep(names(updates), lens), id = unlist(updates))
				
        isolate({
          updateProcOptions(session, annoCol, annoGroupsCol)
          updatePcaOptions(session, mat, inputLoaded)
          updateHmOptions(session, mat, colnames(annoRow), inputLoaded$procAnno)
          
          for(i in 1:nrow(updates2)){
            fun = updates2$fun[i]
            id = updates2$id[i]
            val = inputLoaded[[id]]
            if(id %in% names(inputLoaded)){
              if(fun == "updateCheckboxGroupInput" & is.null(val)) val = ""
              if(fun %in% c("updateSelectInput", "updateCheckboxGroupInput", "updateRadioButtons", "updateTabsetPanel")){
                get(fun)(session, id, selected = val)
              } else if(fun %in% c("updateCheckboxInput", "updateNumericInput", "updateSliderInput", "updateTextInput", "updateDateInput")){
                get(fun)(session, id, value = val)
              } else if(fun %in% c("updateDateRangeInput")){
                #not supported currently
              }
            }
          }
        })
				
				if(settingsLarge){
          fr = filterRows(session, mat, input, organism = "hsapiens", annoGroupsCol = annoGroupsCol)
					mat = fr$mat
          message = fr$message
				} else {
          message = NULL
				}
        l = list(annoCol = annoCol, annoRow = annoRow, mat = mat, 
                 nAnnoCol = nAnnoCol, nAnnoRow = nAnnoRow, sep = sep, 
                 inputSaved = input, annoLevsTab = NULL, message = message, 
                 annoGroupsCol = annoGroupsCol, annoGroupsRow = annoGroupsRow)
				return(l)
			} else {
				return(NULL)
			}
		} else if(input$uploadDataInput == 5){
			if(input$uploadPbDataset != ""){
				ncFile = str_c(projectBrowserPath, input$uploadPbDataset, ".nc")
				if(!file.exists(ncFile)) return(NULL)
				nc = open.nc(ncFile)
				n = file.inq.nc(nc)$nvars #variable indices 0 .. n-1
				vars = sapply(1:n, function(x) var.inq.nc(nc, x - 1)$name)
				technical = c("gene", "data", "array", "MetadataOrder", "Organism", 
					"ExperimentDescription", "InvestigationTitle", "DatasetID", "DatasetLink",
					"FileFormat", "DatasetType", "VariableLabel")
				varsPrefix = str_sub(vars, 1, 2) #technical also if beginning with double underscore
				w = which(!(vars %in% technical) & !(varsPrefix == "__") & (str_sub(vars, 1, 1) %in% LETTERS))
				varsAnno = vars[w]
				rn = var.get.nc(nc, "gene")
				cn = var.get.nc(nc, "array")
				mat = t(var.get.nc(nc, "data"))
				rownames(mat) = rn
				colnames(mat) = cn
				if(length(w) > 0){
					anno = data.frame(sapply(w, function(x) var.get.nc(nc, x - 1)))
					rownames(anno) = cn
					colnames(anno) = varsAnno
					annoLevs = sapply(anno, function(x) length(unique(x)))
					annoLevsTab = as.data.frame(lapply(annoLevs, identity))
					w2 = which((annoLevs >= input$uploadMinAnnoLevels) & 
                       (annoLevs <= input$uploadMaxAnnoLevels))
					if(length(w2) > 0){
						anno = anno[, w2, drop = FALSE]
						n = ncol(anno)
					} else {
						anno = NULL
						n = 0
					}
				} else {
					anno = NULL
					annoLevsTab = NULL
					n = 0
				}
        annoCol = anno
				annoRow = NULL
        nAnnoCol = n
				nAnnoRow = 0
        
				org = input$uploadOrganism
        str = strsplit(org, " ")[[1]]
				org2 = str_c(tolower(str_sub(str[1], 1, 1)), str[2])
				close.nc(nc)
				
        annoGroupsCol = annoGroupsRow = NULL
        fr = filterRows(session, mat, input, organism = org2, annoGroupsCol = annoGroupsCol)
				mat = fr$mat
        message = fr$message
				return(list(annoCol = annoCol, annoRow = annoRow, mat = mat, 
				            nAnnoCol = nAnnoCol, nAnnoRow = nAnnoRow, sep = ",", 
                    inputSaved = input, annoLevsTab = annoLevsTab, message = message,
				            annoGroupsCol = annoGroupsCol, annoGroupsRow = annoGroupsRow))
			} else {
				return(NULL)
			}
		}
		
		#guess delimiter:
		sepList = c(",", "\t", ";")
		if(gSep){
			rl = readLines(f, warn = FALSE)
      rl = rl[rl != ""] #last line can be empty
			sepNbrsMin = sapply(sepList, function(x) min(str_count(rl, x))) #minimal number of separators on a line
			sep = sepList[which.max(sepNbrsMin)]
			f2 = textConnection(rl) #create new connection
		} else {
			sep = switch(input$uploadFileSep, '1' = ",", '2' = "\t", '3' = ";")
			f2 = f
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
    
		#guess border between annotations and numeric data:
		if(gAnno || !is.numeric(input$uploadNbrColAnnos) || input$uploadNbrColAnnos < 0 || input$uploadNbrColAnnos >= nrow(data)){
		  data2num = data2; suppressWarnings(class(data2num) <- "numeric")
		  numericCells = !is.na(data2num) | (data2 == "")
		  numericRows = rowSums(numericCells)
		  numericCols = colSums(numericCells)
		  nAnnoRow = max(c(0, which(numericCols < numericCols[ncol(data2)])))
		  nAnnoCol = max(c(0, which(numericRows < numericRows[nrow(data2)])))
		  if((nAnnoCol < nrow(data)) & (nAnnoRow < ncol(data))){
		    data3 = data2[(nAnnoCol + 1):nrow(data2), (nAnnoRow + 1):ncol(data2), drop = FALSE]
		    data3int = data3; suppressWarnings(class(data3int) <- "integer")
		    data3num = data3; suppressWarnings(class(data3num) <- "numeric")
		    #intCells = !is.na(data3num) & ((data3int - round(data3num, 10)) == 0)
		    intCells = is.na(data3num) | ((data3int - round(data3num, 10)) == 0)
        if(!all(intCells)){
		      intRowLast = as.vector(intCells[nrow(intCells), ])
		      nAnnoRow = nAnnoRow + which(!intRowLast)[1] - 1
		      intColLast = as.vector(intCells[, ncol(intCells)])
		      nAnnoCol = nAnnoCol + which(!intColLast)[1] - 1
		    }
		  }
		} else {
		  nAnnoCol = input$uploadNbrColAnnos
		  nAnnoRow = input$uploadNbrRowAnnos
		}
		if(nAnnoCol == 0){
			annoCol = NULL
		} else {
		  annoCol = as.data.frame(t(data[1:nAnnoCol, (nAnnoRow + 1):ncol(data), drop = FALSE]))
		  annoCol[is.na(annoCol)] = "NA" #to make filtering and heatmap annotations work correctly
		}
    if(nAnnoRow == 0){
		  annoRow = NULL
		} else {
			annoRow = as.data.frame(data[(nAnnoCol + 1):nrow(data), 1:nAnnoRow, drop = FALSE])
	    annoRow[is.na(annoRow)] = "NA"
		}
		if(nAnnoCol == nrow(data) | nAnnoRow == ncol(data)){
			mat = NULL
		} else {
			mat = data2[(nAnnoCol + 1):nrow(data2), (nAnnoRow + 1):ncol(data2), drop = FALSE]
			mat = apply(mat, 1:2, as.numeric)
		}
		annoGroupsCol = annoGroupsRow = NULL
		return(list(annoCol = annoCol, annoRow = annoRow, mat = mat, 
                nAnnoCol = nAnnoCol, nAnnoRow = nAnnoRow, sep = sep, 
                inputSaved = input, annoLevsTab = NULL, message = message,
		            annoGroupsCol = annoGroupsCol, annoGroupsRow = annoGroupsRow))
	})
  
  setAnnoGroups = reactive({
    data = readData()
    if(is.null(data)) return(NULL)
    
    #set default annotation groups:
    if(is.null(data$annoGroupsCol)){
      if(!is.null(data$annoCol)){
        cn = colnames(data$annoCol)
      } else {
        cn = fakeAnno
      }
      data$annoGroupsCol = list()
      data$annoGroupsCol[[allAnno]] = cn
    }
    if(is.null(data$annoGroupsRow)){
      if(!is.null(data$annoRow)){
        cn = colnames(data$annoRow)
      } else {
        cn = fakeAnno
      }
      data$annoGroupsRow = list()
      data$annoGroupsRow[[allAnno]] = cn
    }
    data
  })

	#filter columns and rows based on annotations
	filterColumnsRows = reactive({
    data = setAnnoGroups()
    if(is.null(data)) return(NULL)
    
		wc = findRetainedAfterFiltering(data$annoCol, data$inputSaved, "Column", data$mat)
		wr = findRetainedAfterFiltering(data$annoRow, data$inputSaved, "Row", data$mat)
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
    
    if(toBoolean(data$inputSaved$uploadMatrixTranspose)){
      if(!is.null(data$mat)){
        data$mat = t(data$mat)
      }
      
      #switch annotations
      #avoid assigining NULL to list: it removes the element!
      #http://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
      temp = data$annoCol
      data['annoCol'] = list(data$annoRow)
      data['annoRow'] = list(temp)
      
      temp = data$annoGroupsCol
      data['annoGroupsCol'] = list(data$annoGroupsRow)
      data['annoGroupsRow'] = list(temp)
      
      temp = data$nAnnoCol
      data$nAnnoCol = data$nAnnoRow
      data$nAnnoRow = temp
    }
    
    isolate({
      updateProcOptions(session, data$annoCol, data$annoGroupsCol)
      updatePcaOptions(session, data$mat, data$inputSaved)
      updateHmOptions(session, data$mat, colnames(data$annoRow), data$inputSaved$procAnno)
    })
    
		data
	})
  
	#http://stackoverflow.com/questions/18816666/shiny-change-data-input-of-buttons
	values = reactiveValues()
	observe({
		values$data = filterColumnsRows()
	})
  
  #http://stackoverflow.com/questions/26432789/can-i-save-the-old-value-of-a-reactive-object-when-it-changes
	session$onFlush(once = FALSE, function(){
	  isolate({
	    #hide all previous tooltips
	    session$sendCustomMessage(type = 'hideTooltips', message = list())
      values$procAnnoGroupsOld = input$procAnnoGroups
      values$annoGroupsColOld = values$data$annoGroupsCol
      values$annoGroupsRowOld = values$data$annoGroupsRow
    })
	})
  
  #renew tooltips after page flush
	session$onFlushed(once = FALSE, function(){
	  isolate({
      if(!is.null(values$data)){
        #https://github.com/daattali/shinyjs/issues/39
        session$sendCustomMessage(type = 'activateTooltips', message = list())
        session$sendCustomMessage(type = 'addHandlers', message = list())
      }
	  })
	})
	
	#initial data
	output$uploadAnnoLevelsTable = renderTable({
		values$data$annoLevsTab
	})

	output$uploadAnnoColInfo = renderPrint({
		if(is.null(values$data$annoCol)){
			ns = c(0, 0)
		} else {
			ns = rev(dim(values$data$annoCol))
		}
		plurals = ifelse(ns != 1, "s", "")
		p(str_c("Column annotations (", ns[1], " row", plurals[1], ", ", ns[2], " column", plurals[2], "):"))
	})
	
	output$uploadAnnoColTable = renderTable({
		if(is.null(values$data$annoCol)) return(NULL)
		cutMatrix(t(values$data$annoCol))
	})
  
  output$uploadAnnoRowInfo = renderPrint({
    if(is.null(values$data$annoRow)){
      ns = c(0, 0)
    } else {
      ns = dim(values$data$annoRow)
    }
    plurals = ifelse(ns != 1, "s", "")
    p(str_c("Row annotations (", ns[1], " row", plurals[1], ", ", ns[2], " column", plurals[2], "):"))
  })

  output$uploadAnnoRowTable = renderTable({
    if(is.null(values$data$annoRow)) return(NULL)
    cutMatrix(values$data$annoRow)
  })  
	
	output$uploadDataInfo = renderPrint({
		if(is.null(values$data$mat)){
			ns = c(0, 0)
		} else {
			ns = dim(values$data$mat)
		}
		plurals = ifelse(ns != 1, "s", "")
		p(str_c("Numeric data (", ns[1], " row", plurals[1], ", ", ns[2], " column", plurals[2], "):"))
	})
	
	output$uploadDataTable = renderTable({
		cutMatrix(values$data$mat)
	})
  
  output$uploadWarnings = renderText({
    missing = c(input$uploadPbDataset == "", (input$uploadRowFiltering == 1) & (input$uploadPbPathway == ""), (input$uploadRowFiltering == 4) & (input$uploadGeneList == ""))
    words = c("a dataset", "a pathway (or change row filtering type)", "your gene list to the box on the left (or change row filtering type)")
    verbs = c("choose ", "choose ", "copy ")
    if(missing[1] & missing[2]) verbs[2] = ""
    warnWords = str_c(str_c(verbs[missing], words[missing]), collapse = " and ")
    validate(
      need(is.null(values$data$message), values$data$message),
      need(!(input$uploadDataInput == 2 & is.null(input$uploadFile)), "Please choose a file to upload!"),
      need(!(input$uploadDataInput == 3 & (is.null(input$uploadCopyPaste) || input$uploadCopyPaste == "")), "Please copy your data to the text box on the left!"),
      need(!(input$uploadDataInput == 5 & (input$uploadRowFiltering %in% c(1, 4)) & (missing[1] | missing[2] | missing[3])), 
          str_c("Please ", warnWords, "!")),
      need(!(input$uploadDataInput == 5 & !(input$uploadRowFiltering %in% c(1, 4)) & missing[1]), "Please choose a dataset!"),
      need(!(input$uploadDataInput %in% c(4, 6) & input$uploadSettingsId == ""), "No settings ID found!")
    )
    validate(
      need(!is.null(values$data$mat), "No numeric data found, you can visit 'Help' tab for instructions about data format!")
    )
  })
  
	#pre-processing
	getProc = reactive({
    if(is.null(values$data$mat)) return(NULL)
	  proc = dataProcess(data = values$data)
	  isolate({
      updatePcaOptions(session, proc$matImputed, proc$inputSaved)
      updateHmOptions(session, proc$matImputed, colnames(proc$annoRow), proc$inputSaved$procAnno)
      session$sendCustomMessage(type = 'sendEmpty', message = list()) #reset clicked object
    })
    proc
	})
  
	output$procWarnings = renderText({
	  proc = getProc()
	  validate(
	    need(!is.null(proc), "No data found, please check processing options or revisit 'Data upload' tab!")
	  )
	  if(!is.null(proc$mat)){
	    sd0r = proc$constRows
	    sd0c = proc$constCols
      sd0rString = str_c(str_c(str_c("'", sd0r), "'"), collapse = ", ")
	    sd0cString = str_c(str_c(str_c("'", sd0c), "'"), collapse = ", ")
      sd0rWarning = str_c("The following rows are constant and were removed: ", sd0rString, ".")
      sd0cWarning = ifelse(toBoolean(proc$inputSaved$procRemConstCols),
        str_c("The following columns are constant and were removed: ", sd0cString, "."),
        str_c("The following columns are constant and may cause problems with some clustering distances (e.g. correlation), you can remove them by checking 'remove constant columns': ", sd0cString, ".")
      )
	    validate(
	      need(length(sd0r) == 0, sd0rWarning),
	      need(length(sd0c) == 0, sd0cWarning),
        need(min(dim(proc$mat)) <= maxComponents, str_c("Only first ", maxComponents, " principal components calculated and used for missing value imputation."))
	    )
	  } else {
	    return(NULL)
	  }
	})
  
	output$pcaWarnings = renderText({
	  proc = getProc()
	  if(!is.null(proc$matPca)){
	    validate(
	      need(!toBoolean(proc$inputSaved$pcaInteractivity) || (nrow(proc$matPca) <= maxTooltipsPCA), 
             str_c("More than ", maxTooltipsPCA, " points, showing static plot for performance reasons (no interactivity)."))
	    )
	  } else {
	    return(NULL)
	  }
	})
  
	output$hmWarnings = renderText({
	  if(!is.null(getProc())){
	    transp = getTransposed()
	    if(!is.null(transp$matFinal)){
	      validate(
	        need(!toBoolean(transp$inputSaved$hmInteractivity) || (sum(dim(transp$matFinal)) <= maxTooltipsHm), 
            str_c("The total number of rows and columns is more than ", maxTooltipsHm, ", showing static plot for performance reasons (no interactivity)."))
        )
        validate(
          need(!toBoolean(transp$inputSaved$hmInteractivity) || (sum(dim(transp$matFinal)) + prod(dim(transp$matFinal)) <= maxTooltipsHm), 
            str_c("The total number of tooltips would be more than ", maxTooltipsHm, ", keeping the tooltips only for row and column labels (not for each cell)."))
        )
	    } else {
	      return(NULL)
	    }
	  } else {
      return(NULL)
    }
	})
  
  output$hmRcWarnings = output$pcaRcWarnings = renderText({
    grc = getRowColPlot()
    if(is.null(grc)) return(NULL)
    validate(
      need(nrow(grc$points) <= maxTooltipsRCplot, 
           str_c("More than ", maxTooltipsRCplot, " points, showing static plot for performance reasons (no interactivity)."))
    )
  })
	
	output$procSizeTable = renderTable({
	  getProc()$sizeTable
	})
  
	output$procNAsRows = renderTable({
	  proc = getProc()
	  if(!is.null(proc$mat)){
	    gp = proc$naTableRows
	    validate(
	      need(!is.null(gp), "No NAs in rows.")
	    )
	    return(cutMatrix(gp, digits = 0))
	  } else {
	    return(NULL)
	  }
	})
	
	output$procNAsCols = renderTable({
	  proc = getProc()
	  if(!is.null(proc$mat)){
	    gp = proc$naTableCols
	    validate(
	      need(!is.null(gp), "No NAs in columns.")
	    )
	    return(cutMatrix(gp, digits = 0))
	  } else {
	    return(NULL)
	  }
	})
	
	output$procPcaVarInfo = renderPrint({
		vt = getProc()$varTable
		if(is.null(vt)){
			ns = 0
		} else {
			ns = ncol(vt)
		}
		plurals = ifelse(ns != 1, "s", "")
		p(str_c("Variance explained by principal components (", ns, " component", plurals, "):"))
	})
	
	output$procPcaVarTable = renderTable({
		cutMatrix(getProc()$varTable)
	})
	
	output$procMatPcaInfo = renderPrint({
		mp = getProc()$matPca
		if(is.null(mp)){
			ns = c(0, 0)
		} else {
			ns = dim(mp)
		}
		plurals = ifelse(ns != 1, "s", "")
		p(str_c("Principal components (", ns[1], " data point", plurals[1], " in rows, ", ns[2], " component", plurals[2], " in columns):"))
	})
	
	output$procMatPca = renderTable({
		cutMatrix(getProc()$matPca)
	})

	output$procPcaLoadingsInfo = renderPrint({
	  ld = getProc()$pcaLoadings
	  if(is.null(ld)){
	    ns = c(0, 0)
	  } else {
	    ns = dim(ld)
	  }
	  plurals = ifelse(ns != 1, "s", "")
	  p(str_c("Component loadings (", ns[1], " dimension", plurals[1], " in rows, ", ns[2], " component", plurals[2], " in columns):"))
	})
  
	output$procPcaLoadings = renderTable({
	  cutMatrix(getProc()$pcaLoadings)
	})
  
	#PCA
	getPCA = reactive({
		data = getProc()
		validate(
			need(!is.null(data), "No data found, please revisit 'Data upload' tab!")
		)
    plot = plotPCA(data)
		validate(
		  need(is.null(plot$message), plot$message)
		)
		plot
	})
	
	output$pcaStatic = renderPlot({
		print(getPCA()$q)
	}, height = function() getPCA()$pich, width = function() getPCA()$picw)
	
  output$pca = renderUI({
    gp = getPCA()
    if(is.null(gp)) return(NULL)
    proc = getProc()
    if(toBoolean(proc$inputSaved$pcaInteractivity) && (nrow(proc$matPca) <= maxTooltipsPCA)){
      outfile = tempfile(pattern = "Rplots_", fileext = '.png', tmpdir = sessPath)
      dev.new(file = outfile, width = gp$picwIn, height = gp$pichIn)
      print(gp$q)
      grid.force() #suggested by Paul Murrell
      names = grid.ls(print = FALSE)$name
      grobPts = names[searchPrefix("geom_point", names)]
      
      titles = getTooltipTitles(mapping = proc$mappingCol, names = colnames(proc$mat))
      tooltips = getTooltipTexts(anno = proc$annoCol, titles = titles)
      
      if(hasAnno(proc$annoCol, "plot_link")){
        prefix = "link"
        grid.hyperlink(grobPts, href = proc$annoCol$plot_link, show = "new", group = FALSE)
      } else {
        prefix = "pcaCol"
      }
      
      grid.garnish(grobPts, `data-toggle` = rep("tooltip", length(tooltips)), title = tooltips, 
                   class = rep(prefix, length(tooltips)), 
                   id = str_c(str_c(prefix, "-"), 1:length(tooltips)), group = FALSE)

      svg = grid.export(NULL, strict = FALSE, indent = FALSE, annotate = FALSE)$svg
      dev.off(); unlink(outfile)
      res = HTML(toString.XMLNode(svg))
    } else {
      res = plotOutput("pcaStatic", height = "100%", width = "100%")
    }
    res
  })
  
	#heatmap
  getTransposed = reactive({
    data = getProc()
    validate(
      need(!is.null(data), "No data found, please revisit 'Data upload' tab!")
    )
    validate(
      need(nrow(data$matImputed) <= maxDimensionHeatmap, 
        str_c("Data matrices with more than ", maxDimensionHeatmap, 
        " rows are currently not supported, please revisit 'Data upload' tab and change row filtering options or upload a smaller file!")),
      need(ncol(data$matImputed) <= maxDimensionHeatmap, 
        str_c("Data matrices with more than ", maxDimensionHeatmap,
        " columns are currently not supported, please revisit 'Data upload' tab and filter some columns or collapse columns with similar annotations on 'Data pre-processing' tab or upload a smaller file!"))
    )
    inputSaved = data$inputSaved
    matImputed = data$matImputed
    matScaled = data$matScaled
    if(toBoolean(inputSaved$hmShowImputed)){
      matFinal = matImputed
    } else {
      matFinal = matScaled
    }
    annoCol = data$annoCol
    annoRow = data$annoRow
    cnr = colnames(annoRow)
    if(all(cnr %in% interactivityAnnos)){
      cnr = NULL
    } else {
      cnr = cnr[!(cnr %in% interactivityAnnos)]
    }
    cnc = inputSaved$procAnno
    mappingRow = data$mappingRow
    mappingCol = data$mappingCol
    if(toBoolean(inputSaved$hmTransposeHeatmap)){
      if(!is.null(matImputed)){
        matImputed = t(matImputed)
        matScaled = t(matScaled)
        matFinal = t(matFinal)
      }
      temp = annoCol
      annoCol = annoRow
      annoRow = temp
      
      temp = cnc
      cnc = cnr
      cnr = temp
      
      temp = mappingCol
      mappingCol = mappingRow
      mappingRow = temp
    }
    isolate({
      updateHmOptions(session, matFinal, cnr, cnc)
    })
    list(matFinal = matFinal, matImputed = matImputed, annoCol = annoCol, 
         annoRow = annoRow, inputSaved = inputSaved, 
         mappingCol = mappingCol, mappingRow = mappingRow)
  })
  
	getClust = reactive({
		data = getTransposed()
		data$hcRows = calcOrdering(data$matImputed, data$inputSaved$hmClustDistRows, 
      data$inputSaved$hmClustMethodRows, data$inputSaved$hmTreeOrderingRows)
		data$hcCols = calcOrdering(t(data$matImputed), data$inputSaved$hmClustDistCols, 
		  data$inputSaved$hmClustMethodCols, data$inputSaved$hmTreeOrderingCols)
		validate(
		  need(is.na(data$hcRows) | (class(data$hcRows) == "hclust"), 
           ifelse(class(data$hcRows) == "hclust", NA, str_c("In rows, ", data$hcRows))),
		  need(is.na(data$hcCols) | (class(data$hcCols) == "hclust"), 
		       ifelse(class(data$hcCols) == "hclust", NA, str_c("In columns, ", data$hcCols)))
		)
		data
	})
	
	getHeatmap = reactive({
	  data = getClust()
	  plotHeatmap(data)
	})
  
	#http://shiny.rstudio.com/articles/images.html
	output$heatmapStatic = renderImage({
	  outfile = tempfile(fileext = '.png', tmpdir = sessPath)
		png(outfile, units = "in", res = 300, width = input$hmPlotWidth / 2.54, 
        height = input$hmPlotRatio * input$hmPlotWidth / 2.54)
		gh = getHeatmap()
		validate(
		  need(is.null(gh$message), gh$message)
		)
		if(!is.null(gh)){
      grid.newpage()
      grid.draw(gh$ph$gtable)
		}
    dev.off()
    list(src = outfile, contentType = 'image/png', width = gh$picw, height = gh$pich)
	}, deleteFile = TRUE)
  
	output$heatmap = renderUI({
	  gh = getHeatmap()
	  if(is.null(gh)) return(NULL)
    transp = getTransposed()
	  if(toBoolean(transp$inputSaved$hmInteractivity) && (sum(dim(transp$matFinal)) <= maxTooltipsHm)){
	    outfile = tempfile(pattern = "Rplots_", fileext = '.pdf', tmpdir = sessPath)
	    dev.new(file = outfile, width = gh$picwIn, height = gh$pichIn)
	    grid.newpage()
	    grid.draw(gh$ph$gtable)
	    grid.force() #suggested by Paul Murrell
	    names = grid.ls(print = FALSE)$name
	    
	    titlesRows = getTooltipTitles(mapping = transp$mappingRow, names = rownames(transp$matFinal), rows = TRUE)
	    titlesCols = getTooltipTitles(mapping = transp$mappingCol, names = colnames(transp$matFinal))
      if(!is.na(gh$ph$tree_row)){
        rnOrder = gh$ph$tree_row$order
      } else {
        rnOrder = 1:nrow(transp$matFinal)
      }
	    if(!is.na(gh$ph$tree_col)){
	      cnOrder = gh$ph$tree_col$order
	    } else {
	      cnOrder = 1:ncol(transp$matFinal)
	    }
      annoRowOrdered = transp$annoRow[rnOrder, , drop = FALSE]
      annoColOrdered = transp$annoCol[cnOrder, , drop = FALSE]
	    titlesRowsOrdered = titlesRows[rnOrder]
	    titlesColsOrdered = titlesCols[cnOrder]
	    tooltipsRows = getTooltipTexts(anno = annoRowOrdered, titles = titlesRowsOrdered)
	    tooltipsCols = getTooltipTexts(anno = annoColOrdered, titles = titlesColsOrdered)
	    tooltipsTable = expand.grid(tooltipsRows, tooltipsCols, stringsAsFactors = FALSE)
	    tooltips = str_c(str_c(tooltipsTable[, 1], "<br /><br />"), tooltipsTable[, 2])
      cellIdsTable = expand.grid(1:length(tooltipsRows), 1:length(tooltipsCols), stringsAsFactors = FALSE)
      cellIds = str_c(str_c(cellIdsTable[, 1], "-"), cellIdsTable[, 2])
      
	    grobCells = names[searchPrefix("matrix", names)]
	    grobCellsSub = childNames(grid.get(grobCells)) #one more layer in between
	    grobRows = names[searchPrefix("row_names", names)]
	    grobCols = names[searchPrefix("col_names", names)]
      
	    grid.garnish(grobRows, `data-toggle` = rep("tooltip", length(tooltipsRows)), title = tooltipsRows, 
	                 class = rep("hmRow", length(tooltipsRows)), 
	                 id = str_c("hmRow-", 1:length(tooltipsRows)), group = FALSE)
	    grid.garnish(grobCols, `data-toggle` = rep("tooltip", length(tooltipsCols)), title = tooltipsCols, 
	                 class = rep("hmCol", length(tooltipsCols)), 
	                 id = str_c("hmCol-", 1:length(tooltipsCols)), group = FALSE)
      if((sum(dim(transp$matFinal)) + prod(dim(transp$matFinal)) <= maxTooltipsHm)){
        grid.garnish(grobCellsSub, `data-toggle` = rep("tooltip", length(tooltips)), title = tooltips, 
                     class = rep("hmCell", length(tooltips)), 
                     id = str_c("hmCell-", cellIds), group = FALSE)
      }

	    svg = grid.export(NULL, strict = FALSE, indent = FALSE, annotate = FALSE)$svg
	    dev.off(); unlink(outfile)
	    res = HTML(toString.XMLNode(svg))
	  } else {
	    res = imageOutput("heatmapStatic", height = "100%", width = "100%")
	  }
    res
	})

	getRowColPlot = reactive({
	  plotRowCol(getClust(), values$data$mat)
	})
  
	output$hmRowColPlotStatic = output$pcaRowColPlotStatic = renderPlot({
	  print(getRowColPlot()$q)
	}, height = function() getRowColPlot()$pich, width = function() getRowColPlot()$picw)
  
	output$hmRowColPlot = output$pcaRowColPlot = renderUI({
	  grc = getRowColPlot()
	  if(is.null(grc)) return(NULL)
	  points = grc$points
	  if(nrow(points) <= maxTooltipsRCplot){
	    original = values$data$mat
	    annoRow = values$data$annoRow
	    annoCol = values$data$annoCol
	    
	    inputSaved = values$data$inputSaved
	    
	    if(all(points$rn %in% colnames(original)) && all(points$cn %in% rownames(original))){
	      rn = points$cn
	      cn = points$rn
	    } else if(all(points$rn %in% rownames(original)) && all(points$cn %in% colnames(original))){
	      rn = points$rn
	      cn = points$cn
	    } else {
	      return(NULL)
	    }
	    
	    if(!is.null(annoRow)){
	      annoRowSub = annoRow[match(rn, rownames(annoRow)), , drop = FALSE]
	    } else {
	      annoRowSub = NULL
	    }
	    if(!is.null(annoCol)){
	      keep = c(inputSaved$procAnno, intersect(colnames(annoCol), interactivityAnnos))
	      if(length(keep) > 0){
	        annoColSub = annoCol[match(cn, rownames(annoCol)), keep, drop = FALSE]
	      } else {
	        annoColSub = NULL
	      }
	    } else {
	      annoColSub = NULL
	    }
	    
	    outfile = tempfile(pattern = "Rplots_", fileext = '.pdf', tmpdir = sessPath)
	    dev.new(file = outfile, width = grc$picwIn, height = grc$pichIn)
	    print(grc$q)
	    grid.force()
	    names = grid.ls(print = FALSE)$name
	    grobPts = names[searchPrefix("geom_jitter", names)]
	    
	    titlesRows = getTooltipTitles(mapping = NULL, names = rn, rows = TRUE)
	    tooltipsRows = getTooltipTexts(anno = annoRowSub, titles = titlesRows)
	    titlesCols = getTooltipTitles(mapping = NULL, names = cn)
	    tooltipsCols = getTooltipTexts(anno = annoColSub, titles = titlesCols)
	    tooltips = str_c(str_c(tooltipsRows, "<br /><br />"), tooltipsCols)
	    
	    #check column link in the first place, then row link:
	    if(hasAnno(annoColSub, "plot_link")){
        prefix = "link"
	      grid.hyperlink(grobPts, href = annoColSub$plot_link, show = "new", group = FALSE)
	    } else if(hasAnno(annoRowSub, "plot_link")){
	      prefix = "link"
	      grid.hyperlink(grobPts, href = annoRowSub$plot_link, show = "new", group = FALSE)
	    } else {
	      prefix = "nolink"
	    }
      
	    grid.garnish(grobPts, `data-toggle` = rep("tooltip", length(tooltips)), title = tooltips, 
	                 class = rep(prefix, length(tooltips)), 
	                 id = str_c(str_c(prefix, "-"), 1:length(tooltips)),
	                 group = FALSE)
	    
	    svg = grid.export(NULL, strict = FALSE, indent = FALSE, annotate = FALSE)$svg
	    dev.off(); unlink(outfile)
	    res = HTML(toString.XMLNode(svg))
	  } else {
	    res = imageOutput(str_c(grc$plotType, "RowColPlotStatic"), height = "100%", width = "100%")
	  }
    res
	})
  
	#export
	output$exportSaveSettingsText = renderPrint({
		data = values$data
		#https://gist.github.com/wch/9606002
		clicked = input$exportSaveSettingsButton #takes dependency since outside isolate()
		isolate({
			sessId = convert2safe(input$exportSaveDeleteSettingsId)
			if (clicked){
				lf = list.files(sessPath)
				fname = str_c("settings_", sessId, ".RData")
				if(fname %in% lf){
					text = p(str_c("Settings ID '", sessId, "' is already in use, please choose another ID!"), 
						style = "color:red;")
				} else {
					data$inputSaved = isolate(reactiveValuesToList(data$inputSaved)) #convert to list to save space
					data$inputSaved$uploadFile = data$inputSaved$uploadCopyPaste = NULL #remove to save space
					save(data, file = str_c(sessPath, fname))
					sess = session$clientData
					if(sess$url_port == ""){
						port = ""
					} else {
						port = str_c(":", sess$url_port)
					}
          getpar = "?" #avoid double "s=..." if saved settings are saved once more
					link = str_c(sess$url_protocol, "//", sess$url_hostname, port, 
						sess$url_pathname, getpar, "s=", sessId)
					text = p("Your settings are now saved and can be re-loaded by visiting ", 
						a(link, href = link, target = "_blank"), style = "color:green;")
				}
			}
		})
		text
	})
	
	output$exportDeleteSettingsText = renderPrint({
		clicked = input$exportDeleteSettingsButton #takes dependency since outside isolate()
		isolate({
			sessId = convert2safe(input$exportSaveDeleteSettingsId)
			if (clicked){
				lf = list.files(sessPath)
				fname = str_c("settings_", sessId, ".RData")
				if(fname %in% lf){
					unlink(str_c(sessPath, fname))
					text = p(str_c("Your settings with ID '", sessId, "' are now deleted!"), style = "color:green;")
				} else {
					text = p(str_c("Settings ID '", sessId, "' was not in use!"), style = "color:red;")
				}
			}
		})
		text
	})
	
	output$exportDownloadInitialData = downloadHandler(
	  filename = function() { str_c(toolname, "InitialData.csv") },
	  content = function(file) {
      if(is.null(values$data$annoCol) & is.null(values$data$annoRow)){
        d = values$data$mat
      } else if(is.null(values$data$annoRow)){
        d = rbind(t(values$data$annoCol), values$data$mat)
      } else if(is.null(values$data$annoCol)){
        d = cbind(values$data$annoRow, values$data$mat)
      } else {
        emptyMat = matrix("", ncol(values$data$annoCol), ncol(values$data$annoRow))
        rownames(emptyMat) = colnames(values$data$annoCol)
        colnames(emptyMat) = colnames(values$data$annoRow)
        d = cbind(rbind(emptyMat, values$data$annoRow), 
                  rbind(t(values$data$annoCol), values$data$mat))
      }
	    writeOutput(d, file)
	  }
	)
  
	output$exportDownloadProcessedData = downloadHandler(
	  filename = function() { str_c(toolname, "ProcessedData.csv") },
	  content = function(file) {
      d = getProc()$matImputed
      writeOutput(d, file)
	  }
	)

	output$exportDownloadPCAscores = downloadHandler(
	  filename = function() { str_c(toolname, "PCAscores.csv") },
	  content = function(file) {
	    d = getProc()$matPca
	    writeOutput(d, file)
	  }
	)
  
	output$exportDownloadPCAloadings = downloadHandler(
	  filename = function() { str_c(toolname, "PCAloadings.csv") },
	  content = function(file) {
	    d = getProc()$pcaLoadings
	    writeOutput(d, file)
	  }
	)
  
	output$exportDownloadPCAvartable = downloadHandler(
	  filename = function() { str_c(toolname, "PCAvartable.csv") },
	  content = function(file) {
	    d = getProc()$varTable
	    writeOutput(d, file)
	  }
	)
	
	output$exportDownloadPdfPCA = output$exportDownloadPdfHeatmap = downloadHandler(
	  filename = function() { str_c(toolname, input$tabs1, ".pdf") },
	  content = function(file) {
	    type = input$tabs1 #PCA or Heatmap
	    fun = str_c("get", type)
	    if(type == "PCA"){
	      pdf(file, width = get(fun)()$picw / 72, height = get(fun)()$pich / 72)
	      print(get(fun)()$q)
	    } else if(type == "Heatmap"){
	      pdf(file, width = input$hmPlotWidth / 2.54, height = input$hmPlotRatio * input$hmPlotWidth / 2.54)
	      gt = plotHeatmap(getClust())$ph$gtable
	      grid.draw(gt)
	    }
	    dev.off()
	  },
	  contentType = "application/pdf"
	)
	
	output$exportDownloadEpsPCA = output$exportDownloadEpsHeatmap = downloadHandler(
	  filename = function() { str_c(toolname, input$tabs1, ".eps") },
	  content = function(file) {
	    type = input$tabs1 #PCA or Heatmap
	    fun = str_c("get", type)
	    if(type == "PCA"){
	      postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", 
	                 width = get(fun)()$picw / 72, height = get(fun)()$pich / 72)
	      print(get(fun)()$q)
	    } else if(type == "Heatmap"){
	      postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", 
	                 width = input$hmPlotWidth / 2.54, height = input$hmPlotRatio * input$hmPlotWidth / 2.54)
	      gt = plotHeatmap(getClust())$ph$gtable
	      grid.draw(gt)
	    }
	    dev.off()
	  },
	  contentType = "application/postscript"
	)
	
	output$exportDownloadSvgPCA = output$exportDownloadSvgHeatmap = downloadHandler(
	  filename = function() { str_c(toolname, input$tabs1, ".svg") },
	  content = function(file) {
	    type = input$tabs1 #PCA or Heatmap
	    fun = str_c("get", type)
	    if(type == "PCA"){
	      svglite(file, width = get(fun)()$picw / 72, height = get(fun)()$pich / 72)
	      print(get(fun)()$q)
	    } else if(type == "Heatmap"){
	      svglite(file, width = input$hmPlotWidth / 2.54, height = input$hmPlotRatio * input$hmPlotWidth / 2.54)
	      gt = plotHeatmap(getClust())$ph$gtable
	      grid.draw(gt)
	    }
	    dev.off()
	  },
	  contentType = "image/svg"
	)
  
	observe({
	  set.seed(NULL) #force random start
	  sessId = paste(sample(c(letters, LETTERS), 15, replace = TRUE), collapse = "")
	  updateTextInput(session, "exportSaveDeleteSettingsId", value = sessId)
	})
	
	observe({
	  if (input$uploadClearTextButton == 0) return()
	  isolate({ updateTextInput(session, "uploadCopyPaste", label = ",", value = "") })
	})
	
	observe({
	  if (input$uploadClearGeneListButton == 0) return()
	  isolate({ updateTextInput(session, "uploadGeneList", label = ",", value = "") })
	})
  
	#update calculated default delimiter and number of annotation rows and columns:
	observe({
	  d = values$data
	  if(!is.null(d)){
	    updateNumericInput(session, "uploadNbrColAnnos", value = d$nAnnoCol)
	    updateNumericInput(session, "uploadNbrRowAnnos", value = d$nAnnoRow)
	    updateRadioButtons(session, "uploadFileSep", selected = switch(d$sep, "," = '1', "\t" = '2', ";" = '3'))
	  }
	})
	
	#update settings if there is GET variable:
	observe({
		search = parseQueryString(session$clientData$url_search)
		var = ""
		if("s" %in% names(search) && search$s != ""){
			var = "s"
			sel = "4"
		} else if("p" %in% names(search) && search$p != ""){
			var = "p"
			sel = "6"
		}
		if(var %in% c("s", "p")){
			id = search[[var]]
			updateTabsetPanel(session, "tabs1", selected = "Data upload")
			updateRadioButtons(session, "uploadDataInput", selected = sel)
			updateTextInput(session, "uploadSettingsId", value = id)
		}
	})
	
  #update list of datasets:
	observe({
		if(input$uploadDataInput == 5){
		  org = input$uploadOrganism
		  if(org != ""){
		    str = strsplit(org, " ")
		    org_short = str_c(tolower(str_sub(str[[1]][1], 1, 1)), str[[1]][2])
		    load(file = str_c(pwPath, "clustvisInput_", pwDate, "_spec_", org_short, ".RData"))
		    w = which(uploadDatasetTable$organism == input$uploadOrganism & 
		                uploadDatasetTable$nAnnos >= input$uploadMinAnnoTracks)
		    df = uploadDatasetTable[w, , drop = FALSE]
		    dslist = df$id2
		    names(dslist) = df$label3
		    dslist = c("", dslist)
		    updateSelectizeInput(session, "uploadPbDataset", choices = dslist, server = TRUE)
		  }
		}
	})
	
	#update list of pathways:
	observe({
		if(input$uploadDataInput %in% 5:6){
      if(input$uploadDataInput == 5){
        org = input$uploadOrganism
      } else {
        org = "Homo sapiens" #default for prepared gene expression datasets
      }
      if(org != ""){
        str = strsplit(org, " ")
        org_short = str_c(tolower(str_sub(str[[1]][1], 1, 1)), str[[1]][2])
        load(file = str_c(pwPath, "clustvisInput_", pwDate, "_spec_", org_short, ".RData"))
        pwlist = c("", uploadPathwayList)
        updateSelectizeInput(session, "uploadPbPathway", choices = pwlist, server = TRUE)
      }
		}
	})
	
  #update annotations based on groups:
	observe({
    grNew = values$data$inputSaved$procAnnoGroups
    grOld = values$procAnnoGroupsOld
    #which checkbox has changed state (one at a time)
    checked = setdiff(grNew, grOld)
    unchecked = setdiff(grOld, grNew)
    if(length(checked) > 0){
      updateCheckboxGroupInput(session, "procAnno", 
        selected = union(values$data$inputSaved$procAnno, values$data$annoGroupsCol[[checked[1]]]))
    } else if(length(unchecked) > 0){
      updateCheckboxGroupInput(session, "procAnno", 
        selected = setdiff(values$data$inputSaved$procAnno, values$data$annoGroupsCol[[unchecked[1]]]))
    }
	})
  
	observeEvent(input$hmBackButton, {
	  session$sendCustomMessage(type = 'sendEmpty', message = list())
	})
  
	observeEvent(input$pcaBackButton, {
	  session$sendCustomMessage(type = 'sendEmpty', message = list())
	})
  
	#generate filtering options
	#use original unfiltered data for defining groups
	output$uploadColumnFiltersAnno = renderUI({
		data = setAnnoGroups()
    if(!identical(data$annoGroupsCol, values$annoGroupsColOld)){
      saved = list()
    } else {
      saved = data$inputSaved
    }
    annotationsFilters(data$annoCol, saved, type = "Column", data$annoGroupsCol)
	})
  
	output$uploadRowFiltersAnno = renderUI({
	  data = setAnnoGroups()
	  if(!identical(data$annoGroupsRow, values$annoGroupsRowOld)){
	    saved = list()
	  } else {
	    saved = data$inputSaved
	  }
	  annotationsFilters(data$annoRow, saved, type = "Row", data$annoGroupsRow)
	})
	
	output$legendPCA <- renderUI({
	  data = getProc()
    if(!is.null(data)){
      leg = c()
      if(data$inputSaved$procMethodAgg != "no collapse"){
        leg = append(leg, c("Columns with similar annotations are collapsed by taking ", 
                            data$inputSaved$procMethodAgg, " inside each group. "))
      }
      sc = names(procScalings)[match(data$inputSaved$procScaling, procScalings)]
      meth = names(procMeth)[match(data$inputSaved$procMethod, procMeth)]
      pcs = c(as.numeric(data$inputSaved$pcaPcx), as.numeric(data$inputSaved$pcaPcy))
      leg = append(leg, c(capitalize(sc), " is applied to rows; ", meth, " is used to calculate principal components. X and Y axis show principal component ", data$inputSaved$pcaPcx, 
                          " and principal component ", data$inputSaved$pcaPcy, " that explain ", round(data$varTable[1, pcs[1]] * 100, 1), "% and ", round(data$varTable[1, pcs[2]] * 100, 1), "% of the total variance, respectively. "))
      if(getPCA()$showEllipses){
        leg = append(leg, c("Prediction ellipses are such that with probability ", 
                            data$inputSaved$pcaEllipseConf, 
                            ", a new observation from the same group will fall inside the ellipse. "))
      }
      leg = append(leg, c("N = ", nrow(data$matPca), " data points."))
      #http://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
      return(HTML("<h5>Caption example</h5><p>", str_c(leg, collapse = ""), "</p>"))
    }
	})
  
	output$legendHeatmap <- renderUI({
	  data = getProc()
    if(!is.null(data) && all(dim(data$matImputed) <= maxDimensionHeatmap)){
      leg = c()
      if(data$inputSaved$procMethodAgg != "no collapse"){
        leg = append(leg, c(changeIfTransposed("Columns", data$inputSaved), " with similar annotations are collapsed by taking ", data$inputSaved$procMethodAgg, " inside each group. "))
      }
      sc = names(procScalings)[match(data$inputSaved$procScaling, procScalings)]
      scaling = str_c(sc, " is applied to ", changeIfTransposed("rows", data$inputSaved), ". ")
      if(toBoolean(data$inputSaved$procCentering)){
        leg = append(leg, c(changeIfTransposed("Rows", data$inputSaved), " are centered; ", scaling))
      } else {
        leg = append(leg, capitalize(scaling))
      }
      meth = names(procMeth)[match(data$inputSaved$procMethod, procMeth)]
      if(sum(is.na(data$mat)) > 0){
        leg = append(leg, c(meth, " is used for missing value estimation. "))
      }
      distLabelRows = names(clustDists)[match(data$inputSaved$hmClustDistRows, clustDists)]
      distLabelCols = names(clustDists)[match(data$inputSaved$hmClustDistCols, clustDists)]
      linkLabelRows = names(clustMethods)[match(data$inputSaved$hmClustMethodRows, clustMethods)]
      linkLabelCols = names(clustMethods)[match(data$inputSaved$hmClustMethodCols, clustMethods)]
      if(distLabelRows == distLabelCols & linkLabelRows == linkLabelCols & distLabelRows != noClust){
        leg = append(leg, c("Both rows and columns are clustered using ", distLabelRows, " distance and ", linkLabelRows, " linkage. "))
      } else {
        if(distLabelRows != noClust){
          leg = append(leg, c("Rows are clustered using ", distLabelRows, " distance and ", linkLabelRows, " linkage. "))
        }
        if(distLabelCols != noClust){
          leg = append(leg, c("Columns are clustered using ", distLabelCols, " distance and ", linkLabelCols, " linkage. "))
        }
      }
      return(HTML("<h5>Caption example</h5><p>", str_c(leg, collapse = ""), "</p>"))
    }
	})
  
	#http://shiny.rstudio.com/articles/datatables.html
	output$helpDatasetTable = renderDataTable({
	  helpDatasetTable
	}, options = helpTablesOptions)
	
	output$helpPathwayTable = renderDataTable({
	  helpPathwayTable
	}, options = helpTablesOptions)
})
