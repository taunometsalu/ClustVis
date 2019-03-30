#Author: Tauno Metsalu
#Copyright: 2018 University of Tartu

#source("/srv/shiny-server/global.R")

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
				  if(values$getVar == "e"){
				    sessPathUsed = sessPathExternal
				  } else {
				    sessPathUsed = sessPath
				  }
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
        } else { #backward compatibility
          annoCol = data$anno
          annoRow = NULL
        }
				annoGroupsCol = data$annoGroupsCol
				annoGroupsRow = data$annoGroupsRow
				
				#hardcoded manually - found no better way ...
				updates = list(
					updateSelectInput = c("procTransformation", "procMethodAgg", "procScaling", "procMethod", 
						"pcaPcx", "pcaPcy", "pcaColor", "pcaShape", "pcaEllipseLineType", "pcaLegendPosition", 
						"hmLegendScheme", "hmClustDistRows", "hmClustDistCols", "hmClustMethodRows", "hmClustMethodCols", 
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
            "pcaPlotRatio", "pcaMarginRatio", "pcaEllipseConf", "hmPlotRatio", 
            "hmColorRangeMax", "hmColorRangeMin", "hmCutreeClustersRows", "hmCutreeClustersCols"),
					updateSliderInput = c("pcaPointSize", "pcaPlotWidth", "pcaFontSize", "pcaEllipseLineWidth", 
					  "hmFontSizeGeneral", "hmFontSizeNumbers", "hmPrecisionNumbers", 
            "hmFontSizeRownames", "hmFontSizeColnames", "hmPlotWidth"), 
					updateTextInput = c("pcaAxisLabelPrefix"),
					updateDateInput = c(),
					
					updateDateRangeInput = c()
				)
				lens = sapply(updates, length)
				updates2 = data.frame(fun = rep(names(updates), lens), id = unlist(updates), stringsAsFactors = FALSE)
				
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
        l = list(annoCol = annoCol, annoRow = annoRow, mat = mat, sep = sep, 
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
					anno = data.frame(sapply(w, function(x) var.get.nc(nc, x - 1)), stringsAsFactors = FALSE)
					rownames(anno) = cn
					colnames(anno) = varsAnno
					annoLevs = sapply(anno, function(x) length(unique(x)))
					annoLevsTab = as.data.frame(lapply(annoLevs, identity), stringsAsFactors = FALSE)
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
        
				org = input$uploadOrganism
        str = strsplit(org, " ")[[1]]
				org2 = str_c(tolower(str_sub(str[1], 1, 1)), str[2])
				close.nc(nc)
				
        annoGroupsCol = annoGroupsRow = NULL
        fr = filterRows(session, mat, input, organism = org2, annoGroupsCol = annoGroupsCol)
				mat = fr$mat
        message = fr$message
				return(list(annoCol = annoCol, annoRow = annoRow, mat = mat, 
                    inputSaved = input, annoLevsTab = annoLevsTab, message = message,
				            annoGroupsCol = annoGroupsCol, annoGroupsRow = annoGroupsRow))
			} else {
				return(NULL)
			}
		}
		
		if(gSep){
		  sep = NA
		} else {
		  sep = switch(input$uploadFileSep, '1' = ",", '2' = "\t", '3' = ";")
		}
		
		if(gAnno){
		  nAnnoCol = nAnnoRow = NA
		} else {
		  nAnnoCol = input$uploadNbrColAnnos
		  nAnnoRow = input$uploadNbrRowAnnos
		}
		
		quotes = input$uploadQuotes
		naString = input$uploadNaString
		
		l = readFile(file = f, sep = sep, nbrRowAnnos = nAnnoRow, nbrColAnnos = nAnnoCol, quotes = quotes, naString = naString)
		l$annoGroupsCol = NULL
		l$annoGroupsRow = NULL
		l$inputSaved = input
		l$annoLevsTab = NULL
		return(l)
	})
  
  setAnnoGroups = reactive({
    data = readData()
    if(is.null(data)) return(NULL)
    if(logUsage && !is.null(data$mat)) sendToLog(data)
    
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
    filteringRows = findRetainedAfterFiltering(data$annoRow, data$inputSaved, "Row", data$mat)
    filteringCols = findRetainedAfterFiltering(data$annoCol, data$inputSaved, "Column", data$mat)
    transpose = toBoolean(data$inputSaved$uploadMatrixTranspose)
    
    data = filterData(data = data, filteringRows = filteringRows, filteringCols = filteringCols, transpose = transpose)
    
    if(transpose){
      #switch annotation groups
      #avoid assigining NULL to list: it removes the element!
      #http://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
      temp = data$annoGroupsCol
      data['annoGroupsCol'] = list(data$annoGroupsRow)
      data['annoGroupsRow'] = list(temp)
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
	    values$changeAllOld = getChangeAllCheckboxes(values$data$inputSaved,
        setAnnoGroups()$annoCol, setAnnoGroups()$annoRow)
      for(prefix in c("hm", "pca")){
        if(is.null(values[[str_c(prefix, "ClickedObjectOld")]]) || is.null(input[[str_c(prefix, "ClickedObject")]]) || 
             (values[[str_c(prefix, "ClickedObjectOld")]] != input[[str_c(prefix, "ClickedObject")]])){
          values[[str_c(prefix, "ClickedObjectPrevious")]] = values[[str_c(prefix, "ClickedObjectOld")]]
          values[[str_c(prefix, "ClickedTypePrevious")]] = values[[str_c(prefix, "ClickedTypeOld")]]
        }
        values[[str_c(prefix, "ClickedObjectOld")]] = input[[str_c(prefix, "ClickedObject")]]
        values[[str_c(prefix, "ClickedTypeOld")]] = input[[str_c(prefix, "ClickedType")]]
      }
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
	}, rownames = TRUE, bordered = TRUE, align = "l")

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
	  withProgress(message = 'Loading the data, please wait ...', value = NULL, {
	    if(is.null(values$data$annoCol)) return(NULL)
	    cutMatrix(t(values$data$annoCol))
	  })
	}, rownames = TRUE, bordered = TRUE, align = "l")
  
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
  }, rownames = TRUE, bordered = TRUE, align = "l")  
	
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
	}, rownames = TRUE, bordered = TRUE, align = "l")
  
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
      #reset clicked objects:
      session$sendCustomMessage(type = 'changeClicked', message = list(prefix = "hm", newObject = "", newType = ""))
      session$sendCustomMessage(type = 'changeClicked', message = list(prefix = "pca", newObject = "", newType = ""))
    })
    proc
	})
  
	output$procWarnings = renderText({
	  proc = getProc()
	  validate(
	    need(!is.null(proc), "No data found, please check processing options or revisit 'Data import' tab!")
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
	  proc = getProc()
	  if(!is.null(proc$matImputed)){
	    transp = getTransposed()
	    if(!is.null(transp$matFinal)){
	      gh = getHeatmap()
	      validate(
	        need(!toBoolean(transp$inputSaved$hmInteractivity) || (sum(dim(transp$matFinal)) <= maxTooltipsHm), 
            str_c("The total number of rows and columns is more than ", maxTooltipsHm, ", showing static plot for performance reasons (no interactivity).")),
	        need(is.null(gh$message), gh$message)
        )
	    } else {
	      return(NULL)
	    }
	  } else {
      return(NULL)
    }
	})
  
  output$hmJitterWarnings = output$pcaJitterWarnings = renderText({
    grc = getJitterPlot()
    if(is.null(grc)) return(NULL)
    points = grc$points[!is.na(grc$points$value), , drop = FALSE]
    validate(
      need(nrow(points) <= maxTooltipsJitterPlot, 
           str_c("More than ", maxTooltipsJitterPlot, " points, showing static plot for performance reasons (no interactivity)."))
    )
  })
	
	output$procSizeTable = renderTable({
	  withProgress(message = 'Loading the data, please wait ...', value = NULL, {
	    getProc()$sizeTable
	  })
	}, rownames = TRUE, bordered = TRUE, align = "l")
  
	output$procNAsRows = renderTable({
	  gp = getProc()$naTableRows
	  validate(
	    need(!is.null(gp), "No NAs in rows.")
	  )
	  cutMatrix(gp, digits = 0)
	}, rownames = TRUE, bordered = TRUE, align = "l")
	
	output$procNAsCols = renderTable({
	  gp = getProc()$naTableCols
    validate(
      need(!is.null(gp), "No NAs in columns.")
    )
    cutMatrix(gp, digits = 0)
	}, rownames = TRUE, bordered = TRUE, align = "l")
	
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
	}, rownames = TRUE, bordered = TRUE, align = "l")
	
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
	}, rownames = TRUE, bordered = TRUE, align = "l")

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
	}, rownames = TRUE, bordered = TRUE, align = "l")
  
	#PCA
	getPCA = reactive({
		data = getProc()
		validate(
			need(!is.null(data$matPca), "No data found, please revisit 'Data import' or 'Data pre-processing' tab!")
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
    withProgress(message = 'Loading the plot, please wait ...', value = NULL, {
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
        
        if(hasAnno(proc$annoCol, linkAnno)){
          prefix = "link"
          links = as.vector(proc$annoCol[[linkAnno]])
          grid.hyperlink(grobPts, href = links, show = rep("new", length(links)), group = FALSE)
        } else {
          prefix = "pcaCol"
        }
        
        grid.garnish(grobPts, `data-toggle` = rep("tooltip", length(tooltips)), 
                     title = tooltips, class = rep(prefix, length(tooltips)), 
                     id = str_c(str_c(prefix, "-"), 1:length(tooltips)), group = FALSE)
        
        svg = grid.export(NULL, strict = FALSE, indent = FALSE, annotate = FALSE)$svg
        dev.off(); unlink(outfile)
        res = HTML(toString.XMLNode(svg))
      } else {
        res = plotOutput("pcaStatic", height = "100%", width = "100%")
      }
      res
    })
  })
  
	#heatmap
  getTransposed = reactive({
    proc = getProc()
    validate(
      need(!is.null(proc$matImputed), "No data found, please revisit 'Data import' or 'Data pre-processing' tab!")
    )
    validate(
      need(nrow(proc$matImputed) <= maxDimensionHeatmap, 
        str_c("Data matrices with more than ", maxDimensionHeatmap, 
        " rows are currently not supported, please revisit 'Data import' tab and change row filtering options or upload a smaller file!")),
      need(ncol(proc$matImputed) <= maxDimensionHeatmap, 
        str_c("Data matrices with more than ", maxDimensionHeatmap,
        " columns are currently not supported, please revisit 'Data import' tab and filter some columns or collapse columns with similar annotations on 'Data pre-processing' tab or upload a smaller file!"))
    )
    
    inputSaved = proc$inputSaved
    cnr = colnames(proc$annoRow)
    if(all(cnr %in% interactivityAnnos)){
      cnr = NULL
    } else {
      cnr = cnr[!(cnr %in% interactivityAnnos)]
    }
    cnc = inputSaved$procAnno
    
    showImputed = toBoolean(inputSaved$hmShowImputed)
    transpose = toBoolean(inputSaved$hmTransposeHeatmap)
    
    if(transpose){
      temp = cnc
      cnc = cnr
      cnr = temp
    }
    
    trans = transposeMatrix(proc, showImputed = showImputed, transpose = transpose)
    trans$inputSaved = inputSaved
    isolate({
      updateHmOptions(session, trans$matFinal, cnr, cnc)
    })
    trans
  })
  
	getClust = reactive({
		trans = getTransposed()
		clustDistRows = trans$inputSaved$hmClustDistRows
		if(clustDistRows == noClust) clustDistRows = NA
		clustMethodRows = trans$inputSaved$hmClustMethodRows
		treeOrderingRows = trans$inputSaved$hmTreeOrderingRows
		if(treeOrderingRows == "tightest cluster first") treeOrderingRows = NA
		clustDistCols = trans$inputSaved$hmClustDistCols
		if(clustDistCols == noClust) clustDistCols = NA
		clustMethodCols = trans$inputSaved$hmClustMethodCols
		treeOrderingCols = trans$inputSaved$hmTreeOrderingCols
		if(treeOrderingCols == "tightest cluster first") treeOrderingCols = NA
		
		clust = clusterMatrix(trans, clustDistRows = clustDistRows, clustMethodRows = clustMethodRows, treeOrderingRows = treeOrderingRows, clustDistCols = clustDistCols, clustMethodCols = clustMethodCols, treeOrderingCols = treeOrderingCols)
		
		validate(
		  need(is.na(clust$hcRows) | (class(clust$hcRows) == "hclust") | (class(clust$hcRows) == "logical"), 
           ifelse(class(clust$hcRows) == "hclust", NA, str_c("In rows, ", clust$hcRows))),
		  need(is.na(clust$hcCols) | (class(clust$hcCols) == "hclust") | (class(clust$hcCols) == "logical"), 
		       ifelse(class(clust$hcCols) == "hclust", NA, str_c("In columns, ", clust$hcCols)))
		)
		clust
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
		if(!is.null(gh)){
      grid.newpage()
      grid.draw(gh$q$gtable)
		}
    dev.off()
    list(src = outfile, contentType = 'image/png', width = gh$picw, height = gh$pich)
	}, deleteFile = TRUE)
  
	output$heatmap = renderUI({
	  withProgress(message = 'Loading the plot, please wait ...', value = NULL, {
	    gh = getHeatmap()
	    if(is.null(gh)) return(NULL)
	    transp = getTransposed()
	    if(toBoolean(transp$inputSaved$hmInteractivity) && (sum(dim(transp$matFinal)) <= maxTooltipsHm)){
	      outfile = tempfile(pattern = "Rplots_", fileext = '.pdf', tmpdir = sessPath)
        dev.new(file = outfile, width = gh$picwIn, height = gh$pichIn)
	      grid.newpage()
	      grid.draw(gh$q$gtable)
	      grid.force() #suggested by Paul Murrell
	      names = grid.ls(print = FALSE)$name
	      
	      titlesRows = getTooltipTitles(mapping = transp$mappingRow, names = rownames(transp$matFinal), rows = TRUE)
	      titlesCols = getTooltipTitles(mapping = transp$mappingCol, names = colnames(transp$matFinal))
	      if(!is.na(gh$q$tree_row)){
	        rnOrder = gh$q$tree_row$order
	      } else {
	        rnOrder = 1:nrow(transp$matFinal)
	      }
	      if(!is.na(gh$q$tree_col)){
	        cnOrder = gh$q$tree_col$order
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
	      
	      grid.garnish(grobCellsSub, `data-toggle` = rep("tooltip", length(tooltips)), title = tooltips, 
	                   class = rep("hmCell", length(tooltips)), 
	                   id = str_c("hmCell-", cellIds), group = FALSE)
	      
	      svg = grid.export(NULL, strict = FALSE, indent = FALSE, annotate = FALSE)$svg
	      dev.off(); unlink(outfile)
	      res = HTML(toString.XMLNode(svg))
	    } else {
	      res = imageOutput("heatmapStatic", height = "100%", width = "100%")
	    }
	    res
	  })
	})
  
	getJitterTable = reactive({
    tableJitter(getClust(), values$data$mat, findPrefix(input$tabs1))
	})

	getJitterPlot = reactive({
    plotJitter(getJitterTable())
	})
  
	output$hmJitterPlotStatic = output$pcaJitterPlotStatic = renderPlot({
	  print(getJitterPlot()$q)
	}, height = function() getJitterPlot()$pich, width = function() getJitterPlot()$picw)
  
	output$hmJitterPlot = output$pcaJitterPlot = renderUI({
	  withProgress(message = 'Loading the plot, please wait ...', value = NULL, {
	    grc = getJitterPlot()
	    if(is.null(grc)) return(NULL)
	    points = grc$points[!is.na(grc$points$value), , drop = FALSE] #NAs confuse tooltip generation
	    #if(nrow(points) == 0) return(NULL)
	    validate(need(nrow(points) > 0, "\nOnly missing values, no plot shown."))
	    if((nrow(points) <= maxTooltipsJitterPlot) & 
	         (!(values$data$inputSaved[[str_c(grc$plotType, "JitterPlotType")]] %in% c("violin", "box")))){
	      annoRow = values$data$annoRow
	      annoCol = values$data$annoCol
	      inputSaved = values$data$inputSaved
	      
	      if(!is.null(annoRow)){
	        annoRowSub = annoRow[match(points$rnOrig, rownames(annoRow)), , drop = FALSE]
	      } else {
	        annoRowSub = NULL
	      }
	      if(!is.null(annoCol)){
	        keep = c(inputSaved$procAnno, intersect(colnames(annoCol), interactivityAnnos))
	        if(length(keep) > 0){
	          annoColSub = annoCol[match(points$cnOrig, rownames(annoCol)), keep, drop = FALSE]
	        } else {
	          annoColSub = NULL
	        }
	      } else {
	        annoColSub = NULL
	      }
	      
	      outfile = tempfile(pattern = "Rplots_", fileext = '.pdf', tmpdir = sessPath)
	      dev.new(file = outfile, width = grc$picwIn, height = grc$pichIn, units = "in", res = 72)
	      print(grc$q)
	      grid.force()
	      names = grid.ls(print = FALSE)$name
	      grobPts = c(names[searchPrefix(c("geom_jitter"), names)],
	                  names[searchPrefix(c("geom_point"), names)])
	      
	      titlesRows = getTooltipTitles(mapping = NULL, names = points$rnOrig, rows = TRUE)
	      tooltipsRows = getTooltipTexts(anno = annoRowSub, titles = titlesRows)
	      titlesCols = getTooltipTitles(mapping = NULL, names = points$cnOrig)
	      tooltipsCols = getTooltipTexts(anno = annoColSub, titles = titlesCols)
	      tooltips = str_c(str_c(tooltipsRows, "<br /><br />"), tooltipsCols)
	      
	      #check column link in the first place, then row link:
	      if(hasAnno(annoColSub, linkAnno)){
	        prefix = "link"
	        links = as.vector(annoColSub[[linkAnno]])
	        grid.hyperlink(grobPts, href = links, show = rep("new", length(links)), group = FALSE)
	      } else if(hasAnno(annoRowSub, linkAnno)){
	        prefix = "link"
	        links = as.vector(annoRowSub[[linkAnno]])
	        grid.hyperlink(grobPts, href = links, show = rep("new", length(links)), group = FALSE)
	      } else {
	        prefix = "nolink"
	      }
	      #return(str_c(names, collapse = ", "))
	      grid.garnish(grobPts, `data-toggle` = rep("tooltip", length(tooltips)), 
	                   title = tooltips, class = rep(prefix, length(tooltips)), 
	                   id = str_c(str_c(prefix, "-"), 1:length(tooltips)), group = FALSE)
	      
	      if(length(grc$nextLinks) == length(unique(grc$points$gr))){ #if not cell plot
	        grobX = names[searchPrefix("axis.2-1-2-1", names)]
	        grobXsub = childNames(grid.get(grobX))
	        tooltipsX = rep("", length(grc$nextLinks)) #no tooltips
	        grid.garnish(grobXsub, `data-toggle` = rep("tooltip", length(tooltipsX)), 
	                     class = rep(str_c(grc$plotType, "Cell"), length(tooltipsX)), 
	                     id = grc$nextLinks, group = FALSE)
	      }
	      
	      svg = grid.export(NULL, strict = FALSE, indent = FALSE, annotate = FALSE, res = 72)$svg
	      dev.off(); unlink(outfile)
	      res = HTML(toString.XMLNode(svg))
	    } else {
	      res = tagList(
	        imageOutput(str_c(grc$plotType, "JitterPlotStatic"), height = grc$pich, width = grc$picw),
	        HTML(str_c("<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='", 
	                   grc$picw, "px' height='", grc$pich, "px' viewBox='0 0 ", grc$picw, " ", grc$pich, 
	                   "' version='1.1'></svg>")) #to avoid overlapping with table
	      )
	    }
	    res
	  })
	})
  
  output$hmJitterTable = output$pcaJitterTable = renderDataTable({
    grt = getJitterTable()
    if(is.null(grt)) return(NULL)
    points = grt$points[, c("rnOrig", "cnOrig", "gr", "value")]
    points = points[order(points$gr, points$value, na.last = FALSE), ]
    points$rnOrig = addLinks(v = points$rnOrig, anno = values$data$annoRow)
    points$cnOrig = addLinks(v = points$cnOrig, anno = values$data$annoCol)
    colnames(points) = c("Row ID before processing", "Column ID before processing", "Group ID", "Value")
    points
  }, options = helpTablesOptions, escape = FALSE)
  
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
	      gt = getHeatmap()$q$gtable
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
	      gt = getHeatmap()$q$gtable
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
	      gt = getHeatmap()$q$gtable
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
	  d = readData()
	  if(!is.null(d)){
      if(is.null(d$sep)) d$sep = ","
	    updateNumericInput(session, "uploadNbrColAnnos", value = nAnno(d$annoCol))
	    updateNumericInput(session, "uploadNbrRowAnnos", value = nAnno(d$annoRow))
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
		} else if("e" %in% names(search) && search$e != ""){
		  var = "e"
		  sel = "4"
		} else if("p" %in% names(search) && search$p != ""){
			var = "p"
			sel = "6"
		}
		values$getVar = var
		if(var %in% c("s", "e", "p")){
			id = search[[var]]
			updateTabsetPanel(session, "tabs1", selected = "Data import")
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
  
  #checkbox "change all levels"
  observe({
    annoCol = setAnnoGroups()$annoCol
    annoRow = setAnnoGroups()$annoRow
    grNew = getChangeAllCheckboxes(values$data$inputSaved, annoCol, annoRow)
    grOld = values$changeAllOld
    #which checkbox has changed state (one at a time)
    if(!is.null(grNew) & !is.null(grOld) & (length(grNew) == length(grOld))){
      checked = which(grNew & !grOld)
      unchecked = which(!grNew & grOld)
      if(length(checked) > 0){
        levCol = lapply(annoCol, function(x) names(findCounts(x)))
        levRow = lapply(annoRow, function(x) names(findCounts(x)))
        sel = c(levCol, levRow)[[checked[1]]]
        updateCheckboxGroupInput(session, str_c(names(checked[1]), "tracksub"), selected = sel)
      } else if(length(unchecked) > 0){
        updateCheckboxGroupInput(session, str_c(names(unchecked[1]), "tracksub"), selected = character(0))
      }
    }
  })
  
	observeEvent(input$hmBackButton, {
	  session$sendCustomMessage(type = 'changeClicked', message = list(prefix = "hm", newObject = "", newType = ""))
	})
  
	observeEvent(input$pcaBackButton, {
	  session$sendCustomMessage(type = 'changeClicked', message = list(prefix = "pca", newObject = "", newType = ""))
	})
  
	observeEvent(input$hmBackPreviousButton, {
	  session$sendCustomMessage(type = 'changeClicked', 
      message = list(prefix = "hm", newObject = values$hmClickedObjectPrevious, newType = values$hmClickedTypePrevious))
	})
  
	observeEvent(input$pcaBackPreviousButton, {
	  session$sendCustomMessage(type = 'changeClicked', 
	    message = list(prefix = "pca", newObject = values$pcaClickedObjectPrevious, newType = values$pcaClickedTypePrevious))
	})
  
	#generate filtering options
	#use original unfiltered data for defining groups
	output$uploadColumnFiltersAnno = renderUI({
		data = setAnnoGroups()
    saved = findSaved(data, values, "Col")
    annotationsFilters(data$annoCol, saved, type = "Column", data$annoGroupsCol)
	})
  
	output$uploadRowFiltersAnno = renderUI({
	  data = setAnnoGroups()
	  saved = findSaved(data, values, "Row")
	  annotationsFilters(data$annoRow, saved, type = "Row", data$annoGroupsRow)
	})
	
	output$captionPCA = renderUI({
	  data = getProc()
    if(!is.null(data$matPca)){
      leg = getPCA()$caption
      #http://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
      return(HTML("<h5>Caption example</h5><p>", leg, "</p>"))
    } else {
      return(NULL)
    }
	})
  
	output$captionHeatmap = renderUI({
	  data = getProc()
    if(!is.null(data$matImputed) && all(dim(data$matImputed) <= maxDimensionHeatmap)){
      leg = getHeatmap()$caption
      return(HTML("<h5>Caption example</h5><p>", leg, "</p>"))
    } else {
      return(NULL)
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
