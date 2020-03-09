#' Read JSON files from folder
#'
#' @param folderPath 
#'
#' @return
#' @export
#'
#' @examples
readJSONFiles <- function(folderPath){
  fileList <- list.files(folderPath,pattern = "json$",
                         recursive = T,full.names = T)
  
  combinedList <- list()
  
  for(file in fileList){
    print(file)
    content <- readInfosFromJson(file)
    if(length(combinedList)>0){
      combinedList <- combineList(combinedList,content)
    }else{
      combinedList <- content
    }
  }
  return(combinedList)
}

#tlvar$nonUsedFileInfos <- list()

mergeFileInfos <- function(fi1,fi2){
  if(isTRUE(all.equal(fi1,fi2))){
    #tlvar$nonUsedFileInfos[[fi1$getKey()]] <- fi1
    return(fi2)
  }else if(fi1[["accessInfo"]]=="write"){
      if(fi2[["accessInfo"]]=="write"){
        stop()#Write to error log
      }else {
        #tlvar$nonUsedFileInfos[[fi2$getKey()]] <- fi2
        return(fi1)
      }
    }else if(fi2[["accessInfo"]]=="write"){
      if(fi1[["accessInfo"]]=="write"){
        stop()#Write to error log
      }else {
        return(fi2)
        #tlvar$nonUsedFileInfos[[fi1$getKey()]] <- fi1
    }
  }else{
    #tlvar$nonUsedFileInfos[[fi1$getKey()]] <- fi1
    return(fi2)
  }
}

systemEqual <- function(si1, si2){
  if(isTRUE(all.equal(si1,si2))){
  return(TRUE)
  }else{
    if(!is.null(si1$systemId) && !is.null(si2$systemId)){
      if(si1$systemId != si2$systemId){
        si1List <- si1$toList()
        si2List <- si2$toList()
        si1List$systemId <- NULL
        si2List$systemId <- NULL
        if(identical(si1List,si2List)){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }else{
      return(FALSE)
    }
    }else{
      return(FALSE)
    }
  }
}

actionEqual <- function(ai1, ai2){
  if(ai1$actionName != ai2$actionName){
    return(FALSE)
  }else{
    if(!compareFiList(ai1$outputFileInfos,ai2$outputFileInfos)){
      return(FALSE)
    }else if(!compareFiList(ai1$inputFileInfos,ai2$inputFileInfos)){
      return(FALSE)
    }else if(!identical(ai1$scriptFileInfo,ai2$scriptFileInfo)){
      #  }else if(ai1$scriptFileInfo!=ai2$scriptFileInfo){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
  
}

compareFiList <- function(fl1, fl2){
  val <- if(length(fl1) != length(fl2)){
    FALSE
  }else{
    if(length(fl1)>0){
      fl1 <- fl1[order(names(fl1))]
      fl2 <- fl2[order(names(fl2))]
      
      for(i in 1:length(fl1)){
        if (!identical(fl1[[i]],fl2[[i]])){
          return(FALSE)
        }
      }
    }else{
      TRUE
    }
  }
  return(val)
}

updateReferences <- function(aiList, fiList){
  for(ai in aiList){
    iFis <- ai$inputFileInfos
    ai$inputFileInfos <- list()
    oFis <- ai$outputFileInfos
    ai$outputFileInfos <- list()
    sFi <- ai$scriptFileInfo
    
    ai$scriptFileInfo <- fiList[[sFi$getKey()]]
    
    for(fi in iFis){
      ai$inputFileInfos[[fi$getKey()]] <- fiList[[fi$getKey()]]
    } 
    
    for(fi in oFis){
      ai$outputFileInfos[[fi$getKey()]] <-fiList[[fi$getKey()]]
    }
  }
  return(aiList)
}

combineList <- function (x, y) 
{
  stopifnot(is.list(x), is.list(y))
  
  f1 <- x$fileInfos
  f2 <- y$fileInfos
  
  a1 <- x$actionInfos
  a2 <- y$actionInfos
  
  s1 <- x$systemInfos
  s2 <- y$systemInfos
  
  f1names <- names(f1)
  f2names <- names(f2)
  
  #Combine file Infos
  for (n in f2names) {
    f1[[n]] <- if(n %in% f1names){
      mergeFileInfos(f1[[n]], f2[[n]])
    } else{
      f2[[n]]
    } 
  }
  
  #Combine System infos
  iNew <- length(s1) + 1
  for (i2 in 1:length(s2)){
    si2 <- s2[[i2]]
    
    elementFound <- F
    for (si1 in s1){
      if (systemEqual(si1,si2)){
        elementFound <- T
        # update systemId references in a2
        for (ai2 in a2){
          if (ai2$systemId == si2$systemId) { 
            ai2$systemId <- si1$systemId
          }
        }
        break
      }
      
      if (!elementFound) {
        # update systemId
        si2systemIdOld <- si2$systemId
        si2$systemId <- iNew
        iNew < iNew + 1
        # update systemId references in a2
        for (ai2 in a2){
          if (ai2$systemId == si2systemIdOld) { 
            ai2$systemId <- si2$systemId
          }
        }
        
        # append si2
        s1 <- c( s1, si2 )
      }
      
    }
    
    a1 <- updateReferences(a1,f1)
    a2 <- updateReferences(a2,f1)
    
    iNew <- length(a1)+1
    #Combine action infos
    i=1 # because elements can be removed from list a2 a for loop (1:length(a2)) cannot be used, because length(a2) is evaluated just once in the beginning
    while(i <= length(a2)){
      print(paste0("iterate element ", i))
      ai2 <- a2[[i]]
      # look for all following actionInfos in a2, if we consider them to be equal (and therefore can remove them)
      elementFound <- F
      for(j in 1:length(a1)){
        ai1 <- a1[[j]]
        if(actionEqual(ai1,ai2)){
          elementFound <- T
          
          if(ai2$executed > ai1$executed){
            # use the most recent execution timestamp (because last write of files win)
            ai1$executed <- ai2$executed
            ai1$executedBy <- ai2$executedBy
            ai1$description <- ai2$description
          }
          # replace references, where ai2 is used as superAction
          for(aix in a2){
            if(identical(aix$superAction, ai2)){
              aix$superAction <- ai1
              aix$superActionId <- ai1$actionId
            }
          }
          a2[[i]] <- NULL
          
          print(paste0("removed element ", i, "  length a2 ", length(a2)))
          break
        }
        
      }
      
      if(!elementFound) {
      
        ai2$actionId <- iNew
        
        iNew <- iNew+1
        
        for(aix in a2) { 
          if (identical(aix$superAction, ai2)){
            aix$superActionId <- ai2$actionId
          }
        }
        
        # adjust the actionInfo information at the output files to the new actionId
        for (fi in ai2$outputFileInfos){
          fi$actionInfo <- paste(ai2$actionId, ai2$actionName, collapse = "-")
        }
        
        i <- i+1 # increment i only then, if not by a2[[i]] <- NULL the following elements i+1..n are shifted left to i..n-1
      }
    }
  }
  
  # a1 <- c(a1,a2) does not work properly because keys in named list a2 were not changed
  for(ai in a2){
    a1[[as.character(ai$actionId)]]<- ai  
  }
  
  
  
  return(list(fileInfos = f1,
              actionInfos = a1,
              systemInfos = s1))
}

readSVNInformation <- function(filePathList){
  for(filePath in filePathList){
    if(filePath != "" && !is.null(filePath)){
      svnInfo <- getSVNInfo(filePath)
      
      res[[filePath]] <- svnInfo
    }
  }
  return(res)
}

# implementation started, but not ok
# updateMetadata <- function(metadata,svnInfo){
#   
#   fileInfos <- metadata$fileInfos
#   actionInfos <- metadata$actionInfos
#   systemInfos <- metadata$systemInfos
#   
#   #Determines Activity_ID  and enriches activity and file infos by this activityID
#   #Enriches fileInfos with repoType = "Local"  by information from pairlist
#   for(fi in fileInfos){
#     if(fi$repoType = "Local"){
#       svnInfo <- getSVNInfo(fi$localFilePath)
#       if(!is.null(svnInfo)){
#         fi$repoType <- "SVN"
#         fi$repoPath <- svnInfo$repoPath
#         fi$repoVersion <- svnInfo$repoVersion
#         fi$repoFileStatus <- svnInfo$repoFileStatus
#         fi$repoModified <- svnInfo$repoModified
#         fi$repoRoot <- svnInfo$repoRoot
#         
#         activityId <- determineActivityId(repoPath = fi$repoPath)
#       }else{
#         #Warning: The file infos for the file is not in repository. can't determine activity ID, 
#       }
#     }
#     
#     #Checks/compares repo information for repoType = "SVN" and collects differences
#     if(fi$repoType = "SVN"){
#       activityId <- determineActivityId(repoPath = fi$repoPath)
#       svnInfo <- getSVNInfo(fi$localFilePath)
#       if(fi$repoPath != svnInfo$repoPath){
#         #repopath mismatch
#       }else if(fi$repoVersion != svninfo$repoVersion){
#         #repoVersion mismatch
#       }else if(fi$repoFileStatus != svnInfo$repofileStatus){
#         #repoFile status mismatch
#       }else if(fi$repoModified != svnInfo$repoModified){
#         #repo modified mismatch
#       }else if(fi$repoRoot != svnInfo$repoRoot){
#         #repo root mismatch
#       }else{
#         #no mismatch
#       }
#     }
#   }
#   
#   for (ai in actionInfos){
#     ai$activityId <- activityId
#   }
#   
#   return(list(actionInfos = actionInfos,
#               fileInfos = fileInfos,
#               systemInfos = systemInfos))
#   
# }

# isReferenced <- function(x) {
#   nom <- as.character(substitute(x))
#   ls_ <- ls(parent.frame())
#   ls_ <- ls_[ls_ != nom]
#   tr <- tracemem(x)
#   for (i in ls_) {
#     if (identical(x, get(i, envir = parent.frame()))) {
#       if (identical(tr, tracemem(get(i, envir = parent.frame())))) {
#         untracemem(x)
#         untracemem(get(i, envir = parent.frame()))
#         print(i)
#         return(TRUE)
#       } else {
#         untracemem(get(i, envir = parent.frame()))
#       }
#     }
#   }
#   untracemem(x)
#   FALSE
# } 



# mergeActionInfos <- function(ai1, ai2){
#   if(actionEqual(ai1,ai2)){
#     if(ai2$executed > ai1$executed){
#       return(ai2)
#       tlvar$nonUsedActionInfos <- ai1
#     }else if(ai1$executed > ai2$executed){
#       return(ai1)
#       tlvar$nonUsedActionInfos <- ai2
#     }
#   }else{
#     ##
#   }
# }