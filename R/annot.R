annot <-
function(n=FALSE, path="")
{  
  annotlist<-function(annot, list_id, path, nom, n)
  {
    if(n)  list_id = read.table(file=list_id, header=TRUE, sep="\t")
    rownames(list_id) = list_id[,1]
    #liste seulement d'ID => 1 colonne
    if(ncol(list_id)==1)  res = annot[duplicated(c(rownames(list_id), rownames(annot)))[nrow(list_id):nrow(annot)],]
    #liste seulement d'ID + datas => >=2 colonnes 
    if(ncol(list_id)>=2)  res = cbind(annot[duplicated(c(rownames(list_id), rownames(annot)))[(nrow(list_id)+1):(nrow(annot)+nrow(list_id))],], list_id[,2:ncol(list_id)])  
    if(nrow(res)!=0)
    {
      write.table(res, file = paste(getwd(), "/", nom, "_annot.txt", sep=""), sep="\t")
    }else{
      write("No annotation found.", file="")
    }
    return(TRUE)
  }
  
  write("\t########################################################", file="")
  write("\t#                                                      #", file="")
  write("\t#                     Annot (v0.1)                     #", file="")
  write("\t#                                                      #", file="")
  write("\t########################################################\n", file="")
  write("\t[Run man() for quick help]\n", file="")
  
  write("Annotations file", file="")
  flush.console()
  if(Sys.info()["sysname"]=="Windows") #windows
  {
    annot = read.table(file=file.choose(), header=TRUE, row.names=1, sep="\t")
  }else{
    capabilities( )["tcktk"]  #linux
    require(tcltk)
    annot = read.table(file=tk_choose.files(), header=TRUE, row.names=1, sep="\t")
  }
  
  if(!n)  #One list annotation
  {
    write("List to annotate", file="")
    flush.console()
    if(Sys.info()["sysname"]=="Windows")  list_id = read.table(file=file.choose(), header=TRUE, sep="\t")
    if(Sys.info()["sysname"]!="Windows")  list_id = read.table(file=tk_choose.files(), header=TRUE, sep="\t")
    annotlist(annot, list_id, path=getwd(), nom="list")
  }else{  #Multiple lists annotation
    write("Lists folder", file="")
    flush.console()
    if(Sys.info()["sysname"]=="Windows")  listes = as.matrix(list.files(path = choose.dir(), full.names = TRUE))
    if(Sys.info()["sysname"]!="Windows")  listes = as.matrix(list.files(path = tk_choose.dir(), full.names = TRUE))
    apply(listes, 1, function(x) annotlist(annot, x, path=getwd(), nom=substr(basename(x), 0, (nchar(basename(x))-4)), n))
  }

  write(paste("Resulting files have been placed: ", getwd(), sep=""), file="")
}

