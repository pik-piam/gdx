#' addDimPrideGdx
#' 
#' Function to expand Pride Model GDX's.
#' 
#' 
#' @usage addDimPrideGdx(file)
#' @param file File name of the gdx file
#' @author Anastasis Giannousakis
#' @export
#' @examples
#' 
#' \dontrun{addDimPrideGdx("bla.gdx")}
#' 
addDimPrideGdx<-function(file){
  ogdx_raw<-readGDX(file,types=c("variables","equations"),format="raw")
  for (i in 1:length(ogdx_raw)){
    if (ogdx_raw[i][[1]][[3]]=="q_expobjective") next
    if (ogdx_raw[i][[1]][[3]]=="v_expwelfare") next 
    ogdx_raw[i][[1]][[3]]<-ogdx_raw[i][[1]][[3]]+1
    ogdx_raw[i][[1]][[7]]<-c("*",ogdx_raw[i][[1]][[7]])
    for (j in 1:(ogdx_raw[i][[1]][[3]])){
      ogdx_raw[i][[1]][[6]][[ogdx_raw[i][[1]][[3]]+1-j+1]]<-ogdx_raw[i][[1]][[6]][[ogdx_raw[i][[1]][[3]]-j+1]]
    }
    ogdx_raw[i][[1]][[6]][[length(ogdx_raw[i][[1]][[6]])-1]]<-c("low","high")
    ogdx_raw[i][[1]][[4]]<-rbind(ogdx_raw[i][[1]][[4]],ogdx_raw[i][[1]][[4]])
    ogdx_raw[i][[1]][[4]]<-cbind(ogdx_raw[i][[1]][[4]],ogdx_raw[i][[1]][[4]][,(ogdx_raw[i][[1]][[3]]+1)])
    for (j in 1:(ogdx_raw[i][[1]][[3]])){
      ogdx_raw[i][[1]][[4]][,(ogdx_raw[i][[1]][[3]]+2-j)]<-ogdx_raw[i][[1]][[4]][,(ogdx_raw[i][[1]][[3]]+2-j-1)]
    }
    if(!length(ogdx_raw[i][[1]][[4]][,1])==0) ogdx_raw[i][[1]][[4]][1:(length(ogdx_raw[i][[1]][[4]][,1])/2),(length(ogdx_raw[i][[1]][[4]][1,])-2)]<-rep(1,length(ogdx_raw[i][[1]][[4]][,1])/2)
    if(!length(ogdx_raw[i][[1]][[4]][,1])==0) ogdx_raw[i][[1]][[4]][(length(ogdx_raw[i][[1]][[4]][,1])/2+1):length(ogdx_raw[i][[1]][[4]][,1]),(length(ogdx_raw[i][[1]][[4]][1,])-2)]<-rep(2,length(ogdx_raw[i][[1]][[4]][,1])/2)
    gdxname<-paste("n",file,sep="_")
  }
  writeGDX(ogdx_raw,gdxname)
}
