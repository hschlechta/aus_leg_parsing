extract_divisions<-function(text){
  if(str_detect(text,"pdf$")=="TRUE"){ 
    download.file(url=toString(text), destfile = "poop.pdf", mode="wb")
    text<-pdf_text("poop.pdf")
    text<-strsplit(text, split="\n")
    text<-do.call(c, text)
    division_matrix<-matrix(data=NA, nrow=0, ncol=4)
    div_date<-strsplit(text[3], "—")[[1]][2]
    for (i in 1:length(text)){
      if (max(grep("The Council divided", text[i]),0)==1){
        divtitle<-rev(text)[(length(text)-i):length(text)][min(grep("^\\d",rev(text)[(length(text)-i):length(text)]))]
        divtitle<-strsplit(divtitle, "—")[[1]][1]
        div_split<-(min(grep("Recorded", text[i:length(text)]))+i)
        div_end<-min(grep("Question", text[i:length(text)]))+i
        div_ayes<-str_squish(toString(text[(i+2):(div_split-2)]))
        div_noes<-str_squish(toString(text[(div_split+1):(div_end-3)]))
        division_matrix<-rbind(division_matrix,c(div_date,divtitle, div_ayes, div_noes))
      }
    }
    return(division_matrix)
  }
  else{
    return("ERROR: Document not PDF")
  }
}