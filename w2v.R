w2v4<-function()
{
  cat("__________Data Prep______________","\n")
  N<-readline(prompt="How many lines would you like to view? ")
  file_dir<-readline(prompt="Enter vector file path: ")
  V<-readLines(file_dir, N)
  file_dir_2<-readline(prompt="Enter word file path: ")
  W<-readLines(file_dir_2, N)
  cat("_____________________________","\n")
  i<-readline(prompt="Enter word: ")
  H<-hash(c(W),c(V))
  vi<-as.numeric(unlist(strsplit(str_trim(H[[i]], "left"), " ")))
  vi.s = vi^2
  vi.mag = sqrt(sum(vi.s))
  lstvk<-list()
  lstvk.s<-list()
  lstvk.mag<-list()
  lstcs<-list() 
  x<-readline(prompt="How many instances would you like to view?: ")
  file_n<-readline(prompt="What woud you like to name this session?: ")
  vec_mat<-NULL
  vecs<-NULL
  for (n in (1:N)){ 
    lstvk[[n]]<-as.numeric(unlist(strsplit(str_trim(V[[n]]), " ")))
    vec_mat<-rbind(lstvk[[n]], vec_mat)
    lstvk.s[[n]]<- lstvk[[n]]^2
    lstvk.mag[[n]]<-sqrt(sum(lstvk.s[[n]]))
    lstcs[[n]]<- ((lstvk[[n]]%*%vi)/(lstvk.mag[[n]]*vi.mag))
    vecs[n] = lstcs[[n]]
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  cosine(vi, vec_mat)
  cat("the closest ",x," instances to ", "'",i,"'","(in descending order):","\n")
  a<-c(sort(vecs,decreasing=TRUE))
  p<-a[-1]
  px<-p[1:x]
  print(W[match(px, vecs)])
  words<-hash(px, W[match(px, vecs)])
  print("In Ascending Similarity")
  print(words)
  save(words, file = file_n)
  
  
  
  
}

