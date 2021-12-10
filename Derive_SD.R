library(ggm)
#############################
# manually input the condition
subsets_satisfying_condition <- function(numbers){
  x = c(list(" "), powerset(numbers))
  num = vector(mode = "list", length = 2)
  j=1
  for(i in 1:length(x)){
    if( 
      ( (4 %in% x[[i]] && 1 %in% x[[i]]) || !(4 %in% x[[i]]) )&& 
      ( (2 %in% x[[i]] && 1 %in% x[[i]]) || !(2 %in% x[[i]]) )&& 
      ( (3 %in% x[[i]] && 1 %in% x[[i]]) || !(3 %in% x[[i]]) )&& 
      ( (6 %in% x[[i]] && 1 %in% x[[i]] && 5 %in% x[[i]]) || !(6 %in% x[[i]]) )&& 
      ( (7 %in% x[[i]] && 1 %in% x[[i]] && 4 %in% x[[i]] && 5 %in% x[[i]] && 6 %in% x[[i]]) || !(7 %in% x[[i]]) )&& 
      ( (10 %in% x[[i]] && 1 %in% x[[i]]) || !(10 %in% x[[i]]) )&& 
      ( (11 %in% x[[i]] && 1 %in% x[[i]]) || !(11 %in% x[[i]]) )&& 
      ( (12 %in% x[[i]] && 1 %in% x[[i]] && 8 %in% x[[i]]) || !(12 %in% x[[i]]) )&& 
      ( (13 %in% x[[i]] && 1 %in% x[[i]] && 9 %in% x[[i]]) || !(13 %in% x[[i]]) )
    ){
      num[j] = x[i]
      j= j+1
    }
  }
  return(c(num,list()))
}
numbers = seq(1,18)

start_time <- Sys.time()
k = subsets_satisfying_condition(numbers)
end_time <- Sys.time()
end_time - start_time
# 3.5s
#k = k[-which(sapply(k, is.null))]
length(k)
K=lapply(k,FUN=function(x) c(x,c("19,20,21,22,23,24,25,26,27,28")))
KK=sapply(K, paste, collapse = ",")
#KK=lapply(K,FUN=function(x) c(x,c("19, 20")))


fileConn<-file("selection dictionary.txt")
writeLines(KK, fileConn)
close(fileConn)
##################################



# unit function when the unit rule is select all variables in F
unit=function(F, V){
  # F is a vector, V is a vector
  b=powerset(setdiff(V, F), nonempty = FALSE)
  Result=lapply(b, FUN=function(x) union(F,x))
  return(Result)}



sort.list=function(A){
  return(lapply(A, FUN=function(x) sort(x)))
}

#change character to list
CCL=function(A){
  return(lapply(strsplit(A," "), FUN=function(x) as.numeric(x)))
}

#intersect.list
IL=function(A,B){
  A=sort.list(A)
  AA=sapply(A , paste, collapse = " ")
  B=sort.list(B)
  BB=sapply(B , paste, collapse = " ")
  return(as.list(AA[which(AA %in% BB)]))
}

#power set minus a set
set.difference=function(P,A){
  A=sort.list(A)
  P=sort.list(P)
  AA=sapply(A , paste, collapse = " ")
  PP=sapply(P , paste, collapse = " ")
  return(as.list(PP[-which(PP %in% AA)]))
}

#if then operation
ifthen=function(A,B,V){
  P=powerset(V, nonempty = FALSE)
  C=set.difference(P,A)
  D=IL(A,B)
  Result=append(C,D)
  Result=unique(sapply(Result , paste, collapse = " "))
  return(Result)
}



#################################################
V=seq(18)
start_time <- Sys.time()
a1=as.list(ifthen(unit(c(4),V),unit(c(1),V),V))
a2=as.list(ifthen(unit(c(2),V),unit(c(1),V),V))
a3=as.list(ifthen(unit(c(3),V),unit(c(1),V),V))
a4=as.list(ifthen(unit(c(6),V),unit(c(1,5),V),V))
a5=as.list(ifthen(unit(c(7),V),unit(c(1,4,5,6),V),V))
a6=as.list(ifthen(unit(c(10),V),unit(c(1),V),V))
a7=as.list(ifthen(unit(c(11),V),unit(c(1),V),V))
a8=as.list(ifthen(unit(c(12),V),unit(c(1,8),V),V))
a9=as.list(ifthen(unit(c(13),V),unit(c(1,9),V),V))
M=sapply(IL(IL(IL(IL(IL(IL(IL(IL(a1,a2),a3),a4),
                        a5),a6),a7),a8),a9) , paste, collapse = " ")
end_time <- Sys.time()
end_time - start_time
length(M)
# 5 mins
