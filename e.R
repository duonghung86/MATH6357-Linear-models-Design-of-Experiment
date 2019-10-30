coln=c('X','Y','Z')
rown=c('Asp','Con','Gra')
rows=1
#Read each line of the data file
while (length(oneLine <- readLines(df, n = 1, warn = FALSE)) > 0) {
  num_lis = (strsplit(oneLine, ", ")) #List of values in 1 line
  num_lis = as.numeric(num_lis[[1]]) # Convert them to numeric
  #Category the imported data
  interval=length(num_lis)
  i=1
  while (i<=interval){
    dat$brand[k]=coln[i%/%4+1]
    dat$road[k]=rown[rows]
    dat$ob1[k]=num_lis[i]
    dat$ob2[k]=num_lis[i+1]
    dat$ob3[k]=num_lis[i+2]
    dat$ob4[k]=num_lis[i+3]
    i=i+4
    k=k+1
  }
  rows=rows+1
} 
close(df)
dat=data.frame(dat)