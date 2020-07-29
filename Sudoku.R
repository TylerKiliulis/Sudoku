source("Functions.R")

#List of practice sudoku boards to see how it works
practiceboard<-c(7,9,0,4,0,0,0,0,0,4,3,6,8,0,9,0,1,7,5,0,1,0,0,0,0,0,9,6,0,0,1,0,0,4,0,0,0,0,2,7,0,3,5,0,0,0,0,7,0,0,6,0,0,8,2,0,0,0,0,0,9,0,6,3,6,0,5,0,8,1,2,4,0,0,0,0,0,2,0,5,3)
practiceboard<-c(0,0,1,3,6,9,7,4,0,0,2,0,0,1,7,0,0,0,0,0,0,0,4,0,0,0,9,0,0,2,9,0,0,6,0,7,0,0,0,0,0,0,0,0,0,5,0,3,0,0,1,4,0,0,2,0,0,0,9,0,0,0,0,0,0,0,6,3,0,0,7,0,0,1,4,2,7,5,9,0,0)
practiceboard<-c(9,0,1,0,0,8,0,0,0,5,3,0,0,0,0,0,0,9,6,0,0,0,0,0,7,0,0,1,0,0,4,0,0,8,0,0,0,0,2,9,3,5,1,0,0,0,0,5,0,0,1,0,0,3,0,0,7,0,0,0,0,0,6,2,0,0,0,0,0,0,3,4,0,0,0,1,0,0,5,0,8)
practiceboard<-c(0,0,0,1,0,6,0,5,2,6,0,0,0,0,5,0,9,0,0,0,4,0,0,0,0,0,0,0,0,6,0,0,0,0,4,9,9,0,0,0,8,0,0,0,1,3,2,0,0,0,0,7,0,0,0,0,0,0,0,0,9,0,0,0,1,0,5,0,0,0,0,6,4,8,0,3,0,1,0,0,0)
practiceboard<-c(0,0,1,3,0,0,0,2,8,0,4,0,0,0,6,0,0,0,0,6,0,1,5,0,0,0,0,0,0,0,6,0,0,0,0,7,0,0,4,0,1,0,2,0,0,1,0,0,0,0,5,0,0,0,0,0,0,0,2,4,0,7,0,0,0,0,5,0,0,0,6,0,7,9,0,0,0,1,3,0,0)
practiceboard<-c(7,0,0,0,0,0,0,0,0,3,0,0,0,0,4,8,2,1,0,2,0,3,0,0,6,0,0,9,8,0,7,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,1,0,0,9,5,0,7,0,3,0,8,0,1,0,0,0,0,0,0,0,0,0,0,9,0,5,1,0,0,9,3,0,0,0)
practiceboard<-c(0,8,0,0,0,9,0,0,0,1,5,4,0,0,0,0,9,0,0,0,3,1,0,7,8,0,0,7,0,0,0,0,0,0,0,0,0,0,0,9,8,2,0,0,0,0,0,0,0,0,0,0,0,1,0,0,5,3,0,1,6,0,0,0,6,0,0,0,0,7,5,3,0,0,0,5,0,0,0,4,0)

#Run as a single function:

# sudoku(practiceboard)

#Run as a script:
origboard<-practiceboard
difficulty<-NULL
complete<-FALSE
starttime<-proc.time()
origboard=matrix(origboard,nrow=9,ncol=9)
scratchworkboard <- origboard
scratchworkboard[scratchworkboard==0]<-"123456789"
numchars<-sum(nchar(scratchworkboard))
#Start basic solving (Tier 1 Solve)
scratchworkboard<-basicsolver(scratchworkboard)
while (sum(nchar(scratchworkboard))<numchars) {
  numchars<-sum(nchar(scratchworkboard))
  scratchworkboard<-basicsolver(scratchworkboard)
}
if (numchars==81) {
  difficulty<-"Easy"
  complete<-TRUE
}
#Start probability-based solving (Tier 2 Solve)
if (!complete) {
  scratchworkboard<-probabilitysolver(scratchworkboard)
  while (sum(nchar(scratchworkboard))<numchars) {
    numchars<-sum(nchar(scratchworkboard))
    scratchworkboard<-basicsolver(scratchworkboard)
    scratchworkboard<-probabilitysolver(scratchworkboard)
  }
  if (numchars==81) {
    difficulty<-"Moderate"
    complete<-TRUE
  }
}
#Start pair and pair-like solving (Tier 3 Solve)
if (!complete) {
  scratchworkboard<-tripledoublesolver(scratchworkboard)
  while (sum(nchar(scratchworkboard))<numchars) {
    numchars<-sum(nchar(scratchworkboard))
    scratchworkboard<-basicsolver(scratchworkboard)
    scratchworkboard<-probabilitysolver(scratchworkboard)
    scratchworkboard<-tripledoublesolver(scratchworkboard)
  }
  if (numchars==81) {
    difficulty<-"Hard"
    complete<-TRUE
  }
}
#Start random selection solve (Tier 4 Solve)
if (!complete) {
  scratchworkboard<-randomsolver(scratchworkboard)
  numchars<-sum(nchar(scratchworkboard))
  if (!complete) {difficulty<-"Expert"}
}
endtime<-proc.time()-starttime
scratchworkboard<-matrix(as.numeric(scratchworkboard),nrow=9,ncol=9)
print.noquote("Original board:")
print(origboard)
print.noquote(paste("Difficulty: ",difficulty,sep=""))
print.noquote("Solution:")
print(scratchworkboard)
print.noquote("Time for program to solve:")
print(endtime)
