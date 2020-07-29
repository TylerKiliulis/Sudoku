guarantee<-function(findguar) {
  for (i in c("1","2","3","4","5","6","7","8","9")) {
    if (sum(findguar==i)<1) { #First, does this row/col/grid have our number of interest, i, already there?
      if (length(grep(i,findguar))==1) { #If not, is there only one place where that i could go?
        findguar[grep(i,findguar)]<-i #If so, make that one place the number i
      }
    }
  }
  return(findguar)
}
elimination<-function(elims) {
  eliminate<-elims[nchar(elims)==1] #Which values are already placed in the row/col/grid?
  if (length(eliminate)>0) {
    for (i in 1:length(eliminate)) {
      elims[elims!=eliminate[i]]<-gsub(eliminate[i],"",elims[elims!=eliminate[i]]) #Those numbers can't go anywhere else in this row/col/grid
    }
  }
  return(elims)
}
basicsolver<-function(scratchworkboard) {
  for (i in 1:9) {
    #Row/Col Guarantees First
    scratchworkboard[i,]<-guarantee(scratchworkboard[i,])
    scratchworkboard[,i]<-guarantee(scratchworkboard[,i])
    #Row/Col eliminations
    scratchworkboard[i,]<-elimination(scratchworkboard[i,])
    scratchworkboard[,i]<-elimination(scratchworkboard[,i])
  }
  #First guarantees, then eliminations for 3x3 grids
    for (j in c(1,4,7)) {
      for (k in c(1,4,7)) {
        scratchworkboard[(j:(j+2)),(k:(k+2))]<-guarantee(scratchworkboard[(j:(j+2)),(k:(k+2))])
        scratchworkboard[(j:(j+2)),(k:(k+2))]<-elimination(scratchworkboard[(j:(j+2)),(k:(k+2))])
      }
    }
  return(scratchworkboard)
}
probabilitysolver<-function(scratchworkboard) {
  for (i in c("1","2","3","4","5","6","7","8","9")) {
    probboard<-matrix(0,nrow=9,ncol=9)
    for (j in 1:9) {
      if (length(grep(i,scratchworkboard[,j]))>1) {
        probboard[grep(i,scratchworkboard[,j]),j]<-1/length(grep(i,scratchworkboard[,j])) #probabilities of "number" by column
      }
    }
    for (j in c(1,4,7)) {
      for (k in c(1,4,7)) {
        probgrid<-probboard[(j:(j+2)),(k:(k+2))]
        for (l in 1:3) { #Do the probabilities in one of the columns of the 3x3 grids add to 1? Then there has to be a "number" in that column, and nowhere else
          if (sum(probgrid[,l])>0.9999 && (sum(probgrid[,l])<1.0001)) { #Just to make sure floating point doesn't matter
            scratchgrid<-scratchworkboard[(j:(j+2)),(k:(k+2))]
            scratchgrid[,-l]<-gsub(i,"",scratchgrid[,-l])
            scratchworkboard[(j:(j+2)),(k:(k+2))]<-scratchgrid
          }
        }
      }
    }
  }
  probboard<-matrix(0,nrow=9,ncol=9) #copied from column one, this time with rows
  for (j in 1:9) {
    if (length(grep(i,scratchworkboard[j,]))>1) {
      probboard[j,grep(i,scratchworkboard[j,])]<-1/length(grep(i,scratchworkboard[j,]))
    }
  }
  for (j in c(1,4,7)) {
    for (k in c(1,4,7)) {
      probgrid<-probboard[(j:(j+2)),(k:(k+2))]
      for (l in 1:3) { 
        if (sum(probgrid[l,])>0.9999 && (sum(probgrid[l,])<1.0001)) { 
          scratchgrid<-scratchworkboard[(j:(j+2)),(k:(k+2))]
          scratchgrid[-l,]<-gsub(i,"",scratchgrid[-l,])
          scratchworkboard[(j:(j+2)),(k:(k+2))]<-scratchgrid
        }
      }
    }
  }
  probboard<-matrix(0,nrow=9,ncol=9) #now this is the other way around, checking grids then looking at row/col
  for (j in c(1,4,7)) {
    for (k in c(1,4,7)) {
      if(length(grep(i,scratchworkboard[(j:(j+2)),(k:(k+2))]))>1) {#probabilities of each "number" in each box
        scratchgrid<-scratchworkboard[(j:(j+2)),(k:(k+2))]
        probgrid<-probboard[(j:(j+2)),(k:(k+2))]
        probgrid[grep(i,scratchgrid)]<-1/length(grep(i,scratchgrid))
        probboard[(j:(j+2)),(k:(k+2))]<-probgrid
      }
    }
  }
  for (j in 1:9) {
    for (k in c(1,4,7)) { #go through columns, if a column in the 3x3 grids add to 1, then get rid of all other "numbers" in that column
      if ((sum(probboard[(k:(k+2)),j])>0.9999) && (sum(probboard[(k:(k+2)),j])<1.0001)) {
        scratchworkboard[-(k:(k+2)),j]<-gsub(i,"",scratchworkboard[-(k:(k+2)),j])
      }
      if ((sum(probboard[j,(k:(k+2))])>0.9999) && (sum(probboard[j,(k:(k+2))])<1.0001)) {#same here for rows
        scratchworkboard[j,-(k:(k+2))]<-gsub(i,"",scratchworkboard[j,-(k:(k+2))])
      }
    }
  }
  return(scratchworkboard)
}
pairtest<-function(findpair) {
  potentialpairs<-combn(9,2) #We'll be checking every single combination of 2 in this row/col/grid
  for (i in 1:36) {
    Double<-findpair[potentialpairs[(1:2),i]]
      if (sum(nchar(Double))==4) {#Make sure the thing we're testing can even be a pair, eliminates a lot of extra stuff too
        Double<-paste(Double,collapse="")
        elim1<-substr(Double,1,1) #removing one of the numbers, suppose that is the number that will go in this combination of 2
        Double<-gsub(elim1,"",Double)
        elim2<-substr(Double,1,1) #doing the same thing, assume one of the remaining numbers will go in this set
        Double<-gsub(elim2,"",Double)
        if (nchar(Double)==0) { #If Double is now an empty string, then 2 numbers made up those 2 potential spots
          findpair[-potentialpairs[(1:2),i]]<-gsub(elim1,"",findpair[-potentialpairs[(1:2),i]])
          findpair[-potentialpairs[(1:2),i]]<-gsub(elim2,"",findpair[-potentialpairs[(1:2),i]])
      }
    }
  }
  return(findpair)
}
tripletest<-function(findtriple) {
  potentialtriples<-combn(9,3) #We'll be checking every single combination of 3 in this row/col/grid
  for (i in 1:84) {
    Triple<-findtriple[potentialtriples[(1:3),i]]
    if (sum(nchar(Triple))>5) {
      if (sum(nchar(Triple))<10) {#Since we're checking every combination, narrow down anything that can't fit in our categories
        Triple<-paste(Triple,collapse="")
        elim1<-substr(Triple,1,1) #removing one of the numbers, suppose that is the number that will go in this combination of 3
        Triple<-gsub(elim1,"",Triple)
        elim2<-substr(Triple,1,1) #doing the same thing, assume one of the remaining numbers will go in this set
        Triple<-gsub(elim2,"",Triple)
        elim3<-substr(Triple,1,1) #and once again
        Triple<-gsub(elim3,"",Triple)
        if (nchar(Triple)==0) { #If Triple is now an empty string, then 3 numbers made up those 3 potential spots
          findtriple[-potentialtriples[(1:3),i]]<-gsub(elim1,"",findtriple[-potentialtriples[(1:3),i]])
          findtriple[-potentialtriples[(1:3),i]]<-gsub(elim2,"",findtriple[-potentialtriples[(1:3),i]])
          findtriple[-potentialtriples[(1:3),i]]<-gsub(elim3,"",findtriple[-potentialtriples[(1:3),i]])
        }
      }
    }
  }
  return(findtriple)
}
tripledoublesolver<-function(scratchworkboard) {
  for (i in 1:9) {
    scratchworkboard[,i]<-pairtest(scratchworkboard[,i])
    scratchworkboard[i,]<-pairtest(scratchworkboard[i,])
    scratchworkboard[i,]<-tripletest(scratchworkboard[i,])
    scratchworkboard[,i]<-tripletest(scratchworkboard[,i])
  }
  for (i in c(1,4,7)) {
    for (j in c(1,4,7)) {
      scratchworkboard[(i:(i+2)),(j:(j+2))]<-pairtest(scratchworkboard[(i:(i+2)),(j:(j+2))])
      scratchworkboard[(i:(i+2)),(j:(j+2))]<-tripletest(scratchworkboard[(i:(i+2)),(j:(j+2))])
    }
  }
  return(scratchworkboard)
}
CheckIfCorrect<-function(board,numunique) {
  mistake<-FALSE
  for (i in 1:9) {
    if (length(unique(board[1:numunique,i]))!=numunique) {
      mistake<-TRUE
    }
  }
  if (numunique==9) {
    if (!mistake) {
      for (i in 1:9) {
        if (length(unique(board[i,1:9]))!=9) {
          mistake<-TRUE
        }
      }
      for (i in c(1,4,7)) {
        for (j in c(1,4,7)) {
          if(length(unique(c(board[(i:(i+2)),(j:(j+2))])))!=9) {
            mistake<-TRUE
          }
        }
      }
    }
  }
  return(mistake)
}
randomsolver<-function(scratchworkboard) {
  everypossibility<-list(NA,NA,NA,NA,NA,NA,NA,NA,NA)
  for (j in 1:9) {
    focus<-scratchworkboard[j,] #chooses which row we're focusing on
    possibleoptions<-strsplit(focus,"") #gets all the different characters in each segment in the row
    allpossibilities<-matrix(NA,nrow=prod(nchar(focus)),ncol=9)
    k=1 #We're creating a big list of possible *singular* rows, next we'll compare them with each other
    for (a in 1:length(possibleoptions[[1]])) {
      for (b in 1:length(possibleoptions[[2]])) {
        for (c in 1:length(possibleoptions[[3]])) {
          for (d in 1:length(possibleoptions[[4]])) {
            for (e in 1:length(possibleoptions[[5]])) {
              for (f in 1:length(possibleoptions[[6]])) {
                for (g in 1:length(possibleoptions[[7]])) {
                  for (h in 1:length(possibleoptions[[8]])) {
                    for (i in 1:length(possibleoptions[[9]])) {
                      allpossibilities[k,]<-c(possibleoptions[[1]][a],possibleoptions[[2]][b],possibleoptions[[3]][c],possibleoptions[[4]][d],possibleoptions[[5]][e],possibleoptions[[6]][f],possibleoptions[[7]][g],possibleoptions[[8]][h],possibleoptions[[9]][i])
                      k=k+1
                    }}}}}}}}}
    for (i in 1:prod(nchar(focus))) { #takes the possible singular rows and gets rid of repeats in the rows themselves
      if (length(unique(allpossibilities[i,]))<9) {
        allpossibilities[i,]<-NA
      }
    }
    allpossibilities<-matrix(allpossibilities[!is.na(allpossibilities)],ncol=9)
    everypossibility[[j]]<-allpossibilities
  }
  refiningpossibilities<-list(NULL)
  for (a in 1:length(everypossibility[[1]][,1])) { #This just starts off the pairing
    for (b in 1:length(everypossibility[[2]][,1])) {
      allpossibilities<-rbind(everypossibility[[1]][a,1:9],everypossibility[[2]][b,1:9])
      if (!CheckIfCorrect(allpossibilities,2)) {
        refiningpossibilities<-c(refiningpossibilities,list(allpossibilities))
      }
    }
  } #After this, it's the same bit of code, except adding 2 rows to 1, then 3 rows to 1, etc.
  for (i in 3:9) {
  refinedpossibilities<-refiningpossibilities[-1]
  refiningpossibilities<-list(NULL)
  for (a in 1:length(refinedpossibilities)) {
    for (b in 1:length(everypossibility[[i]][,1])) {
      allpossibilities<-rbind(refinedpossibilities[[a]],everypossibility[[i]][b,1:9])
      if (!CheckIfCorrect(allpossibilities,i)) {#will only check boards that are totally possible
        refiningpossibilities<-c(refiningpossibilities,list(allpossibilities))
      }
    }
  }
  }
  refinedpossibilities<-refiningpossibilities[-1]#get rid of the null needed to create the list in the first place
  return(matrix(unlist(refinedpossibilities),nrow=9,ncol=9))
}
sudoku<-function(origboard) {
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
}
