
#--# Title: Sudoku solver, 28/10/2021

#### Set up ####

rm(list=ls())                                                       # Clear the environment
setwd("D:/Z_Teaching and outreach/2021 Code club")                  # Set working directory

#### Solve Sudoku ####

Puzzle <- as.matrix(read.csv("Sudoku.csv", header = F))             # Import a Sudoku puzzle

while (is.na(mean(Puzzle))) {                                       # Run while there a gaps to be filled                                                                                   
  
  for (row in 1:9) {                                                # Work through the rows

    for (column in 1:9) {                                           # and the columns
    
      if (is.na(Puzzle[row, column])) {                             # Select a cell, Is the cell filled?

      row.poss <- c(1:9)[match(1:9, Puzzle[row,], 0L) == 0L]        # Find possible values for the cell in this row.
 
      col.poss <- c(1:9)[match(1:9, Puzzle[, column], 0L) == 0L]    # Find possible values for the cell in this column.
  
        if ((row-1) %/% 3 == 0 & (column-1) %/% 3 == 0)  {          # Is our cell in the top left box?

          box.poss <-c(1:9)[match(1:9, Puzzle[1:3, 1:3], 0L) == 0L] # Find possible values for the cell in this box
    
        }
  
        if ((row - 1) %/% 3 == 1 & (column - 1) %/% 3 == 0)  {      # Repeat for each other possible box
    
          box.poss <- c(1:9)[match(1:9, Puzzle[4:6, 1:3], 0L) == 0L]
    
        }
  
        if ((row - 1) %/% 3 == 2 & (column - 1) %/% 3 == 0)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[7:9, 1:3], 0L) == 0L]
    
        }
  
        if ((row - 1) %/% 3 == 0 & (column - 1) %/% 3 == 1)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[1:3, 4:6], 0L) == 0L]
          
        }
  
        if ((row - 1) %/% 3 == 1 & (column - 1) %/% 3 == 1)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[4:6, 4:6], 0L) == 0L]
    
        }
  
        if ((row - 1) %/% 3 == 2 & (column - 1) %/% 3 == 1)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[7:9, 4:6], 0L) == 0L]
    
        }
  
        if ((row - 1) %/% 3 == 0 & (column - 1) %/% 3 == 2)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[1:3, 7:9], 0L) == 0L]
    
        }
  
        if ((row - 1) %/% 3 == 1 & (column - 1) %/% 3 == 2)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[4:6, 7:9], 0L) == 0L]
    
        }
    
        if ((row - 1) %/% 3 == 2 & (column - 1) %/% 3 == 2)  {
    
          box.poss <- c(1:9)[match(1:9, Puzzle[7:9, 7:9], 0L) == 0L]
    
        }

 all.poss <- box.poss[box.poss %in% row.poss[row.poss %in% col.poss]] # Find the possible values using all conditions

        if (length(all.poss) == 1 ){                                  # If there is only 1 possible
          
          if(!is.na(all.poss)) Puzzle[row, column] <- all.poss ; print(c(row, column)) # Insert the number in the cell and print position
          
        }
      }
    }
  }  
}

#### Sense checking solution ####

for (row in 1:9) {                                                    # Check whether all rows sum to the same value

  print(sum(Puzzle[row,]))  
}

for (column in 1:9) {                                                 # Check whether all columns sum to the same value
  
  print(sum(Puzzle[, column]))  
}

Pold <- as.matrix(read.csv("Sudoku.csv", header = F))                 # reimport the original puzzle
