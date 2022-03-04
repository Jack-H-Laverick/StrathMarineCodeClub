
#--# Title: Sudoku solver, 04/03/2022

#### My functions ####

Find_possible_values <- function(Filled_already) {

  if(length(Filled_already) != 9) stop("Not 9 values supplied") # Return error if we don't pass 9 values
  
  Possibles <- which(!1:9 %in% Filled_already)                  # Return the values currently missing from Filled_already
  
  return(Possibles)
}

Find_box <- function(row_or_column) {

  test <- (row_or_column-1) %/% 3                                        # Find the box for current cell
  to_select <- 0:8 %/% 3 == test                                         # Find other cells in the same box

  return(to_select)  
}

#### Solve Sudoku ####

Puzzle <- as.matrix(read.csv("./Data/Sudoku.csv", header = F))      # Import a Sudoku puzzle

while (anyNA(Puzzle)) {                                             # Run while there a gaps to be filled                                                                                   
  
  for (row in 1:9) {                                                # Work through the rows

    for (column in 1:9) {                                           # and the columns
    
      if (is.na(Puzzle[row, column])) {                             # Select a cell, Is the cell filled?

      row.poss <- Find_possible_values(Puzzle[row,])                # Find possible values for the cell in this row.
 
      col.poss <- Find_possible_values(Puzzle[,column])             # Find possible values for the cell in this column.

      box.poss <- Find_possible_values(Puzzle[Find_box(row), Find_box(column)]) # Find possible values for the cell in this box
      
      all.poss <- box.poss[box.poss %in% row.poss[row.poss %in% col.poss]] # Find the possible values using all conditions

      if (length(all.poss) == 1 ){                                  # If there is only 1 possible
        
        if(!is.na(all.poss)) Puzzle[row, column] <- all.poss ; print(c(row, column)) # Insert the number in the cell and print position
          
        }
      }
    }
  }  
}

#### Sense checking solution ####

rowSums(Puzzle)                                                     # Check whether all rows sum to the same value
colSums(Puzzle)                                                     # Check whether all columns sum to the same value
  
Pold <- as.matrix(read.csv("./Data/Sudoku.csv", header = F))        # reimport the original puzzle
