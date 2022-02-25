
#--# Title: Sudoku solver, 25/02/2022

#### My functions ####

Find_possible_values <- function(Filled_already) {
  
  Possibles <- which(!1:9 %in% Filled_already)         # Return the values currently missing from Filled_already
  
  return(Possibles)
}

#### Solve Sudoku ####

Puzzle <- as.matrix(read.csv("./Data/Sudoku.csv", header = F))      # Import a Sudoku puzzle

while (anyNA(Puzzle)) {                                              # Run while there a gaps to be filled                                                                                   
  
  for (row in 1:9) {                                                 # Work through the rows

    for (column in 1:9) {                                            # and the columns
    
      if (is.na(Puzzle[row, column])) {                              # Select a cell, Is the cell filled?

      row.poss <- Find_possible_values(Puzzle[row,])                 # Find possible values for the cell in this row.
 
      col.poss <- Find_possible_values(Puzzle[,column])              # Find possible values for the cell in this column.
  
        if ((row-1) %/% 3 == 0 & (column-1) %/% 3 == 0)  {           # Is our cell in the top left box?

          box.poss <- Find_possible_values(Puzzle[1:3, 1:3])         # Find possible values for the cell in this box
    
        }
  
        if ((row - 1) %/% 3 == 1 & (column - 1) %/% 3 == 0)  {      # Repeat for each other possible box
    
          box.poss <- Find_possible_values(Puzzle[4:6, 1:3])
        }
  
        if ((row - 1) %/% 3 == 2 & (column - 1) %/% 3 == 0)  {
    
          box.poss <- Find_possible_values(Puzzle[7:9, 1:3])    
        }
  
        if ((row - 1) %/% 3 == 0 & (column - 1) %/% 3 == 1)  {
    
          box.poss <- Find_possible_values(Puzzle[1:3, 4:6])          
        }
  
        if ((row - 1) %/% 3 == 1 & (column - 1) %/% 3 == 1)  {
    
          box.poss <- Find_possible_values(Puzzle[4:6, 4:6])    
        }
  
        if ((row - 1) %/% 3 == 2 & (column - 1) %/% 3 == 1)  {
    
          box.poss <- Find_possible_values(Puzzle[7:9, 4:6])    
        }
  
        if ((row - 1) %/% 3 == 0 & (column - 1) %/% 3 == 2)  {
    
          box.poss <- Find_possible_values(Puzzle[1:3, 7:9])    
        }
  
        if ((row - 1) %/% 3 == 1 & (column - 1) %/% 3 == 2)  {
    
          box.poss <- Find_possible_values(Puzzle[4:6, 7:9])    
        }
    
        if ((row - 1) %/% 3 == 2 & (column - 1) %/% 3 == 2)  {
    
          box.poss <- Find_possible_values(Puzzle[7:9, 7:9])    
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

Pold <- as.matrix(read.csv("./Data/Sudoku.csv", header = F))                 # reimport the original puzzle
