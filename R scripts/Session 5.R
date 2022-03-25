
#--# Title: Sudoku solver, 25/03/2022

#### Set up ####

library(tidyverse) 
library(furrr)
library(tictoc)
library(microbenchmark)
library(profvis)                                                                    

plan("multisession")

Puzzle <- as.matrix(read.csv("./Data/Sudoku.csv", header = F))                      # Import a Sudoku puzzle

Many_puzzles <- list(Puzzle)                                      
Many_puzzles <- Many_puzzles[rep(1, 10)]                                            # Create an example list of multiple puzzles

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

Sudoku_solver <- function(Puzzle) {
  
  while (anyNA(Puzzle)) {                                                             # Run while there a gaps to be filled                                                                                   
    
    for (row in 1:9) {                                                                # Work through the rows
      
      for (column in 1:9) {                                                           # and the columns
        
        if (is.na(Puzzle[row, column])) {                                             # Select a cell, Is the cell filled?
          
          row.poss <- Find_possible_values(Puzzle[row,])                                # Find possible values for the cell in this row.
          
          col.poss <- Find_possible_values(Puzzle[,column])                             # Find possible values for the cell in this column.
          
          box.poss <- Find_possible_values(Puzzle[Find_box(row), Find_box(column)])     # Find possible values for the cell in this box
          
          all.poss <- box.poss[box.poss %in% row.poss[row.poss %in% col.poss]]          # Find the possible values using all conditions
          
          if (length(all.poss) == 1 & !is.na(all.poss) ){                               # If there is only 1 possible value
            
            Puzzle[row, column] <- all.poss                                            # Insert the number in the cell
            print(c(row, column))                                                      # print position
          }
        }
      }
    }  
  }
  return(Puzzle)}

Sudoku_solver_np <- function(Puzzle) {
  
  while (anyNA(Puzzle)) {                                                             # Run while there a gaps to be filled                                                                                   
    
    for (row in 1:9) {                                                                # Work through the rows
      
      for (column in 1:9) {                                                           # and the columns
        
        if (is.na(Puzzle[row, column])) {                                             # Select a cell, Is the cell filled?
          
          row.poss <- Find_possible_values(Puzzle[row,])                                # Find possible values for the cell in this row.
          
          col.poss <- Find_possible_values(Puzzle[,column])                             # Find possible values for the cell in this column.
          
          box.poss <- Find_possible_values(Puzzle[Find_box(row), Find_box(column)])     # Find possible values for the cell in this box
          
          all.poss <- box.poss[box.poss %in% row.poss[row.poss %in% col.poss]]          # Find the possible values using all conditions
          
          if (length(all.poss) == 1 & !is.na(all.poss) ){                               # If there is only 1 possible value
            
            Puzzle[row, column] <- all.poss                                            # Insert the number in the cell
#            print(c(row, column))                                                      # print position
          }
        }
      }
    }  
  }
  return(Puzzle)}                                          # Turn off printing in the function

#### Benchmarking ####

tic()
Solved_puzzle <- Sudoku_solver(Puzzle)                                              # Simple way to time code
toc()

microbenchmark(Sudoku_solver(Puzzle))                                               # Timing repeated evaluations for performance statistics

Bench_printing <- microbenchmark(printing = Sudoku_solver(Puzzle),                  # What happens if we turn off the printing?
                                 no_printing = Sudoku_solver_np(Puzzle))

autoplot(Bench_printing)                                                            # Visually inspect performance differences

#### Profiling iterators  ####

Multi_Bench <- microbenchmark(lapply = lapply(Many_puzzles, Sudoku_solver),         # Comparing different versions of the same code
                              map = map(Many_puzzles, Sudoku_solver),
                              parallel = future_map(Many_puzzles, Sudoku_solver),
                              times = 10)                                           # Control the number of repeats

autoplot(Multi_Bench)                                                               # Why is code in parallel slower?

profvis({                                                                           # Identify the bottlenecks with code profiling
  future_map(Many_puzzles, Sudoku_solver)                                           # Lots of the runtime is committed to copying the data
}) 



