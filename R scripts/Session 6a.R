
#--# Title: Sudoku solver, 01/04/2022

#### Set up ####

#source("./R scripts/Session 6b.R") 
## OR
library(sudokusolver)

Puzzle <- as.matrix(read.csv("./Data/Sudoku.csv", header = F))                  # Import a Sudoku puzzle

Many_puzzles <- list(Puzzle)                                      
Many_puzzles <- Many_puzzles[rep(1, 10)]                                        # Create an example list of multiple puzzles

#### Solve many sudoku puzzles ####

Solved <- lapply(Many_puzzles, Sudoku_solver_np)
