
#--# Title: Functions to solver a sudoku puzzle

#' Check for unused numbers between 1 and 9
#'
#' Takes a vector and compares against the numbers 1 to 9, to find which are
#' missing.
#'
#'
#' @param Filled_already A vector of length 9 from a sudoku puzzle
#'
#' @return Vector of numbers which have not been used
#'
#' @examples
#'
#' Find_possible_values(c(1,2,3,4,NA,NA,7,8,9)) # Test a single row or column
#'
#' @export
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

#' Solve a sudoku puzzle
#'
#' Takes a 9 X 9 matrix with missing values and replaces this with the solution.
#'
#'
#' @param Puzzle A 9 x 9 numeric matrix representing a sudoku puzzle, with NA values for gaps.
#'
#' @return a 9 x 9 matrix with all cells filled.
#'
#' @export
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
