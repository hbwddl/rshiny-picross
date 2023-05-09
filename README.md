# Picross in R Shiny

This is a [Picross](https://en.wikipedia.org/wiki/Nonogram) game, created using R Shiny.

Picross, also known as Nonograms, is a logic puzzle game that involves filling in cells in a grid to reveal a hidden image. In this case, the image is randomly generated. The objective of the game is to use the clues provided around the edges of the grid to determine which cells in the grid need to be filled in and which cells should be left blank.

The clues for each row and column of the grid are given outside of the grid. These clues indicate how many cells in that row or column should be filled in and where blank squares could be. For example, a clue of "6 2" for a row means that there are six consecutive cells that need to be filled in, followed by at least one blank cell, and then two more consecutive cells that must be filled in. Players can use the clues around the edge, already-filled cells, or clues in other rows/columns to determine the correct solution. As the player fills in more and more cells correctly, more rows become easier to complete. Once all cells have been filled in correctly, the puzzle is solved.

Solution grids are generated randomly, and players are able to choose the number of rows and columns on the game board to increase or decrease the challenge level. Players single-click to fill in a cell or double-click to block off a cell. A single click on any filled or blocked-off cell will clear the cell.

Picross is my favorite puzzle game, and I am proud that this app runs from one app.R file with less than 250 lines of code. I have also hosted a demo on [shinyapps.io](https://hannahwaddel.shinyapps.io/rshiny-picross/)
