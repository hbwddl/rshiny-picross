# Picross in RShiny

This is a [Picross](https://en.wikipedia.org/wiki/Nonogram) game, created using RShiny.

Picross, also known as Nonograms, is a logic puzzle game that involves filling in cells in a grid to reveal a hidden image. In this case, the image is randomly generated. The objective of the game is to use the clues provided around the edges of the grid to determine which cells in the grid need to be filled in and which cells should be left blank.

The clues for each row and column of the grid are given outside of the grid. These clues indicate how many cells in that row or column should be filled in and how they should be arranged. For example, a clue of "6 2" for a row means that there are six consecutive cells that need to be filled in, followed by at least one blank cell, and then two more consecutive cells that need to be filled in. It is up to the player to determine the correct configuration based on the already-filled cells and clues in other rows/columns.

As the player fills in more and more cells correctly, more rows become easier to complete. Once all the cells have been filled in correctly, the puzzle is solved.

Solution grids are generated randomly, and users are able to choose the number of rows and columns on the game board to increase or decrease the challenge level. Users single click to color in a cell and double click to block off a cell (this is not necessarily part of the game's solution, but is useful for keeping track of your progress). A single click on a filled or blocked off cell will empty the cell.

Picross is my favorite puzzle game, and I am proud that this app runs from one app.R file with less than 250 lines of code. I have also hosted a demo on [shinyapps.io](https://hbwaddel.shinyapps.io/Picross/)
