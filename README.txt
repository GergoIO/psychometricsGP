Notes on adding a new function


- May need to run the following in Console before starting edits (check this):

library(devtools)
library(roxygen2)
install.packages("devtools")
install.packages("roxygen2")

Then,
- Create a new R Script (Ctrl Shift N)
- Add the function:
      fnName <- function(){

      }
- Code > Insert Roxgen skeleton (Ctrl Alt Shift R)
- Fill in added text with descriptions, uses etc.
- Console > document()
    This creates the associated documentation files to go with the function
- Build > Install and Restart
- Git > Commit (select and add a comment/description)
- Git > Push
