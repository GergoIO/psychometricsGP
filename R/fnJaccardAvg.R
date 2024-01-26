#' Calculate the Average Jaccard Index
#'
#' This function calculates the average Jaccard index between pairs of students.
#' The Jaccard index is a measure of the similarity between two sets, defined as
#' the size of the intersection divided by the size of the union of the two sets.
#'
#' @param data A data frame with at least the columns StudentID and ItemID.
#'
#' @return The average Jaccard index between pairs of students.
#'
#' @examples
#' # Create a data frame with overlapping items
#' df <- data.frame(
#'   StudentID = rep(c("Student1", "Student2", "Student3", "Student4"), each = 10),
#'   ItemID = c(1:10, 5:14, 10:19, 15:24)
#' )
#'
#' # Calculate the average Jaccard index
#' avg_jaccard <- fnJaccardAvg(df)
#'
#' # Print the average Jaccard index
#' print(avg_jaccard)
#'
#' @export
#'
################################################################################
#'
fnJaccardAvg <- function(data) {
  # Define Jaccard Similarity function
  jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
  }

  # Get the list of all students
  students <- unique(data$StudentID)

  # Initialize an empty data frame to store the Jaccard indices
  jaccard_df <- data.frame(Student1 = character(), Student2 = character(), JaccardIndex = numeric())

  # Calculate the Jaccard index for each pair of students
  for(i in 1:(length(students)-1)) {
    for(j in (i+1):length(students)) {
      itemIDs_student1 <- data[data$StudentID == students[i],]$ItemID
      itemIDs_student2 <- data[data$StudentID == students[j],]$ItemID
      jaccard_index <- jaccard(itemIDs_student1, itemIDs_student2)

      # Add the Jaccard index to the data frame
      jaccard_df <- rbind(jaccard_df, data.frame(Student1 = students[i], Student2 = students[j], JaccardIndex = jaccard_index))
    }
  }

  # Calculate and return the average of all the Jaccard indices
  return(mean(jaccard_df$JaccardIndex))
}
