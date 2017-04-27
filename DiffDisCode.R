# The file needs to have a header row
# The first column should be the students raw score
# The other columns should be 1 if the student's answer was correct
# and 0 if the student's answer was incorrect
Raw_Scores = read.csv("mydata.csv")

# Order dataframe with highest Scores at the top
Sorted_Scores <- Raw_Scores[order(-Raw_Scores$Score),]

# Calculate the difficulty index for each question
# This dataframe also has the average score as the first entry
Difficulty_Index <- colSums(Sorted_Scores)/nrow(Sorted_Scores)

# Plot just the difficulty indexes i.e. skip the average scores
# No annotations or axes
# Set y axis to range 0 to 1 the min and max for a difficulty index
plot (Difficulty_Index[2:length(Difficulty_Index)],ann=FALSE,axes=FALSE, ylim=c(0,1))

# Add a box
box ()

# Add the X axis
axis(1, at = 1:length(Difficulty_Index))
title(xlab = "Question Number")


# Add the y axis
axis(2, at = 0.25)
abline(h = 0.25, col = "red")
axis(2, at = 0.75)
abline(h = 0.75, col = "red")
title(ylab = "Difficulty Index")

# Add Title
title(main = "Difficulty Indexes for Test Questions")

# Discrimination index is the number of students correct in the upper group
# minus the number of students correct in the lower group
# divided by the number of students in the groups
Discrimination_Index <- (apply(Sorted_Scores[1:(nrow(Sorted_Scores)/2),],2,function(x) sum(x)) - apply(Sorted_Scores[(nrow(Sorted_Scores)/2):(nrow(Sorted_Scores)),],2,function(x) sum(x)))/nrow(Sorted_Scores/2)

# Plot just the discrimination indexes i.e. skip the score column
# No annotations or axes
# Set y axis to range -1 to 1 the min and max for a discrimination index
plot (Discrimination_Index[2:length(Discrimination_Index)],ann=FALSE, axes=FALSE, ylim=c(-1,1))

# Add a box
box ()

# Add the X axis
axis(1, at = 1:length(Discrimination_Index))
title(xlab = "Question Number")

# Add the y axis
axis(2, at = 1)
axis(2, at = 0)
axis(2, at = -1)
abline(h = 0, col = "red")
title(ylab = "Discrimination_Index")

# Add Title
title(main = "Discrimination Index for Test Questions")
