# Supporting script for Q1(a) and Q1(b) (not called by run_assignment.R).
# Produces the inspection tables, range-check plots, course-size chart and the
# second-method check of the conflict matrix used in the report.
# Run from the project root:  Rscript assignment_files/q1_explore.R

# 1. Setup ====
if (!exists("q1_find_project_root", mode = "function")) {
  source(file.path("assignment_files", "q1_clean.R"), local = TRUE)
}
root <- q1_find_project_root()
source(file.path(root, "assignment_files", "q1_conflict.R"), local = TRUE)

fig_dir <- file.path(root, "outputs", "q1", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Draw a plot on screen and also save it as a PNG for the report
show_and_save <- function(filename, draw) {
  draw()
  png(file.path(fig_dir, filename), width = 900, height = 650)
  draw()
  invisible(dev.off())
}


# 2. Inspect raw data ====
registrations <- read_q1_registration_lists(root)

sapply(registrations, length)          # lines per course
lapply(registrations, head, 10)        # first 10 lines of each course

# One table: course, raw text, prefix (letters), digits
split_id <- function(x, course) {
  x <- trimws(x)
  data.frame(
    course    = course,
    raw       = x,
    prefix    = sub("\\d.*$", "", x),            # everything before the first digit
    digits    = gsub("\\D", "", x),              # digits only
    digit_len = nchar(gsub("\\D", "", x)),
    stringsAsFactors = FALSE
  )
}

parts <- do.call(rbind, Map(split_id, registrations, names(registrations)))
rownames(parts) <- NULL

head(parts, 15)
table(parts$prefix)                    # prefixes found (NAME, NAME-, NAME_, NEMA, ...)
table(parts$prefix, parts$course)      # prefix by course
table(parts$digit_len)                 # 2 digits = missing leading zero


# 3. Compare raw text with the pipeline's cleaned IDs ====
# Uses normalize_q1_student_ids() from q1_clean.R, so this matches the pipeline.
parts$id_num  <- as.integer(parts$digits)
parts$clean   <- normalize_q1_student_ids(parts$raw)$clean
parts$changed <- parts$raw != parts$clean   # TRUE = text rewritten, FALSE = already NAME###

table(parts$changed)                                           # rewritten vs unchanged
head(parts[parts$changed, c("course", "raw", "clean")], 20)    # changed rows
parts[grepl("^NEMA", parts$raw), c("course", "raw", "clean")]  # typo rule
parts[parts$digit_len < 3, c("course", "raw", "clean")]        # padding rule


# 4. Checks after standardising ====
# Duplicates inside the same course (both copies shown)
dup_rows <- duplicated(parts[, c("course", "clean")])
parts[dup_rows | duplicated(parts[, c("course", "clean")], fromLast = TRUE),
      c("course", "raw", "clean")]

# ID range (look for out-of-range numbers)
range(parts$id_num)
tapply(parts$id_num, parts$course, range)

show_and_save("q1_id_range_boxplot.png", function() {
  boxplot(id_num ~ course, data = parts,
          main = "Student ID number by course (range check)",
          xlab = "Course", ylab = "ID number")
})

show_and_save("q1_id_range_stripchart.png", function() {
  stripchart(id_num ~ course, data = parts, method = "jitter",
             vertical = TRUE, pch = 16, col = rgb(0, 0, 0, 0.4),
             main = "Every student ID by course", ylab = "ID number")
})

# Coverage: numbers not registered in any course (a gap is not an error)
ids_all <- sort(unique(parts$id_num))
length(ids_all)                                 # distinct students across all courses
setdiff(seq_len(max(ids_all)), ids_all)         # numbers that appear in no course


# 5. Cleaned data and course sizes ====
cleaning <- clean_q1_registrations(registrations)
courses  <- cleaning$courses
audit    <- cleaning$audit
audit

# Counts must add up
stopifnot(sum(audit$raw_rows) - sum(audit$blank_rows) - sum(audit$invalid_id_rows) -
            sum(audit$duplicate_rows_removed) == sum(audit$cleaned_students))
stopifnot(sum(audit$standardized_rows) == sum(parts$changed))

sizes <- lengths(courses)

# Bar chart: course size against the two venue capacities
show_and_save("q1_course_sizes.png", function() {
  bp <- barplot(sizes,
                ylim = c(0, 80),                                  # room for the 70 line
                main = "Students per course (after cleaning)",
                xlab = "Course", ylab = "Number of students",
                col  = ifelse(sizes <= 40, "blue", "orange"))     # blue = fits Venue 1
  abline(h = c(40, 70), lty = 2)                                  # venue capacities
  text(x = max(bp) + 0.6, y = c(40, 70) + 2,
       labels = c("Venue 1 capacity (40)", "Venue 2 capacity (70)"),
       adj = 1, cex = 0.8)
  text(bp, sizes + 2, labels = sizes)                             # count on each bar
  legend("topleft", fill = c("blue", "orange"),
         legend = c("Fits Venue 1 or 2 (<= 40)", "Fits Venue 2 only (41-70)"),
         cex = 0.8, bty = "n")
})


# 6. Conflict matrix: second-method check ====
# conflict_matrix comes from build_q1_conflict_matrix() in q1_conflict.R
# (pairwise intersect()). Here it is rebuilt a second way to confirm it.
conflict_matrix <- build_q1_conflict_matrix(courses)
conflict_matrix

# M = student x course table of 0/1 (1 = student is registered in that course)
students <- sort(unique(unlist(courses)))
M <- sapply(courses, function(ids) as.integer(students %in% ids))
rownames(M) <- students

head(M, 10)
stopifnot(all(conflict_matrix == t(M) %*% M))      # both methods agree

upper <- upper.tri(conflict_matrix)                # TRUE above the diagonal (each pair once)
stopifnot(all(conflict_matrix[upper] <= outer(sizes, sizes, pmin)[upper]))  # overlap <= smaller course

nrow(M)                    # distinct students across all courses
sum(sizes)                 # total registrations
table(rowSums(M))          # number of students taking 1, 2, 3, ... courses

# Pairs with no shared students (can share a time period)
pair_summary <- read.csv(file.path(root, "outputs", "q1", "conflict_pairs.csv"))
nrow(pair_summary)                                   # 15 pairs
pair_summary[pair_summary$shared_students == 0, ]