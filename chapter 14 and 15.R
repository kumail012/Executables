# Prerequisites (Packages & Libraries)

library(tidyverse)

library(ggrepel)

library(nycflights13)

library(ggthemes)

library(ggplot2)

library(rlang)

library(forcats)

library(gt)

library(naniar)

library(gtExtras)

library(babynames)

library(janitor)



## 14.2.4 Exercises

#1. Create strings that contain the following values:



#1. He said "That's amazing!"

x = "He said \"That's amazing!\""

str_view(x)



#2. \a\b\c\d

x = "\\a\\b\\c\\d"

str_view(x)



#3. \\\\\\

x = "\\\\\\\\\\\\"

str_view(x)



#2. Create the string in your R session and print it. What happens to the special “\u00a0”? How does str_view() display it? Can you do a little googling to figure out what this special character is?



x <- "This\u00a0is\u00a0tricky"

print(x)

str_view(x)



# The `"\u00a0"` represents a white space. By google, I find out that this represents No-Break Space (NBSP). But, `str_view()` displays it in form of a greenish-blue font `{\u00a0}.`



## 14.3.4 Exercises

#1. Compare and contrast the results of paste0() with str_c() for the following inputs:



# str_c("hi ", NA)

# str_c(letters[1:2], letters[1:3])



# As we can see below, `paste0` converts `NA` into a string `"NA"` and simply joins it with another string. However, `str_c()` behaves more sensibly - it generates `NA` if any of the strings being joined is `NA`.



str_c("hi ", NA)

paste0("hi ", NA)





# Further, we see below that we are joining two string vectors of unequal length, i.e., `letters[1:2]` is `"a" "b"` and `letters[1:3]` is `"a" "b" "c"`, both `str_c()` and `paste0()` behave differently.



# - `str_c()` throws an error and informs us that the string vectors being joined are of unequal length.



# - `paste0` simple recycles the shorter string vector silently.



# str_c(letters[1:2], letters[1:3])

paste0(letters[1:2], letters[1:3])



#2. What’s the difference between paste() and paste0()? How can you recreate the equivalent of paste() with str_c()?



# In R, both `paste()` and `paste0()` functions are used to concatenate strings together. However, they differ in how they handle separating the concatenated elements. `paste()` concatenates its arguments with a space character as the default separator. We can specify a different separator using the `sep` argument. `paste0()` is similar to `paste()`, but it does not add any separator between the concatenated elements. It simply combines them as-is. We can recreate the equivalent of `paste()` using the `str_c()` function from the `stringr` package in `R`.



#3. Convert the following expressions from str_c() to str_glue() or vice versa:

food <- "burger"

price <- "$9"

#a. str_c("The price of ", food, " is ", price)

str_glue("The price of {food} is {price}")



age <- 18

country <- "UK"

#b. str_glue("I'm {age} years old and live in {country}")

str_c("I'm ", age, " years old and live in ", country)



title <- "Conclusion"

#c. str_c("\\section{", title, "}")

str_glue("\\\\section{{{title}}}")



## 14.5.3 Exercises



#1. When computing the distribution of the length of babynames, why did we use wt = n?

# The `babynames` data-set displays the column `n` to reflect the frequency, i.e., number of observations of that name in that year. Thus, when we are computing the distribution of the length of baby names, we need to weigh the observations by `n` otherwise each row will be treated as 1, instead of the actual number reflected in `n` leading to erroneous results.



#2. Use str_length() and str_sub() to extract the middle letter from each baby name. What will you do if the string has an even number of characters?

# The code displayed below extracts the middle letter from each baby name, and the results for first 10 names are displayed in. If the string has an even number of characters, we can pick the middle two characters.



df3 = babynames |>
  
  mutate(
    
    name_length = str_length(name),
    
    middle_letter_start = if_else(name_length %% 2 == 0,
                                  
                                  name_length/2,
                                  
                                  (name_length/2) + 0.5),
    
    middle_letter_end = if_else(name_length %% 2 == 0,
                                
                                (name_length/2) + 1,
                                
                                (name_length/2) + 0.5),
    
    middle_letter = str_sub(name, 
                            
                            start = middle_letter_start, 
                            
                            end = middle_letter_end)
    
  ) |>
  
  select(-c(year, sex, n, prop)) |>
  
  slice_head(n = 10)



df3 |>
  
  gt() |>
  
  cols_label_with(fn = ~ janitor::make_clean_names(., case = "title")) |>
  
  cols_align(align = "center",
             
             columns = -name) |>
  
  gt_theme_538()



#3. Are there any major trends in the length of babynames over time? What about the popularity of first and last letters?



df4 = babynames |>
  
  mutate(
    
    name_length = str_length(name),
    
    name_start = str_sub(name, 1, 1),
    
    name_end = str_sub(name, -1, -1)
    
  )

y_coord = c(5.4, 6.3)

df4 |>
  
  group_by(year) |>
  
  count(name_length, wt = n) |>
  
  summarise(mean_length = weighted.mean(name_length, w = n)) |>
  
  ggplot(aes(x = year, y = mean_length)) +
  
  theme_classic() +
  
  labs(y = "Average name length (for each year)",
       
       x = "Year", 
       
       title = "Baby names have become longer over the past 12 decades",
       
       subtitle = "Between 1890-1920, and 1960-1990 baby names became longer\nBut, since 1990 the names are becoming shorter again") +
  
  scale_x_continuous(breaks = seq(1880, 2000, 20)) +
  
  geom_rect(mapping = aes(xmin = 1890, xmax = 1920,
                          
                          ymin = y_coord[1], ymax = y_coord[2]),
            
            alpha = 0.01, fill = "grey") +
  
  geom_rect(mapping = aes(xmin = 1960, xmax = 1990,
                          
                          ymin = y_coord[1], ymax = y_coord[2]),
            
            alpha = 0.01, fill = "grey") +
  
  geom_line(lwd = 1) +
  
  coord_cartesian(ylim = y_coord) +
  
  theme(plot.title.position = "plot")





ns_vec = df4 |>
  
  count(name_start, wt = n, sort = TRUE) |>
  
  slice_head(n = 5) |>
  
  select(name_start) |>
  
  as_vector() |>
  
  unname()



df4 |>
  
  filter(name_start %in% ns_vec) |>
  
  group_by(year) |>
  
  count(name_start, wt = n) |>
  
  mutate(prop = 100*n/sum(n)) |>
  
  mutate(lbl = if_else(year == 2017, 
                       
                       name_start, 
                       
                       NA)) |>
  
  ggplot(aes(x = year, y = prop, 
             
             col = name_start, label = lbl)) +
  
  geom_line(lwd = 1) +
  
  ggrepel::geom_label_repel(nudge_x = 1) +
  
  labs(x = "Year",
       
       y = "Percentage of names starting with character",
       
       title = "People's preferences for baby names' starting letter change over time",
       
       subtitle = "Names starting with A are most popular now\nNames starting with J were popular in the 1940s\nIn 1950s, names starting with D became popular, while those starting with A lost popularity") +
  
  theme_classic() +
  
  theme(legend.position = "none",
        
        plot.title.position = "plot") +
  
  scale_x_continuous(breaks = seq(1880, 2020, 20))





ns_vec = df4 |>
  
  count(name_end, wt = n, sort = TRUE) |>
  
  slice_head(n = 5) |>
  
  select(name_end) |>
  
  as_vector() |>
  
  unname()



df4 |>
  
  filter(name_end %in% ns_vec) |>
  
  group_by(year) |>
  
  count(name_end, wt = n) |>
  
  mutate(prop = 100*n/sum(n)) |>
  
  mutate(lbl = if_else(year == 2017, 
                       
                       name_end, 
                       
                       NA)) |>
  
  ggplot(aes(x = year, y = prop, 
             
             col = name_end, label = lbl)) +
  
  geom_line(lwd = 1) +
  
  ggrepel::geom_label_repel(nudge_x = 1) +
  
  labs(x = "Year",
       
       y = "Percentage of names ending with character",
       
       title = "People's preferences for baby names' ending letter change over time",
       
       subtitle = "Names ending in N have risen in popularity over the decades.\nNames ending with E have become less popular over time") +
  
  theme_classic() +
  
  theme(legend.position = "none",
        
        plot.title.position = "plot") +
  
  scale_x_continuous(breaks = seq(1880, 2020, 20))





## 15.3.5 Exercises

#1. What baby name has the most vowels? What name has the highest proportion of vowels? (Hint: what is the denominator?)

# - The baby names with most vowels, i.e., 8 of them are ***Mariadelrosario*** and ***Mariaguadalupe***.

# - The baby names with highest proportion of vowels, i.e. 1 (they are entirely composed of vowels) are Ai, Aia, Aoi, Ea, Eua, Ia, Ii and Io.



b1 = babynames |>
  
  mutate(
    
    nos_vowels = str_count(name, pattern = "[AEIOUaeiou]"),
    
    name_length = str_length(name),
    
    prop_vowels = nos_vowels / name_length
    
  )



b1 |> 
  
  group_by(name) |>
  
  summarise(nos_vowels = mean(nos_vowels)) |>
  
  arrange(desc(nos_vowels)) |>
  
  slice_head(n = 5)



b1 |> 
  
  group_by(name) |>
  
  summarise(prop_vowels = mean(prop_vowels)) |>
  
  filter(prop_vowels == 1) |>
  
  select(name) |>
  
  as_vector() |>
  
  str_flatten(collapse = ", ", last = " and ")



#2. Replace all forward slashes in "a/b/c/d/e" with backslashes. What happens if you attempt to undo the transformation by replacing all backslashes with forward slashes? (We’ll discuss the problem very soon.)

# When we try to do the same in reverse, there is an error because "`\`" is an escape character. Thus, we need to add four `\` to include one in the final output.



#3. Implement a simple version of str_to_lower() using str_replace_all().

test_string3 = "Take The Match And Strike It Against Your Shoe."



str_replace_all(test_string3,
                
                pattern = "[A-Z|a-z]",
                
                replacement = tolower)



#4. Create a regular expression that will match telephone numbers as commonly written in your country.



telephone_numbers = c(
  
  "555-123-4567",
  
  "(555) 555-7890",
  
  "888-555-4321",
  
  "(123) 456-7890",
  
  "555-987-6543",
  
  "(555) 123-7890"
  
)



telephone_numbers |>
  
  str_replace(" ", "-") |>
  
  str_replace("\\(", "") |>
  
  str_replace("\\)", "") |>
  
  as_tibble() |>
  
  separate_wider_regex(
    
    cols = value,
    
    patterns = c(
      
      area_code = "[0-9]+",
      
      "-| ",
      
      exchange_code = "[0-9]+",
      
      "-| ",
      
      line_number = "[0-9]+"
      
    )
    
  ) |>
  
  gt() |>
  
  gtExtras::gt_theme_538() |>
  
  cols_label_with(fn = ~ janitor::make_clean_names(., case = "title"))



# You can use the following regular expression to match these commonly written telephone number formats:

# `^(\(\d{3}\)\s*|\d{3}[-.]?)\d{3}[-.]?\d{4}$`



## 15.4.7 Exercises

#1. How would you match the literal string "'\? How about "$^$"?



# The string you want to match

input_string <- "\"'\\"

str_view(input_string)



# Pattern to match the literal string

match_pattern <- "\"\'\\\\"

str_view(match_pattern)



# Use str_detect to check if the string contains the pattern

if (str_detect(input_string, match_pattern)) {
  
  print("Pattern found in the input string.")
  
} else {
  
  print("Pattern not found in the input string.")
  
}



#2. Explain why each of these patterns don’t match a \: "\", "\\", "\\\".

# Each of the patterns you provided does not match a single backslash `\` for the following reasons: ---



# 1.`"\"` - This pattern does not match a single backslash because the backslash is an escape character in regular expressions. In most regular expression engines, a single backslash is used to escape special characters. So, when you use `"\"` alone, it is interpreted as an escape character, and it doesn't match a literal backslash in the input string.



# 2.  `"\\"` - This pattern also does not match a single backslash. It may seem like it should work because you're escaping the backslash with another backslash, but in many regular expression engines, `"\\"` represents a literal backslash when you're defining the regular expression. However, when applied to the input string, it's still interpreted as a single backslash.



# 3.  `"\\\"` - This pattern does not match a single backslash for the same reason as the previous ones. The combination `"\\\"` is treated as a literal backslash in the regular expression definition, but when applied to the input string, it's still interpreted as a single backslash, and the extra `"\"` followed by a quotation mark is not part of the pattern.



# To match a single backslash `"\"`, you would typically need to use four backslashes in the regular expression pattern, `"\\\\"` . This way, the first two backslashes represent a literal backslash, and the next two backslashes escape each other, resulting in a pattern that matches a single backslash in the input string.



#3. Given the corpus of common words in stringr::words, create regular expressions that find all words that:



#a. Start with “y”.

words |>
  
  str_view(pattern = "^y")



#b. Don’t start with “y”.

# To view words not sarting with a y

words |>
  
  str_view(pattern = "^(?!y)")

# Check the number of such words

words |>
  
  str_view(pattern = "^(?!y)") |>
  
  length()

# Check the number of words starting with y and total number of words

# to confirm the matter

words |> length()



#c. End with “x”.

words |>
  
  str_view(pattern = "x$")



#d. Are exactly three letters long. (Don’t cheat by using str_length()!)

# Finding letters exactly three letters long using regex

words |>
  
  str_subset(pattern = "\\b\\w{3}\\b")



# Finding letters exactly three letters long using str_length()

three_let_words = str_length(words) == 3

words[three_let_words]



# Checking results

words[three_let_words] |>
  
  length()

words |>
  
  str_view(pattern = "\\b\\w{3}\\b") |>
  
  length()



#e. Have seven letters or more.

words |>
  
  str_subset(pattern = "\\b\\w{7,}\\b")



#f. Contain a vowel-consonant pair.

words |>
  
  str_view(pattern = "[aeiou][^aeiou]")



#g. Contain at least two vowel-consonant pairs in a row.

words |>
  
  str_view(pattern = "[aeiou][^aeiou][aeiou][^aeiou]")



#h. Only consist of repeated vowel-consonant pairs.

words |>
  
  str_view(pattern = "^(?:[aeiou][^aeiou]){2,}$")  



#4. Create 11 regular expressions that match the British or American spellings for each of the following words: airplane/aeroplane, aluminum/aluminium, analog/analogue, ass/arse, center/centre, defense/defence, donut/doughnut, gray/grey, modeling/modelling, skeptic/sceptic, summarize/summarise. Try and make the shortest possible regex!

# Sample passage with mixed spellings



sample_text <- "The airplane is made of aluminum. The analog signal is stronger. Don't be an ass. The center is closed for defense training. I prefer a donut, while she likes a doughnut. His hair is gray, but hers is grey. We're modeling a new project. The skeptic will not believe it. Please summarize the report."



# Define the regular expressions

patterns_to_detect <- c(
  
  "air(?:plane|oplane)",
  
  "alumin(?:um|ium)",
  
  "analog(?:ue)?",
  
  "ass|arse",
  
  "cent(?:er|re)",
  
  "defen(?:se|ce)",
  
  "dou(?:gh)?nut",
  
  "gr(?:a|e)y",
  
  "model(?:ing|ling)",
  
  "skep(?:tic|tic)",
  
  "summar(?:ize|ise)"
  
)



# Find and highlight the spellings

for (pattern in patterns_to_detect) {
  
  matches <- str_extract_all(sample_text, pattern)
  
  if (length(matches[[1]]) > 0) {
    
    sample_text <- str_replace_all(sample_text, 
                                   
                                   pattern, 
                                   
                                   paste0("**", matches[[1]], "**"))
    
  }
  
}



sample_text



#5. Switch the first and last letters in words. Which of those strings are still words?

# Code to switch the first and last letters in words

new_words = words |>
  
  str_replace_all(pattern = "\\b(\\w)(\\w*)(\\w)\\b", 
                  
                  replacement = "\\3\\2\\1")



# Finding which of the new strings are part of the original "words"

tibble(
  
  original_words = words[new_words %in% words],
  
  new_words = new_words[new_words %in% words]
  
) |>
  
  gt() |>
  
  cols_label_with(columns = everything(),
                  
                  fn = ~ make_clean_names(., case = "title")) |>
  
  opt_interactive()



#6. Describe in words what these regular expressions match: (read carefully to see if each entry is a regular expression or a string that defines a regular expression.)



#a. ^.*$

# This regular expression matches an entire string. It starts with **`^`** (caret), which anchors the match to the beginning of the string, followed by **`.*`** which matches any number of characters (including none), and ends with **`$`** (dollar sign), which anchors the match to the end of the string. So, it essentially matches any string, including an empty one.



#b. "\\{.+\\}"

# This is a string defining a regular expression matches strings that contain curly braces **`{}`** with content inside them. The double backslashes **`\\`** are used to escape the curly braces, and **`.+`** matches one or more of any characters within the braces. So, it would match strings like "{abc}" or "{123}".



#c. \d{4}-\d{2}-\d{2}

# This regular expression matches a date-like pattern in the format "YYYY-MM-DD." Here, **`\d`** matches a digit, and **`{4}`**, **`{2}`**, and **`{2}`** specify the exact number of digits for the year, month, and day, respectively. So, it matches strings like "2023-09-14."



#d. "\\\\{4}"

# This is a string that defines a regular expression which matches strings that contains exactly four backslashes. Each backslash is escaped with another backslash, so **`\\`** matches a single backslash, and **`{4}`** specifies that exactly four backslashes must appear consecutively in the string. It matches strings like "\\\\\\\\abcd" but not "\\\\efg" (which contains only two backslashes).



#e. \..\..\..

# This regular expression matches strings that have three dots separated by any character. The dot **`.`** is a special character in regular expressions, so it's escaped with a backslash **`\.`** to match a literal dot **`.`** . Thereafter, the `.` matches any character, and this pattern is repeated three times. So, it matches strings like ".a.b.c" or ".1.2.3"



#f. (.)\1\1

# This regular expression matches strings that contain three consecutive identical characters. The parentheses **`(.)`** capture any single character, and **`\1`** refers to the first captured character. So, it matches strings like "aaa" or "111."



#g. "(..)\\1"

# This is a string that represents a regular expression which matches strings consisting of two identical characters repeated twice. The **`(..)`** captures any two characters, and **`\\1`** refers to the first captured two characters. So, it matches strings like `aa` or `11` within double quotes.



#7. Solve the beginner regexp crosswords at https://regexcrossword.com/challenges/beginner.

# Done



## 15.6.4 Exercises

#1. For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.



#a. Find all words that start or end with x.

# Using a singular regular expression

str_view(words, "(^xX)|(x$)")



# Using a combination of multiple str_detect() calls

words[str_detect(words, "^xX") | str_detect(words, "x$")]



#b. Find all words that start with a vowel and end with a consonant.

# Using a singular regular expression

pattern_b = "^(?i)[aeiou].*[^aeiou]$"

str_subset(words, pattern_b)



# Using a combination of multiple str_detect() calls

words[
  
  str_detect(words, "^(?i)[aeiou]") &
    
    str_detect(words, "[^aeiou]$")  
  
]



#c. Are there any words that contain at least one of each different vowel?

# No, there are no such words in `words`.

pattern_c = "^(?=.*a)(?=.*e)(?=.*i)(?=.*o)(?=.*u).+"

str_subset(words, pattern_c)



#2. Construct patterns to find evidence for and against the rule “i before e except after c”?

# Creating the regexp's first to use in stringr functions

pattern_1a = "\\b\\w*ie\\w*\\b"

pattern_1b = "\\b\\w+ei\\w*\\b"



pattern_2a = "\\b\\w*cei\\w*\\b"

pattern_2b = "\\b\\w*cie\\w*\\b"



# Words which contain "i" before "e"

words[str_detect(words, pattern_1a)]



# Words which contain "e" before an "i", thus giving evidence against

# the rule, unless there is a preceeding "c"

words[str_detect(words, pattern_1b)]



# Words which contain "e" before an "i" after "c", thus following the rule.

# That is, evidence in favour of the rule

words[str_detect(words, pattern_2a)]



# Words which contain an "i" before "e" after "c", thus violating the rule.

# That is, evidence against the rule

words[str_detect(words, pattern_2b)]



#3. colors() contains a number of modifiers like “lightgray” and “darkblue”. How could you automatically identify these modifiers? (Think about how you might detect and then remove the colors that are modified).

# The R code `col_vec = colours(distinct = TRUE)` creates a vector `col_vec` containing a set of distinct color names available in R's default color palette.



# The code `col_vec = col_vec[!str_detect(col_vec, "\\b\\w*\\d\\w*\\b")]` filters the vector `col_vec` to exclude color names that contain any digits within them.



# Finally, the code `col_vec[str_detect(col_vec, "\\b(?:light|dark)\\w*\\b")]` will return a subset of the `col_vec` vector containing color names that have modifiers like "light" or "dark" in them, effectively identifying color names with modifiers.



col_vec = colours(distinct = TRUE)

col_vec = col_vec[!str_detect(col_vec, "\\b\\w*\\d\\w*\\b")]

col_vec[str_detect(col_vec, "\\b(?:light|dark)\\w*\\b")]



#4. Create a regular expression that finds any base R dataset. You can get a list of these datasets via a special use of the data() function: data(package = "datasets")$results[, "Item"]. Note that a number of old datasets are individual vectors; these contain the name of the grouping “data frame” in parentheses, so you’ll need to strip those off.

# Extract all base R datasets into a character vector

base_r_packs = data(package = "datasets")$results[, "Item"]



# Remove all the names of grouping data.frames in parenthesis 

base_r_packs = str_replace_all(base_r_packs, 
                               
                               pattern = "\\([^()]+\\)", 
                               
                               replacement = "")

# Remove the whitespace, i.e., " " let after removing the parenthesis words

base_r_packs = str_replace_all(base_r_packs, 
                               
                               pattern = "\\s+$", 
                               
                               replacement = "")



# Create the regular expression

huge_regex = str_c("\\b(", str_flatten(base_r_packs, "|"), ")\\b")