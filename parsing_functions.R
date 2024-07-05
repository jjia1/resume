# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to eachother in number. 
strip_links_from_cols <- function(data, cols_to_strip){
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(position_data, section_id){
  position_data %>% 
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>% 
    filter(!is.na(description) | description_num == 'description_1') %>%
    group_by(id) %>% 
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(first(description))
    ) %>% 
    ungroup() %>% 
    filter(description_num == 'description_1') %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        as.character(end),
        glue('{end} - {start}')
      ),
      timeline_center = ifelse(
        is.na(start) | start == end,
        end,
        (end + start) / 2
      ),
        description_bullets = ifelse(
        no_descriptions | !include_description,
        '',
        map_chr(descriptions, ~paste('-', ., collapse = '\n'))
      )
    ) %>% 
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    mutate_all(~ifelse(is.na(.), '', .)) %>% 
    glue_data(
      "### {title}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n"
    )
}

print_education <- function(position_data, section_id){
  position_data %>% 
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      ),
      # Check if include_description exists, if not, set it to TRUE
      include_description = if("include_description" %in% names(.)) include_description else TRUE,
      # Prepare description text
      description_text = ifelse(include_description & !is.na(description_1), 
                                paste0("\n\n", description_1),
                                "")
    ) %>% 
    strip_links_from_cols(c('title')) %>% 
    mutate_all(~ifelse(is.na(.), '', .)) %>% 
    glue_data(
      "### {title}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}", 
      "{description_text}",
      "\n\n"
    )
}


# Construct a bar chart of skills
build_skill_bars <- function(skills, selection, out_of = 5){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  skills %>%
    filter(in_resume) %>%
    filter(type == selection) %>%
    mutate(width_percent = round(100*level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}

#build list of skills 
build_skill_list <- function(skills, category) {
  skills %>%
    filter(category == !!category) %>%
    skills <- skills %>% filter(in_resume) %>%
    arrange(desc(level)) %>%
    mutate(skill_level = case_when(
      level >= 4 ~ "★★★",
      level == 3 ~ "★★☆",
      level <= 2 ~ "★☆☆"
    )) %>%
    glue_data(
      "<div class='skill-item'>",
      "<span class='skill-name'>{skill}</span>",
      "<span class='skill-level'>{skill_level}</span>",
      "</div>"
    ) %>%
    paste(collapse = "\n")
}

# Prints out from text_blocks spreadsheet blocks of text for the intro and asides. 
print_text_block <- function(text_blocks, label){
  filter(text_blocks, loc == label)$text %>%
    sanitize_links() %>%
    cat()
}

#print awards
print_awards <- function(awards){
  awards %>%
    arrange(desc(end)) %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      )
    ) %>% 
    strip_links_from_cols(c('title')) %>% 
    mutate_all(~ifelse(is.na(.), '', .)) %>% 
    glue_data(
      "### {title}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}", 
      "\n\n",
    )
}

print_certificates <- function(certificates){
  certificates %>%
    arrange(desc(date)) %>%
    glue_data(
      "**{title}** *{institution}* ({date})",
      "\n\n"
    ) %>%
    paste(collapse = "") %>%  # Change this from "\n\n" to ""
    cat()  # Add cat() to print without [1]
}

print_publications <- function(publications) {
  publications %>%
    arrange(desc(Year)) %>%
    mutate(
      Authors = str_replace_all(Authors, "J\\.Jia", "**J.Jia**"),
      # Truncate the title if it's too long
      Title = str_trunc(Title, 70, "right"),
      # Format authors to include only the first few
      Authors = map_chr(str_split(Authors, ","), ~ paste(head(.x, 3), collapse = ","))
    ) %>%
    glue_data(
      "**{Title}** *{Journal}* ({Year})",
      "\n",
      "{Authors}",
      "\n"
    ) %>%
    paste(collapse = "\n\n") %>%
    cat()
}
# print text block
print_text_block <- function(text_blocks, label) {
  filter(text_blocks, loc == label)$text %>%
    sanitize_links() %>%
    paste() %>%
    cat()
}
