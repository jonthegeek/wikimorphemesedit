
# For submitting edits via API

# get_section_number ------------------------------------------------------

# Wiki sections must be specified by number for editing. Find the number, given
# the name.
# get_section_number("Wiktionary:Sandbox")
get_section_number <- function(word, section = "Etymology") {
  url <- paste0("https://en.wiktionary.org/w/api.php?action=parse&page=",
                word, "&prop=sections&format=json")
  resp <- httr::GET(url)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)
  # get number for english sections
  en_num <- list_content$parse$sections %>%
    dplyr::filter(line == "English") %>%
    dplyr::pull(number)
  english_part <- list_content$parse$sections %>%
    dplyr::filter(stringr::str_starts(number, pattern = paste0(en_num, ".")))
  section_index <- english_part %>%
    dplyr::filter(line == section) %>%
    dplyr::pull(index)
  return(section_index)
}

## hmm, maybe make a safety check to avoid making the same edit twice?
# get_section_text <- function(word, section_number) {
#   url <- paste0("https://en.wiktionary.org/w/api.php?action=parse&page=",
#                 word, "&format=json&prop=wikitext&section=",
#                 section_number)
#   resp <- httr::GET(url)
#   html_content <- httr::content(resp, "text")
#   list_content <- jsonlite::fromJSON(html_content)
#   section_text <- list_content$parse$wikitext$`*`
#   return(section_text)
# }
# word <- "Wiktionary:Sandbox"
# sn <- get_section_number(word)
# get_section_text(word, sn)

# generate_morphology_template --------------------------------------------

generate_morphology_prefix_template <- function(prefix, base_word) {
  # Note: before now, figure out whether second piece is a suffix or not
  return(paste0("Morphologically {{prefix|en|", prefix, "|", base_word,"}}"))
}

generate_morphology_confix_template <- function(prefix, suffix) {
  # Note: before now, figure out whether first piece is a prefix or not, e.g.
  # Morphologically {{confix|en|astro|logy}}
  # or
  # Morphologically {{suffix|en|conserve|ation}}
  return(paste0("Morphologically {{confix|en|", prefix, "|", suffix,"}}"))
}

generate_morphology_suffix_template <- function(base_word, suffix) {
  # Note: before now, figure out whether first piece is a prefix or not, e.g.
  # Morphologically {{confix|en|astro|logy}}
  # or
  # Morphologically {{suffix|en|conserve|ation}}
  # This function assumes that base_word is NOT a prefix here.
  return(paste0("Morphologically {{suffix|en|", base_word, "|", suffix,"}}"))
}
generate_morphology_suffix_template("conserve", "ation")




# submit_morphology_edit --------------------------------------------------

submit_morphology_edit <- function(word = "Project:Sandbox",
                                   template_text = "",
                                   username,
                                   password) {
  sn <- get_section_number(word, section = "Etymology")
  # put edit on a new line
  template_text <- paste0("\n", template_text, "\n")

  base_url <- "https://en.wiktionary.org/w/api.php"
  url <- paste0(base_url, "?action=query&meta=tokens&type=login&format=json")

  # Step 1: GET request to fetch login token
  resp <- httr::GET(url)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)
  token <- list_content$query$tokens$logintoken

  # Step 2: POST request to log in. Use of main account for login is not
  # supported. Obtain credentials via Special:BotPasswords
  # https://www.mediawiki.org/wiki/Special:BotPasswords for lgname & lgpassword
  params2 <- list(action = "login",
                  lgname = username,
                  lgpassword = password,
                  lgtoken = token,
                  format = "json")
  # just logging in; not doing anything with response
  resp <- httr::POST(url = base_url, body = params2)

  # Step 3: GET request to fetch CSRF token and some timestamps
  url <- paste0(base_url,
                "?action=query&titles=", # why not "title", as in docs??
                word,
                "&prop=revisions&rvprop=timestamp&meta=tokens",
                "&curtimestamp=true&format=json")

  resp <- httr::GET(url = url)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)
  csrf_token <- list_content$query$tokens$csrftoken

  # submit these timestamps with the edit request to prevent conflicts
  starttimestamp <- list_content$curtimestamp
  basetimestamp <- list_content$query$pages$`243088`$revisions$timestamp
  # NOW we can submit edit
  params4 <- list(action = "edit",
                  title = word,
                  basetimestamp = basetimestamp,
                  starttimestamp = starttimestamp,
                  sectiontitle = "Etymology",
                  section = sn,
                  token = csrf_token,
                  format = "json",
                  bot = "true",
                  minor = "true",
                  appendtext = template_text)
  resp <- httr::POST(url = base_url, body = params4)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)
  # return result of POST
  return(list_content$edit$result)
}


# run it ------------------------------------------------------------------


submit_morphology_edit(template_text = generate_morphology_suffix_template("converse", "ation"))

submit_morphology_edit(word = "conversation",
                       template_text = generate_morphology_suffix_template("converse", "ation"))
