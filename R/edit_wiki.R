# misc imports ------------------------------------------------------------

#' @importFrom rlang .data
rlang::.data
#' @importFrom rlang .env
rlang::.env

#' @importFrom magrittr %>%
magrittr::`%>%`


# get_section_number ------------------------------------------------------


#' Get the Section Number for a Wiktionary Page
#'
#' Wiki sections must be specified by number for editing. Find the number, given
#' the name.
#'
#' @param word Character; the Wiktionary page in question.
#' @param section Character; which section to find the number for.
#'
#' @return The number of the specified section.
#' @export
#' @examples
#' get_section_number(word = "Wiktionary:Sandbox")
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



# get_section_text --------------------------------------------------------

#' Get the Section Text for a Wiktionary Page
#'
#' Given a page and the number of a section, return the text of the section.
#'
#' @param word Character; the Wiktionary page in question.
#' @param section_number Character; the number of the section to retrieve.
#'
#' @return The text of the specified section.
#' @export
#' @examples
#' word <- "Wiktionary:Sandbox"
#' sn <- get_section_number(word)
#' get_section_text(word, sn)
get_section_text <- function(word, section_number) {
  url <- paste0("https://en.wiktionary.org/w/api.php?action=parse&page=",
                word, "&format=json&prop=wikitext&section=",
                section_number)
  resp <- httr::GET(url)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)
  section_text <- list_content$parse$wikitext$`*`
  return(section_text)
}

# generate_morphology_*_template --------------------------------------------

# Based on the discussion at
# https://en.wiktionary.org/wiki/Wiktionary:Beer_parlour/2021/January#Adding_surface_analyses_to_fill_in_gaps_in_suffix_categories
# ... I have added the `|nocat=1` parameter to these templates. This prevents
# the breakdown showing up in the corresponding affix categories, but the
# template will still be in the page wikitext.

#' @export
generate_morphology_prefix_template <- function(prefix, base_word) {
  # Note: before now, figure out whether second piece is a suffix or not
  return(paste0("Morphologically {{prefix|en|",
                prefix, "|", base_word,
                "|nocat=1}}"))
}

#' @export
generate_morphology_confix_template <- function(prefix, suffix) {
  # Note: before now, figure out whether first piece is a prefix or not, e.g.
  # Morphologically {{confix|en|astro|logy}}
  # or
  # Morphologically {{suffix|en|conserve|ation}}
  return(paste0("Morphologically {{confix|en|",
                prefix, "|", suffix,
                "|nocat=1}}"))
}

#' @export
generate_morphology_suffix_template <- function(base_word, suffix) {
  # Note: before now, figure out whether first piece is a prefix or not, e.g.
  # Morphologically {{confix|en|astro|logy}}
  # or
  # Morphologically {{suffix|en|conserve|ation}}
  # This function assumes that base_word is NOT a prefix here.
  return(paste0("Morphologically {{suffix|en|",
                base_word, "|", suffix,
                "|nocat=1}}"))
}


# submit_morphology_edit --------------------------------------------------

#' Submit a Morphology Edit to a Wiktionary Page
#'
#' Given a Wiktionary page by title, and some text (generated by one of the
#' morphology templates), along with bot credentials, edits the Wiktionary
#' entry by appending the text at the end of the (English) Etymology section.
#'
#' @param word Character; the Wiktionary page in question.
#' @param template_text Character; text to append to Etymology section.
#' @param username Character; bot username.
#' @param password Character; bot password.
#'
#' @return Logical; TRUE if the result of the edit POST request was "Success",
#'  FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' template_text = generate_morphology_suffix_template("converse", "ation")
#' submit_morphology_edit(word = "conversation",
#'                        template_text = template_text,
#'                        "usernamehere", "passwordhere")
#' }
submit_morphology_edit <- function(word = "Project:Sandbox",
                                   template_text = "",
                                   username,
                                   password) {
  # watch out! Section might be "Etymology 1", if there are multiple.
  # make this more robust...
  # https://github.com/jonthegeek/wikimorphemesedit/issues/2
  sn <- get_section_number(word, section = "Etymology")

  # make sure that this is not a repeat edit.
  current_section_text <- get_section_text(word, sn)

  # if etymology section not found, don't make edit! Maybe later we can do this
  # automatically, but for now, require section to exist already
  if (length(current_section_text) != 1) {
    message("No Etymology section found for ",
            word,
            "; no edits will be made.")
    return(FALSE)
  }

  if (stringr::str_detect(current_section_text,
                          stringr::coll(trimws(template_text)))) {
    message("No new edits to make on page ", word)
    return(FALSE)
  }

  # put edit on a new line.
  template_text <- paste0("\n\n", template_text, "\n")

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
  httr::POST(url = base_url, body = params2)

  # Step 3: GET request to fetch CSRF token and some timestamps
  url <- paste0(base_url,
                "?action=query&titles=", # why not "title", as in docs??
                word,
                "&prop=revisions&rvprop=timestamp&meta=tokens",
                "&curtimestamp=true&format=json")

  resp <- httr::GET(url)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)
  csrf_token <- list_content$query$tokens$csrftoken

  # submit these timestamps with the edit request to prevent conflicts
  starttimestamp <- list_content$curtimestamp
  basetimestamp <- list_content$query$pages[[1]]$revisions$timestamp
  # NOW we can submit edit
  params4 <- list(action = "edit",
                  title = word,
                  basetimestamp = basetimestamp,
                  starttimestamp = starttimestamp,
                  sectiontitle = "Etymology",
                  section = sn,
                  token = csrf_token,
                  format = "json",
                  # bot = "true", # not technically a bot yet (maybe not ever)
                  minor = "true",
                  appendtext = template_text)
  resp <- httr::POST(url = base_url, body = params4)
  html_content <- httr::content(resp, "text")
  list_content <- jsonlite::fromJSON(html_content)

  return(list_content$edit$result == "Success")
}


