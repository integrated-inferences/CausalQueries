latex_supported = list(
  # Greek letters and variants
  "greek letters" = list(
    "\\alpha" = "alpha",
    "\\Alpha" = "Alpha",
    "\\beta" = "beta",
    "\\Beta" = "Beta",
    "\\chi" = "chi",
    "\\Chi" = "Chi",
    "\\delta" = "delta",
    "\\Delta" = "Delta",
    "\\epsilon" = "epsilon",
    "\\Epsilon" = "Epsilon",
    "\\eta" = "eta",
    "\\Eta" = "Eta",
    "\\gamma" = "gamma",
    "\\Gamma" = "Gamma",
    "\\iota" = "iota",
    "\\Iota" = "Iota",
    "\\kappa" = "kappa",
    "\\Kappa" = "Kappa",
    "\\lambda" = "lambda",
    "\\Lambda" = "Lambda",
    "\\mu" = "mu",
    "\\Mu" = "Mu",
    "\\nu" = "nu",
    "\\Nu" = "Nu",
    "\\omega" = "omega",
    "\\Omega" = "Omega",
    "\\omicron" = "omicron",
    "\\Omicron" = "Omicron",
    "\\phi" = "phi",
    "\\Phi" = "Phi",
    "\\pi" = "pi",
    "\\Pi" = "Pi",
    "\\psi" = "psi",
    "\\Psi" = "Psi",
    "\\rho" = "rho",
    "\\Rho" = "Rho",
    "\\sigma" = "sigma",
    "\\Sigma" = "Sigma",
    "\\tau" = "tau",
    "\\Tau" = "Tau",
    "\\theta" = "theta",
    "\\Theta" = "Theta",
    "\\upsilon" = "upsilon",
    "\\Upsilon" = "Upsilon",
    "\\zeta" = "zeta",
    "\\Zeta" = "Zeta",
    "\\Upsilon" = "Upsilon1",
    "\\varpi" = "omega1",
    "\\varphi" = "varphi"
  ),

  "arithmetic operators" = list(
    "+" = "$LEFT + $RIGHT",
    "-" = "$LEFT - $RIGHT",
    "/" = "$LEFT / $RIGHT",
    "*" = "$LEFT ~ symbol('\052') ~ $RIGHT"
  ),

  "binary operators" = list(
    "=" = "$LEFT * {phantom() == phantom()} * $RIGHT",
    ">" = "$LEFT * {phantom() > phantom()} * $RIGHT",
    "<" = "$LEFT * {phantom() < phantom()} * $RIGHT",
    "\\ne" = "$LEFT * {phantom() != phantom()} * $RIGHT",
    "\\neq" = "$LEFT * {phantom() != phantom()} * $RIGHT",
    "\\geq" = "$LEFT * {phantom() >= phantom()} * $RIGHT",
    "\\leq" = "$LEFT * {phantom() <= phantom()} * $RIGHT",

    "\\div" = "$LEFT %/% $RIGHT",
    "\\pm" = "$LEFT %+-% $RIGHT",
    "\\approx" = "$LEFT %~~% $RIGHT",
    "\\sim" = "$LEFT %~% $RIGHT",
    "\\propto" = "$LEFT %prop% $RIGHT",
    "\\equiv" = "$LEFT %==% $RIGHT",
    "\\cong" = "$LEFT %=~% $RIGHT",
    "\\in" = "$LEFT %in% $RIGHT",
    "\\notin" = "$LEFT %notin% $RIGHT",
    "\\cdot" = "$LEFT %.% $RIGHT",
    "\\times" = "$LEFT %*% $RIGHT",
    "\\circ" = "$LEFT ~ '\u25e6' ~ $RIGHT",
    "\\ast" = "$LEFT ~ symbol('\\053') ~ $RIGHT",
    "\\perp" = "$LEFT ~ symbol('\\136') ~ $RIGHT",
    "\\bullet" = "$LEFT ~ symbol('\\267') ~ $RIGHT",
    "\\otimes" = "$LEFT ~ symbol('\\304') ~ $RIGHT",
    "\\oplus" = "$LEFT ~ symbol('\\305') ~ $RIGHT",
    "\\oslash" = "$LEFT ~ symbol('\\306') ~ $RIGHT",
    "\\vee" = "$LEFT ~ symbol('\\332') ~ $RIGHT",
    "\\wedge" = "$LEFT ~ symbol('\\331') ~ $RIGHT",
    "\\angle" = "$LEFT ~ symbol('\\320') ~ $RIGHT",
    "\\cdots" = "$LEFT ~ cdots ~ $RIGHT",
    "\\ldots" = "$LEFT ~ ldots ~ $RIGHT",
    "\\mod" = "$LEFT ~ 'mod' ~ $RIGHT"
  ),

  "set operators" = list(
    "\\subset" = "$LEFT %subset% $RIGHT",
    "\\subseteq" = "$LEFT %subseteq% $RIGHT",
    "\\nsubset" = "$LEFT %notsubset% $RIGHT",
    "\\supset" = "$LEFT %supset% $RIGHT",
    "\\supseteq" = "$LEFT %supseteq% $RIGHT",
    "\\setminus" = "$LEFT ~ '\\\\' ~ $RIGHT",
    "\\cup" = "$LEFT ~ symbol('\\310') ~ $RIGHT",
    "\\cap" = "$LEFT ~ symbol('\\307') ~ $RIGHT"
  ),

  "other operators" = list(
    "\\forall" = "symbol('\\042')",
    "\\exists" = "symbol('\\044')",
    "\\Im" = "symbol('\\301')",
    "\\Re" = "symbol('\\302')",
    "\\wp" = "symbol('\\303')",
    "\\surd" = "symbol('\\326')",
    "\\neg" = "symbol('\\330')",
    "\\ni" = "symbol('\\047')",
    "\\pmod" = "$LEFT ~ group('(', 'mod' ~ $arg1, ')')"
  ),

  # Square root, sum, prod, integral, etc.
  "operators with subscripts and superscripts" = list(
    "\\sqrt" = "sqrt($arg1, $opt)",
    "\\sum" = "sum($arg1, $sub, $sup)",
    "\\prod" = "prod($arg1, $sub, $sup)",
    "\\int" = "integral($arg1, $sub, $sup)",
    "\\bigcup" = "union($arg1, $sub, $sup)",
    "\\bigcap" = "intersect($arg1, $sub, $sup)",
    "\\lim" = "lim($arg1, $sub)",
    "\\min" = "min($arg1, $sub)",
    "\\max" = "max($arg1, $sub)"
  ),

  # Text size
  "text size" = list(
    "\\normalsize" = "displaystyle($arg1)",
    "\\small" = "scriptstyle($arg1)",
    "\\tiny" = "scriptscriptstyle($arg1)"
  ),

  # Arrows
  "arrows" = list(
    "\\rightarrow" = "$LEFT %->% $RIGHT",
    "\\leftarrow" = "$LEFT %<-% $RIGHT",
    "\\leftrightarrow" = "$LEFT %<->% $RIGHT",
    "\\Rightarrow" = "$LEFT %=>% $RIGHT",
    "\\Leftarrow" = "$LEFT %<=% $RIGHT",
    "\\Leftrightarrow" = "$LEFT %<=>% $RIGHT",
    "\\uparrow" = "$LEFT %up% $RIGHT",
    "\\downarrow" = "$LEFT %down% $RIGHT",
    "\\Uparrow" = "$LEFT %dblup% $RIGHT",
    "\\Downarrow" = "$LEFT %dbldown% $RIGHT",
    # Some synonyms
    "\\to" = "$LEFT %->% $RIGHT",
    "\\iff" = "$LEFT %<=>% $RIGHT"
  ),

  # Layout
  "layout and spacing" = list(
    "\\overset" = "atop($arg1, $arg2)",
    "\\frac" = "frac($arg1, $arg2)",


    "\\@SPACE1" = "$LEFT * phantom(.) * $RIGHT",
    "\\@SPACE2" = "$LEFT ~~ $RIGHT",
    "\\phantom" = "phantom($arg1)",

    # Dummy symbols
    "\\ " = "",
    "\\;" = "",
    "\\," = ""
  ),

  # Formatting
  "formatting" = list(
    "\\textbf" = "bold($arg1)",
    "\\textit" = "italic($arg1)",
    "\\bf" = "bold($arg1)",
    "\\it" = "italic($arg1)",
    "\\textrm" = "plain($arg1)"
  ),

  # Symbols
  "symbols" = list(
    "\\infty" = " infinity ",
    "\\partial" = " partialdiff ",
    "\\degree" = " degree ",
    "\\clubsuit" = "symbol('\\247')",
    "\\diamondsuit" = "symbol('\\250')",
    "\\heartsuit" = "symbol('\\251')",
    "\\spadesuit" = "symbol('\\252')",
    "\\aleph" = "symbol('\\300')",
    "\\euro" = "symbol('\\240')",
    "\\textbackslash" = "'\\\\'",
    "\\diamond" = "'\\u25ca'",
    "\\uptriangle" = "'\\u25b2'",
    "\\righttriangle" = "'\\u25ba'",
    "\\downtriangle" = "'\\u25bc'",
    "\\lefttriangle" = "'\\u25c4'",
    "\\smiley" = "'\u263a'",
    "\\sharp" = "'\u266f'",
    "\\eighthnote" = "'\u266a'",
    "\\twonotes" = "'\u266b'",
    "\\sun" = "'\u263c'",
    "\\venus" = "'\u2640'",
    "\\mars" = "'\u2642'",
    "\\Exclam" = "'\\u203c'",
    "\\dagger" = "'\\u2020'",
    "\\ddagger" = "'\\u2021'",
    "''" = "$LEFT * second ",
    "'" = "$LEFT * minute ",
    "\\degree" = "'\\u0b0'",
    "\\prime" = "$LEFT * minute ",
    "\\second" = "$LEFT * second ",
    "\\third" = "$LEFT * '\\u2034'",
    "\\%" = "symbol('\\045')",
    "\\S" = "'\u00a7'",
    "\\permil" = "'\u2030'",
    "\\blacksquare" = "'\u25a0'",
    "\\square" = "'\u25a1'",
    "\\smwhtsquare" = "'\u25ab'",
    "\\smblksquare" = "'\u25aa'",
    "\\smallint" = "'\u222b'",
    "\\ell" = "'\u2113'",
    "\\house" = "'\u2302'",
    "\\dots" = "'\u2026'"
  ),

  # Decorations
  "decorations" = list(
    "\\tilde" = "tilde($arg1)",
    "\\hat" = "hat($arg1)",
    "\\widehat" = "widehat($arg1)",
    "\\widetilde" = "widetilde($arg1)",
    "\\bar" = "bar($arg1)",
    "\\dot" = "dot($arg1)",
    "\\underline" = "underline($arg1)",
    "\\mathring" = "ring($arg1)"
  ),

  # Characters that need to be treated in a special way by the parser
  # when in math mode
  "specials" = list(
    "," = "list(,)",
    "|" = "group('|', phantom(), '')"
  ),

  # Parentheses
  "parentheses" = list(
    "\\left(" = "bgroup('(', $RIGHT",
    "\\left[" = "bgroup('[', $RIGHT",
    "\\left{" = "bgroup('{', $RIGHT",
    "\\left|" = "bgroup('|', $RIGHT",
    "\\left." = "bgroup('', $RIGHT",
    "\\right)" = "$LEFT, ')')",
    "\\right]" = "$LEFT, ']')",
    "\\right}" = "$LEFT, '}')",
    "\\right|" = "$LEFT, '|')",
    "\\right." = "$LEFT, '')",
    "\\|" = ""
  ),

  "parentheses (not scalable)" = list(
    "\\lbrack" = "group('[', $P, '')",
    "\\rbrack" = "group('', $P, ']')",
    "\\langle" = "group(langle,$P, '')",
    "\\rangle" = "group('', $P, rangle)",
    "\\lceil" = "group(lceil, $P, '')",
    "\\rceil" = "group('', $P, rceil)",
    "\\lfloor" = "group(lfloor, $P, '')",
    "\\rfloor" = "group('', $P, rfloor)",
    "\\@pipe" = "group('|', group('|', $P, ''), '')"
  ),

  "vector" = list(
    "\\norm" = "group('|', group('|', $arg1, '|'), '|')",
    "\\bra" = "group(langle, $arg1, '|')",
    "\\ket" = "group('|', $arg1, rangle)",
    "\\braket" = "group(langle, $arg1, rangle)"
  ),

  # Approximations to the TeX and LaTeX symbols
  "miscellanea" = list(
    "\\LaTeX" = "L^{$P[$P[$P[scriptstyle(A)]]]}*T[textstyle(E)]*X",
    "\\TeX" = "T[textstyle(E)]*X"
  )
)

latex_supported_map <- Reduce(c, latex_supported)

.base_separators <- c("$", "{", "}", "\\", "[", "]", ",", ";", " ")
.math_separators <- unique(
  c(.base_separators,
    names(latex_supported[['arithmetic operators']]),
    "|",
    "&",
    "^",
    "_",
    "(",
    ")",
    "!",
    "?",
    "'",
    "=", ">", "<"))

`%??%` <- function(x, y) {
  return(if (is.null(x) || is.na(x) || (is.logical(x) && !x))  y else x)
}

str_replace_fixed <- function(string, pattern, replacement) {
  #str_replace_all(string, fixed(pattern), replacement)
  # This is a correct replacement of str_replace_all() only if one never
  # uses a function for replacement... This is checked in v0.9.8
  gsub(pattern, replacement, string, fixed = TRUE)
}


print.latextoken2 <- function(x, depth = 0, ...) {
  token <- x
  pad <- strrep(" ", depth)
  cat(pad,
      if (depth > 0) paste0("| :", token$command, ":"),
      if (!is.null(token$rendered)) paste0(" -> ", token$rendered),
      "\n",
      sep = "")

  for (children_type in
       c("children", "args", "optional_arg", "sup_arg", "sub_arg")) {
    if (length(token[[children_type]]) > 0) {
      if (children_type != "children") {
        cat(pad, "* <", children_type, ">", "\n", sep = "")
      }
      for (tok_idx in seq_along(token[[children_type]])) {
        c <- token[[children_type]][[tok_idx]]
        if (is.list(c)) {
          cat(pad, " | [argument ", tok_idx, "]\n", sep = "")
          for (cc in c) {
            print(cc, depth + 1)
          }
        } else {
          print(c, depth + 1)
        }
      }
    }
  }
}


cat_trace <- function(...) {
  trace <- getOption("latex2exp.debug.trace", FALSE)
  if (trace) {
    cat("Trace:", ..., "\n")
  }
}

.token2 <- function(command, text_mode) {
  tok <- new.env()
  tok$args <- list()
  tok$optional_arg <- list()
  tok$sup_arg <- list()
  tok$sub_arg <- list()
  tok$children <- list()
  tok$command <- command
  tok$is_command <- startsWith(command, "\\")
  tok$text_mode <- text_mode
  tok$left_operator <- tok$right_operator <- FALSE
  class(tok) <- "latextoken2"
  tok
}

clone_token <- function(tok) {
  if (is.list(tok)) {
    return(lapply(tok, clone_token))
  }
  new_tok <- .token2(tok$command, tok$text_mode)
  # clone all the linked tokens
  for (field in c("children", "args", "optional_arg", "sup_arg", "sub_arg")) {
    new_tok[[field]] <- lapply(tok[[field]], clone_token)
  }
  for (field in c("is_command", "left_operator", "right_operator")) {
    new_tok[[field]] <- tok[[field]]
  }
  new_tok
}

.find_substring <- function(string, boundary_characters) {
  # This appears overly complex, based on the value returned by str_match()
  #pattern <- paste0("^[^",
  #  paste0("\\", boundary_characters, collapse = ""),
  #                 "]+")
  #ret <- str_match(string, pattern)[1,1]
  #if ((is.na(ret) || nchar(ret) == 0) && nchar(string) > 0) {
  #  substring(string, 1, 1)
  #} else {
  #  ret
  #}
  # Empty string is returned as such
  if (nchar(string) == 0)
    return("")
  # Boundary characters at the beginning of the string is returned
  first_char <- substring(string, 1, 1)
  if (first_char %in% boundary_characters)
    return(first_char)
  # Otherwise, anything, starting from a boundary character is eliminated
  boundary_pattern <- paste0("\\", boundary_characters, collapse = "")
  pattern <- paste0("[", boundary_pattern, "].*$")
  sub(pattern, "", string, perl = TRUE)
}

.find_substring_matching <- function(string, opening, closing) {
  chars <- strsplit(string, "", fixed = TRUE)[[1]]
  depth <- 0
  start_expr <- -1

  for (i in seq_along(chars)) {
    if (chars[i] == opening) {
      if (depth == 0) {
        start_expr <- i
      }
      depth <- depth + 1
    } else if (chars[i] == closing) {
      depth <- depth - 1
      if (depth == 0) {
        return(substring(string, start_expr + 1, i - 1))
      }
    }
  }
  if (depth != 0) {
    stop("Unmatched '", opening, "' (opened at position: ", start_expr,
         ") while parsing '", string, "'")
  } else {
    return(string)
  }
}


parse_latex <- function(latex_string, text_mode = TRUE, depth = 0, pos = 0,
                        parent = NULL) {
  input <- latex_string

  if (depth == 0) {
    validate_input(latex_string)
  }
  if (depth == 0) {
    latex_string <- str_replace_fixed(latex_string, '\\|', '\\@pipe ')
    # This one must be replaced by several calls to gsub()
    #latex_string <- str_replace_all(latex_string,
    #  "\\\\['\\$\\{\\}\\[\\]\\!\\?\\_\\^]", function(char) {
    #    paste0("\\ESCAPED@",
    #          as.integer(charToRaw(str_replace_fixed(char, "\\", ""))),
    #          "{}")
    #  })
    latex_string <- gsub("\\\\'", "\\\\ESCAPED@39{}", latex_string)
    latex_string <- gsub("\\\\\\$", "\\\\ESCAPED@36{}", latex_string)
    latex_string <- gsub("\\\\\\{", "\\\\ESCAPED@123{}", latex_string)
    latex_string <- gsub("\\\\\\}", "\\\\ESCAPED@125{}", latex_string)
    latex_string <- gsub("\\\\\\[", "\\\\ESCAPED@91{}", latex_string)
    latex_string <- gsub("\\\\\\]", "\\\\ESCAPED@93{}", latex_string)
    latex_string <- gsub("\\\\\\!", "\\\\ESCAPED@33{}", latex_string)
    latex_string <- gsub("\\\\\\?", "\\\\ESCAPED@63{}", latex_string)
    latex_string <- gsub("\\\\\\_", "\\\\ESCAPED@95{}", latex_string)
    latex_string <- gsub("\\\\\\^", "\\\\ESCAPED@94{}", latex_string)

    latex_string <- gsub("([^\\\\]?)\\\\,", "\\1\\\\@SPACE1{}", latex_string)
    latex_string <- gsub("([^\\\\]?)\\\\;", "\\1\\\\@SPACE2{}", latex_string)
    latex_string <- gsub("([^\\\\]?)\\\\\\s", "\\1\\\\@SPACE2{}", latex_string)

    cat_trace("String with special tokens substituted: ", latex_string)
  }

  i <- 1

  tokens <- list()
  token <- NULL

  withCallingHandlers({
    while (i <= nchar(latex_string)) {
      # Look at current character, previous character, and next character
      ch <- substring(latex_string, i, i)
      prevch <- if (i == 1) "" else substring(latex_string, i - 1, i - 1)
      nextch <- if (i == nchar(latex_string)) "" else
        substring(latex_string, i + 1, i + 1)

      # LaTeX string left to be processed
      current_fragment <- substring(latex_string, i)

      cat_trace("Position: ", i, " ch: ", ch, " next: ", nextch,
                " current fragment: ", current_fragment,
                " current token: ", token$command,
                " text mode: ", text_mode)


      separators <- if (text_mode) {
        .base_separators
      } else {
        .math_separators
      }

      # We encountered a backslash. Continue until we encounter
      # another backslash, or a separator, or a dollar
      if (ch == "\\" && nextch != "\\") {
        # Continue until we encounter a separator
        current_fragment <- substring(current_fragment, 2)

        command <- paste0("\\",
                          .find_substring(current_fragment, .math_separators))
        cat_trace("Found token ", command, " in text_mode: ", text_mode)
        token <- .token2(command, text_mode)
        tokens <- c(tokens, token)

        i <- i + nchar(command)
      } else if (!text_mode &&
                 !is.null(token) &&
                 token$command %in% c("\\left", "\\right") &&
                 ch %in% c(".", "{", "}", "[", "]", "(", ")", "|")) {
        # a \\left or \\right command has started. eat up the next character
        # and append it to the command.
        token$command <- paste0(token$command, ch)
        i <- i + 1
      } else if (ch == "{") {
        argument <- .find_substring_matching(current_fragment, "{", "}")
        if (is.null(token)) {
          token <- .token2("", text_mode)
          tokens <- c(tokens, token)
        }

        args <- parse_latex(argument, text_mode = text_mode,
                            depth = depth + 1, parent = token, pos = i)
        if (length(args) > 0) {
          token$args <- c(token$args, list(args))
        }
        # advance by two units (the content of the braces + two braces)
        i <- i + nchar(argument) + 2
      }  else if (ch == "[") {
        argument <- .find_substring_matching(current_fragment, "[", "]")
        if (is.null(token)) {
          token <- .token2("", text_mode)
          tokens <- c(tokens, token)
        }

        token$optional_arg <- c(
          token$optional_arg,
          parse_latex(argument, text_mode = text_mode,
                      depth = depth + 1, parent = token, pos = i)
        )

        # advance by two units (the content of the braces + two braces)
        i <- i + nchar(argument) + 2
      } else if (ch %in% c("^", "_") && !text_mode) {
        if (is.null(token)) {
          token <- .token2("", text_mode)
          tokens <- c(tokens, token)
        }

        arg_type <- if (ch == "^") "sup_arg" else "sub_arg"

        advance <- 1

        # If there are spaces after the ^ or _ character,
        # consume them and advance past the spaces
        if (nextch == " ") {
          #n_spaces <- str_match(substring(current_fragment, 2), "\\s+")[1, 1]
          n_spaces <- regmatches(current_fragment,
                                 regexpr("\\s+", substring(current_fragment, 2)))
          advance <- advance + nchar(n_spaces)
          nextch <- substring(current_fragment, advance + 1, advance + 1)
        }

        # Sub or sup arguments grouped with braces. This is easy!
        if (nextch == "{") {
          argument <- .find_substring_matching(substring(current_fragment,
                                                         advance + 1), "{", "}")

          # advance by two units (the content of the braces + two braces)
          advance <- advance + nchar(argument) + 2
        } else if (nextch == "\\") {
          # Advance until a separator is found
          argument <- paste0("\\",
                             .find_substring(substring(current_fragment, advance + 2), separators))
          advance <- advance + nchar(argument)
        } else {
          argument <- substring(current_fragment, advance + 1, advance + 1)
          advance <- advance + nchar(argument)
        }

        token[[arg_type]] <- parse_latex(argument, text_mode = text_mode,
                                         depth = depth + 1, parent = token, pos = i)

        i <- i + advance
      } else if (ch == "$") {
        # Switch between "text mode" and "math mode", and advance.
        text_mode <- !text_mode
        if (text_mode) {
          token <- NULL
        }
        i <- i + 1
      } else if (ch == " ") {
        if (text_mode) {

          if (is.null(token) || token$is_command) {
            token <- .token2(" ", text_mode)
            tokens <- c(tokens, token)
          } else {
            token$command <- paste0(token$command, " ")
          }
        }
        i <- i + 1
      } else {
        # Other characters:
        if (text_mode) {
          # either add to a string-type token...
          if (is.null(token) || !token$text_mode || token$is_command) {
            token <- .token2("", TRUE)
            tokens <- c(tokens, token)
          }
          if (ch == "'") {
            ch <- "\\'"
          }
          token$command <- paste0(token$command, ch)
          i <- i + 1
        } else if (ch %in% c("?", "!", "@", ":", ";")) {
          # ...or escape them to avoid introducing illegal characters in the
          # plotmath expression...
          token <- .token2(paste0("\\ESCAPED@", utf8ToInt(ch)), TRUE)
          tokens <- c(tokens, token)
          i <- i + 1
        } else if (ch == "'") {
          # special-case single quotes in math mode to render them as \prime
          # or \second
          if (nextch == "'") {
            token <- .token2("\\second", TRUE)
            i <- i + 2
          } else {
            token <- .token2("\\prime", TRUE)
            i <- i + 1
          }
          tokens <- c(tokens, token)
        } else {
          # or, just add everything to a single token
          str <- .find_substring(current_fragment, separators)

          # If in math mode, ignore spaces
          token <- .token2(gsub("\\s+", "", str), text_mode)
          tokens <- c(tokens, token)
          i <- i + nchar(str)
        }
      }
    }

  }, error = function(e) {
    token_command <- if (is.null(token)) {
      ""
    } else {
      token$command
    }
    message("Error while parsing LaTeX string: ", input)
    message("Parsing stopped at position ", i + pos)
    if (!is.null(token)) {
      message("Last token parsed:", token$command)
    }
    if (!is.null(parent)) {
      message("The error happened within the arguments of :",
              parent$command, "\n")
    }
  })

  if (depth == 0) {
    root <- .token2("<root>", TRUE)
    root$children <- tokens
    root
  } else {
    tokens
  }
}

render_latex <- function(tokens, user_defined = list(),
                         hack_parentheses = FALSE) {
  if (!is.null(tokens$children)) {
    return(render_latex(tokens$children, user_defined,
                        hack_parentheses = hack_parentheses))
  }
  translations <- c(user_defined, latex_supported_map)

  for (tok_idx in seq_along(tokens)) {
    tok <- tokens[[tok_idx]]
    tok$skip <- FALSE

    tok$rendered <- if (grepl("^\\\\ESCAPED@", tok$command)) {
      # a character, like '!' or '?' was escaped as \\ESCAPED@ASCII_SYMBOL.
      # return it as a string.
      #arg <- str_match(tok$command, "@(\\d+)")[1,2]
      arg <- substring(regmatches(tok$command,
                                  regexpr("@(\\d+)", tok$command)), 2)
      arg <- intToUtf8(arg)

      if (arg == "'") {
        arg <- "\\'"
      }


      if (tok_idx == 1) {
        tok$left_separator <- ''
      }

      paste0("'", arg, "'")
      #next
    } else if (!tok$text_mode || tok$is_command) {
      # translate using the translation table in symbols.R
      translations[[trimws(tok$command)]] %??% tok$command
    } else {
      # leave as-is
      tok$command
    }

    # empty command; if followed by arguments such as sup or sub, render as
    # an empty token, otherwise skip
    if (tok$rendered == "") {
      if (length(tok$args) > 0 || length(tok$sup_arg) > 0 ||
          length(tok$sub_arg) > 0) {
        tok$rendered <- "{}"
      } else {
        tok$skip <- TRUE
      }
    }

    if (tok$text_mode && !tok$is_command) {
      tok$rendered <- paste0("'", tok$rendered, "'")
    }


    # If the token starts with a number, break the number from
    # the rest of the string. This is because a plotmath symbol
    # cannot start with a number.
    if (grepl("^[0-9]", tok$rendered) && !tok$text_mode) {
      # This is ultra-complex for something simple using sub()
      #split <- str_match(tok$rendered, "(^[0-9\\.]*)(.*)")
      #if (split[1, 3] != "") {
      #  tok$rendered <- paste0(split[1, 2], "*", split[1, 3])
      #} else {
      #  tok$rendered <- split[1, 2]
      #}
      tok$rendered <- sub("^([0-9\\.]+)([^0-9\\.].*)", "\\1*\\2", tok$rendered)

      if (startsWith(tok$rendered, "0") && nchar(tok$rendered) > 1) {
        tok$rendered <- paste0("0*", substring(tok$rendered, 2))
      }
      # I need this to avoid double zeros before the decimal point
      tok$rendered <- gsub("0*.", "0.", tok$rendered, fixed = TRUE)
    }

    tok$left_operator <- grepl("$LEFT", tok$rendered, fixed = TRUE)
    tok$right_operator <- grepl("$RIGHT", tok$rendered, fixed = TRUE)

    if (tok_idx == 1) {
      tok$left_separator <- ""
    }

    if (tok$left_operator) {
      if (tok_idx == 1) {
        # Either this operator is the first token...
        tok$rendered <- str_replace_fixed(tok$rendered, "$LEFT", "phantom()")
      } else if (tokens[[tok_idx - 1]]$right_operator) {
        # or the previous token was also an operator or an open parentheses.
        # Bind the tokens using phantom()
        tok$rendered <- str_replace_fixed(tok$rendered, "$LEFT", "phantom()")
      } else {
        tok$rendered <- str_replace_fixed(tok$rendered, "$LEFT", "")
        tok$left_separator <- ""
      }
    }
    if (tok$right_operator) {
      if (tok_idx == length(tokens)) {
        tok$rendered <- str_replace_fixed(tok$rendered, "$RIGHT", "phantom()")
      } else {
        tok$rendered <- str_replace_fixed(tok$rendered, "$RIGHT", "")
        tokens[[tok_idx + 1]]$left_separator <- ""
      }
    }
    if (length(tok$args) > 0) {
      for (argidx in seq_along(tok$args)) {
        args <- render_latex(tok$args[[argidx]], user_defined,
                             hack_parentheses = hack_parentheses)
        argfmt <- paste0("$arg", argidx)
        if (grepl(argfmt, tok$rendered, fixed = TRUE)) {
          tok$rendered <- str_replace_fixed(tok$rendered, argfmt, args)
        } else {
          if (tok$rendered != "{}") {
            tok$rendered <- paste0(tok$rendered, " * {", args, "}")
          } else {
            tok$rendered <- paste0("{", args, "}")
          }
        }
      }
    }

    if (length(tok$optional_arg) > 0) {
      optarg <- render_latex(tok$optional_arg, user_defined,
                             hack_parentheses = hack_parentheses)
      if (grepl("$opt", tok$rendered, fixed = TRUE)) {
        tok$rendered <- str_replace_fixed(tok$rendered, "$opt", optarg)
      } else {
        # the current token is not consuming an optional argument, so render
        # it as square brackets
        tok$rendered <- paste0(tok$rendered, " * '[' *", optarg, " * ']'")
      }
    }

    for (type in c("sub", "sup")) {
      arg <- tok[[paste0(type, "_arg")]]
      argfmt <- paste0("$", type)

      if (length(arg) > 0) {
        rarg <- render_latex(arg, user_defined,
                             hack_parentheses = hack_parentheses)

        if (grepl(argfmt, tok$rendered, fixed = TRUE)) {
          tok$rendered <- str_replace_fixed(tok$rendered, argfmt, rarg)
        } else {
          if (type == "sup") {
            tok$rendered <- sprintf("%s^{%s}", tok$rendered, rarg)
          } else {
            tok$rendered <- sprintf("%s[%s]", tok$rendered, rarg)
          }
        }

      }
    }

    # Replace all $P tokens with phantom(), and consume
    # any arguments that were not specified (e.g. if
    # there is no argument specified for the command,
    # substitute '' for '$arg1')
    tkr <- tok$rendered
    tkr <- str_replace_fixed(tkr, "$P", "phantom()")
    tkr <- str_replace_fixed(tkr, "$arg1", "")
    tkr <- str_replace_fixed(tkr, "$arg2", "")
    tkr <- str_replace_fixed(tkr, "$sup", "")
    tkr <- str_replace_fixed(tkr,"$sub", "")
    tkr <- str_replace_fixed(tkr, "$opt", "")
    tok$rendered <- tkr

    if (tok_idx != length(tokens) && tok$command == "\\frac") {
      tok$right_separator <- " * phantom(.)"
    }

    if (!hack_parentheses) {
      if (tok$command %in% c("(", ")")) {
        tok$left_separator <- ""
        tok$right_separator <- ""
      }
      if (tok_idx > 1 && tokens[[tok_idx - 1]]$command == "(") {
        tok$left_separator <- ""
      }
      if (tok_idx > 1 && tokens[[tok_idx]]$command ==
          "(" && length(tokens[[tok_idx - 1]]$sup_arg) > 0) {
        tok$left_separator <- "*"
      }
    } else {
      if (tok$command %in% c("(", ")") && !tok$text_mode) {
        cat_trace("Using hack for parentheses")
        if (tok$command == "(") {
          tok$rendered <- "group('(', phantom(), '.')"
        } else if (tok$command == ")") {
          tok$rendered <- "group(')', phantom(), '.')"
        }
      }
    }

    # If the token still starts with a "\", substitute it
    # with the corresponding expression
    tok$rendered <- sub("^\\\\", "", tok$rendered)

    if (tok$rendered == "{}") {
      tok$skip <- TRUE
    }
  }


  rendered_tokens <- sapply(tokens, function(tok) {
    if (tok$skip) {
      ""
    } else {
      paste0(tok$left_separator %??% "*",
             tok$rendered,
             tok$right_separator %??% "")
    }
  })
  paste0(rendered_tokens, collapse = "")
}

validate_input <- function(latex_string) {
  for (possible_slash_pattern in c("\a", "\b", "\f", "\v")) {
    if (grepl(possible_slash_pattern, latex_string, fixed = TRUE)) {
      repr <- deparse(possible_slash_pattern)
      message("latex2exp: Detected possible missing backslash: you entered ",
              repr, ", did you mean to type ",
              sub("\\\\?", "?", repr))
    }
  }

  if (grepl("\\\\", latex_string, fixed = TRUE)) {
    stop("The LaTeX string '", latex_string,
         "' includes a '\\\\' command. Line breaks are not currently supported.")
  }

  test_string <- str_replace_fixed(latex_string, "\\{", "")
  test_string <- str_replace_fixed(test_string, "\\}", "")

  n_match_all <- function(x, pattern) {
    res <- gregexpr(pattern, x, perl = TRUE)[[1]]
    if (length(res) == 1 && res == -1) 0 else length(res)
  }

  # check that opened and closed braces match in number
  #opened_braces <- nrow(str_match_all(test_string, "[^\\\\]*?(\\{)")[[1]]) -
  #  nrow(str_match_all(test_string, "\\\\left\\{")[[1]])
  opened_braces <- n_match_all(test_string, "[^\\\\]*?(\\{)") -
    n_match_all(test_string, "\\\\left\\{")
  #closed_braces <- nrow(str_match_all(test_string, "[^\\\\]*?(\\})")[[1]]) -
  #  nrow(str_match_all(test_string, "\\\\right\\}")[[1]])
  closed_braces <- n_match_all(test_string, "[^\\\\]*?(\\})") -
    n_match_all(test_string, "\\\\right\\}")

  if (opened_braces != closed_braces) {
    stop("Mismatched number of braces in '", latex_string, "' (",
         opened_braces, " { opened, ",
         closed_braces, " } closed)")
  }

  # check that the number of \left* and \right* commands match
  #lefts <- nrow(str_match_all(test_string,
  #  "[^\\\\]*\\\\left[\\(\\{\\|\\[\\.]")[[1]])
  lefts <- n_match_all(test_string, "[^\\\\]*\\\\left[\\(\\{\\|\\[\\.]")
  #rights <- nrow(str_match_all(test_string,
  #  "[^\\\\]*\\\\right[\\)\\}\\|\\]\\.]")[[1]])
  rights <- n_match_all(test_string, "[^\\\\]*\\\\right[\\)\\}\\|\\]\\.]")

  if (lefts != rights) {
    stop("Mismatched number of \\left and \\right commands in '",
         latex_string, "' (",
         lefts, " left commands, ",
         rights, " right commands.")
  }

  TRUE
}

TeX_internal <- function(input, bold = FALSE, italic = FALSE, user_defined = list(),
                output = c('expression', 'character', 'ast')) {
  if (length(input) > 1) {
    return(sapply(input, TeX, bold = bold, italic = italic,
                  user_defined = user_defined, output = output))
  }
  stopifnot(is.character(input))

  output <- match.arg(output)
  parsed <- parse_latex(input)

  # Try all combinations of "hacks" in this grid, until one succeeds.
  # As more hacks are introduced, the resulting expression will be less and
  # less tidy, although it should still be visually equivalent to the
  # desired output given the latex string.
  grid <- expand.grid(hack_parentheses = c(FALSE, TRUE))
  successful <- FALSE
  for (row in seq_len(nrow(grid))) {
    # Make a deep clone of the LaTeX token tree
    parsed_clone <- clone_token(parsed)
    rendered <- render_latex(parsed_clone, user_defined,
                             hack_parentheses = grid$hack_parentheses[[row]])

    if (bold && italic) {
      rendered <- paste0("bolditalic(", rendered, ")")
    } else if (bold) {
      rendered <- paste0("bold(", rendered, ")")
    } else if (italic) {
      rendered <- paste0("italic(", rendered, ")")
    }

    cat_trace("Rendered as ", rendered, " with parameters ",
              toString(grid[row, ]))

    if (output == "ast") {
      return(parsed)
    }

    rendered_expression <- try({
      str2expression(rendered)
    }, silent = TRUE)

    if (inherits(rendered_expression, "try-error")) {
      error <- rendered_expression
      cat_trace("Failed, trying next combination of hacks, error:", error,
                " parsed as: ", rendered)

      if (row == 1) {
        original_error <- error
      }
    } else {
      successful <- TRUE
      break
    }
  }

  if (!successful) {
    stop("Error while converting LaTeX into valid plotmath.\n",
         "Original string: ", input, "\n",
         "Parsed expression: ", rendered, "\n",
         original_error)
  }
  if (output == "character") {
    return(rendered)
  }

  # if the rendered expression is empty, return expression('') instead.
  if (length(rendered_expression) == 0) {
    rendered_expression <- expression('')
  }

  class(rendered_expression) <- c("latexexpression", "expression")
  attr(rendered_expression, "latex") <- input
  attr(rendered_expression, "plotmath") <- rendered

  rendered_expression
}


