library(data.table); library(tidyverse); library(stringr); library(zoo)

# %% # paths (can be wrapped in argparse)
tex     = "tex/paper.tex"
bib     = "bibs/bigLibrary.bib"
bib.out = "bibs/miniLibrary.bib"

# %% # read the article tex file
dt.tex = read_lines(tex) %>%
  as.data.table() %>%
  setnames("V1")  %>%
  .[str_detect(V1, "\\\\.*cite")] # subset to lines with citations

cites = dt.tex[,
      cite := V1 %>%
      str_extract_all("\\\\.*cite.+?\\}", simplify = TRUE) %>%
      str_replace("\\\\.*cite.?", "") %>%
      str_replace_all("[{}]", "") %>%
      str_replace_all("\\[.*?\\]", "")]$cite
# citation keys
allcites = strsplit(cites, ",")  |> unlist() |> as.character() |> unique()

# %% read the bibfile
dt.bib = fread(bib, sep = "\n", blank.lines.skip = TRUE, header = FALSE, quote = "") %>%
  .[, start := str_detect(V1, "@")] %>%
  .[, end := V1 == "}"] %>%
  # don't include abstract or file in the output
  .[str_sub(V1, 1, 4) != "file"] %>%
  .[str_sub(V1, 1, 8) != "abstract"] %>%
  # remove preamble stuff
  .[cumsum(start) != 0] %>%
  # citation and fill forward
  .[str_sub(V1, 1, 1) == "@", cite := V1 %>%
      str_replace_all(., "@.*?\\{", "") %>%
      str_replace_all(., ",$", "")] %>%
  .[, cite := na.locf(cite)]

# %% subset and export
dt.bib[cite %in% allcites][, list(V1)] %>%
  fwrite(., file = bib.out, quote = FALSE, col.names = FALSE)
# %%
