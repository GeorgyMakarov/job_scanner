source('helper_functions.R')

descr <- c('job_description_sample.txt', 'resume_example.txt')
preps <- 'prepositions.txt'


# Build tokens to share them across table and plot functions -------------------
input_tokens <- lapply(
  descr,
  function(i){build_tokens(my_readtext(i, preps), 1)}
)

stemmed_tokens <- lapply(
  input_tokens,
  function(i){quanteda::tokens_wordstem(
    i, language = quanteda::quanteda_options("language_stemmer"))}
)

names(stemmed_tokens) = names(input_tokens) <- c('job', 'resume')


# Prepare output tables for frequencies both resume and job description --------
freq_tables <- lapply(
  stemmed_tokens,
  function(i){
    convert_to_dt(
      data.table::as.data.table(
        data.frame(
          as.list(
            make_freq(i, 1, 1)
          )
        )
      )
    )
  }
)


# Prepare word cloud of job description ----------------------------------------
make_wd_plot(input_tokens$job)


# Compare intersections of resume and job description --------------------------
merged_dt <- 
  merge(x = freq_tables$job, y = freq_tables$resume, by = 'words', all.x = T) %>% 
  data.table::as.data.table(.) %>% 
  data.table::setnames(., old = colnames(.), new = c('words', 'job', 'resume')) %>% 
  data.table::setnafill(., cols = c('job', 'resume'), fill = 0)


# Omit all meaningless words ---------------------------------------------------
stop_words <- 
  tidytext::stop_words %>% 
  select(word)         %>% 
  unique(.)            %>% 
  pull(word)           %>% 
  build_tokens(.)      %>% 
  quanteda::tokens_wordstem(.) %>% 
  as.list(.) %>% 
  data.frame(.) %>% 
  data.table::as.data.table(.) %>% 
  convert_to_dt(.) %>% 
  pull(value) %>% 
  unique(.)

merged_dt <- merged_dt[!(words %in% stop_words)]


# Compute intersection rate between meaningful words ---------------------------
merged_dt$job[merged_dt$job > 0]       <- 1
merged_dt$resume[merged_dt$resume > 0] <- 1

intersect_rate <- round(sum(merged_dt$resume) / sum(merged_dt$job), 2)




