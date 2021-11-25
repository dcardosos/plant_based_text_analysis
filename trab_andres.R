library(tidyverse)
library(magrittr)
library(tidytext)
library(topicmodels)
library(patchwork)
library(glue)
library(lexiconPT)

# total
vegan_0021 <- read_csv("data/veganismo_00_21.csv", col_select = c(year, long_text))
# near brazil
vegan_1521 <- read_csv("data/veganismo_15_21.csv", col_select = c(year, long_text))
# carne vegana 
carne_1521 <- read_csv("data/carne_vegana_15_21.csv", col_select = c(year, long_text))
# hamburguer do futuro
burguer_futuro <- read_csv("data/hamburguer_futuro_15_21.csv", 
                           col_select = c(year, long_text))
# futuro burguer
futuro_burguer <- read_csv("data/futuro_burguer_15_21.csv", 
                           col_select = c(year, long_text))
# carne vegetal
carne_vegetal <- read_csv("data/carne_vegetal_15_21.csv", 
                           col_select = c(year, long_text))
# plant based
plant_based <- read_csv("data/carne_plant_based_15_21.csv", 
                          col_select = c(year, long_text))
# sustentabilidade
sustent <- read_csv("data/sustentabilidade.csv", 
                    col_select = c(year, long_text))
# stop words
pt_stopwords <- tidytext::get_stopwords("pt")
others_words <- c("veganismo", "sobre", "nao", "pra", "q", "so", "ser", "ta", 
                  "sao", "pq", "http", "t.co", "tambem", "ate", "ter", "voce",
                  "gente", "ja", "to", "ai", "n", "pro", "vai", "vc", "sim", 
                  "faz", "coisa", "bem", "falar", "vou", "sei", "acho", "tao", 
                  "eh", "coisa", "coisas", "entao", "hoje", "dia", "quer", 
                  "forma", "todos", "sabe", "cara", "pode", "fazer", "mim", 
                  "todo", "fala", "tipo", "etc", "quer", "algo", "ha", "ne", 
                  "p", "la", "vcs", "vez", "mto", "alem", "ver", "cada", "outros", 
                  "acha", "mt", "vem", "alguem", "parte", "via", "serao", "uns",
                  "quero", "quiser", "comecar", "ganesha", "restauranteganesha",
                  "santos", "dado", "existe", "encontro", "video", "assunto",
                  "comuns", "casa", "fica", "tava", "amiga", "galera", "s",
                  "ano", "lindo")

count_word <- function(tab){
  
  tab %>% 
    group_by(year) %>% 
    unnest_tokens(word, long_text) %>% 
    mutate(word = abjutils::rm_accent(word)) %>%
    filter(!grepl('[0-9]', word)) %>% 
    anti_join(pt_stopwords) %>% 
    count(word, sort = TRUE) %>% 
    filter(! word %in% others_words)
}


# word cloud --------------------------------------------------------------

wordcloud_stop <- c("vegano", "animal", "veganos", "vegano", "vegana", "nada",
                    "tudo", "falando", "ainda", "contra", "assim", "agora", 
                    "porque", "aqui", "sempre", "meio", "come", "anos", "bom",
                    "nunca", "sendo", "melhor", "vegetariana", "vegetariano", 
                    "vegan", "comer", "pessoa")

frequencia <- function(w_count, .year){
  
  w_count %>% 
    filter(!word %in% wordcloud_stop) %>% 
    slice(1:15) %>% 
    ggplot(aes(y = reorder(word, n), x = n)) +
    geom_bar(stat = "identity", fill = "#88a286") +
    scale_x_continuous("", expand = c(0, 0)) +
    theme_light() +
    labs(title = glue("Palavras não-vazias mais comuns - {as.character(.year)}")) +
    theme(
      plot.title = element_text(size = rel(2),
                                hjust = 0.5,
                                face = "bold",
                                family = "mono",
                                color = "gray28"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(face = "bold",
                               family = "mono",
                               size = rel(1)),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "black",
                                  size = 0.8),
      axis.line.x = element_line(linetype = "solid",
                                 size = 1),
      panel.border = element_blank(),
      legend.position = "none",
    
    )
  
}

# grafico de frequencia ------------------------------------------------------
graf_anos <- function(df){
  
  count_word(df) %>% 
    nest() %>% 
    filter(year != 2015) %>%
    arrange(year) %>% 
    mutate(grafico = map2(.x = data,
                          .y = year,
                          ~ frequencia(.x, .y))) %$%
    reduce(grafico, `+`) +
    plot_annotation(title = "Pesquisa: 'veganismo'",
                    theme = theme(plot.title = element_text(
                      face = "bold",
                      family = "mono",
                      color = "gray28",
                      size = 18)))
  
  
}


graf_geral <- function(df, more_stop = ""){
  
  df %>% 
    unnest_tokens(word, long_text) %>% 
    mutate(word = abjutils::rm_accent(word)) %>%
    filter(!grepl('[0-9]', word)) %>% 
    anti_join(pt_stopwords) %>% 
    count(word, sort = TRUE) %>% 
    filter(! word %in% c(others_words, more_stop)) %>% 
    frequencia('2015-2021')
  
  
}

# carne_stop <- c("carne", "fiz", "comi", "nome", "mae", "boa",
#                 "gt", "fazendo", "comendo", "feita", "comida",
#                 "comprar", "gosto", "ficou", "parece")
# 
# graf_geral(carne_1521, carne_stop)


# topic modelling ---------------------------------------------------------
my_words <- c("carne", "vegano", "vegana", "agora", "comi", "nome", "nada", "boa",
              "feita", "assim", "comer", "ainda", "bom", "fiz", "gosto", "aqui",
              "ficou", "veganos", "mae", "come", "tudo", "mae", "vegetariana",
              "parece", "fazendo", "nunca", "gt", "vi", "pessoas", "pessoa", 
              "queria", "porra", "porque", "comendo", "comprei", "gostei",
              "falando", "tal", "futuro", "burguer", "hamburguer", "vi", "vegetal", 
              "vegetariano")
modelagem <- function(df, k, .ot = ""){
  df %>% 
    unnest_tokens(word, long_text) %>% 
    mutate(word = abjutils::rm_accent(word)) %>%
    filter(!grepl('[0-9]', word)) %>% 
    anti_join(pt_stopwords) %>% 
    count(word, sort = TRUE) %>% 
    filter(! word %in% c(others_words, my_words, .ot)) %>%
    rowid_to_column("id") %>% 
    cast_dtm(id, word, n) %>% 
    LDA(k = k, control = list(seed = 1254)) %>% 
    broom::tidy() %>% 
    group_by(topic) %>% 
    top_n(5, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
}

grafico <- function(modelo_tidy){
  
  modelo_tidy %>% 
    mutate(term = reorder_within(term, beta, topic)) %>%
    mutate(term = str_remove_all(term, "[0-9_]")) %>% 
    ggplot(aes(y = term, x = beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    theme_light()
  
}

modelagem(carne_1521) %>% 
  grafico()

modelagem(carne_vegetal) %>% 
  grafico()

modelagem(plant_based) %>% 
  grafico()

modelagem(sustent, 4, .ot = c("sustentavel", "sustentabilidade")) %>% 
  grafico()

modelagem(sustent, 6, .ot = c("sustentavel", "sustentabilidade")) %>% 
  grafico()

modelagem(sustent, 10, .ot = c("sustentavel", "sustentabilidade")) %>% 
  grafico()

sustent %>% 
  filter(str_detect(long_text, "consumo")) %>% 
  modelagem(4, .ot = c("sustentavel", "sustentabilidade",  
                       "consumo", "consumoconsciente")) %>% 
  grafico()

sustent %>% 
  filter(str_detect(long_text, "consumo")) %>% 
  modelagem(2, .ot = c("sustentavel", "sustentabilidade",  
                       "consumo", "consumoconsciente")) %>% 
  grafico()



# hamburguer do futuro ----------------------------------------------------

burguer_futuro %>% 
  bind_rows(futuro_burguer) %>% 
  graf_geral(more_stop = c("futuro", "hamburguer", "comi", "parece", "achei",
                           "burguer", "tal", "gt", "comendo", "comida", "queria",
                           "demais", "comprar", "feito"))

burguer_futuro %>% 
  bind_rows(futuro_burguer) %>% 
  modelagem() %>% 
  grafico() +
  theme(
    plot.title = element_text(size = rel(2),
                              hjust = 0.5,
                              face = "bold",
                              family = "mono",
                              color = "gray28"),
    plot.caption = element_text(color = "gray28"),
    axis.title.y = element_blank(),
    axis.text = element_text(face = "bold",
                             family = "mono",
                             size = rel(1)),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "black",
                                size = 0.8),
    axis.line.x = element_line(linetype = "solid",
                               size = 1),
    panel.border = element_blank())
  


# sentiment analysis --------------------------------------------------------
burguer_stop <- c("futuro", "hamburguer", "comi", "parece", "achei",
                  "burguer", "tal", "gt", "comendo", "comida", "queria",
                  "demais", "comprar", "feito", "so", "ser", "puta",  "dar",
                  "ter", "ver", "sabe", "amiga", "vi")

sentiment_graf <- function(df, lex_sent, .caption = "", your_stop = ""){
  
  if(lex_sent == 1){
    lex <- lexiconPT::oplexicon_v2.1
  } else if (lex_sent == 2){
    lex <- lexiconPT::oplexicon_v3.0
  } else {
    lex <- lexiconPT::sentiLex_lem_PT02
  }
   
  df %>% 
    unnest_tokens(word, long_text) %>% 
    anti_join(pt_stopwords) %>% 
    inner_join(lex, by = c("word" = "term")) %>% 
    filter(!word %in% your_stop) %>% 
    mutate(sentiment = case_when(
      polarity == 1 ~ "positivo",
      polarity == -1 ~ "negativo",
      polarity == 0 ~ "neutro"
    )) %>% 
    group_by(sentiment) %>% 
    count(word, sentiment, sort = TRUE) %>% 
    top_n(10) %>% 
    ggplot(aes(reorder(word, n), n, fill = sentiment)) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribuição para o sentimento", x = NULL,
         title = "Análise de sentimento",
         caption = .caption) +
    coord_flip() +
    theme(
      plot.title = element_text(size = rel(2),
                                hjust = 0.5,
                                face = "bold",
                                family = "mono",
                                color = "gray28"),
      plot.caption = element_text(color = "gray28"),
      axis.title.y = element_blank(),
      axis.text = element_text(face = "bold",
                               family = "mono",
                               size = rel(1)),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "black",
                                  size = 0.8),
      axis.line.x = element_line(linetype = "solid",
                                 size = 1),
      panel.border = element_blank(),
      legend.position = "none"
    )
}


b_df <- burguer_futuro %>% 
  bind_rows(futuro_burguer)

sentiment_graf(b_df, lex_sent = 2, 
               your_stop = burguer_stop)


sentiment_graf(sustent, 2)
# 
# sentiment_graf(carne_vegetal, lex_sent = 3)
# graf_geral(carne_vegetal, c("carne", "vegetal", "comi", "comprar"))
# sentiment_graf(df_0021, lex_sent = 1, your_stop = c(others_words, "dar", "vi", "precisa"))

               