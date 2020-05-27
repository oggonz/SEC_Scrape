
FIND_WORDS = ['covid',
              'guidance',
              'outlook']

LOGFILE = 'sec_nlp_beta.log'
f = open(LOGFILE, "w")
t = time.localtime()
current_time = time.strftime("%H:%M:%S", t)
f.write(current_time+": process started")
f.close()

def check_if_list_found_in_text(text, words=[], return_offset=False, lower_text=True):
    result = []
    text = (
        " "
        + text.replace("_", " ")
        .replace("-", " ")
        .replace(",", " ")
        .replace(";", " ")
        .replace('"', " ")
        .replace(":", " ")
        .replace(".", " ")
        + " "
    )
    if lower_text:
        text = text.lower()
    for word in words:
        word = (
            " "
            + word.replace("_", " ")
            .replace("-", " ")
            .replace(",", " ")
            .replace(";", " ")
            .replace('"', " ")
            .replace(":", " ")
            .replace(".", " ")
            + " "
        )
        if lower_text:
            word = word.lower()
        if word in text:
            if return_offset:
                offset = text.find(word)
                # offset = offset if not offset else offset-1
                result.append(offset)
            else:
                result.append(word.strip())
    return result

def filter_stopwords(sent):
    stop_words = set(stopwords.words('english'))
    word_tokens = word_tokenize(sent)
    filtered_sentence = [w for w in word_tokens if not w in stop_words]
    filtered_sentence = []
    for w in word_tokens:
        if w not in stop_words:
            filtered_sentence.append(w)
    return ' '.join(filtered_sentence)

def sentiment_from_text(sentence):
  sentence = filter_stopwords(sentence)
  list_found = check_if_list_found_in_text(sentence,FIND_WORDS)
  num_found = len(list_found)

  ss = sid.polarity_scores(sentence) #NLTK
  df = pd.DataFrame.from_dict(ss,orient = "index").T
  df['transformers_score'] = dict_transformers['score'] #tranformers
  df['transformers_label'] = dict_transformers['label']
  df['text'] = sentence
  df['keywords_found'] = num_found
  return pd.concat(dict_sentiment)

def filter_stopwords(sent):
  stop_words = set(stopwords.words('english'))
  word_tokens = word_tokenize(sent)
  filtered_sentence = [w for w in word_tokens if not w in stop_words]
  filtered_sentence = []
  for w in word_tokens:
      if w not in stop_words:
          filtered_sentence.append(w)
  return ' '.join(filtered_sentence)

def df_from_text(text):
  sentence_list = tokenize.sent_tokenize(text)
  sentence_list
  sid = SentimentIntensityAnalyzer()
  list_df = []
  for sentence in sentence_list:
      #
      # YOUR CODE HERE TO SCORE EACH sentence
      #
      sentence = filter_stopwords(sentence)
      list_found = check_if_list_found_in_text(sentence,FIND_WORDS)
      num_found = len(list_found)
      ss = sid.polarity_scores(sentence)
      df = pd.DataFrame.from_dict(ss,orient = "index").T
      df['text'] = sentence
      df['keywords_found'] = num_found
      list_df.append(df)
      return pd.concat(list_df)

def py_write_log(str_text):
    t = time.localtime()
    current_time = time.strftime("%H:%M:%S", t)
    print(str_text)
    f = open(LOGFILE, "a")
    f.write(current_time+": "+str_text)
    f.close()
    return

def func_sentiment(row):
    df = df_from_text(row['text']) #neg neu pos compound text keywords_found
    neu = df.iloc[0]['neu']
    pos = df.iloc[0]['pos']
    neg = df.iloc[0]['neg']
    num_rows = 1
    compound = df.iloc[0]['compound']
    text = df.iloc[0]['text']
    keywords_found = df.iloc[0]['keywords_found']
    return pd.Series([row['ticker'],row['section'],row['type'],row['period_date'],neu,pos,neg,compound,keywords_found,text,num_rows])
