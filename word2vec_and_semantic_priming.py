# ------------------------------------------------------------------------------
# using W2V to predict priming in lexical decision  
# T. Wu
# ------------------------------------------------------------------------------
import time
import os
import tempfile
# part of problem fixing
import glob
import pandas as pd
#import numpy as np
import gensim
from gensim.models import Word2Vec

# ------------------------------------------------------------------------------
# Get models running
# ------------------------------------------------------------------------------
# for API, check https://radimrehurek.com/gensim/models/word2vec.html#module-gensim.models.word2vec
# function for model training using ENCOW corpus,
# generates the dictionary of word vectors and the cosine similarity values,
# attached to the original dataset from Hutchison et al.(2013)
# new dataset can be found in data_[model_name].csv
# a log file including parameter settings for the current model can be found in
# info_[model_name].txt
# parameters are of default values and can be later specified:
# sg, 0 for CBOW, 1 for Skip-gram
# size, size of hidden layer
# window, Maximum distance between the current and predicted word within a sentence.
# Note: min_count and iter can be adjusted for faster/slower training speed

# load the data, in case the original file is altered
df_lexdec = pd.read_csv('data_lexdec.csv')


def tweak_data(model_name="default", model_type=0, layer_size=100, n_win=5):
    t1 = time.time()
    # import data from Hutchison et al.(2013);
    # isi: the Inter Stimulus Interval
    # target: the target word(non-words excluded)
    # condition: semantic relation between prime and target,
    # 'weak', 'strong', 'unrel_weak', 'unrel_strong'
    # meanRT: by participants on correct trials
    df = df_lexdec
    # run model with adjustable parameters
    # min_count, Ignores all words with total frequency lower than this
    # iter, Number of iterations (epochs) over the corpus.
    # workers, working in parallels, can be adjusted by devices
	# original corpus text was used (with minor adjustments),
	# without extra preprocessing, e.g stoplist, tokenization,
	# though considering the size of corpus, 
	# tokenizing the text may lead to cosine similarities 
	# that can better(accurately) capture words' characteristics 
	# and better(accurately) represent the relation between two words
    model = Word2Vec(corpus_file="train_en.txt", sg=model_type,
                     size=layer_size, window=n_win, min_count=5, iter=5, workers=2)
    t2 = time.time()
    # get model running time
    timestamp = round((t2 - t1) / 60, 2)
    # save the model to folder log
    with tempfile.NamedTemporaryFile(prefix=f'gensim-model-{model_name}-',
                                     dir="./log", delete=False) as tmp:
        temporary_filepath = tmp.name
        model.save(temporary_filepath)
    # get a list of vocabularies from the trained model
    vocab = list(model.wv.vocab)
    # add columns to panda dataframe
    # cosine, word similarities,
    # in the range <-1, 1> (the greater, the more similar)
    df["cosine"] = ""
    # exist, whether the primes and the targets is in the original dataset
    # 1 for yes, 0 for no
    df["exist"] = ""
    # tag current model, for later comparison
    df["model"] = model_name
    df["type"] = model_type
    df["size"] = layer_size
    df["win"] = n_win
    # get a loop to add value by rows
    for index, row in df.iterrows():
        # there creates the problem of retrieving vectors for FALSE and TRUE,
        # which somehow recognized as boolean when data was imported
        # can be fixed by lower()
        # if row["prime"].lower() in vocab and row["target"].lower() in vocab:
        if row["prime"] in vocab and row["target"] in vocab:
            df.at[index, "exist"] = 1
            # IMP: model.wv.similarity(row["prime"].lower(), row["target"].lower())
            df.at[index, "cosine"] = model.wv.similarity(row["prime"], row["target"])
        else:
            df.at[index, "exist"] = 0
    # save the data to a .csv file for analysis, named after model_name
    df.to_csv(os.path.join('./log', f"data_{model_name}.csv"), index=False)
    # get model settings to a list, and save as a .txt file
    infolist = [f"time: {timestamp}", f"tag: {model_name} ",
                f"type: {model_type}", f"size: {layer_size}",
                f"window: {n_win}", "count: 5", "iter: 5"]
    with open(os.path.join('./log', f"info_{model_name}.txt"), "w") as outfile:
        outfile.write("\n".join(infolist))
    # show the timestamp
    print(f"time taken in mins: {timestamp}")
    return df


# to use the function tweak_data()
# call [variable] = tweak_data([parameters])
# leave [parameters] blank, accept the default settings, e.g. m_default
# important the change the model_name for each run, by default, it's "default"
# using model_type = 1 to change it into Skip-gram,
# other settings can be adjusted likewise
# data_default = tweak_data(model_name="try300", model_type=0, layer_size=300, n_win=5)

# get a list of vocabulary from model
# model = gensim.models.Word2Vec.load("./log/gensim-model-try300-5lssyrt1")
# vocabulary = list(model.wv.vocab)


# set function to train models by sets of parameters
def all_night():
    parameter_settings = pd.read_csv("parameter_settings_temp.csv")
    for index, row in parameter_settings.iterrows():
        tweak_data(model_name=row["m_name"],
                   model_type=row["m_type"],
                   layer_size=row["m_size"],
                   n_win=row["m_win"])


all_night()

# get sentence length
# with open("train_en.txt", 'r') as file:
#    sent_len = sorted([len(sent.split()) for sent in file])
# write to a dataframe
# df_sent_len = pd.DataFrame(sent_len, columns=['length'])
# df_sent_len.to_csv("sent_length.csv", index=False)


# ------------------------------------------------------------------------------
# deal with missing values - TRUE or FALSE
# ------------------------------------------------------------------------------
# TRUE and FALSE in the lexdec.csv are recognized as boolean,
# may be caused by the coding status of .csv file, e.g. not UTF-8
# thus the vector of which cannot be retrieve,
# this could be fixed in the original code, with lower()
# and the following codes are the effort of fixing this problem, afterwards

# get a list of name/dir for the data files
df_files = glob.glob("./log/*.csv")
# get a list of name/dir for the model
df_model = glob.glob("./log/gensim-model-*")

# for each data file, get the corresponding model,
# to extract the missing cosine similarity for prime and target pairs
# containing either TRUE and FALSE
for d in df_files:
    # import the dataframe
    df_temp = pd.read_csv(d)
    # import the correct model
    m_temp = gensim.models.Word2Vec.load(df_model[df_files.index(d)])
    # change the name to save the altered data file
    name = d.strip("./log\data_")
    # little check on the matching between the model
    # and data-frame that need to be rewritten a bit
    print(name, df_model[df_files.index(d)])
    # get the vocabulary list of that model
    v = list(m_temp.wv.vocab)
    # check every prime-target pair which do not have the cosine similarity
    # try to retrieve one for it
    for i, r in df_temp.iterrows():
        # the key is to use the lowercase of prime and target
        # to search in the model vocabulary list
        if r["exist"] == 0 and \
                r["prime"].lower() in v and r["target"].lower() in v:
            # rewrite the value, IMP: lower()
            df_temp.at[i, "cosine"] = m_temp.wv.similarity(r["prime"].lower(),
                                                           r["target"].lower())
            # rewrite the existence statue of cosine similarity
            df_temp.at[i, "exist"] = 1
            # save the data file to the new folder, fixed
            df_temp.to_csv(os.path.join('./log/fixed/', f"data_{name}"), index=False)
