import pandas as pd
import numpy as np
import string
import os

from keras.preprocessing.sequence import pad_sequences
from keras.layers import Embedding, LSTM, Dense, Dropout
from keras.preprocessing.text import Tokenizer
from keras.callbacks import EarlyStopping
from keras.models import Sequential
import keras.utils as ku


def preprocess():
    f = open("way_station_short.txt", "r")

    raw_lines = f.readlines()
    proc_line = " ".join(raw_lines).replace("\n", "").replace("\"", "").replace("...", ".").lower()
    no_white_line = " ".join(proc_line.split())
    return no_white_line.split(".")
    # [ " skdjfsdklf  sdflkjsdlkfj lsdfj", ]


def get_sequence_of_tokens(tokenizer, corpus):
    tokenizer.fit_on_texts(corpus)  # ['sdf', 'saf']
    total_words = len(tokenizer.word_index) + 1

    input_sequences = []  # [[4 ], [ 4 8 ], [ 4 8 3]]
    for line in corpus:
        token_list = tokenizer.texts_to_sequences([line])[0]
        for i in range(1, len(token_list)):
            n_gram_sequence = token_list[:i+1]
            input_sequences.append(n_gram_sequence)
    return input_sequences, total_words


def generate_padded_sequences(input_sequences, total_words):
    max_sequence_len = max([len(x) for x in input_sequences])
    # [[pre pre | 4 ], [ pre 4 | 8 ], [ 4 8 3]]
    input_sequences = np.array(pad_sequences(input_sequences,
                                             maxlen=max_sequence_len,
                                             padding='pre'))

    predictors, label = input_sequences[:, :-1], input_sequences[:, -1]
    label = ku.to_categorical(label, num_classes=total_words)
    return predictors, label, max_sequence_len


def create_model(max_sequence_len, total_words):
    input_len = max_sequence_len - 1

    model = Sequential()

    # [[4],[4 8],[4 8 3]]
    model.add(Embedding(total_words, 10, input_length=input_len))

    model.add(LSTM(100))
    model.add(Dropout(0.1))

    model.add(Dense(total_words, activation='softmax'))

    model.compile(loss='categorical_crossentropy', optimizer='adam')

    return model


def generate_text(tokenizer, seed_text, next_words, model, max_sequence_len):
    for _ in range(next_words):
        tokenizer.texts_to_sequences
        token_list = tokenizer.texts_to_sequences([seed_text])[0]
        token_list = pad_sequences([token_list],
                                   maxlen=max_sequence_len-1,
                                   padding='pre')
        predicted = model.predict_classes(token_list, verbose=0)

        output_word = ""
        for word, index in tokenizer.word_index.items(): # very dumb
            if index == predicted:
                output_word = word
                break
        seed_text += " " + output_word
    return seed_text


def main():
    corpus = preprocess()
    tokenizer = Tokenizer()
    seqs, words = get_sequence_of_tokens(tokenizer, corpus)

    predictors, label, max_sequence_len = generate_padded_sequences(seqs, words)

    model = create_model(max_sequence_len, words)
    model.summary()

    model.fit(predictors, label, epochs=30)

    print(generate_text(tokenizer, "He loved messing around with", 10, model, max_sequence_len))
    print(generate_text(tokenizer, "He went down the hill and", 8, model, max_sequence_len))
    print(generate_text(tokenizer, "Enoch Wallace fired and reloaded", 6, model, max_sequence_len))
    print(generate_text(tokenizer, "Somewhere in the distance was the sound", 6, model, max_sequence_len))
    print(generate_text(tokenizer, "The postman was not coming early today, because", 3, model, max_sequence_len))

main()
