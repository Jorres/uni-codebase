from scipy.sparse import dok_matrix
import random
from random import random

import pandas as pd
import numpy as np


def preprocess():
    f = open("way_station.txt", "r")

    raw_lines = f.readlines()
    proc_line = " ".join(raw_lines).replace("\n", "").replace("\"", "").replace("...", ".").lower()
    corpus = " ".join(proc_line.split())
    f.close()

    for spaced in ['.', '-', ',', '!', '?', '(', '—', ')']:
        corpus = corpus.replace(spaced, ' {0} '.format(spaced))

    return corpus

def init_sparse_matrix(corpus_words, word_idx_dict, distinct_words):
    k = 3
    sets_of_k_words = [' '.join(corpus_words[i:i+k]) for i, _ in enumerate(corpus_words[:-k])]

    sets_count = len(list(set(sets_of_k_words)))
    next_after_k_words_matrix = dok_matrix((sets_count, len(distinct_words)))

    distinct_sets_of_k_words = list(set(sets_of_k_words))
    k_words_idx_dict = {word: i for i, word in enumerate(distinct_sets_of_k_words)}

    for i, word in enumerate(sets_of_k_words[:-k]):
        word_sequence_idx = k_words_idx_dict[word]
        next_word_idx = word_idx_dict[corpus_words[i+k]]
        next_after_k_words_matrix[word_sequence_idx, next_word_idx] += 1

    return (next_after_k_words_matrix, k_words_idx_dict)


def weighted_choice(objects, weights):
    weights = np.array(weights, dtype=np.float64)
    sum_of_weights = weights.sum()
    np.multiply(weights, 1 / sum_of_weights, weights)
    weights = weights.cumsum()
    x = random()
    for i in range(len(weights)):
        if x < weights[i]:
            return objects[i]


def sample_next_word_after_sequence(next_after_k_words_matrix, k_words_idx_dict, distinct_words, word_sequence):
    next_word_vector = next_after_k_words_matrix[k_words_idx_dict[word_sequence]]
    likelihoods = next_word_vector/next_word_vector.sum()

    return weighted_choice(distinct_words, likelihoods.toarray())


def stochastic_chain(seed, next_after_k_words_matrix, k_words_idx_dict, distinct_words, chain_length=15, seed_length=3):
    current_words = seed.split(' ')
    sentence = seed

    for _ in range(chain_length):
        sentence += ' '
        next_word = sample_next_word_after_sequence(next_after_k_words_matrix, k_words_idx_dict, distinct_words, ' '.join(current_words))
        sentence += next_word
        current_words = current_words[1:] + [next_word]
    return sentence


def main():
    corpus = preprocess()
    corpus_words = corpus.split(' ')
    corpus_words = [word for word in corpus_words if word != '']
    print(len(corpus_words))

    distinct_words = list(set(corpus_words))
    word_idx_dict = {word: i for i, word in enumerate(distinct_words)}
    distinct_words_count = len(list(set(corpus_words)))
    print(distinct_words_count)

    (next_k, idx_dict) = init_sparse_matrix(corpus_words, word_idx_dict, distinct_words)

    print(stochastic_chain('the earth was', next_k, idx_dict, distinct_words, chain_length=100))


main()
