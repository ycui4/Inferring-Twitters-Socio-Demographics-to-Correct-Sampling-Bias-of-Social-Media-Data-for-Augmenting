import nltk
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
import numpy as np
import random
import pickle
from collections import Counter

lemmatizer = WordNetLemmatizer()
hm_lines = 10000

def create_lexicon(age30, age3045, age45):
    lexicon = []
    for fi in [age30, age3045, age45]:
        with open(fi, 'r') as f:
            contents = f.readlines()
            for l in contents[:hm_lines]:
                all_words = word_tokenize(l.lower())
                lexicon += list(all_words)

    lexicon = [lemmatizer.lemmatize(i) for i in lexicon]
    word_counts = Counter(lexicon)


    l2 = []
    for w in word_counts:
        if 10000 > word_counts[w] > 1000:
           l2.append(w)
    return l2


def sample_handling(sample, lexicon, classification):
    featureset = []

    with open(sample, 'r') as f:
        contents = f.readlines()
        for l in contents[:hm_lines]:
            current_words = word_tokenize(l.lower())
            current_words = [lemmatizer.lemmatize(i) for i in current_words]
            features = np.zeros(len(lexicon))
            for word in current_words:
                if word.lower() in lexicon:
                    index_value = lexicon.index(word.lower())
                    features[index_value] += 1
            features = list(features)
            featureset.append([features, classification])

    return featureset

def creat_featuresets_and_labels(age30, age3045, age45, text_size=0.1):
    lexicon = create_lexicon(age30, age3045, age45)
    features = []
    features += sample_handling('Age30Text.txt', lexicon, [1, 0, 0])
    features += sample_handling('Age3045Text.txt', lexicon, [0, 1, 0])
    features += sample_handling('Age45Text.txt', lexicon, [0, 0, 1])
    random.shuffle(features)

    features = np.array(features)

    testing_size = int(text_size * len(features))
    train_x = list(features[:, 0][:-testing_size])
    train_y = list(features[:, 1][:-testing_size])

    test_x = list(features[:, 0][-testing_size:])
    test_y = list(features[:, 1][-testing_size:])

    return train_x, train_y, test_x, test_y


if __name__ == "__main__":
    train_x, train_y, text_x, text_y = creat_featuresets_and_labels('Age30Text.txt', 'Age3045Text.txt', 'Age45Text.txt')
    with open('sentiment_set.pickle', 'wb') as f:
        pickle.dump([train_x, train_y, text_x, text_y], f)












