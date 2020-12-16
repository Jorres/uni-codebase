import sys
import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt
from tensorflow.keras import layers

CLASSES = 10

def main():
    fashion_mnist = tf.keras.datasets.fashion_mnist
    (pre_train_images, train_labels), (pre_test_images, test_labels) = fashion_mnist.load_data()

    pre_train_images = pre_train_images / 255
    pre_test_images = pre_test_images / 255

    print(pre_train_images.shape)
    train_images = pre_train_images.reshape(60000, 28, 28, 1)
    test_images = pre_test_images.reshape(10000, 28, 28, 1)

    model = tf.keras.Sequential()
    # tf.keras.layers
    
    # f(.+. 
    #   .+.)

    #                           ------  ----------------   
    model.add(layers.Conv2D(32, (2, 2), activation='relu', input_shape=(28, 28, 1)))
    # (27 * 27 * 32)
    model.add(layers.MaxPooling2D((2, 2)))

    # (14 * 14 * 32)
    #  . .|x .
    #  x .|. .
    # ----+---- 
    #  . x|. .
    #  . .|x .

    model.add(layers.Conv2D(48, (2, 2), activation='relu'))
    # (13 * 13 * 48)
    model.add(layers.MaxPooling2D((2, 2)))
    # (7 * 7 * 48)
    model.add(layers.Conv2D(64, (2, 2), activation='relu'))
    # (6 * 6 * 64)

    # model.summary()
    # sys.exit()

    model.add(layers.Flatten()) # (6, 6, 64) -> (6 * 6 * 64)
    model.add(layers.Dense(128, activation='relu'))
    # model.add(layers.Dense(64, activation='sigmoid'))
    model.add(layers.Dense(10, activation='relu'))
    model.add(layers.Softmax())

    model.compile(optimizer='adam',
                  loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
                  metrics=['accuracy'])

    history = model.fit(train_images, train_labels, epochs=10)

    # plt.plot(history.history['acc'], label='accuracy')
    # plt.xlabel('Epoch')
    # plt.ylabel('Accuracy')
    # plt.ylim([0.5, 1])
    # plt.legend(loc='lower right')
    # plt.show()

    conf_matrix = [[0] * CLASSES for _ in range(CLASSES)]

    most_similar = [[[-1, 0] for _ in range(CLASSES)] for _ in range(CLASSES)]
    # [i, j] - (img, prob) - real class i, predicted class j
    #           NUM  MAX

    img_num = 0
    correct = 0
    total = 0

    predictions = model.predict(test_images) # [[] X 10]
    for (img, lab, prediction) in zip(test_images, test_labels, predictions):
        maxPred = np.argmax(prediction)
        conf_matrix[lab][maxPred] += 1
        if lab == maxPred:
            correct += 1
        total += 1
        for i in range(CLASSES):
            if most_similar[lab][i][1] < prediction[i]:
                most_similar[lab][i][0] = img_num
                most_similar[lab][i][1] = prediction[i]

        img_num += 1
    print("\n Accuracy: ", correct / total)

    for row in conf_matrix: 
        print(row)

    for row in most_similar: 
        print(row)

main()
