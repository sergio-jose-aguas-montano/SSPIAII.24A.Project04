# -*- coding: utf-8 -*-
"""
Created on Thu Apr 11 14:27:26 2024

@author: Oscar Fabian Barba Gomez
Kevin Ruvalcaba Padilla
"""
#import tensorflow as tf
#print(tf.__version__)

#Librerías
#pandas
import pandas as pd
#scikit-learn
from sklearn.preprocessing import LabelEncoder, OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
#Correlación
import seaborn as sns
#Matriz de confusión
from sklearn.metrics import confusion_matrix
#Modelo ANN
from keras.models import Sequential
from keras.layers import Dense
#Guardar el modelo
from keras.models import load_model
#Visualizar la arquitectura de la ANN
from keras.utils import plot_model

#Dataset
df_venta = pd.read_csv('Atencion a cliente audioferre.csv')

#Selección de variables
X = df_venta.iloc[:, 1:11].values
Y = df_venta.iloc[:, 11].values

#Preprocesamiento

#Dummies
X[:, 0] = LabelEncoder().fit_transform(X[:, 0])
X[:, 6] = LabelEncoder().fit_transform(X[:, 6])
X[:, 7] = LabelEncoder().fit_transform(X[:, 7])
X[:, 8] = LabelEncoder().fit_transform(X[:, 8])
X[:, 9] = LabelEncoder().fit_transform(X[:, 9])

Y= LabelEncoder().fit_transform(Y)



one = ColumnTransformer(
    [('one_hot_encoder', OneHotEncoder(categories='auto'), [1])],
    remainder='passthrough'
)

X = one.fit_transform(X)
X = X[:, 1:]

#Escalado
X = StandardScaler().fit_transform(X)

#Split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y,
                                                    test_size=0.2,
                                                    random_state=10)



ann = Sequential()

#Capa de entrada
ann.add(
    Dense(units=6,  #Variables/2
          kernel_initializer='uniform',
          input_dim=15)
)

#Capas ocultas
ann.add(
    Dense(units=10,
          kernel_initializer='uniform',
          activation='relu')
)

ann.add(
    Dense(units=10,
          kernel_initializer='uniform',
          activation='relu')
)

#Capa de salida
ann.add(
    Dense(units=1,
          activation='sigmoid',
          kernel_initializer='uniform')
)

#Entrenador
ann.compile(optimizer='adam',
            loss='binary_crossentropy',
            metrics=['accuracy'])

ann.fit(X_train, Y_train,
        epochs=100,
        batch_size=50)



ann.save('S22.ann_class.h5')

#Cargar modelo
modelo = load_model('S22.ann_class.h5')

#Predicción
Y_pred = modelo.predict(X_test)
Y_pred = (Y_pred > 0.5)



plot_model(modelo,
           to_file='modelo.ann_class.png',
           show_shapes=True,
           show_layer_activations=True,
           show_layer_names=True)



cm = confusion_matrix(Y_test, Y_pred)

print("Confusion Matrix")
print(cm)




