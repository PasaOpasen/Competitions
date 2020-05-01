# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.colors import ListedColormap
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.datasets import make_moons, make_circles, make_classification
from sklearn.neural_network import MLPClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.svm import LinearSVC
from sklearn.gaussian_process import GaussianProcessClassifier
from sklearn.gaussian_process.kernels import RBF
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from sklearn.metrics import classification_report
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import SGDClassifier
from sklearn import linear_model
import sklearn



names = [
  #  "Nearest Neighbors", 
  #      "Linear SVM", 
  #      "RBF SVM", 
  # !!!     'poly svm',
  #      'sigmoid svm',
       # 'sgd',
       #!!! "Decision Tree", 
        # "Random Forest", 
       # !!! "Neural Net", 
       #  "AdaBoost",
        # "Naive Bayes gau",

         "QDA",
         'logreg',
         'ridge',
         'boost'
         ]


classifiers = [
  #  KNeighborsClassifier(5),
  #  LinearSVC(C=1,verbose=1),
  #  SVC(gamma=2, C=1,verbose=True),
  #  SVC(kernel='poly'),
  #  SVC(kernel='sigmoid'),
  #  SGDClassifier(),
  #  DecisionTreeClassifier(max_depth=8),
  #  RandomForestClassifier(max_depth=5, n_estimators=10, max_features=1,verbose=True),
  #  MLPClassifier(alpha=1, max_iter=1000,verbose=True),
  # AdaBoostClassifier(),
  #  GaussianNB(),

  #!  QuadraticDiscriminantAnalysis() ,
  #!  LogisticRegression(),
    linear_model.RidgeClassifier(alpha=0.1),
  #!  sklearn.ensemble.GradientBoostingClassifier()
    ]




dt=pd.read_csv('train for py.csv')

dt['open_channels']=dt['open_channels'].astype('category')
dt['batches']=dt['batches'].astype('category')

X = dt.select_dtypes(include=['float64'])

y=dt['open_channels']


tr=StandardScaler()

X = tr.fit_transform(X)


X_train, X_test, y_train, y_test =  train_test_split(X, y, test_size=.02, train_size=.01, shuffle = True )



for name, clf in zip(names, classifiers):
    print('Now: {}'.format(name))
    clf.fit(X_train, y_train)
    score = clf.score(X_test, y_test)
    #f1 = f1_score(y_test, clf.predict(X_test))
    print(classification_report(y_test, clf.predict(X_test), digits = 7))
    print("model = {}  score = {}".format(name,score))





from sklearn.model_selection import GridSearchCV

params = [{
    'kernel':["poly"],
    'C':[0.1],
    'degree':[3,4,5],
    'coef0':[0,0.01,0.1,0.05,1]
           }]
clf= SVC()

gr= GridSearchCV(clf,params,cv=5,verbose=1)

gr.fit(X_train, y_train)






clf = SVC(C=0.1, kernel = "poly", degree=3, coef0=1)
clf.fit(X, y)




clf = MLPClassifier(alpha=0.1, max_iter=100, verbose=True)

clf = QuadraticDiscriminantAnalysis() 

clf.fit(X_train, y_train)
score = clf.score(X_test, y_test)
print(classification_report(y_test, clf.predict(X_test), digits = 7))
print("score = {}".format(score))


#for lay in [(100,),(50,50,)]:

als=[0,0.00001,0.0001,0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5,1,2]
#als.reverse()

for al in als:
    clf = MLPClassifier(alpha=al, max_iter=100, verbose=True)

    clf.fit(X_train, y_train)
    score = clf.score(X_test, y_test)
    print(classification_report(y_test, clf.predict(X_test), digits = 7))
    print("alpha = {}   score = {}".format(al,score))



T= pd.read_csv('test for py.csv')
T = tr.transform(T)


ft=MLPClassifier(alpha=0, max_iter=100, verbose=True)
ft.fit(X, y)

#ft.score(X,y)

predictions= ft.predict(T)

pd.DataFrame(predictions).to_csv('best py result.csv')

















