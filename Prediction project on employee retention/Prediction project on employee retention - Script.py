import pandas as pd
hr_df = pd.read_csv('HR Project/HR_data.csv')

print(hr_df.info())
descriptive = hr_df.describe()

# Attrition values frequencies
hr_df['Attrition'].value_counts()
# --> 237 Yes, 1233 No

# missing values
print(hr_df.isnull().sum())

# Preprocessing
hr_df = hr_df.drop(columns = 'EmployeeNumber')
hr_df = pd.get_dummies(hr_df, columns = ['OverTime', 'BusinessTravel', 'Department', 'Education', 'EducationField', 'Gender', 'JobLevel', 'JobRole',  'MaritalStatus', 'Over18',  'StockOptionLevel'], drop_first = False)
hr_df = pd.get_dummies(hr_df, columns = ['Attrition'], drop_first = True)

# Splitting into train and test set
train = hr_df.sample(frac=0.8,random_state=200)
test = hr_df.drop(train.index)

train['Attrition_Yes'].value_counts()
test['Attrition_Yes'].value_counts()


y_train = train['Attrition_Yes']
X_train = train.drop(columns = 'Attrition_Yes')

y_test = test['Attrition_Yes']
y_test = pd.get_dummies(y_test, drop_first = True)
X_test = test.drop(columns = 'Attrition_Yes')

# Correlations
correlations = hr_df.corr()

## MinMaxScaler
from sklearn.preprocessing import MinMaxScaler
scaling = MinMaxScaler(feature_range=(0,1)).fit(X_train)
X_train = pd.DataFrame(scaling.transform(X_train), columns = X_train.columns)
X_test = pd.DataFrame(scaling.transform(X_test), columns = X_train.columns)

## Parameter tuning & Performance Evaluation
from sklearn.metrics import f1_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import plot_roc_curve
from sklearn.model_selection import GridSearchCV

# KNN
from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier()
param_grid = {'n_neighbors': [x for x in range(1, 100)], 'weights': ['uniform', 'distance']}

CV_knn = GridSearchCV(scoring = "f1", estimator = knn, param_grid=param_grid, cv= 3, n_jobs = 8)
CV_knn.fit(X_train, y_train.values.ravel())


# find best parameters
CV_knn.best_params_
CV_knn.best_estimator_

# predict
knn_final = CV_knn.best_estimator_
knn_final.fit(X_train, y_train.values.ravel())
knn_pred = knn_final.predict(X_test)
knn_f1 = f1_score(y_test, knn_pred)
knn_con = confusion_matrix(y_test, knn_pred)


## random forest
from sklearn.ensemble import RandomForestClassifier
rfc=RandomForestClassifier(random_state=42)
param_grid = {
    'n_estimators': [50, 100, 200, 500],
    'max_features': ['auto', 'sqrt', 'log2'],
    'max_depth' : [5, 10, 50, 100, 500],
    'criterion' : ['gini', 'entropy']
}

CV_rfc = GridSearchCV(scoring = "f1", estimator=rfc, param_grid=param_grid, cv= 3, n_jobs = 14)
CV_rfc.fit(X_train, y_train.values.ravel())
CV_rfc.cv_results_

# find best parameters
CV_rfc.best_params_
CV_rfc.best_estimator_

# predict
rf_final = CV_rfc.best_estimator_
rf_final.fit(X_train, y_train.values.ravel())
rf_pred = rf_final.predict(X_test)
rf_f1 = f1_score(y_test, rf_pred)
rf_con = confusion_matrix(y_test, rf_pred)


## AdaBoost
from sklearn.ensemble import AdaBoostClassifier

ada=AdaBoostClassifier(random_state=42)
param_grid = {
    'n_estimators': [100, 500, 1000, 5000],
    'learning_rate': [0.0001, 0.001, 0.01, 0.1, 0.5, 1.0],
}

CV_ada = GridSearchCV(scoring = "f1", estimator=ada, param_grid=param_grid, cv= 3, n_jobs = 14)
CV_ada.fit(X_train, y_train.values.ravel())
CV_ada.cv_results_

# find best parameters
CV_ada.best_params_
CV_ada.best_estimator_

# predict
ada_final = CV_ada.best_estimator_
ada_final.fit(X_train, y_train.values.ravel())
ada_pred = ada_final.predict(X_test)
ada_f1 = f1_score(y_test, ada_pred)
ada_con = confusion_matrix(y_test, ada_pred)

## SVM
from sklearn.svm import SVC

svm=SVC()
param_grid = {'C': [0.1,1, 10, 100], 'gamma': [1,0.1,0.01,0.001],
              'kernel': ['rbf', 'poly', 'sigmoid']}

CV_svm = GridSearchCV(scoring = "f1", estimator=svm, param_grid=param_grid, cv= 3, n_jobs = 8, verbose = True)
CV_svm.fit(X_train, y_train.values.ravel())
CV_svm.cv_results_

# find best parameters
CV_svm.best_params_
CV_svm.best_estimator_

# predict
svm_final = CV_svm.best_estimator_
svm_final.fit(X_train, y_train.values.ravel())
svm_pred = svm_final.predict(X_test)
svm_f1 = f1_score(y_test, svm_pred)
svm_con = confusion_matrix(y_test, svm_pred)


## Logistic Regression
from sklearn.linear_model import LogisticRegressionCV
reg = LogisticRegressionCV(random_state=42, max_iter = 4000)
param_grid = {'class_weight': ['None', 'balanced']}
CV_reg = GridSearchCV(scoring = "f1", estimator=reg, param_grid=param_grid, cv= 3, n_jobs = 8)
CV_reg.fit(X_train, y_train.values.ravel())

# predict
reg_final = CV_reg.best_estimator_
reg_final.fit(X_train, y_train.values.ravel())
reg_pred = reg_final.predict(X_test)
reg_f1 = f1_score(y_test, reg_pred)
reg_con = confusion_matrix(y_test, reg_pred)


## Visualize Results
from sklearn.metrics import plot_confusion_matrix
classifiers = [knn_final,
               rf_final,
               ada_final,
               svm_final,
               reg_final]
fig, axes = plt.subplots(nrows=2, ncols=3, figsize=(15,10))

for cls, ax in zip(classifiers, axes.flatten()):
    plot_confusion_matrix(cls,
                          X_test,
                          y_test,
                          ax=ax,
                          cmap='Blues',
                         display_labels= ['No', 'Yes'])
    ax.title.set_text(type(cls).__name__)
fig.delaxes(axes[1][2])
plt.tight_layout()
plt.show()

fig, axes = plt.subplots(nrows=2, ncols=3, figsize=(15,10))
plot_confusion_matrix(knn_final, X_test, y_test, cmap='Blues', ax=axes.flatten())
plot_confusion_matrix(rf_final, X_test, y_test, cmap='Blues', ax=axes.flatten())
plot_confusion_matrix(ada_final, X_test, y_test, cmap='Blues', ax=axes.flatten())
plot_confusion_matrix(svm_final, X_test, y_test, cmap='Blues', aax=axes.flatten())
plot_confusion_matrix(reg_final, X_test, y_test, cmap='Blues', ax=axes.flatten())
plt.tight_layout()
plt.show()

model_results = {'knn': knn_f1, 'rf': rf_f1, 'ada': ada_f1, 'svm': svm_f1, 'log_reg': reg_f1}

keys = model_results.keys()
values = model_results.values()
import matplotlib.pyplot as plt
plt.bar(keys, values)
plt.ylabel('f1 score')
plt.title('Model Performances')
plt.show()

ax = plt.gca()
knn_roc = plot_roc_curve(knn_final, X_test, y_test, ax=ax, alpha=0.8)
rf_roc = plot_roc_curve(rf_final, X_test, y_test, ax=ax, alpha=0.8)
ada_roc = plot_roc_curve(ada_final, X_test, y_test, ax=ax, alpha=0.8)
svm_roc = plot_roc_curve(svm_final, X_test, y_test, ax=ax, alpha=0.8)
logreg_roc = plot_roc_curve(reg_final, X_test, y_test, ax=ax, alpha=0.8)
plt.show()

# Feature Importance of AdaBoost
import numpy as np
importances = CV_ada.best_estimator_.feature_importances_
features = X_train.columns
indices = np.argsort(importances)
num_features = 10

plt.figure()
plt.title('Feature Importances of AdaBoost')
plt.barh(range(num_features), importances[indices[-num_features:]], color='b', align='center')
plt.yticks(range(num_features), [features[i] for i in indices[-num_features:]])
plt.xlabel('Relative Importance')
plt.tight_layout()
plt.show()

# Feature Importance Logistic Regression
final_reg = CV_reg.best_estimator_
coef_times_std = final_reg.coef_[0] * np.std(X_train, 0)
features = X_train.columns
indices = np.argsort(coef_times_std)
num_features = 10

plt.figure()
plt.title('Feature Importance of Logistic Regression')
plt.barh(range(num_features), coef_times_std[indices[-num_features:]], color='b', align='center')
plt.yticks(range(num_features), [features[i] for i in indices[-num_features:]])
plt.xlabel('Coef * Std.dev')
plt.tight_layout()
plt.show()

