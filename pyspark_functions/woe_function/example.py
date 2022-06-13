'''
arguments:
      df: pyspark dataframe to ttransform
      column_to_transform: categorical variable to calculate the WOE's
      label: label column of the data
'''

woe = WOE_IV(df,['column_to_transform'],'label',1.0) #1.0 is the 'sucess' for the 'label'
woe.fit()
tdc_woe = woe.transform(df) 
