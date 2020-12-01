#!/usr/bin/env python
# coding: utf-8

# # Library Import and Data Loading

# In[2]:


import matrixprofile as mp


# In[3]:


import warnings


# In[4]:


from matplotlib import pyplot as plt


# In[9]:


get_ipython().run_line_magic('matplotlib', 'inline')


# In[6]:


dataset = mp.datasets.load('nyc-taxi-anomalies.csv')


# In[13]:


str(dataset)


# # Visualize Data

# In[25]:


plt.figure(figsize=(15,3))
plt.plot(dataset['datetime'], dataset['data'])
plt.title('NYC Taxi Passager Count')
plt.ylabel('Passanger Count')
plt.xlabel('Datetime')
plt.tight_layout()
#plt.savefig('fig.png', bbox_inches='tight')
plt.show()


# In[50]:


window_size = 48 # 30min intervals > To find daily events
profile = mp.compute(dataset['data'], windows=window_size)
profile = mp.discover.discords(profile, k = 5)

mp.visualize(profile)
plt.show()


# In[45]:


for dt in dataset['datetime'][profile['discords']]:
    print(dt)


# # Discord Discovery Tuning

# In[46]:


profile = mp.discover.discords(profile, exclusion_zone=window_size, k = 10)


# In[47]:


from matrixprofile.visualize import plot_discords_mp


# In[48]:


plot_discords_mp(profile)
plt.show()


# In[43]:


for dt in dataset['datetime'][profile['discords']]:
    print(dt)


# ### 2015-01-27: Snowstorm
# ### 2014-11-03: New York marathon
# ### 2015-01-25: Snowstorm
# ### 2014-12-31: Happy New Year
# ### 2014-12-24: Christmas eve
# ### 2015-01-18: Black ice-related accidents
# ### 2014-07-04: Independence day
# ### 2014-11-26: Protesting Ferguson Decision Block Traffic

# In[ ]:




