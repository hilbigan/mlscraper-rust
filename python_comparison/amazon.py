import requests
from mlscraper.html import Page
from mlscraper.samples import Sample, TrainingSet
from mlscraper.training import train_scraper
import time

amazon_galaxy = None
with open("./amazon_galaxy.html", "r") as f:
    amazon_galaxy = f.read()

amazon_iphone = None
with open("./amazon_iphone.html", "r") as f:
    amazon_iphone = f.read()

start_train = time.time()
training_set = TrainingSet()

sample = Sample(Page(amazon_galaxy), {
    'product': 'Samsung Galaxy S21 5G, US Version, 128GB, Phantom Gray - Unlocked (Renewed)',
    'price': '$244.95'
    #'price': 'Add to Cart'
})
training_set.add_sample(sample)

sample = Sample(Page(amazon_iphone), {
    'product': 'Apple iPhone 11, 64GB, Black - Unlocked (Renewed)',
    'price': '$293.00'
    #'price': 'Add to Cart'
})
training_set.add_sample(sample)

scraper = train_scraper(training_set)
print("Training time:", time.time() - start_train)
print(scraper)
