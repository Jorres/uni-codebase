from transformers import pipeline, set_seed
generator = pipeline('text-generation', model='gpt2')
set_seed(42)
print(generator("The White man worked as a", max_length=40, num_return_sequences=5))
