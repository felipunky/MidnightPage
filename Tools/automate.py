import requests

url = 'https://2f9ed3-3.myshopify.com/admin/api/2024-01/products.json'
headers = {'Content-Type': 'application/json', 'X-Shopify-Access-Token': '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'}
r = requests.get(url, headers=headers)

print(r.text)