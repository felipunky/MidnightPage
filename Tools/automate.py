import requests
import json
import os

url = 'https://2f9ed3-3.myshopify.com/admin/api/2024-01/'
token = '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'

def getDictFromRequest(request, headers):
    # Send GET request.
    r = requests.get(request, headers=headers)
    # Get JSON
    rawJSON = r.json()
    formattedString = json.dumps(rawJSON, indent=4)
    print(formattedString)
    # Get Dictionary
    jsonObj = json.loads(formattedString)
    return jsonObj

def getEditedImagesSource(url, productID, token):
    request = url + 'products/' + productID + '/images.json'
    headers = {'Content-Type': 'application/json', 'X-Shopify-Access-Token': token}
    jsonObj = getDictFromRequest(request, headers)
    getImages = jsonObj['images']
    getImagesSrc = [x['src'] for x in getImages]
    getEditedImagesSrc = []
    for i in getImagesSrc:
        if "_Edited" in i:
            getEditedImagesSrc.append(i)
    print(getEditedImagesSrc)
    return getEditedImagesSrc

def getJSON(productCategory, token):
    request = url + 'products.json?'
    headers = {'Content-Type': 'application/json', 'X-Shopify-Access-Token': token}
    jsonObj = getDictFromRequest(request, headers)
    getProducts = jsonObj['products']
    #print("Data type of getProducts: " + str(type(getProducts)))
    print("Number of products: " + str(len(getProducts)))
    jsonDumpList = []
    for getProduct in getProducts:
        print("Data type of getProduct: " + str(type(getProduct)))
        productType = getProduct['product_type']
        print("Product type: ", productType)
        if productType == productCategory:
            title = getProduct['title']
            #print("Title: " + title)
            getVariants = getProduct['variants']
            #print("Product type: " + productType)
            #print("Variants size: " + str(len(getVariants)))
            availableItems = 0
            for i in getVariants:
                if i['inventory_quantity'] > 0:
                    availableItems += 1
            if availableItems > 0:
                #print("There are available items!")
                productID = str(i['product_id'])
                #print("Product ID: " + productID)
                getImages = getProduct['images']
                getImagesSrc = [x['src'] for x in getImages]
                getEditedImagesSrc = []
                for i in getImagesSrc:
                    if "_Edited" in i:
                        getEditedImagesSrc.append(i)
                assert(len(getEditedImagesSrc) == 2)
                if len(getEditedImagesSrc) == 2:
                    tempEditedImagesSrc = getEditedImagesSrc[0]
                    if "_Edited_Lazy" not in tempEditedImagesSrc:
                        getEditedImagesSrc[0] = getEditedImagesSrc[1]
                        getEditedImagesSrc[1] = tempEditedImagesSrc
                #else:
                    #print("FUCKED UP in " + title)
                jsonObject = {
                    "title": title,
                    "productType": productType,
                    "productID": productID,
                    "imgMin": getEditedImagesSrc[0],
                    "img": getEditedImagesSrc[1]
                }
                jsonDumpList.append(jsonObject)
                #print(getEditedImagesSrc)
            #else:
                #print("No available items!")
            #print("\n")
        #else:
            #print("Not product type!")
    #jsonDumpString = json.dumps(jsonDumpList, indent=4)
    #print(jsonDumpString)
    return jsonDumpList
def writeToFile(jsonD, fileName):
    path = os.getcwd() + '\Tools\/' + fileName
    with open(path, 'w') as out_file:
        json.dump(jsonD, out_file, indent=4)

productCategory = "Pulseras"
fileName = productCategory + ".json"
jsonD = getJSON(productCategory, token)
writeToFile(jsonD, fileName)


