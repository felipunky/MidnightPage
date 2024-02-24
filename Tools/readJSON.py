import os
import json
import math
import re
import zlib

rx_al = r"(<[\/A-Za-z]+[^>]*>)"
rx_op = r"<([A-Za-z]+)[^\/]+>"
rx_cl = r"</([A-Za-z]+)>"
# self-closing not needed
#rx_sc = r"<([A-Za-z]+).* \/>"

def lookup_key(match, indent):
  return f"{match[0]}:{indent}"

def balance(nodes):
  builder = []
  indent = 0
  lookup = {}
  for node in nodes:
    is_open = re.match(rx_op, node)
    is_close = re.match(rx_cl, node)
    padl = " " * indent
    if is_open:
      k = lookup_key(is_open, indent)
      lookup[k] = node
      indent += 2
    elif is_close:
      for i in range(0, indent, -2):
        if lookup.pop(lookup_key(is_close, i), None):
          break
      indent -= 2
      padl = padl[:-2]
    builder.append(padl + node)
  return "\n".join(builder)

def logXA(x, a):
    return math.log(a) / math.log(x)

def colBootstrap(img, imgLazy, productID, productName, productPrice, productImages):
    href = 'item_1.html'
    productN = productName
    productP = str(productPrice)
    altS = productN + ' ' + productP
    alt = ' alt="' + productN + ' ' + productP + '">'
    if productID == 12456:
      href = '#'
      #alt = ' alt="Dummy">'
    imageString = ''
    for i in range(0, len(productImages)):
      image = productImages[i]
      imageString += image
      if i < len(productImages)-1:
        imageString += ","
    rowText = '<div class="col-sm-4 item-selected" alt="{altString}"> \
                <a href="{href}" id="{imgString}"> \
                  <img data-src="{imgOriginal}" src="{imgLazy}" loading="lazy" alt="{productID}" class="lazyload img-fluid lazy" width="1008" height="1008"> \
                </a> \
              </div>'.format(altString = altS, imgString = imageString, imgOriginal = img, imgLazy = imgLazy, productID = productID, href = href)
    #print(alt)
    return rowText

def Run(jsonFileName):
  path = os.getcwd() + "\Tools" + "\\" + jsonFileName
  print("Path " + path)
  
  # returns JSON object as 
  # a dictionary
  with open(path, "r") as infile:
      data = json.loads(infile.read())
  #dataPretty = json.dumps(data, indent=4)
  #print(dataPretty)
  originalSize = len(data)
  size = round(logXA(3, originalSize))
  t = math.ceil(originalSize / 3)
  s = t * 3
  print(str(t))
  print(str(s))

  print(str(originalSize))
  print(str(size))

  rowStart = '<div class="row">'
  rowEnd = '</div>'
  counterMod = 0
  counter = 0
  accumulateHTML = ""
  dummyImg = "Images/Logo.png"
  dummyImgLazy = "Images/Logo_Lazy.png"

  iterations = originalSize
  if originalSize % 3 != 0:
    iterations = s
  if originalSize < 3:
    iterations = 3
  print(iterations)
  for i in range(0, iterations):
    tempRow = ""
    #iString = str(i)
    mod3Counter = i % 3
    #mod3CounterString = str(mod3Counter)
    title = "Dummy"
    imgLazy = dummyImgLazy
    img = dummyImg
    productID = 12456
    productPrice = 0.0
    images = ["None", "None"]
    if i < len(data):
      title = data[i]['title']
      imgLazy = data[i]["imgMin"]
      img = data[i]["img"]
      productID = data[i]["productID"]
      productPrice = data[i]["productPrice"]
      images = data[i]["images"]
      # for im in images:
      #   print(im)
    #print("Product id: ", productID)
    #print("Product price: ", productPrice)
    if mod3Counter == 0:
      tempRow = rowStart
      tempRow += colBootstrap(img, imgLazy, productID, title, productPrice, images)
      counterMod += 1
    elif mod3Counter == 2:
      tempRow += colBootstrap(img, imgLazy, productID, title, productPrice, images)
      tempRow += rowEnd
    else:
      tempRow =  colBootstrap(img, imgLazy, productID, title, productPrice, images)
    accumulateHTML += tempRow
  #print(accumulateHTML)
  matches = re.findall(rx_al, accumulateHTML, flags=re.M)
  #print(balance(matches))   
  #print("Counter: ", counterMod)
  pathHTML = os.getcwd() + '\\' + "Test_1" + ".html"
  print(pathHTML)
  file = open(pathHTML, "w")
  file.write(balance(matches))

typeOfProduct = [["Aretes", "Candongas"], ["Earcuffs", ""], ["Anillos", ""], \
                 ["Collares", ""], ["Pulseras", ""], ["Tobilleras", ""]]

jsonFileName = typeOfProduct[4][0] + ".json"
Run(jsonFileName)

    