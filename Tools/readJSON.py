import os
import json
import math
import re

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

def colBootstrap(img = "", imgLazy = "", productID = 12345):
    href = '"item_1.html">'
    if productID == 12456:
      href = '"#">'
    rowText = '<div class="col-sm-4 item-selected"> \
                    <a href=' + href + ' \
                        <img data-src="' + img + '" \
                        src="' + imgLazy + '" \
                        loading="lazy" \
                        alt="' + str(productID) + '" class="lazyload img-fluid lazy" \
                        width="1080" height="1080"> \
                    </a> \
                </div>'
    return rowText

def Run(jsonFileName):
  path = os.getcwd() + "\Tools" + "\\" + jsonFileName
  print("Path " + path)
  
  # returns JSON object as 
  # a dictionary
  with open(path, "r") as infile:
      data = json.loads(infile.read())
  dataPretty = json.dumps(data, indent=4)
  #print(dataPretty)
  originalSize = len(data)
  size = round(logXA(3, len(data)))
  t = round(originalSize / 3)
  s = t * size
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
  for i in range(0, iterations):
    tempRow = ""
    iString = str(i)
    mod3Counter = i % 3
    mod3CounterString = str(mod3Counter)
    title = 'dummy'
    imgLazy = dummyImgLazy
    img = dummyImg
    productID = 12456
    if i < len(data):
      title = data[i]['title']
      imgLazy = data[i]["imgMin"]
      img = data[i]["img"]
      productID = data[i]["productID"]
    if mod3Counter == 0:
      tempRow = rowStart
      tempRow += colBootstrap(img, imgLazy, productID)
      counterMod += 1
    elif mod3Counter == 2:
      tempRow += colBootstrap(img, imgLazy, productID)
      tempRow += rowEnd
    else:
      tempRow = colBootstrap(img, imgLazy, productID)
    accumulateHTML += tempRow

  matches = re.findall(rx_al, accumulateHTML, flags=re.M)
  print(balance(matches))   
  print("Counter: ", counterMod)

jsonFileName = "Aretes.json"
Run(jsonFileName)

    