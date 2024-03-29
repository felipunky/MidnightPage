function ProductNames(className)
{
    let elements = document.getElementsByClassName(className);
    //console.log(elements.length);
    for (var i = 0; i < elements.length; ++i)
    {
        let text = elements[i].getAttribute("alt");
        if (text == "Dummy 0.0")
        {
        continue;
        }
        let textSplit = text.split(' ');
        let numberWhole = textSplit[textSplit.length-1];
        let numberSplit = numberWhole.split('.');
        let number = numberSplit[0];
        let numberValue = "";
        for (var j = 0; j < number.length; ++j)
        {
        if (j == 2)
        {
            numberValue += ",";
        }
        numberValue += number[j];
        }
        let textValue = "";
        for (var j = 0; j < textSplit.length-1; ++j)
        {
        textValue += textSplit[j] + " ";
        }
        const newContentText = document.createElement("h3");
        newContentText.innerHTML = textValue;
        newContentText.setAttribute("class", "item-selected-text");
        elements[i].appendChild(newContentText);

        const newContentPrice = document.createElement("div");
        newContentPrice.innerHTML = "$" + numberValue;
        newContentPrice.setAttribute("class", "item-selected-price");
        elements[i].appendChild(newContentPrice);
    }
}
function GetProductID(className)
{
    var el = document.getElementsByClassName(className);
    for(var i = 0; i < el.length; ++i)
    {
        el[i].addEventListener("click", storeValue);
        function storeValue()
        {
            let attribute = this.getAttribute("alt");
            localStorage.setItem('productID', attribute);
            let imagesAttribute = this.parentElement.getAttribute("id");
            localStorage.setItem('productImages', imagesAttribute);
        };
    }
}

function Shopify(productID, productComponent) 
{
    var scriptURL = 'https://sdks.shopifycdn.com/buy-button/latest/buy-button-storefront.min.js';
    if (window.ShopifyBuy) 
    {
      if (window.ShopifyBuy.UI) 
      {
        ShopifyBuyInit();
      } 
      else 
      {
        loadScript();
      }
    } 
    else 
    {
      loadScript();
    }
    function loadScript() 
    {
      var script = document.createElement('script');
      script.async = true;
      script.src = scriptURL;
      (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(script);
      script.onload = () => 
      {
        //ShopifyBuyInit();
        buyInit();
      }; 
    }
    async function buyInit() 
    {
        await ShopifyBuyInit();
        //await new Promise((resolve, reject) => setTimeout(resolve, 3000));
        //await Logger();
    }
    function Logger()
    {
        //console.log("Hey");
    }
    function ShopifyBuyInit()
    {
        var client = ShopifyBuy.buildClient({
            domain: '2f9ed3-3.myshopify.com',
            storefrontAccessToken: '564e97734af28b07935b62227448d359',
        });
        ShopifyBuy.UI.onReady(client).then(function (ui) 
        {
            ui.createComponent('product', 
            {
                id: productID,
                node: document.getElementById(productComponent),
                moneyFormat: '%24%7B%7Bamount_with_comma_separator%7D%7D',
                options: 
                {
                    "product": {
                        "styles": {
                        "product": {
                            "@media (min-width: 601px)": {
                            "max-width": "100%",
                            "margin-left": "0",
                            "margin-bottom": "50px"
                            },
                            "text-align": "left"
                        },
                        "title": {
                            "font-size": "26px",
                            "color": "#000000"
                        },
                        "button": {
                            ":hover": {
                            "background-color": "#000000"
                            },
                            "background-color": "#000000",
                            ":focus": {
                            "background-color": "#000000"
                            },
                            "border-radius": "19px",
                            "padding-left": "38px",
                            "padding-right": "38px"
                        },
                        "price": {
                            "font-size": "18px"
                        },
                        "compareAt": {
                            "font-size": "15.299999999999999px"
                        },
                        "unitPrice": {
                            "font-size": "15.299999999999999px"
                        }
                        },
                        "layout": "horizontal",
                        "contents": {
                        "img": false,
                        "imgWithCarousel": false,
                        "button": false,
                        "buttonWithQuantity": true,
                        "description": true
                        },
                        "width": "100%",
                        "text": {
                        "button": "Agregar al carrito"
                        }
                    },
                    "productSet": {
                        "styles": {
                        "products": {
                            "@media (min-width: 601px)": {
                            "margin-left": "-20px"
                            }
                        }
                        }
                    },
                    "modalProduct": {
                        "contents": {
                        "img": false,
                        "imgWithCarousel": false,
                        "button": false,
                        "buttonWithQuantity": false
                        },
                        "styles": {
                        "product": {
                            "@media (min-width: 601px)": {
                            "max-width": "100%",
                            "margin-left": "0px",
                            "margin-bottom": "0px"
                            }
                        },
                        "button": {
                            ":hover": {
                            "background-color": "#000000"
                            },
                            "background-color": "#000000",
                            ":focus": {
                            "background-color": "#000000"
                            },
                            "border-radius": "19px",
                            "padding-left": "38px",
                            "padding-right": "38px"
                        },
                        "title": {
                            "font-family": "Helvetica Neue, sans-serif",
                            "font-weight": "bold",
                            "font-size": "26px",
                            "color": "#4c4c4c"
                        },
                        "price": {
                            "font-family": "Helvetica Neue, sans-serif",
                            "font-weight": "normal",
                            "font-size": "18px",
                            "color": "#4c4c4c"
                        },
                        "compareAt": {
                            "font-family": "Helvetica Neue, sans-serif",
                            "font-weight": "normal",
                            "font-size": "15.299999999999999px",
                            "color": "#4c4c4c"
                        },
                        "unitPrice": {
                            "font-family": "Helvetica Neue, sans-serif",
                            "font-weight": "normal",
                            "font-size": "15.299999999999999px",
                            "color": "#4c4c4c"
                        }
                        },
                        "text": {
                        "button": "Agregar al carrito"
                        }
                    },
                    "option": {},
                    "cart": {
                        "styles": {
                        "button": {
                            ":hover": {
                            "background-color": "#000000"
                            },
                            "background-color": "#000000",
                            ":focus": {
                            "background-color": "#000000"
                            },
                            "border-radius": "19px"
                        }
                        },
                        "text": {
                        "total": "Subtotal",
                        "button": "Checkout"
                        }
                    },
                    "toggle": {
                        "styles": {
                        "toggle": {
                            "background-color": "#000000",
                            ":hover": {
                            "background-color": "#000000"
                            },
                            ":focus": {
                            "background-color": "#000000"
                            }
                        }
                        }
                    }
                }
            }
            );
            }
        );
    }
};