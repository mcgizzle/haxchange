# haxchange 🤑

## About
The goal of this project is to create a uniform E-DSL for various cryptocurrency exchanges. This could have many uses from arbitraging to algorithmic trading. I will initially only be implementing a select set of functions, but the hope is to extend it in the future.

## Progress

Exchange | getMarkets | getTicker | getBalance | buyLimit | sellLimit  
---|---|---|---|---|---
Kraken | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:
Binance | :heavy_multiplication_x: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:| :heavy_check_mark:
Bittrex | :heavy_multiplication_x: | :heavy_check_mark: | :heavy_multiplication_x:* | :heavy_multiplication_x:* | :heavy_multiplication_x:*

*New accounts are currently blocked 

## Templates
 
There is a template folder which contains the modules and tests necessary for adding a new exchange, along with most of the bolier plate. 

There is a script for creating the files needed to add a new exchange.

### Adding a new exchange

run the following

```
cd templates
stack make.hs <name of new exchange>
```

This will copy the template files replacing all `<newmodule>` tags with the name of the new exchange.

Contributions are welcomed with open arms :)

## Implementation
There are shared ADT's that are uniform accross all exchanges. Each exchange implements custom `FromJSON` instances as appropriate.

In order to improve effeciency, the data types are mostly [Text](http://sorryiwillinsertalinkatsomestage.com).

There is an *TextConvert* type class which has the following functions
```Haskell
class TextConvert where
  toText :: a -> Text
  fromText :: Text -> a
```

Each exchange then implements its own type class
```Haskell
class <Exchange>Text where
  toText :: a -> Text
```
This helps with wrangling data into the unified model and vice-versa. The `toText` function is used to override the general version for special cases. 

Implementations of the exchanges will often follow the following pattern:
```Haskell
import qualified Types as T
instance KrakenText Currency where
  toText (COIN BTC) = "XBT"
  toText a          = T.toText a
  
```
Kraken refers to Bitcoin as XBT whereas most exchanges refer to it as BTC

For more information please see the source

