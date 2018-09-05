doubleMe x = x + x

unitPrice :: Double
unitPrice = 231

pricePrawn kg = kg * unitPrice

threshold = 50 :: Double
discountPerKg = 0.8 :: Double

price2 :: Double -> Double
price2 kg | kg <= threshold = pricePrawn kg
          | kg > threshold = (pricePrawn threshold) + (discountPerKg * pricePrawn (kg - threshold))

prop_monotonicPrice kg delta = price2 kg <= price2 (kg + delta)

factorial n | n == 0 = 1
            | n > 0 = n * factorial (n - 1)
            | n < 0 = error "negative factorial is undefined"
