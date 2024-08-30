module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

-- From the spec, it isn't really clear on how to handle
-- Quality values that fail the check, in the interest
-- of saving some time, I chose to default it to 0 if it
-- is illegal - however, using `Maybe` or even `Either`
-- would be more robust, but this would likely require more
-- updates to the Item data type, which the spec says to avoid..
validateQuality :: Int -> Int
validateQuality quality = validatedQuality
  where validatedQuality
          | quality > 0 && quality <= 50 = quality
          | quality < 1                  = 0
          | otherwise                    = 50

updateAgedBrie :: Item -> Item
updateAgedBrie (Item "Aged Brie" sellIn quality) = Item "Aged Brie" updatedSellIn (validateQuality updatedQuality)
    where updatedSellIn
            | sellIn > 1 = sellIn - 1
            | otherwise  = 0
          updatedQuality
            | sellIn > 0 && sellIn < 5  = quality + 3
            | otherwise                 = quality + 2

updateBackstagePass :: Item -> Item
updateBackstagePass (Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality)
  = Item "Backstage passes to a TAFKAL80ETC concert" updatedSellIn (validateQuality updatedQuality)
    where updatedSellIn
            | sellIn > 1 = sellIn - 1
            | otherwise  = 0
          updatedQuality
            | sellIn < 1                 = 0
            | sellIn > 0 && sellIn <= 5  = quality + 3
            | sellIn > 5 && sellIn <= 10 = quality + 2
            | otherwise                  = quality + 1

updateSulfuras :: Item -> Item
updateSulfuras (Item "Sulfuras, Hand of Ragnaros" sellIn quality)
  = Item "Sulfuras, Hand of Ragnaros" sellIn 80

updateConjuredItem :: Item -> Item
updateConjuredItem (Item "Conjured Mana Cake" sellIn quality)
  = Item "Conjured Mana Cake" updatedSellIn (validateQuality updatedQuality)
    where updatedSellIn 
            | sellIn > 1 = sellIn - 1
            | otherwise  = 0
          updatedQuality
            | quality < 1 = 0
            | sellIn < 1  = quality - 4
            | sellIn > 0  = quality - 2
            | otherwise   = 0

updateOtherItem :: Item -> Item
updateOtherItem (Item name sellIn quality)
  = Item name updatedSellIn (validateQuality updatedQuality)
    where updatedSellIn 
            | sellIn > 1 = sellIn - 1
            | otherwise  = 0
          updatedQuality
            | quality < 1 = 0
            | sellIn < 1  = quality - 2
            | sellIn > 0  = quality - 1
            | otherwise   = 0

updateQualityItem :: Item -> Item
updateQualityItem(Item name sellIn quality) =
  case name of
    "Aged Brie"                                 -> updateAgedBrie (Item name sellIn quality)
    "Backstage passes to a TAFKAL80ETC concert" -> updateBackstagePass (Item name sellIn quality)
    "Sulfuras, Hand of Ragnaros"                -> updateSulfuras (Item name sellIn quality)
    "Conjured Mana Cake"                        -> updateConjuredItem (Item name sellIn quality)
    _                                           -> updateOtherItem (Item name sellIn quality)

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
