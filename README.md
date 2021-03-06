# Pers
Ideas for persistent. Investigations of working with Named Records.

## Introduction

Current `persistent` package is good but have some disadvantages:
* Code is generated by TH which reduce composability
* No decision for projections of `entity`
* No instruments for working with views/stored procedures/direct select
* Others?....

Here I want to try to check some ideas in this field.

The first one is storing data in some kind of Named Records using `Symbols` for type-level checking.
I was excited with using `Symbols` in [Servant](http://haskell-servant.github.io/) library. 
And I want to try something similar for Persistency.

Another example of Named Record I saw was [Record](github.com/nikita-volkov/record) library. But there is too much TH and length of record too restrictive.

## Named Records

I hoped to find decision with properties:
* O(log n) time to get field from record by name
* record construction independent from order of fields definitions
* no TH preferrable, some TH possible
* projections and joins are desirable

I tried to implement it using "very-well balanced tree" (I don't know the right name) on the type level. 
This is a binary search (by name) tree with count of left items is the same or one more than on the right side (for each subtree).
The problem is construction. I realized it with [FunDeps](https://github.com/odr/pers/blob/bdab60d0ba34614b8ac3772c38a2ad44adaab3d1/src/NamedBTree.hs) and  with [TF](https://github.com/odr/pers/blob/bdab60d0ba34614b8ac3772c38a2ad44adaab3d1/src/NamedBTree2.hs). 

But compile time for [user application](https://github.com/odr/pers/blob/bdab60d0ba34614b8ac3772c38a2ad44adaab3d1/app/Main.hs) is absolutely blocked this idea!

Later I'd tried to make balanced (not ordered) typed tree on tuples for record representation. And result is rather successfull! I got O(log n) access time and good compile time. 

In this case record preserve field's order (the second aid is not realized). But now I suppose that that is better than canonical (ordered) representation.

All was done without any TH.

## The Result

Now one can
* Construct record sequentially:
```haskell
type T = "a":>Int +> "b":>String +> "c":>Maybe Int
rec = V 5 +> V "b" +> V (Just 3) :: T
```
* Get `Lens'` for fields or record (group of fields) with O(log n) access:
```haskell
lb = fieldLens (Proxy :: Proxy ("b":>String)) :: Lens' T String
lca = recLens :: Lens' T ("c:>Maybe Int +> "a":>Int)
```
Note that `recLens` is Projection!
* Lift all fields into functor
```haskell
type LT = Lifted Maybe T
-- LT == "a":>Maybe Int +> "b":>Maybe String +> "c":>Maybe (Maybe Int)
```
Record is just a tuple so it has all common instances (`Default`, `Monoid` and so on). Particulary, `Lifted Maybe T` has `Default` instance. So we can construct a new record:
```haskell
newRec = def :: Lifted Maybe T
```
* Convert from and to Map of fields (using `PersistField` and `PersistValue` from [persistent package](https://hackage.haskell.org/package/persistent))
```haskell
m = M.fromList  [ (someSymbolVal "a", toPersistValue 1)
                , (someSymbolVal "b", toPersistValue "xx")
                -- value for "c" is optional
                ]
recEither = mapToRec (Proxy :: Proxy T) m :: Either [SomeSymbol] T
m' = fmap toMap recEither
```

