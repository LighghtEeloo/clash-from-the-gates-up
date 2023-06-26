# The Unforgivably Convenient: Baby Records and Baby Lens

`obj`

`fieldName`

`item`

## Theorist. Baby Record.

TBD.

## Engineer. Baby Lens.

Ultimately, you only need to use 3 sorts of lens operators.

The first one is `^.`, which projects out a field.

```haskell
obj ^. fieldName
obj ^. (fieldName . nestedFieldName)
```

The second one is `&` and `.~`, which replaces a field.

```haskell
obj & fieldName .~ newItem
```

The third one is `&` and `%~`, which updates (modifies) a field.

```haskell
obj & fieldName %~ (\oldItem -> newItem)
obj & fieldName %~ updateFunction
	where
		updateFunction oldItem = newItem
-- updateFunction :: Item -> Item
```

With some variants

```haskell
obj & fieldName +~ 3
obj & fieldName -~ 4
obj & fieldName *~ 5
```

