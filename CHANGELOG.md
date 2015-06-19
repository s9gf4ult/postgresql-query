# CHANGELOG

## 1.2.1

* remove version constraints for `transformers` package

## 1.2.0

### Added
* `pgWithTransactionMode` and its friends, like
  `pgWithTransactionSerializable` added.

## 1.1.1

### Changed
* `pgInsertManyEntities` returns count of inserted entities
* `pgDeleteEntity` returns True if entity was actually deleted
* `pgUpdateEntity` returns True if entity was actually updated

## 1.1.0

### Added
* `deriveEntity` - TH derivation `Entity` instances for you types
* `deriveEverything` - TH derivation of `Entity`, `ToRow` and `FromRow` in one shot

### Changed
* Docs improoved

## 1.0.1
The first usable version
