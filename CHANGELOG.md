# CHANGELOG

## 3.0.1
### Changed
* Buildability with ghc-8.0.1
## 3.0.0
### Added
* Ability to mask some sensitive query arguments in query logs. In case you dont
  want someone can see your secret data in logs.
  * Added interpolation syntax `#?{argumentExpression}` for masked parameters
  * Added `pgQueryWithMasker` and `pgExecuteWithMasker` for custom log masker
* `MonadPostgres` type synonim
### Changed
* Refactoring of `SqlBuilder` and `Entity` modules
* Some low-level intefaces changed
## 2.3.0
### Added
* `derivePgEnum` TH generator for enum fields
### Changed
* TH code splitted to modules

## 2.2.0
### Added
* `MonadHReader` instance for `PgMonadT`

## 2.1.0
### Changed
* use hset-2.0 and hreader-1.0

## 2.0.0
### Changed
* `Entity` typeclass now use `FN` instead of `Text`. This provides an
  ability to define dot-separated table names like
  `schemaname.tablename`. This changes breaks backward compatibility,
  so major version is bumped to 2.

* TH code changed according to changes in `Entity` typeclass.

## 1.4.0
### Changed
* `eoDeriveClasse` renamed to `eoDeriveClasses`
### Added
* Grammar added to documentation

## 1.3.1
### Changed
* work with `hset-1.0.0`

## 1.3.0
### Added
* dependency from `hreader` and `hset`
* `HasPostgres` instance for `HReaderT` transformer

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
