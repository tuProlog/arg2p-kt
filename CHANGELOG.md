## [0.7.1](https://github.com/tuProlog/arg2p-kt/compare/0.7.0...0.7.1) (2023-08-08)


### Bug Fixes

* **deps:** update ch.qos.logback to v1.4.9 ([b7df582](https://github.com/tuProlog/arg2p-kt/commit/b7df5829ad92eb3cae23ebd2a32708e18abbabca))


### Build and continuous integration

* add github token ([68e789c](https://github.com/tuProlog/arg2p-kt/commit/68e789c79d6fe905ba403589b445d74cd603dd00))
* add jsSourcesJar dependency ([ea45efa](https://github.com/tuProlog/arg2p-kt/commit/ea45efae8b51afc7f7759d6c7649a0994a21bfe0))
* add release ci (dryRun) ([59c1ed2](https://github.com/tuProlog/arg2p-kt/commit/59c1ed2a9a754e6c50760f3dccd9b6d2bc3267d7))
* clean dependencies ([84ea2b4](https://github.com/tuProlog/arg2p-kt/commit/84ea2b4f86e40c4d72146505a222fd10e1e804db))
* configure assembleJs ([a42aa23](https://github.com/tuProlog/arg2p-kt/commit/a42aa232b586996e26a8fc7d0763889f30ccc55a))
* **deps:** update danysk/build-check-deploy-gradle-action action to v2.2.8 ([bd82de8](https://github.com/tuProlog/arg2p-kt/commit/bd82de8e36f43c267fa05366cf54dd4c17bc3571))
* disable dry run ([275f56a](https://github.com/tuProlog/arg2p-kt/commit/275f56aaa476adb8bab39226f492ff0fc474a4c2))
* enable semantic commits in renovate ([5b6ad71](https://github.com/tuProlog/arg2p-kt/commit/5b6ad71e10d6c1312a1f1d01eb20b4bb5aa12a62))
* introduce semantic-release ([ad221d8](https://github.com/tuProlog/arg2p-kt/commit/ad221d8d52c760f9c9bc590b1c90e03e3baeffab))
* main to master ([e1d2ce5](https://github.com/tuProlog/arg2p-kt/commit/e1d2ce5ecb64166d904ff870edaec05841331168))
* no check on release ([2e567e5](https://github.com/tuProlog/arg2p-kt/commit/2e567e55c6ce41a53adfde4fdf53db3a7380817b))
* refactor core project ([23e5f05](https://github.com/tuProlog/arg2p-kt/commit/23e5f059a2589df3d163711964614da5ddc3aef2))
* remove tuPrologVersion ([a064c56](https://github.com/tuProlog/arg2p-kt/commit/a064c56ba057d15a1e1d8792adad8dc998dbca4b))

# Changelog

## [0.3.0]
### Added
- meta-bp evaluation

## [0.2.5]
### Added
- arguments caching in structured mode
### Fixed
- flags handling in Java IDE
- dynamic KB reset (buildLabelSets/0)

## [0.2.4]
### Added
- queryMode and unrestrictedRebut defaults in IDE
- structured answerQuery/4 performance optimizations (caching)
### Fixed
- non-parsed terms in structured answerQuery/4 output
- contrary rebut/undermine on strict rules
- rebut restriction

## [0.2.3]
### Fixed
- defeat definition in abstract mode 

## [0.2.2]
### Fixed
- theory visualisation on Java IDE
- preferences handling in structured answerQuery/3
- buildArgumentationGraph performances

## [0.2.1]
### Fixed
- experimentalQueryMode -> queryMode 
- missing interface predicate (buildLabelSets/3)

## [0.2.0]
### Added
- call_module/2 primitive
### Fixed
- contrary rebut/undermine
- argument building in structured reasoning

## [0.1.0]
### Added
- 2p library porting
- Jvm IDE
