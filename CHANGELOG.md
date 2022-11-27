# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.2.0] - 2022-11-28
### Added
- Clojure CLI configuration
### Removed
- Leiningen configuration

## [0.1.8] - 2021-12-15
### Added
- Reformed dependencies

## [0.1.7] - 2021-08-20
### Added
- Revision to function signature for igraph/traverse (optional acc arg)

## [0.1.6] - 2021-07-25
### Added
- Cleanup
- Updated documentation
## [0.1.5] - 2020-06-3

### Added
- Source now includes test module to be leveraged by downstream
  implementation tests.

## [0.1.4] - 2020-02-25
### Added
- P-arguments can now be traversal functions mapping s to a set of o's.
- clj files are now cljc, and there are unit tests for cljs using doo
- t-comp provodes compositional traversal functions
- defined utility traversal function generators
- Mutability models are broken out into their own protocols

## [0.1.3] - 2018-12-14
### Added
- A traverse function, with a utility to define transitive-closure.

## [0.1.2] - 2018-12-14
### Added
- A new protocol ISet for basic set operations, with support in Graph

## [0.1.1] - 2018-12-9
### Added
- Subtraction functions to the IGraph protocol, with support in Graph
