# Revision history for runic

## [0.0.0.0] -- YYYY-mm-dd
### Changed
- Basic project structure has been substantially overhauled
  - Project now only relies on Cabal, not Stack
- Math expressions will now be evaluated using Parsec lexemes (I'm assuming here, but probably smarter to make a recursive parser in a language tailored toward recursion with world-class parser libraries.)

### Added
- Added a few basic tests and a crude, self-made harness that needs more love later
- units.json source file for unit conversion data. 