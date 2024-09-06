# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Add

- Add the `-K/--key` option to the script `gen_update.sh` generate a signed
  package.
- Add the `--tar` option to `grisp_updater_tools` to directly generate a
  tarball.

### Changed

- Now use the grisp_update_packager library to generate the software update
  package.

## [0.1.0] - 2024-08-07

First release.

[Unreleased]: https://github.com/grisp/grisp_updater_tools/compare/0.1.0...HEAD
[0.1.0]: https://github.com/grisp/grisp_updater_tools/compare/e8bc0c5b824f45ffd4ce38c244801667bfdfb72b...0.1.0
