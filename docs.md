---
name: parachute
description: A container for running Parachute test suites.
author: Yukari Hafner
tags: [commonlisp, lisp, testing]
url: https://shinmera.com/projects/parachute
---

# parachute
Lets you easily run test suites in your CI.

## Example:
```yaml
when:
  event: [push]
matrix:
  LISP:
    - ccl
    - clasp
    - clisp
    - cmucl
    - ecl
    - sbcl
steps:
  run:
    image: shinmera/parachute:latest
    settings:
      systems: my-project/tests
      implementation: ${LISP}
```

## Settings
The settings pretty much just correspond to the command line arguments that can be passed to parachute:

| Settings Name   | Default          | Description                                          |
| --------------- | ---------------- | ---------------------------------------------------- |
| systems         | $CI_REPO_NAME    | Systems to load, comma or space separated.           |
| tests           | $systems         | Packages to test, comma or space separated.          |
| implementation  | sbcl             | The name of the implementation to run.               |
