# Glados - Haskell Project


## Table of Contents
- [Introduction](#introduction)
- [Building Glados](#building-glados)
- [Running Glados](#running-glados)
- [Usage](#usage)
- [Available Files](#available-files)
- [Documentation](#documentation)
- [Project Members](#project-members)

## Introduction
The goal of the Glados project is to implement a programming language of your own design using Haskell. While this may sound daunting, this README will guide you through the process step by step.

## Building Glados
Before you can start using Glados, you need to build the project. You can do this by running the following command:

### Unitary Tests
To run unit tests, execute the following command:
```
make unittest_run
```

### Functional Tests
For functional tests, use the following command:
```
make functest_run
```

## Release (Continuous Deployment)
To release a new version of Glados, follow these steps:
1. Tag a Version:
```
git tag vX.Y
```
2. Push the Tags:
```
git push --tags
```
This will ensure that the latest version is tagged and pushed to the repository, making it available for deployment or distribution.

### CI/CD Configuration
The CI/CD pipeline for Glados is configured using GitHub Actions. You can find the workflow files that manage the Continuous Integration and Continuous Deployment in the following directory:
```
cd ./github/workflows
```

## Usage
Glados provides various modes for compiling and evaluating Scheme files. Here are the different usage options:

### Compilation and Evaluation
1.  **Compile and Evaluate**: Compile a `.scm` file into a `.go` file and evaluate it.
```
./glados file.scm
```
2. **Compile with AST Printing (optional)**: Compile a `.scm` file into a `.go` file and optionally print the Abstract Syntax Tree (AST).
```
./glados -compiler [-ast] file.scm
```
3. **Evaluate Pre-compiled Go File**: Evaluate a pre-compiled `.go` file.
```
./glados -evaluate file.go
```
4. **Help**: Display the usage information.
```
./glados -help
```

## Available Files
In the `Lisp` folder, you will find a collection of `.scm` files that you can use for testing and experimentation with the Glados programming language.

## Documentation

### Projet glados

Go to the Doc_glados folder and type the command :
```
npm install
npm run dev
```
### Language Lispp

Go to the doc_Lispp folder and type the command :
```
npm install
npm run dev
```

## Project Members
-   Clarisse Eynard
-   Alexia Martinez-Lee
-   Elisa Plas

Feel free to explore and experiment with Glados, and don't hesitate to reach out for any further assistance or questions.