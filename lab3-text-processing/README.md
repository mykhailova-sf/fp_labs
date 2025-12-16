# Lab 3: Text Processing in Haskell

## Description
This program processes a text file and finds words that appear **only in the first sentence** but not in any other sentences.

## Quick Start

### Build and Run with Stack

```bash
cd Lab3/lab3-text-processing
stack build
stack run
```

## How It Works

1. Reads text from `textbook.txt`
2. Splits the text into sentences (by `.`, `!`, `?`)
3. Splits each sentence into words
4. Compares words in the first sentence with all other sentences
5. Returns words that appear only in the first sentence

## Project Structure

```
lab3-text-processing/
├── stack.yaml           # Stack configuration
├── package.yaml         # Project dependencies and metadata
├── textbook.txt         # Input text file
├── README.md            # This file
└── app/
    └── Main.hs         # Main program source code
```

## Usage

The program automatically reads from `textbook.txt` in the project directory and outputs:
- The original text
- List of unique words from the first sentence

### Example Output

```
Reading file textbook.txt...

Original text:
[Text content here...]

=========================================
Words that appear only in the first sentence:
=========================================
  - word1
  - word2
  - word3
=========================================
```

## Features

- **Sentence Splitting**: Recognizes `.`, `!`, and `?` as sentence delimiters
- **Word Normalization**: Converts all words to lowercase for comparison
- **Whitespace Handling**: Normalizes tabs and multiple spaces
- **Case-Insensitive**: Treats "Word" and "word" as the same
- **Clean Output**: Removes punctuation and non-alphabetic characters

## Input File

The program expects a file named `textbook.txt` in the project directory. You can modify this file to process different texts.

## Dependencies

All dependencies are managed by Stack:

- `base >= 4.7`: Haskell standard library

Stack will automatically install GHC and all dependencies when you run `stack build`.

## Troubleshooting

If `stack build` fails, try:
```bash
stack clean
stack build
```

If the program can't find `textbook.txt`, make sure:
1. The file exists in the project directory
2. You're running the program from the correct directory

## Technical Details

The program uses functional programming concepts including:
- **Pure Functions**: All text processing functions are pure
- **List Comprehensions**: For filtering and mapping
- **Pattern Matching**: For handling different sentence cases
- **Higher-Order Functions**: `map`, `filter`, `concatMap`
- **Custom Types**: Type aliases for clarity

## Modifying the Input

To process a different text file:
1. Replace the contents of `textbook.txt` with your text
2. Run `stack run` again

The program will automatically process the new content.
