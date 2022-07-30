- Page 17:
  - Use the `Text` type for processing textual information instead of `String`.
  - Enable the `OverloadedStrings` extension to make it more convenient to use string literals as `Text` values.
    - TODO: add link
  - Choose your own favorite package for representing data in text: `formatting` and `fmt` are good candidates.

- Page 19:
  - An experienced Haskeller often looks for a type class first and then starts coding.

- TODO:
- `IsString` type class
- Enabling and disabling GHC extensions in GHCi
- List all Extensions
- ormolu directives
- nvim shorthand `<leader>sh` for unicode
- random lib
- numeric types and most important functions (different prelude with different number hierarchy)
- fixed precision (2.2.3)
  - error function from Ch. 1

- Page 46:
  - Arguably the best advice on `Show` and `Read` is to avoid implementing them manually.
  - In fact, we don't even need to. It's always better to use some formatting (for `Show`) or a parsing library (for `Read`) instead.
  - Derived `Show` and `Read` instances may still be used for simple cases when debugging or exploring code in GHCi.

