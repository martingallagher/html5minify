[![Actions Status](https://github.com/martingallagher/html5minify/workflows/CI/badge.svg)](https://github.com/martingallagher/html5minify/actions)
[![HTML5 minify documentation](https://docs.rs/html5minify/badge.svg)](https://docs.rs/html5minify)

# HTML5 minify

HTML5 minifier implementation based on Servo's [html5ever](https://github.com/servo/html5ever).

## Features

- High performance
- Input using `AsRef<[u8]>` / `io::Read`
- Output to `Vec<u8>` / `io::Write`

## Examples

```rust
use html5minify::Minify;

// Using Minify trait on &str:
let html = "<html> \n<link href=\"test.css\">\n<h2   id=\"id_one\"    >Hello\n</h2>    \n<p>\nWorld</p>";
let minified = html.minify().expect("Failed to minify HTML");

// Using minifier with omit doctype option set:
let mut minified = vec![];

Minifier::new(&mut minified)
    .omit_doctype(true)
    .minify(&mut html.as_bytes())
    .expect("Failed to minify HTML");
```