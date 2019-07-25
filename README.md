# HTML5 minify

HTML5 minifier implementation based on Servo's [html5ever](https://github.com/servo/html5ever).

## Features

- High performance
- Input using string types or `io::Read`
- Output to `String` or `io::Write`

## Examples

```rust
use html5minify::Minify;

let html = "<html> \n<link href=\"test.css\">\n<h2   id=\"id_one\"    >Hello\n</h2>    \n<p>\nWorld</p>";
let minified = html.minify().expect("Failed to minify HTML");
```

## TODO

- Inline element space trimming logic
- Comprehensive test cases
