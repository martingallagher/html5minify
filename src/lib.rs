#![warn(missing_docs)]
#![deny(warnings, clippy::pedantic, clippy::nursery)]

//! HTML5 markup minifier.

use std::io;

use html5ever::tendril::TendrilSink;
use html5ever::{parse_document, Attribute, ParseOpts, QualName};
use markup5ever_rcdom::{Node, NodeData, RcDom};

/// Defines the minify trait.
pub trait Minify {
    /// Minifies the source returning the minified HTML5.
    ///
    /// # Errors
    ///
    /// Will return `Err` if unable to read from the input reader or unable to
    /// write to the output writer.
    fn minify(&self) -> Result<Vec<u8>, io::Error>;
}

/// Minifies the HTML input to the destination writer.
/// Outputs HTML5; non-HTML5 input will be transformed to HTML5.
///
/// # Errors
///
/// Will return `Err` if unable to read from the input reader or unable to write
/// to the output writer.
#[inline]
pub fn minify<R: io::Read, W: io::Write>(mut r: &mut R, w: &mut W) -> io::Result<()> {
    let dom = parse_document(RcDom::default(), ParseOpts::default())
        .from_utf8()
        .read_from(&mut r)?;

    w.write_all(b"<!doctype html>")?;
    Minifier { w }.minify(&dom.document)
}

impl<T> Minify for T
where
    T: AsRef<[u8]>,
{
    #[inline]
    fn minify(&self) -> Result<Vec<u8>, io::Error> {
        let mut minified = Vec::new();

        minify(&mut self.as_ref(), &mut minified)?;

        Ok(minified)
    }
}

struct Minifier<'a, W: io::Write> {
    w: &'a mut W,
}

impl<'a, W> Minifier<'a, W>
where
    W: io::Write,
{
    #[inline]
    fn minify(&mut self, node: &Node) -> io::Result<()> {
        match node.data {
            NodeData::Text { ref contents } => {
                let contents = contents.borrow();
                let trim = node.parent.take().map_or(true, |parent| {
                    parent
                        .upgrade()
                        .map_or(false, |ref parent| !is_inline_element(parent))
                });
                let contents = if trim {
                    contents.trim_matches(is_whitespace)
                } else {
                    contents.as_ref()
                };

                if contents.is_empty() {
                    return io::Result::Ok(());
                }

                self.w.write_all(contents.as_bytes())?
            }

            NodeData::Document => {
                node.children
                    .borrow()
                    .iter()
                    .try_for_each(|node| self.minify(node))?;
            }

            NodeData::Element {
                ref name,
                ref attrs,
                ..
            } => {
                let attrs = attrs.borrow();
                let tag = name.local.as_ref();
                let omit_start_element = attrs.is_empty() && omit_start_element(tag);

                if !omit_start_element {
                    self.write_start_tag(name, &attrs)?;
                }

                if can_have_children(tag) {
                    node.children
                        .borrow()
                        .iter()
                        .try_for_each(|node| self.minify(node))?;
                }

                if !omit_start_element && !omit_end_element(tag) {
                    self.write_end_tag(name)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    #[inline]
    fn write_qualified_name(&mut self, name: &QualName) -> io::Result<()> {
        if let Some(ref prefix) = name.prefix {
            self.w
                .write_all(prefix.as_ref().to_ascii_lowercase().as_bytes())?;
            self.w.write_all(b":")?;
        }

        self.w
            .write_all(name.local.as_ref().to_ascii_lowercase().as_bytes())
    }

    #[inline]
    fn write_start_tag(&mut self, name: &QualName, attrs: &[Attribute]) -> io::Result<()> {
        self.w.write_all(b"<")?;
        self.write_qualified_name(name)?;

        attrs
            .iter()
            .try_for_each(|attr| self.write_attribute(attr))?;

        self.w.write_all(b">")
    }

    #[inline]
    fn write_end_tag(&mut self, name: &QualName) -> io::Result<()> {
        self.w.write_all(b"</")?;
        self.write_qualified_name(name)?;
        self.w.write_all(b">")
    }

    #[inline]
    fn write_attribute(&mut self, attr: &Attribute) -> io::Result<()> {
        self.w.write_all(b" ")?;
        self.write_qualified_name(&attr.name)?;

        let value = attr.value.as_ref();

        if value.is_empty() {
            return io::Result::Ok(());
        }

        self.w.write_all(b"=")?;

        let (unquoted, double, single) =
            value
                .chars()
                .fold((true, false, false), |(unquoted, double, single), c| {
                    let (double, single) = (double || c == '"', single || c == '\'');
                    let unquoted = unquoted && !double && !single && c != '=' && !c.is_whitespace();

                    (unquoted, double, single)
                });

        if unquoted {
            self.w.write_all(value.as_bytes())
        } else if double {
            self.w.write_all(b"'")?;

            if single {
                self.w.write_all(value.replace('\'', "&#39;").as_bytes())?;
            } else {
                self.w.write_all(value.as_bytes())?;
            }

            self.w.write_all(b"'")
        } else {
            self.w.write_all(b"\"")?;
            self.w.write_all(value.as_bytes())?;
            self.w.write_all(b"\"")
        }
    }
}

fn is_whitespace(c: char) -> bool {
    matches!(c, '\n' | '\t') || c.is_whitespace()
}

fn omit_start_element(name: &str) -> bool {
    matches!(name, "body" | "head" | "html")
}

fn is_inline_element(node: &Node) -> bool {
    if let NodeData::Element { ref name, .. } = &node.data {
        !matches!(
            name.local.as_ref(),
            "address"
                | "article"
                | "aside"
                | "blockquote"
                | "body"
                | "details"
                | "dialog"
                | "dd"
                | "div"
                | "dl"
                | "dt"
                | "fieldset"
                | "figcaption"
                | "figure"
                | "footer"
                | "form"
                | "h1"
                | "h2"
                | "h3"
                | "h4"
                | "h5"
                | "h6"
                | "head"
                | "header"
                | "hgroup"
                | "hr"
                | "li"
                | "link"
                | "main"
                | "meta"
                | "nav"
                | "ol"
                | "p"
                | "pre"
                | "section"
                | "table"
                | "title"
                | "ul"
        )
    } else {
        false
    }
}

fn omit_end_element(name: &str) -> bool {
    matches!(
        name,
        "area"
            | "base"
            | "basefont"
            | "br"
            | "col"
            | "colgroup"
            | "dd"
            | "dt"
            | "frame"
            | "hr"
            | "img"
            | "input"
            | "isindex"
            | "li"
            | "link"
            | "meta"
            | "option"
            | "p"
            | "param"
            | "tbody"
            | "td"
            | "tfoot"
            | "th"
            | "thead"
            | "tr"
    )
}

fn can_have_children(name: &str) -> bool {
    !matches!(
        name,
        "area"
            | "base"
            | "br"
            | "col"
            | "embed"
            | "hr"
            | "img"
            | "input"
            | "link"
            | "meta"
            | "param"
            | "source"
            | "track"
            | "wbr"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;

    const HTML: &str = "<html> \n<link href=\"test.css\">\n<h2   id=\"id_one\"    >Hello\n</h2>    \n<p>\nWorld</p>";

    #[test]
    fn test_minify() {
        const EXPECTED: &str =
            "<!doctype html><link href=test.css><h2 id=id_one>Hello</h2><p>World";

        let minified = HTML.minify().expect("Failed to minify HTML");
        let minified = str::from_utf8(&minified).expect("Failed to convert to string");

        assert_eq!(EXPECTED, minified);
    }
}
