#![feature(test)]
#![warn(missing_docs)]

//! HTML5 markup minifier.

use std::io::{self, Read, Write};
use std::ops::Deref;

use html5ever::rcdom::{Node, NodeData, RcDom};
use html5ever::tendril::TendrilSink;
use html5ever::{local_name, parse_document, Attribute, LocalName, QualName};

/// Defines the minify trait.
pub trait Minify {
    /// Minifies the source returning the minified HTML5.
    fn minify(&self) -> Result<String, io::Error>;
}

/// Minifies the HTML input to the destination writer.
/// Outputs HTML5; non-HTML5 input will be transformed to HTML5.
#[inline]
pub fn minify(mut r: &mut dyn Read, w: &mut dyn Write) -> io::Result<()> {
    let dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut r)?;

    w.write_all(b"<!doctype html>")?;
    Minifier { w }.minify(&dom.document)
}

impl Minify for str {
    fn minify(&self) -> Result<String, io::Error> {
        let mut minified = Vec::new();

        minify(&mut self.as_bytes(), &mut minified)?;

        Ok(String::from_utf8_lossy(&minified).into())
    }
}

impl Minify for Node {
    /// This trait implementation doesn't add the HTML5 doctype on the
    /// assumption it could be used to minify interior nodes.
    fn minify(&self) -> Result<String, io::Error> {
        let mut minified = Vec::new();

        Minifier { w: &mut minified }.minify(&self)?;

        Ok(String::from_utf8_lossy(&minified).into())
    }
}

struct Minifier<'a> {
    w: &'a mut dyn Write,
}

impl<'a> Minifier<'a> {
    fn minify(&mut self, node: &Node) -> io::Result<()> {
        match node.data {
            NodeData::Text { ref contents } => {
                let contents = contents.borrow();
                let trim = if let Some(ref parent) = node.parent.take() {
                    match parent.upgrade() {
                        Some(ref parent) => !is_inline_element(parent),
                        _ => false,
                    }
                } else {
                    true
                };
                let contents = if trim {
                    contents.trim_matches(is_whitespace)
                } else {
                    contents.deref()
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
                let omit_start_element = attrs.is_empty() && omit_start_element(&name.local);

                if !omit_start_element {
                    self.write_start_tag(name, &attrs)?;
                }

                if can_have_children(&name.local) {
                    node.children
                        .borrow()
                        .iter()
                        .try_for_each(|node| self.minify(node))?;
                }

                if !omit_start_element && !omit_end_element(&name.local) {
                    self.write_end_tag(name)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    fn write_qualified_name(&mut self, name: &QualName) -> io::Result<()> {
        if let Some(ref prefix) = name.prefix {
            self.w
                .write_all(prefix.deref().to_ascii_lowercase().as_bytes())?;
            self.w.write_all(b":")?;
        }

        self.w
            .write_all(name.local.deref().to_ascii_lowercase().as_bytes())
    }

    fn write_start_tag(&mut self, name: &QualName, attrs: &[Attribute]) -> io::Result<()> {
        self.w.write_all(b"<")?;
        self.write_qualified_name(name)?;

        attrs
            .iter()
            .try_for_each(|attr| self.write_attribute(attr))?;

        self.w.write_all(b">")
    }

    fn write_end_tag(&mut self, name: &QualName) -> io::Result<()> {
        self.w.write_all(b"</")?;
        self.write_qualified_name(name)?;
        self.w.write_all(b">")
    }

    fn write_attribute(&mut self, attr: &Attribute) -> io::Result<()> {
        self.w.write_all(b" ")?;
        self.write_qualified_name(&attr.name)?;

        let value = attr.value.deref();

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
    match c {
        '\n' | '\t' => true,
        _ => c.is_whitespace(),
    }
}

fn omit_start_element(name: &LocalName) -> bool {
    match name {
        local_name!("body") | local_name!("head") | local_name!("html") => true,
        _ => false,
    }
}

fn is_inline_element(node: &Node) -> bool {
    match node.data {
        NodeData::Document => false,
        NodeData::Element { ref name, .. } => match name.local {
            local_name!("address")
            | local_name!("article")
            | local_name!("aside")
            | local_name!("blockquote")
            | local_name!("body")
            | local_name!("details")
            | local_name!("dialog")
            | local_name!("dd")
            | local_name!("div")
            | local_name!("dl")
            | local_name!("dt")
            | local_name!("fieldset")
            | local_name!("figcaption")
            | local_name!("figure")
            | local_name!("footer")
            | local_name!("form")
            | local_name!("h1")
            | local_name!("h2")
            | local_name!("h3")
            | local_name!("h4")
            | local_name!("h5")
            | local_name!("h6")
            | local_name!("head")
            | local_name!("header")
            | local_name!("hgroup")
            | local_name!("hr")
            | local_name!("li")
            | local_name!("link")
            | local_name!("main")
            | local_name!("meta")
            | local_name!("nav")
            | local_name!("ol")
            | local_name!("p")
            | local_name!("pre")
            | local_name!("section")
            | local_name!("table")
            | local_name!("title")
            | local_name!("ul") => false,
            _ => true,
        },
        _ => true,
    }
}

fn omit_end_element(name: &LocalName) -> bool {
    match name {
        local_name!("area")
        | local_name!("base")
        | local_name!("basefont")
        | local_name!("br")
        | local_name!("col")
        | local_name!("colgroup")
        | local_name!("dd")
        | local_name!("dt")
        | local_name!("frame")
        | local_name!("hr")
        | local_name!("img")
        | local_name!("input")
        | local_name!("isindex")
        | local_name!("li")
        | local_name!("link")
        | local_name!("meta")
        | local_name!("option")
        | local_name!("p")
        | local_name!("param")
        | local_name!("tbody")
        | local_name!("td")
        | local_name!("tfoot")
        | local_name!("th")
        | local_name!("thead")
        | local_name!("tr") => true,
        _ => false,
    }
}

fn can_have_children(name: &LocalName) -> bool {
    match name {
        local_name!("area")
        | local_name!("base")
        | local_name!("br")
        | local_name!("col")
        | local_name!("embed")
        | local_name!("hr")
        | local_name!("img")
        | local_name!("input")
        | local_name!("link")
        | local_name!("meta")
        | local_name!("param")
        | local_name!("source")
        | local_name!("track")
        | local_name!("wbr") => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    extern crate test;

    use self::test::Bencher;
    use super::*;

    const HTML: &str = "<html> \n<link href=\"test.css\">\n<h2   id=\"id_one\"    >Hello\n</h2>    \n<p>\nWorld</p>";

    #[test]
    fn test_minify() {
        const EXPECTED: &str =
            "<!doctype html><link href=test.css><h2 id=id_one>Hello</h2><p>World";

        assert_eq!(EXPECTED, HTML.minify().expect("Failed to minify HTML"));
    }

    #[bench]
    fn bench_minify(b: &mut Bencher) {
        let html = HTML.to_string();

        b.iter(|| {
            html.minify().expect("Failed to minify HTML");
        });
    }
}
