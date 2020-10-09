#![warn(missing_docs)]
#![deny(warnings, clippy::pedantic, clippy::nursery)]

//! HTML5 markup minifier.

use std::io;
use std::rc::Rc;
use std::str;

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
    Minifier::new(w).minify(&mut r)
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

/// Minifier implementation for `io::Write`.
#[allow(clippy::struct_excessive_bools)]
pub struct Minifier<'a, W: io::Write> {
    w: &'a mut W,
    omit_doctype: bool,
    collapse_whitespace: bool,
    preserve_comments: bool,
    preceding_whitespace: bool,
}

/// Holds node positional context.
struct Context<'a> {
    parent: &'a Node,
    parent_context: Option<&'a Context<'a>>,
    left: Option<&'a [Rc<Node>]>,
    right: Option<&'a [Rc<Node>]>,
}

impl<'a> Context<'a> {
    /// Determine whether to trim whitespace.
    /// Uses naive HTML5 whitespace collapsing rules.
    fn trim(&self, preceding_whitespace: bool) -> (bool, bool) {
        (preceding_whitespace || self.trim_left(), self.trim_right())
    }

    fn trim_left(&self) -> bool {
        self.left.map_or_else(
            || {
                is_block_element(self.parent)
                    || self.parent_context.map_or(true, Context::trim_left)
            },
            |siblings| {
                siblings
                    .iter()
                    .rev()
                    .find_map(Self::element_name)
                    .map_or_else(
                        || {
                            // Should short circuit in the majority of cases
                            self.parent_context.map_or(true, Context::trim_left)
                        },
                        is_block_element_name,
                    )
            },
        )
    }

    fn trim_right(&self) -> bool {
        self.right.map_or(true, |siblings| {
            siblings
                .iter()
                .find_map(Self::element_name)
                .map_or(true, is_block_element_name)
        })
    }

    fn element_name(node: &Rc<Node>) -> Option<&str> {
        if let NodeData::Element { name, .. } = &node.data {
            Some(name.local.as_ref())
        } else {
            None
        }
    }
}

impl<'a, W> Minifier<'a, W>
where
    W: io::Write,
{
    /// Creates a new `Minifier` instance.
    #[inline]
    pub fn new(w: &'a mut W) -> Self {
        Self {
            w,
            omit_doctype: false,
            collapse_whitespace: true,
            preserve_comments: false,
            preceding_whitespace: false,
        }
    }

    /// Collapse whitespace between elements and in text when whitespace isn't preserved by default.
    /// Enabled by default.
    #[inline]
    pub fn collapse_whitespace(&mut self, collapse: bool) -> &mut Self {
        self.collapse_whitespace = collapse;
        self
    }

    /// Omit writing the HTML5 doctype.
    /// Disabled by default.
    #[inline]
    pub fn omit_doctype(&mut self, omit: bool) -> &mut Self {
        self.omit_doctype = omit;
        self
    }

    /// Preserve HTML comments.
    /// Disabled by default.
    #[inline]
    pub fn preserve_comments(&mut self, preserve: bool) -> &mut Self {
        self.preserve_comments = preserve;
        self
    }

    /// Minifies the given reader input.
    ///
    /// # Errors
    ///
    /// Will return `Err` if unable to write to the output writer.
    #[inline]
    pub fn minify<R: io::Read>(&mut self, mut r: &mut R) -> io::Result<()> {
        let dom = parse_document(RcDom::default(), ParseOpts::default())
            .from_utf8()
            .read_from(&mut r)?;

        if !self.omit_doctype {
            self.w.write_all(b"<!doctype html>")?;
        }

        self.minify_node(None, &dom.document)
    }

    fn minify_node<'b>(&mut self, ctx: Option<Context<'b>>, node: &'b Node) -> io::Result<()> {
        match &node.data {
            NodeData::Text { contents } => {
                // Check if whitespace collapsing disabled
                let contents = contents.borrow();
                let contents = contents.as_ref();

                if !self.collapse_whitespace {
                    return self.w.write_all(contents.as_bytes());
                }

                // Check if parent is whitespace preserving element or contains code (<script>, <style>)
                let (skip_collapse_whitespace, contains_code) =
                    ctx.as_ref().map_or((false, false), |ctx| {
                        if let NodeData::Element { name, .. } = &ctx.parent.data {
                            let name = name.local.as_ref();

                            (preserve_whitespace(name), contains_code(name))
                        } else {
                            (false, false)
                        }
                    });

                if skip_collapse_whitespace {
                    return self.w.write_all(contents.as_bytes());
                }

                if contains_code {
                    return self
                        .w
                        .write_all(contents.trim_matches(is_ascii_whitespace).as_bytes());
                }

                // Early exit if empty to forego expensive trim logic
                if contents.is_empty() {
                    return io::Result::Ok(());
                }

                let (trim_left, trim_right) = ctx
                    .as_ref()
                    .map_or((true, true), |ctx| ctx.trim(self.preceding_whitespace));
                let contents = match (trim_left, trim_right) {
                    (true, true) => contents.trim_matches(is_ascii_whitespace),
                    (true, false) => contents.trim_start_matches(is_ascii_whitespace),
                    (false, true) => contents.trim_end_matches(is_ascii_whitespace),
                    _ => contents,
                };

                // Second empty check after trimming whitespace
                if !contents.is_empty() {
                    self.write_collapse_whitespace(
                        contents.as_bytes(),
                        check_reserved_entity,
                        None,
                    )?;

                    self.preceding_whitespace = !trim_right
                        && contents
                            .as_bytes()
                            .iter()
                            .last()
                            .map_or(false, u8::is_ascii_whitespace);
                }

                Ok(())
            }

            NodeData::Comment { contents } if self.preserve_comments => {
                self.w.write_all(b"<!--")?;
                self.w.write_all(contents.as_bytes())?;
                self.w.write_all(b"-->")
            }

            NodeData::Document => self.minify_children(ctx, node),

            NodeData::Element { name, attrs, .. } => {
                let attrs = attrs.borrow();
                let tag = name.local.as_ref();
                let omit_start_element = attrs.is_empty() && omit_start_element(tag);

                if !omit_start_element {
                    self.write_start_tag(name, &attrs)?;
                }

                if !is_self_closing(tag) {
                    self.minify_children(ctx, node)?;

                    if !optional_end_element(tag) {
                        self.write_end_tag(name)?;
                    }
                }

                Ok(())
            }

            _ => Ok(()),
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn minify_children(&mut self, ctx: Option<Context>, node: &Node) -> io::Result<()> {
        let children = node.children.borrow();
        let l = children.len();

        children.iter().enumerate().try_for_each(|(i, child)| {
            if self.preceding_whitespace && is_block_element(child) {
                self.preceding_whitespace = false;
            }

            self.minify_node(
                Some(Context {
                    parent: node,
                    parent_context: ctx.as_ref(),
                    left: if i > 0 { Some(&children[..i]) } else { None },
                    right: if i + 1 < l {
                        Some(&children[i + 1..])
                    } else {
                        None
                    },
                }),
                child,
            )
        })
    }

    fn write_qualified_name(&mut self, name: &QualName) -> io::Result<()> {
        if let Some(prefix) = &name.prefix {
            self.w
                .write_all(prefix.as_ref().to_ascii_lowercase().as_bytes())?;
            self.w.write_all(b":")?;
        }

        self.w
            .write_all(name.local.as_ref().to_ascii_lowercase().as_bytes())
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

        let value = attr.value.as_ref();
        let value = if self.collapse_whitespace {
            value.trim_matches(is_ascii_whitespace)
        } else {
            value
        };

        if value.is_empty() {
            return io::Result::Ok(());
        }

        self.w.write_all(b"=")?;

        let b = value.as_bytes();
        let (unquoted, double, single) =
            b.iter()
                .fold((true, false, false), |(unquoted, double, single), &c| {
                    let (double, single) = (double || c == b'"', single || c == b'\'');
                    let unquoted =
                        unquoted && !double && !single && c != b'=' && !c.is_ascii_whitespace();

                    (unquoted, double, single)
                });

        match (unquoted, double, single) {
            (true, ..) => self.w.write_all(b),
            (_, true, true) => self.write_attribute_value(b, b"'", check_reserved_entity_with_apos),
            (_, true, false) => {
                self.write_attribute_value(b, b"'", check_reserved_entity_with_apos)
            }
            _ => self.write_attribute_value(b, b"\"", check_reserved_entity),
        }
    }

    fn write_attribute_value<T: AsRef<[u8]>>(
        &mut self,
        v: T,
        quote: &[u8],
        f: EntityFn,
    ) -> io::Result<()> {
        self.w.write_all(quote)?;

        let b = v.as_ref();

        if self.collapse_whitespace {
            self.write_collapse_whitespace(b, f, Some(false))
        } else {
            self.w.write_all(b)
        }?;

        self.w.write_all(quote)
    }

    /// Efficiently writes blocks of content, e.g. a string with no collapsed
    /// whitespace would result in a single write.
    fn write_collapse_whitespace(
        &mut self,
        b: &[u8],
        f: EntityFn,
        preceding_whitespace: Option<bool>,
    ) -> io::Result<()> {
        let mut pos = 0;
        let mut preceding_whitespace = preceding_whitespace.unwrap_or(self.preceding_whitespace);

        b.iter().enumerate().try_for_each(|(i, &c)| {
            let is_whitespace = c.is_ascii_whitespace();

            if is_whitespace && preceding_whitespace {
                if i != pos {
                    self.write(&b[pos..i], f)?;
                }

                // ASCII whitespace = 1 byte
                pos = i + 1;
            }

            preceding_whitespace = is_whitespace;

            Ok::<_, io::Error>(())
        })?;

        if pos < b.len() {
            self.write(&b[pos..], f)?;
        }

        Ok(())
    }

    fn write(&mut self, b: &[u8], f: EntityFn) -> io::Result<()> {
        let mut pos = 0;

        b.iter().enumerate().try_for_each(|(i, &c)| {
            if let Some(entity) = f(c) {
                self.w.write_all(&b[pos..i])?;
                self.w.write_all(entity)?;

                // Reserved characters are 1 byte
                pos = i + 1;
            }

            Ok::<_, io::Error>(())
        })?;

        // Write remaining bytes
        if pos < b.len() {
            self.w.write_all(&b[pos..])?;
        }

        Ok(())
    }
}

type EntityFn = fn(u8) -> Option<&'static [u8]>;

const fn check_reserved_entity(v: u8) -> Option<&'static [u8]> {
    match v {
        b'<' => Some(b"&lt;"),
        b'>' => Some(b"&gt;"),
        b'&' => Some(b"&#38;"),
        _ => None,
    }
}

fn check_reserved_entity_with_apos(v: u8) -> Option<&'static [u8]> {
    if v == b'\'' {
        Some(b"&#39;")
    } else {
        check_reserved_entity(v)
    }
}

fn omit_start_element(name: &str) -> bool {
    matches!(name, "body" | "head" | "html")
}

fn is_block_element_name(name: &str) -> bool {
    matches!(
        name,
        "address"
            | "article"
            | "aside"
            | "blockquote"
            | "body"
            | "br"
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
            | "html"
            | "li"
            | "link"
            | "main"
            | "meta"
            | "nav"
            | "ol"
            | "option"
            | "p"
            | "pre"
            | "script"
            | "section"
            | "source"
            | "table"
            | "td"
            | "th"
            | "title"
            | "tr"
            | "ul"
    )
}

fn is_block_element(node: &Node) -> bool {
    match &node.data {
        NodeData::Element { ref name, .. } => is_block_element_name(name.local.as_ref()),
        NodeData::Document => true,
        _ => false,
    }
}

#[allow(clippy::missing_const_for_fn)]
fn is_ascii_whitespace(c: char) -> bool {
    c.is_ascii_whitespace()
}

fn preserve_whitespace(name: &str) -> bool {
    matches!(name, "pre" | "textarea")
}

fn contains_code(name: &str) -> bool {
    matches!(name, "script" | "style")
}

fn is_self_closing(name: &str) -> bool {
    matches!(
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
            | "command"
            | "keygen"
            | "menuitem"
    )
}

fn optional_end_element(name: &str) -> bool {
    matches!(
        name,
        "basefont"
            | "body"
            | "colgroup"
            | "dd"
            | "dt"
            | "frame"
            | "head"
            | "html"
            | "isindex"
            | "li"
            | "option"
            | "p"
            | "tbody"
            | "td"
            | "tfoot"
            | "th"
            | "thead"
            | "tr"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::str;

    use glob::glob;

    #[test]
    fn test_minify() {
        let entries = glob("testdata/*.html").expect("Failed to read glob pattern");

        for path in entries {
            let path = path.expect("Failed to get entry");

            if path.is_dir() {
                continue;
            }

            let html = fs::read_to_string(&path).expect("Failed to read HTML");
            let path = path.to_string_lossy().to_string();
            let minified_expected =
                fs::read_to_string(path + ".minified").expect("Failed to read minified HTML");
            let minified = html.minify().expect("Failed to minify HTML");
            let minified = str::from_utf8(&minified).expect("Failed to convert to string");

            assert_eq!(minified_expected, minified);
        }
    }

    #[test]
    fn test_write_collapse_whitespace() {
        [
            ("", "", false),
            ("  ", " ", false),
            ("   ", " ", false),
            ("   ", "", true),
            (" x      y  ", " x y ", false),
            (" x      y  ", "x y ", true),
            (" x   \n  \t \n   y  ", " x y ", false),
            (" x   \n  \t \n   y  ", "x y ", true),
        ]
        .iter()
        .for_each(|&(input, expected, preceding_whitespace)| {
            let mut w = vec![];
            let mut minifier = Minifier::new(&mut w);
            minifier.preceding_whitespace = preceding_whitespace;
            minifier
                .write_collapse_whitespace(
                    input.as_bytes(),
                    check_reserved_entity,
                    Some(preceding_whitespace),
                )
                .expect("Failed to write string");

            let s = str::from_utf8(&w).expect("Failed to convert to string");

            assert_eq!(expected, s);
        });
    }
}
