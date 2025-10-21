use pretty::RcDoc;

pub trait PrettyDoc {
    fn to_doc<'a>(&'a self) -> RcDoc<'a>;
}

impl PrettyDoc for String {
    fn to_doc(&self) -> RcDoc<'_> {
        RcDoc::text(self)
    }
}

impl PrettyDoc for &String {
    fn to_doc(&self) -> RcDoc<'_> {
        RcDoc::text(*self)
    }
}

impl PrettyDoc for &str {
    fn to_doc(&self) -> RcDoc<'_> {
        RcDoc::text(*self)
    }
}

impl<T: PrettyDoc> PrettyDoc for Option<T> {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Some(v) => RcDoc::text("Some")
                .append(RcDoc::space())
                .append(v.to_doc()),
            None => RcDoc::text("None"),
        }
    }
}

impl PrettyDoc for bool {
    fn to_doc(&self) -> RcDoc<'_> {
        RcDoc::as_string(self)
    }
}

impl<T: PrettyDoc> PrettyDoc for Vec<T> {
    fn to_doc(&self) -> RcDoc<'_> {
        paren_list_printer(self)
    }
}

pub fn pretty_print_doc(doc: &RcDoc<'_>) -> String {
    let mut w = Vec::new();
    doc.render(120, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

pub fn pretty_print_width<T: PrettyDoc>(width: usize, item: &T) -> String {
    let doc = item.to_doc();
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

pub fn pretty_print<T: PrettyDoc>(item: &T) -> String {
    pretty_print_width(120, item)
}

pub fn string_printer<'a, T: PrettyDoc>(obj: &'a T) -> RcDoc<'a> {
    let obj_s = pretty_print(obj)
        .replace('\\', "\\\\")
        .replace('\"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t");
    RcDoc::text("\"").append(obj_s).append(RcDoc::text("\""))
}

pub fn option_printer<'a, T: PrettyDoc>(obj: &'a Option<T>) -> RcDoc<'a> {
    match obj {
        Some(v) => RcDoc::text("Some")
            .append(RcDoc::space())
            .append(v.to_doc()),
        None => RcDoc::text("None"),
    }
}

pub fn option_printer_apply<'a, T: PrettyDoc>(
    f: impl Fn(&'a T) -> RcDoc<'a>,
    obj: &'a Option<T>,
) -> RcDoc<'a> {
    match obj {
        Some(v) => RcDoc::text("Some").append(RcDoc::space()).append(f(v)),
        None => RcDoc::text("None"),
    }
}

pub fn named_object_printer<'a>(name: &'a str, fields: Vec<(&'a str, RcDoc<'a>)>) -> RcDoc<'a> {
    let fields_doc = RcDoc::intersperse(
        fields.into_iter().map(|(field_name, field_doc)| {
            RcDoc::text(field_name)
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(field_doc)
        }),
        RcDoc::text(",").append(RcDoc::hardline()),
    );

    RcDoc::text(name)
        .append(RcDoc::space())
        .append(RcDoc::text("{"))
        .append(RcDoc::hardline().append(fields_doc).nest(2))
        .append(RcDoc::hardline())
        .append(RcDoc::text("}"))
        .group()
}

pub fn paren_doc_list_printer<'a>(items: &'a Vec<RcDoc<'a>>) -> RcDoc<'a> {
    let items_doc = RcDoc::intersperse(
        items.iter().cloned(),
        RcDoc::text(",").append(RcDoc::line()),
    );
    RcDoc::text("(")
        .append(RcDoc::softline().append(items_doc).nest(2))
        .append(RcDoc::softline())
        .append(RcDoc::text(")"))
        .group()
}

pub fn paren_list_printer<'a, T, C>(items: C) -> RcDoc<'a>
where
    T: PrettyDoc + 'a,
    C: IntoIterator<Item = &'a T>,
{
    let items_doc = RcDoc::intersperse(
        items.into_iter().map(|item| item.to_doc()),
        RcDoc::text(",").append(RcDoc::line()),
    );
    RcDoc::text("(")
        .append(RcDoc::softline().append(items_doc).nest(2))
        .append(RcDoc::softline())
        .append(RcDoc::text(")"))
        .group()
}

pub fn paren_list_printer_apply<'a, T, C>(f: impl Fn(&'a T) -> RcDoc<'a>, items: C) -> RcDoc<'a>
where
    T: 'a,
    C: IntoIterator<Item = &'a T>,
{
    let items_doc = RcDoc::intersperse(
        items.into_iter().map(|item| f(item)),
        RcDoc::text(",").append(RcDoc::line()),
    );
    RcDoc::text("(")
        .append(RcDoc::softline().append(items_doc).nest(2))
        .append(RcDoc::softline())
        .append(RcDoc::text(")"))
        .group()
}

pub fn paren_string_list_printer(items: &Vec<String>) -> RcDoc<'_> {
    let items_doc = RcDoc::intersperse(
        items.iter().map(|item| RcDoc::text(item)),
        RcDoc::text(",").append(RcDoc::line()),
    );
    RcDoc::text("(")
        .append(RcDoc::softline().append(items_doc).nest(2))
        .append(RcDoc::softline())
        .append(RcDoc::text(")"))
        .group()
}

pub fn named_tuple_printer<'a>(name: &'a str, items: Vec<RcDoc<'a>>) -> RcDoc<'a> {
    let items_doc = RcDoc::intersperse(items.into_iter(), RcDoc::text(",").append(RcDoc::line()));
    RcDoc::text(name)
        .append(RcDoc::space())
        .append(RcDoc::text("("))
        .append(RcDoc::softline().append(items_doc).nest(2))
        .append(RcDoc::softline())
        .append(RcDoc::text(")"))
        .group()
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestStruct {
        name: String,
        value: i32,
    }

    impl PrettyDoc for TestStruct {
        fn to_doc(&self) -> RcDoc<'_> {
            named_object_printer(
                "TestStruct",
                vec![
                    ("name", RcDoc::text(&self.name)),
                    ("value", RcDoc::as_string(self.value)),
                ],
            )
        }
    }

    #[test]
    fn test_pretty_print() {
        let test = TestStruct {
            name: "example".to_string(),
            value: 42,
        };
        let output = pretty_print(&test);
        let expected = "TestStruct {\n  name = example,\n  value = 42\n}";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_string_printer() {
        let s = "Hello, \"world\"!";
        let doc = string_printer(&s);
        let mut w = Vec::new();
        doc.render(80, &mut w).unwrap();
        let output = String::from_utf8(w).unwrap();
        println!("{}", output);
        let expected = "\"Hello, \\\"world\\\"!\"";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_paren_list_printer() {
        let items = vec!["item1", "item2", "item3"];
        let doc = paren_list_printer(&items);
        let mut w = Vec::new();
        doc.render(80, &mut w).unwrap();
        let output = String::from_utf8(w).unwrap();
        let expected = "( item1, item2, item3 )";
        assert_eq!(output, expected);

        let items = vec![
            "a_very_long_item_name_1",
            "a_very_long_item_name_2",
            "a_very_long_item_name_3",
        ];
        let doc = paren_list_printer(&items);
        let mut w = Vec::new();
        doc.render(30, &mut w).unwrap();
        let output = String::from_utf8(w).unwrap();
        let expected =
            "( a_very_long_item_name_1,\n  a_very_long_item_name_2,\n  a_very_long_item_name_3 )";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_named_tuple_printer() {
        let items = vec![
            RcDoc::text("item1"),
            RcDoc::text("item2"),
            RcDoc::text("item3"),
        ];
        let doc = named_tuple_printer("MyTuple", items);
        let mut w = Vec::new();
        doc.render(80, &mut w).unwrap();
        let output = String::from_utf8(w).unwrap();
        let expected = "MyTuple ( item1, item2, item3 )";
        assert_eq!(output, expected);
    }
}
