use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token("module")]
    KwModule,
    #[token("import")]
    KwImport,
    #[token("export")]
    KwExport,

    #[token("fn")]
    KwFn,
    #[token("type")]
    KwType,

    #[token("let")]
    KwLet,
    #[token("in")]
    KwIn,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("match")]
    KwMatch,
    #[token("do")]
    KwDo,

    #[token("unit")]
    KwUnit,
    #[token("bool")]
    KwBool,
    #[token("char")]
    KwChar,
    #[token("str")]
    KwStr,
    #[token("i32")]
    KwI32,
    #[token("i64")]
    KwI64,
    #[token("u32")]
    KwU32,
    #[token("u64")]
    KwU64,
    #[token("f32")]
    KwF32,
    #[token("f64")]
    KwF64,

    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    LitString(String),

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,

    #[token(".")]
    Dot,
    #[token("+")]
    Plus,
    #[token("|")]
    Bar,

    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,

    #[token("\\")]
    Backslash,

    #[token("->")]
    SmallArrow,
    #[token("=>")]
    BigArrow,

    #[regex(r"\n")]
    Newline,
    /* Note: We require indentations to be done with spaces
     *       since the lexer determines if whitespace is
     *       valid indentation by checking if the number of
     *       whitespace characters is greater than or equal
     *       to two. A tab character would only count as one
     *       space.
     */
    #[regex(r"[ \t]+", |lex| lex.slice().chars().count() >= 2)]
    Space(bool),

    #[regex("[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Ident(String),
}

pub fn parse(input: String) {
    for res in Token::lexer(&input) {
        match res {
            Ok(token) => println!("{:#?}", token),
            Err(_) => panic!("Error"),
        }
    }
}
