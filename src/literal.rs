// Split into own module so we don't duplicate code between HIR and MIR
#[derive(Clone, Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Unit,
}
