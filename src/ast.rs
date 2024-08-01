use crate::types::Type;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Var(pub u32);

#[derive(Debug)]
pub struct TypedVar(pub Var, pub Type);

#[derive(Debug)]
pub enum Expr<V> {
    Var(V),
    Int(i64),
    Lam(V, Box<Self>),
    App(Box<Self>, Box<Self>),
}


// An item is a top-level let-definition.
// Functions are desugared into curried
// lambda expressions by the parser.
#[derive(Debug)]
struct Item<V> {
    name: String,
    type_ann: Type,
    expr: Expr<V>,
}
