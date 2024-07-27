use crate::types::Type;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Var(u32);

pub struct TypedVar(pub Var, pub Type);

pub enum Ast<V> {
    Var(V),
    Int(i64),
    Lam(V, Box<Self>),
    App(Box<Self>, Box<Self>),
}
