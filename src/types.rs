use ena::unify::{EqUnifyValue, UnifyKey};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum TypeVar {
    Bound(u32),
    Unbound(u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Var(TypeVar),
    Unit,
    Int,
    Fun(Box<Self>, Box<Self>),
}

impl EqUnifyValue for Type {}

impl UnifyKey for TypeVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        match self {
            TypeVar::Bound(id) => *id,
            TypeVar::Unbound(id) => *id,
        }
    }

    fn from_index(id: u32) -> Self {
        Self::Unbound(id)
    }

    fn tag() -> &'static str {
        todo!()
    }
}
