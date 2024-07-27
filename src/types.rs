pub enum TypeVar {
    Bound(u32),
    Unbound(u32),
}

pub enum Type {
    Var(TypeVar),
    Unit,
    Int,
    Fun(Box<Self>, Box<Self>),
}
