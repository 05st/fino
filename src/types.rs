use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};

use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{ast::Name, cache::DefinitionId, location::Location};

// It is important to note the distinction made between type variables and
// unification variables. A type variable is rigid, they are universally
// quantified, and they should not be present anywhere during unification. They
// are only introduced through type annotations as of now. When generalized
// types (type schemes) are instantiated, any occurence of a type variable will
// be replaced by a fresh unification variable. A unification variable is
// flexible, they are placeholders for a rigid, concrete type. This could be a
// type constant, function type, or a type variable as well.

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeVar(pub String);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct TypeUniVar(u32);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypePrim(pub String);

#[derive(Clone, Debug)]
pub struct TypeScheme(pub BTreeSet<TypeVar>, pub Type);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Var(TypeVar),
    UniVar(TypeUniVar),
    Prim(TypePrim), // Primitive type
    Const {
        // Type constant
        name: Name,
        location: Location,
        definition_id: Option<DefinitionId>,
    },
    App(Box<Type>, Box<Type>),
    Fun(Box<Type>, Box<Type>),
}

impl EqUnifyValue for Type {}

impl UnifyKey for TypeUniVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        todo!()
    }
}

// Macro for generating base type constructors
macro_rules! prim_types {
    ( $( $t:ident ),+ ) => {
        $(
            pub fn $t() -> Type {
                Type::Prim(TypePrim(String::from(stringify!($t))))
            }
        )+
    };
}

impl Type {
    // Insert primitive type constructors
    prim_types!(unit, bool, char, str, int, float);

    // Checks if self contains uvar, since unifying them would result in attempting
    // to construct an infinite type.
    pub fn occurs_check(&self, uvar: TypeUniVar) -> Result<(), Type> {
        match self {
            Type::Var(_) | Type::Prim(_) | Type::Const { .. } => Ok(()),

            Type::UniVar(uvar_self) => {
                if *uvar_self == uvar {
                    Err(Type::UniVar(*uvar_self))
                } else {
                    Ok(())
                }
            }

            Type::App(base, arg) => {
                base.occurs_check(uvar).map_err(|_| self.clone())?;
                arg.occurs_check(uvar).map_err(|_| self.clone())
            }

            Type::Fun(param, ret) => {
                param.occurs_check(uvar).map_err(|_| self.clone())?;
                ret.occurs_check(uvar).map_err(|_| self.clone())
            }
        }
    }

    // Collects type variables into a BTreeSet which is faster to iterate through
    // than a HashSet.
    pub fn extract_type_vars(&self, set: &mut BTreeSet<TypeVar>) {
        match self {
            Type::Var(tvar) => {
                set.insert(tvar.clone());
            }

            Type::UniVar(_) => (),
            Type::Prim(_) => (),
            Type::Const { .. } => (),

            Type::App(base, arg) => {
                base.extract_type_vars(set);
                arg.extract_type_vars(set);
            }

            Type::Fun(arg, ret) => {
                arg.extract_type_vars(set);
                ret.extract_type_vars(set);
            }
        }
    }

    pub fn substitute(&self, subst: &HashMap<Type, Type>) -> Type {
        match self {
            Type::Var(_) | Type::UniVar(_) | Type::Prim(_) | Type::Const { .. } => {
                match subst.get(&self) {
                    None => self.clone(),
                    Some(new) => new.clone(),
                }
            }

            Type::App(base, arg) => Type::App(
                Box::new(base.substitute(subst)),
                Box::new(arg.substitute(subst)),
            ),

            Type::Fun(arg, ret) => Type::Fun(
                Box::new(arg.substitute(subst)),
                Box::new(ret.substitute(subst)),
            ),
        }
    }

    pub fn flatten_fun_type(&self, params: &mut Vec<Type>) -> Type {
        if let Type::Fun(param, ret) = self {
            params.push(*param.clone());
            ret.flatten_fun_type(params)
        } else {
            self.clone()
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(type_var) => write!(f, "{}", type_var),
            Type::UniVar(uni_var) => write!(f, "{}", uni_var),
            Type::Prim(prim) => write!(f, "{}", prim),
            Type::Const {
                name,
                location: _,
                definition_id: _,
            } => write!(f, "{}", name.get_unqualified_part()),
            Type::App(base, arg) => write!(f, "{} {}", base, arg),
            Type::Fun(arg, ret) => write!(f, "{} -> {}", arg, ret),
        }
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for TypeUniVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl Display for TypePrim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
