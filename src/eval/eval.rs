use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Primitive, Statement},
    object::{
        environment::Environment, Boolean, Error, Float, Function, Hex, Integer, Object, ObjectType, Return, StringObj, FALSE, NULL, TRUE
    }, 
    token::Token
};

pub fn eval(program: crate::ast::Program, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;
    
    for statement in program.statements {
        object = eval_statement(statement, env.clone());

        if let Object::Return(return_) = object {
            return *return_.value;
        }

        if let Object::Error(error) = object {
            return Object::Error(error);
        }
    }

    object
}

pub fn eval_statement(statement: crate::ast::Statement, env: Rc<RefCell<Environment>>) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Block(block) => eval_block_statement(block, env),
        Statement::Return(return_) => eval_return_statement(return_, env),
        Statement::Let(let_) => eval_let_statement(let_, env),
    }
}

fn eval_expression(expression: crate::ast::Expression, env: Rc<RefCell<Environment>>) -> Object {
    match expression {
        Expression::Primitive(primitive) => {
            match primitive {
                Primitive::Int(value) => {
                    Object::Integer(Integer { value })
                },
                Primitive::Float(value) => {
                    Object::Float(Float { value })
                },
                Primitive::Hex(value) => {
                    Object::Hex(Hex { value })
                }
                Primitive::String(value) => {
                    Object::String(StringObj { value })
                }
                Primitive::Bool(value) => {
                    Object::Boolean(Boolean { value })
                }
            }
        },
        Expression::Prefix(prefix) => {
            let right = eval_expression(*prefix.right, env);

            if is_error(&right) {
                return right;
            }

            eval_prefix_expression(prefix.operator, right)
        },
        Expression::Infix(infix) => {
            let left = eval_expression(*infix.left, env.clone());

            if is_error(&left) {
                return left;
            }

            let right = eval_expression(*infix.right, env.clone());

            if is_error(&right) {
                return right;
            }

            eval_infix_expression(left, infix.operator, right)
        },
        Expression::Conditional(conditional) => {
            let condition = eval_expression(*conditional.condition, env.clone());

            if is_error(&condition) {
                return condition;
            }

            match condition {
                TRUE => eval_statement(Statement::Block(conditional.resolution), env.clone()),
                FALSE | NULL | Object::Integer(Integer { value: 0 }) => {
                    match conditional.alternative {
                        Some(alternative) => eval_statement(Statement::Block(alternative), env),
                        None =>NULL
                    }
                },
                Object::Integer(_) => eval_statement(Statement::Block(conditional.resolution), env),
                _ => NULL,
            }
        },
        Expression::Identifier(identifier) => {
            let val = env.borrow().get(&identifier.value);

            if let Some(val) = val {
                return val.clone();
            } else {
                Object::Error(Error::from(format!("identifier `{}` not found", identifier.value)))
            }
        },
        Expression::FunctionDefinition(func_def) => {
            // env.set(func_def.identifier.value.clone(), Object::Function(Function::from(func_def)))
            env.borrow_mut().set(func_def.identifier.value.clone(), Object::Function(Function::new(func_def, env.clone())))
        },
        Expression::FunctionCall(func_call) => {
            let function = eval_expression(*func_call.function, env.clone());

            if is_error(&function) {
                return function;
            }

            let args = eval_expressions(func_call.arguments, env.clone());

            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }

            apply_function(function, args)
        }
    }
}

fn eval_expressions(expressions: Vec<crate::ast::Expression>, env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut result = vec![];

    for expression in expressions {
        let evaluated = eval_expression(expression, env.clone());

        if is_error(&evaluated) {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn eval_prefix_expression(operator: crate::token::Token, right: Object) -> Object {
    match operator {
        Token::Bang => {
            match right {
                TRUE => FALSE,
                FALSE => TRUE,
                NULL => TRUE,
                Object::Integer(integer) => {
                    match integer.value {
                        0 => TRUE,
                        _ => FALSE,
                    }
                },
                _ => Object::Error(Error::from(format!("cannot apply unary operator `{}` to `{}`", operator.as_literal(), right._type()))),
            }
        },
        Token::Dash => {
            match right {
                Object::Integer(integer) => Object::Integer(Integer::from(-integer.value)),
                _ => Object::Error(Error::from(format!("cannot apply unary operator `{}` to `{}`", operator.as_literal(), right._type())))
            }
        },
        _ => Object::Error(Error::from(format!("unknown unary operator `{}`", operator.as_literal())))
    }
}

fn eval_infix_expression(left: Object, operator: crate::token::Token, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            let left_value = left.value;
            let right_value = right.value;

            match operator {
                Token::Plus => Object::Integer(Integer::from(left_value + right_value)),
                Token::Dash => Object::Integer(Integer::from(left_value - right_value)),
                Token::Asterisk => Object::Integer(Integer::from(left_value * right_value)),
                Token::ForwardSlash => Object::Integer(Integer::from(left_value / right_value)),
                Token::LessThan => Object::Boolean(Boolean::from(left_value < right_value)),
                Token::LessThanOrEqual => Object::Boolean(Boolean::from(left_value <= right_value)),
                Token::GreaterThan => Object::Boolean(Boolean::from(left_value > right_value)),
                Token::GreaterThanOrEqual => Object::Boolean(Boolean::from(left_value >= right_value)),
                Token::Equal => Object::Boolean(Boolean::from(left_value == right_value)),
                Token::NotEqual => Object::Boolean(Boolean::from(left_value != right_value)),
                _ => Object::Error(Error::from(
                    format!("no implementation for `{}` {} `{}`", ObjectType::Integer, operator.as_literal(), ObjectType::Integer)
                )),
            }
        },
        (Object::String(left), Object::String(right)) => {
            match operator {
                Token::Plus => Object::String(StringObj::from(format!("{}{}", left.value, right.value))),
                Token::Equal => Object::Boolean(Boolean::from(left.value == right.value)),
                Token::NotEqual => Object::Boolean(Boolean::from(left.value != right.value)),
                _ => Object::Error(Error::from(
                    format!("no implementation for `{}` {} `{}`", ObjectType::Integer, operator.as_literal(), ObjectType::Integer)
                )),
            }
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            let left_value = left.value;
            let right_value = right.value;

            match operator {
                Token::LessThan => Object::Boolean(Boolean::from(left_value < right_value)),
                Token::GreaterThan => Object::Boolean(Boolean::from(left_value > right_value)),
                Token::Equal => Object::Boolean(Boolean::from(left_value == right_value)),
                Token::NotEqual => Object::Boolean(Boolean::from(left_value != right_value)),
                _ => Object::Error(Error::from(
                    format!("no implementation for `{}` {} `{}`", ObjectType::Boolean, operator.as_literal(), ObjectType::Boolean)
                )),
            }
        }
        (left, right) => {
            match operator {
                Token::Plus => {
                    Object::Error(Error::from(
                        format!("cannot add `{}` to `{}`", left._type(), right._type())
                    ))
                },
                Token::Dash => {
                    Object::Error(Error::from(
                        format!("cannot subtract `{}` from `{}`", left._type(), right._type())
                    ))
                },
                Token::Asterisk => {
                    Object::Error(Error::from(
                        format!("cannot multiply `{}` by `{}`", left._type(), right._type())
                    ))
                },
                Token::ForwardSlash => {
                    Object::Error(Error::from(
                        format!("cannot divide `{}` by `{}`", left._type(), right._type())
                    ))
                },
                Token::LessThan | 
                Token::GreaterThan | 
                Token::Equal | 
                Token::NotEqual | 
                Token::LessThanOrEqual |
                Token::GreaterThanOrEqual => {
                    Object::Error(Error::from(
                        format!("mismatched types, expected `{}`, found `{}`", left._type(), right._type())
                    ))
                },
                _ => Object::Error(Error::from(
                    format!("expected operator, got `{}`", operator.as_literal()),
                )),
            }
        }
    }
}

fn eval_block_statement(block: crate::ast::Block, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;

    for statement in block.statements {
        object = eval_statement(statement, env.clone());

        if object._type() == ObjectType::Return || 
            object._type() == ObjectType::Error
        {
            return object;
        }
    }

    object
}

fn eval_return_statement(return_: crate::ast::Return, env: Rc<RefCell<Environment>>) -> Object {
    let return_value = eval_expression(return_.value, env);

    if is_error(&return_value) {
        return return_value;
    }

    Object::Return(Return { value: Box::new(return_value) })
}

fn eval_let_statement(let_: crate::ast::Let, env: Rc<RefCell<Environment>>) -> Object {
    let val = eval_expression(let_.value, env.clone());
    
    if is_error(&val) {
        return val;
    }

    env.borrow_mut().set(let_.name.value, val)
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    if let Object::Function(function) = function {
        function.parameters.iter()
            .zip(args.iter())
            .map(|(ident, arg)| {
                function.env.borrow_mut().set(ident.value.clone(), arg.to_owned())
            })
            .count();

        let evaluated = eval_block_statement(function.body, function.env.clone());

        if let Object::Return(Return { value }) = evaluated {
            *value
        } else {
            evaluated
        }

        
        // env.borrow_mut().in_new_scope(|scope | {
        //     function.parameters.iter()
        //         .zip(args.iter())
        //         .map(|(ident, arg)| {
        //             scope.set(ident.value.clone(), arg.to_owned())
        //         })
        //         .count();

        //     // function.env = scope.clone();

        //     println!("{:?}", &function.env);
            
        //     let evaluated = eval_block_statement(function.body, function.env);
            
        //     if let Object::Return(Return { value }) = evaluated {
        //         *value
        //     } else {
        //         evaluated
        //     }
        // })


        // env.in_new_scope(|scope| {
        //     function.parameters.iter()
        //         .zip(args.iter())
        //         .map(|(ident, arg)| {
        //             scope.set(ident.value.clone(), arg.to_owned())
        //         }
        //     ).count();

        //     let evaluated = eval_block_statement(function.body, scope);

        //     if let Object::Return(Return { value }) = evaluated {
        //         *value
        //     } else {
        //         evaluated
        //     }
        // })
    } else {
        Object::Error(
            Error::from(
                format!("cannot find function `{}` in this scope", function.inspect())
            )
        )
    }
}

fn is_error(object: &Object) -> bool {
    if let Object::Error(_) = &object {
        true
    } else {
        false
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use crate::{ 
        lexer::Lexer, 
        object::{environment::Environment, Boolean, Error, Integer, Object}, 
        parser::Parser
    };

    use super::eval;

    #[test]
    fn test_eval_integers() {
        let tests = vec![
            "5", 
            "10", 
            "-5", 
            "-10",
            "5 + 5 + 5 + 5 - 10",
            "2 * 2 * 2 * 2 * 2",
            "-50 + 100 + -50",
            "5 * 2 + 10",
            "5 + 2 * 10",
            "20 + 2 * -10",
            "50 / 2 * 2 + 10",
            "2 * (5 + 10)",
            "3 * 3 * 3 + 10",
            "(5 + 10 * 2 + 15 / 3) * 2 + -10"
        ];

        let expected = vec![
            5, 
            10, 
            -5, 
            -10,
            10,
            32,
            0,
            20,
            25,
            0,
            60,
            30,
            37,
            50
        ];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Integer(Integer { value }) = actual {
                    assert_eq!(expected, value, "Object has wrong value, got {}", value);
                } else {
                    panic!("Object is not Integer, got {:?}", actual);
                }
            });
    }

    #[test]
    fn test_eval_booleans() {
        let tests = vec!["true", "false", "1 < 2", "1 > 2", "1 == 1", "2 != 1", "1 == 2", "2 != 2", "true == true", "false != false"];

        let expected = vec![true, false, true, false, true, true, false, false, true, false];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Boolean(Boolean { value }) = actual {
                    assert_eq!(expected, value, "Object has wrong value, got {}", value);
                } else {
                    panic!("Object is not Boolean, got {:?}", actual);
                }
            });
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec!["!true", "!false", "!5", "!0", "!!true", "!!false", "!!5"];

        let expected = vec![false, true, false, true, true, false, true];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Boolean(Boolean { value }) = actual {
                    assert_eq!(expected, value, "Object has wrong value, got {}", value);
                } else {
                    panic!("Object is not Boolean, got {:?}", actual);
                }
            });
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1) { 10 }",
            "if (0) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 }",
            "if (1 < 2) { 10 } else { 20 }",
            "if (1 > 2) { 10 } else { 20 }",
        ];

        let expected = vec![
            "10",
            "null",
            "10",
            "null",
            "10",
            "null",
            "10",
            "20"
        ];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                assert_eq!(expected.to_string(), actual.inspect(), "Got wrong resolution, expected: {}, got: {}", expected, actual.inspect());
            });
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9",
            "2 * 4; return 2 * 5; 9",
            "if (10 > 1) { if (10 > 1) { return 10; } return 1; }"
        ];

        let expected = vec![
            10,
            10,
            10,
            10,
            10
        ];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Integer(Integer { value }) = actual {
                    assert_eq!(expected, value, "Integer has wrong value, got {}", value);
                } else {
                    panic!("Object is not Integer, got {:?}", actual);
                }
            });
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            "let a = 5; a;",
            "let a = 5 * 5; a;",
            "let a = 5; let b = a; b;",
            "let a = 5; let b = a; let c = a + b + 5; c;"
        ];

        let expected = vec![
            5,
            25,
            5,
            15
        ];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Integer(Integer { value }) = actual {
                    assert_eq!(expected, value, "Integer has wrong value, got {}", value);
                } else {
                    panic!("Object is not Integer, got {:?}", actual);
                }
            });
    }

    #[test]
    fn test_function_object() {
        let input = "fn main() { return 1; }";

        let result = test_eval(input.to_string());

        if let Object::Function(function) = result {
            if function.identifier != String::from("main") {
                panic!("Function identifier is not `main`, got {}", function.identifier);
            }

            if function.parameters.len() > 0 {
                panic!("Function has more than 0 parameters, got {:?}", function.parameters.len());
            }

            if function.body.to_string() != String::from("{ return 1; }") {
                panic!("Function has wrong body, got {}", function.body.to_string());
            }
        } else {
            panic!("Object is not Function, got {:?}", result);
        }
    }

    #[test]
    fn test_function_statements() {
        let tests = vec![
            "fn add(one, two) { return one + two; }; add(1, 2);",
            "fn add(one, two) { return one + two; }; add(1, add(1, 2));",
            "fn add_three(x) { return x + 3; }; add_three(5);",
            "(fn add(one, two) { one + two; })(1, 2)",
            "fn ident() { 1 }; ident();"
        ];

        let expected = vec![
            3,
            4,
            8,
            3,
            1
        ];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Integer(Integer { value }) = actual {
                    assert_eq!(expected, value, "Integer has wrong value, got {}", value);
                } else {
                    panic!("Object is not Integer, got {:?}", actual);
                }
            });
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            "5 + true;",
            "5 * true; 5;",
            "-true",
            "true + false;",
            "if (10 > 1) { true / false; }",
            "foobar"
        ];

        let expected = vec![
            "cannot add `Integer` to `Boolean`",
            "cannot multiply `Integer` by `Boolean`",
            "cannot apply unary operator `-` to `Boolean`",
            "no implementation for `Boolean` + `Boolean`",
            "no implementation for `Boolean` / `Boolean`",
            "identifier `foobar` not found",
        ];

        let results = tests.iter().map(|input| {
            test_eval(input.to_string())
        }).collect::<Vec<_>>();

        expected.iter()
            .zip(results.iter())
            .for_each(|(expected, actual)| {
                if let Object::Error(Error { message }) = actual {
                    assert_eq!(&expected.to_string(), message, "Error has wrong message, got {}", message);
                } else {
                    panic!("Object is not Error, got {:?}", actual);
                }
            });
    }

    fn test_eval(input: String) -> Object {
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        let env = Rc::new(RefCell::new(Environment::new(None)));

        eval(program, env)
    }
}