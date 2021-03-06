use crate::UserInputReader::FakeStdins;
use std::collections::btree_map::BTreeMap;
use std::collections::hash_map::Entry;
use std::collections::Bound::Unbounded;
use std::collections::HashMap;
use std::iter::zip;
use std::ops;
use std::ops::Bound::{Excluded, Included};
use std::result::Result;
use std::vec::Vec;

#[derive(Clone, Debug)]
enum Expression {
    Integer(i32),
    Text(String),
    NumberVariable(String),
    StringVariable(String),
    Plus(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    AreEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    Slice(
        Box<Expression>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
    ),
    Len(Box<Expression>),
    Number(f64),
    Str(Box<Expression>),
    Sgn(Box<Expression>),
    Abs(Box<Expression>),
    Int(Box<Expression>),
    Sqr(Box<Expression>),
    Fn(String, Vec<Expression>),
    RaiseTo(Box<Expression>, Box<Expression>),
    Exp(Box<Expression>),
    Ln(Box<Expression>),
    Sin(Box<Expression>),
    Cos(Box<Expression>),
    Tan(Box<Expression>),
    Asn(Box<Expression>),
    Acs(Box<Expression>),
    Atn(Box<Expression>),
    Rnd,
    NumberArrayElement(String, Vec<Expression>),
    StringArrayElement(String, Vec<Expression>),
}

impl ops::Add for Expression {
    type Output = Expression;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Plus(Box::from(self), Box::from(rhs))
    }
}

impl ops::Sub for Expression {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Subtract(Box::from(self), Box::from(rhs))
    }
}

impl ops::Mul for Expression {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Multiply(Box::from(self), Box::from(rhs))
    }
}

impl ops::Div for Expression {
    type Output = Expression;

    fn div(self, rhs: Self) -> Self::Output {
        Expression::Divide(Box::from(self), Box::from(rhs))
    }
}

impl Expression {
    fn text(value: &str) -> Expression {
        Expression::Text(value.to_string())
    }
    fn number_variable(name: &str) -> Expression {
        Expression::NumberVariable(name.to_string())
    }
    fn string_variable(name: &str) -> Expression {
        Expression::StringVariable(name.to_string())
    }
    fn greater_than(&self, other: &Expression) -> Expression {
        Expression::GreaterThan(Box::from(self.clone()), Box::from(other.clone()))
    }
    fn are_equal(&self, other: &Expression) -> Expression {
        Expression::AreEqual(Box::from(self.clone()), Box::from(other.clone()))
    }
    fn less_than(&self, other: &Expression) -> Expression {
        Expression::LessThan(Box::from(self.clone()), Box::from(other.clone()))
    }
    fn slice(&self, begin: Option<Expression>, end: Option<Expression>) -> Expression {
        Expression::Slice(
            Box::from(self.clone()),
            begin.map(Box::from),
            end.map(Box::from),
        )
    }
    fn len(&self) -> Expression {
        Expression::Len(Box::from(self.clone()))
    }
    fn str(&self) -> Expression {
        Expression::Str(Box::from(self.clone()))
    }
    fn sgn(&self) -> Expression {
        Expression::Sgn(Box::from(self.clone()))
    }
    fn abs(&self) -> Expression {
        Expression::Abs(Box::from(self.clone()))
    }
    fn int(&self) -> Expression {
        Expression::Int(Box::from(self.clone()))
    }
    fn sqr(&self) -> Expression {
        Expression::Sqr(Box::from(self.clone()))
    }
    fn function(name: &str, parameter_values: Vec<Expression>) -> Expression {
        Expression::Fn(name.to_string(), parameter_values)
    }
    fn raise_to(&self, power: Expression) -> Expression {
        Expression::RaiseTo(Box::from(self.clone()), Box::from(power))
    }
    fn exp(&self) -> Expression {
        Expression::Exp(Box::from(self.clone()))
    }
    fn ln(&self) -> Expression {
        Expression::Ln(Box::from(self.clone()))
    }
    fn sin(&self) -> Expression {
        Expression::Sin(Box::from(self.clone()))
    }
    fn cos(&self) -> Expression {
        Expression::Cos(Box::from(self.clone()))
    }
    fn tan(&self) -> Expression {
        Expression::Tan(Box::from(self.clone()))
    }
    fn asn(&self) -> Expression {
        Expression::Asn(Box::from(self.clone()))
    }
    fn acs(&self) -> Expression {
        Expression::Acs(Box::from(self.clone()))
    }
    fn atn(&self) -> Expression {
        Expression::Atn(Box::from(self.clone()))
    }
    fn number_array_element(name: &str, lengths: Vec<Expression>) -> Expression {
        Expression::NumberArrayElement(name.to_string(), lengths)
    }
    fn string_array_element(name: &str, lengths: Vec<Expression>) -> Expression {
        Expression::StringArrayElement(name.to_string(), lengths)
    }
    fn to_string(&self, env: &mut Env) -> Result<String, String> {
        match self {
            Expression::Integer(value) => Ok(value.to_string()),
            Expression::Text(text) => Ok(text.to_string()),
            Expression::NumberVariable(number_variable_name) => Ok(env
                .number_variables
                .get(number_variable_name)
                .unwrap()
                .to_string()),
            Expression::StringVariable(string_variable_bname) => Ok(env
                .string_variables
                .get(string_variable_bname)
                .unwrap()
                .to_string()),
            Expression::Plus(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Subtract(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Multiply(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Divide(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::AreEqual(_, _) => self.to_string(env).map(|v| v.to_string()),
            Expression::LessThan(_, _) => self.to_string(env).map(|v| v.to_string()),
            Expression::GreaterThan(_, _) => self.to_string(env).map(|v| v.to_string()),
            Expression::Slice(string, begin, end) => {
                match (
                    string.to_string(env),
                    begin.clone().map(|b| b.to_f64(env)),
                    end.clone().map(|e| e.to_f64(env)),
                ) {
                    (Err(s), _, _) => Err(s),
                    (_, Some(Err(s)), _) => Err(s),
                    (_, _, Some(Err(s))) => Err(s),
                    (Ok(s), None, None) => Ok(s),
                    (Ok(s), Some(Ok(b)), None) => Ok(s[(b as usize - 1)..].to_string()),
                    (Ok(s), None, Some(Ok(e))) => Ok(s[..(e as usize)].to_string()),
                    (Ok(s), Some(Ok(b)), Some(Ok(e))) => {
                        Ok(s[(b as usize - 1)..(e as usize)].to_string())
                    }
                }
            }
            Expression::Len(string_expression) => match string_expression.to_string(env) {
                Ok(s) => Ok(s.len().to_string()),
                Err(m) => Err(m),
            },
            Expression::Number(value) => Ok(value.to_string()),
            Expression::Str(number_value) => match number_value.to_string(env) {
                Ok(value) => Ok(value),
                Err(m) => Err(m),
            },
            Expression::Sgn(number_value) => match number_value.to_f64(env) {
                Ok(value) => Ok(if value == 0.0 {
                    "0".to_string()
                } else {
                    value.signum().to_string()
                }),
                Err(m) => Err(m),
            },
            Expression::Abs(number_value) => number_value.to_f64(env).map(|v| v.abs().to_string()),
            Expression::Int(number_value) => {
                number_value.to_f64(env).map(|v| (v as i32).to_string())
            }
            Expression::Sqr(number_value) => number_value.to_f64(env).map(|v| v.sqrt().to_string()),
            Expression::Fn(function_name, parameter_values) => {
                match env.clone().functions.get(function_name) {
                    None => Err("No function found".to_string()),
                    Some(f) => match Self::create_fn_env(env, parameter_values, f) {
                        Ok(mut e) => f.body.to_string(&mut e),
                        Err(m) => Err(m),
                    },
                }
            }
            Expression::RaiseTo(number, power) => match (number.to_f64(env), power.to_f64(env)) {
                (Err(m), _) => Err(m),
                (_, Err(m)) => Err(m),
                (Ok(n), Ok(p)) => Ok(n.powf(p).to_string()),
            },
            Expression::Exp(number) => number.to_f64(env).map(|v| v.exp().to_string()),
            Expression::Ln(number) => number.to_f64(env).map(|v| v.ln().to_string()),
            Expression::Sin(number) => number.to_f64(env).map(|v| v.sin().to_string()),
            Expression::Cos(number) => number.to_f64(env).map(|v| v.cos().to_string()),
            Expression::Tan(number) => number.to_f64(env).map(|v| v.tan().to_string()),
            Expression::Asn(number) => number.to_f64(env).map(|v| v.asin().to_string()),
            Expression::Acs(number) => number.to_f64(env).map(|v| v.acos().to_string()),
            Expression::Atn(number) => number.to_f64(env).map(|v| v.atan().to_string()),
            Expression::Rnd => {
                env.seed += 1; // one of the worst random number generators.
                Ok(env.seed.to_string())
            }
            Expression::NumberArrayElement(array_name, indices) => {
                let mut indices_values = vec![];
                for index in indices {
                    indices_values.push(index.to_f64(env).unwrap());
                }
                match env.clone().number_arrays.get(array_name) {
                    None => Err(format!(
                        "No number array found with the name {}",
                        array_name
                    )),
                    Some(array) => Ok(array.data[indices_values[0] as usize - 1]
                        .unwrap_or(0.0)
                        .to_string()),
                }
            }
            Expression::StringArrayElement(array_name, indices) => {
                let indices_values = indices_values(indices, env);
                match env.clone().string_arrays.get(array_name) {
                    None => Err(format!("No string array found with the name {}", array_name)),
                    Some(array) => Ok(array.data[indices_values[0] as usize - 1].clone()
                                          .unwrap_or("".to_string()))
                }
            }
        }
    }

    fn create_fn_env(
        env: &mut Env,
        parameter_values: &Vec<Expression>,
        f: &Function,
    ) -> Result<Env, String> {
        let mut new_env = env.clone();
        for (name, value) in zip(&f.parameter_names, parameter_values) {
            if name.ends_with("$") {
                match value.to_string(env) {
                    Ok(v) => {
                        new_env.string_variables.insert(name.to_string(), v);
                    }
                    Err(m) => return Err(m),
                }
            } else {
                match value.to_f64(env) {
                    Ok(v) => {
                        new_env.number_variables.insert(name.to_string(), v);
                    }
                    Err(m) => return Err(m),
                }
            }
        }
        Ok(new_env.clone())
    }

    fn to_f64(&self, env: &mut Env) -> Result<f64, String> {
        match self {
            Expression::Integer(value) => Ok(*value as f64),
            Expression::Text(_text) => Err(format!("Expected a number, found {:#?}", self)),
            Expression::NumberVariable(number_variable_name) => {
                match env.number_variables.get(number_variable_name) {
                    Some(value) => Ok(*value),
                    None => Err(format!("No variable {} found.", number_variable_name)),
                }
            }
            Expression::Plus(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l + r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Subtract(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l - r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Multiply(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l * r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Divide(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l / r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::AreEqual(_, _) => Err("AreEqual gives a bool, not an f64".to_string()),
            Expression::LessThan(_, _) => Err("LessThan gives a bool, not an f64".to_string()),
            Expression::GreaterThan(_, _) => {
                Err("GreaterThan gives a bool, not an f64".to_string())
            }
            Expression::StringVariable(_) => {
                Err("String variables hold a string, not an f64".to_string())
            }
            Expression::StringArrayElement(_, _) =>
                Err("String array elements hold a string, not an f64".to_string()),
            Expression::Slice(_, _, _) => Err("A slice is not a number".to_string()),
            Expression::Len(string_expression) => match string_expression.to_string(env) {
                Ok(value) => Ok(value.len() as f64),
                Err(m) => Err(m),
            },
            Expression::Number(value) => Ok(*value),
            Expression::Str(_) => Err("Str returns a string, not a number".to_string()),
            Expression::Sgn(number_expression) => match number_expression.to_f64(env) {
                Ok(value) => Ok(if value == 0.0 { 0.0 } else { value.signum() }),
                Err(m) => Err(m),
            },
            Expression::Abs(number_expression) => match number_expression.to_f64(env) {
                Ok(value) => Ok(value.abs()),
                Err(m) => Err(m),
            },
            Expression::Int(number_expression) => {
                number_expression.to_f64(env).map(|v| v as i32 as f64)
            }
            Expression::Sqr(number_expression) => number_expression.to_f64(env).map(|v| v.sqrt()),
            Expression::Fn(function_name, parameter_values) => {
                match env.clone().functions.get(function_name) {
                    None => Err("No function found".to_string()),
                    Some(f) => {
                        let mut new_env = env.clone();
                        for (name, value) in zip(&f.parameter_names, parameter_values) {
                            if name.ends_with("$") {
                                match value.to_string(env) {
                                    Ok(v) => {
                                        new_env.string_variables.insert(name.to_string(), v);
                                    }
                                    Err(m) => return Err(m),
                                }
                            } else {
                                match value.to_f64(env) {
                                    Ok(v) => {
                                        new_env.number_variables.insert(name.to_string(), v);
                                    }
                                    Err(m) => return Err(m),
                                }
                            }
                        }
                        f.body.to_f64(&mut new_env)
                    }
                }
            }
            Expression::RaiseTo(number, power) => match (number.to_f64(env), power.to_f64(env)) {
                (Err(m), _) => Err(m),
                (_, Err(m)) => Err(m),
                (Ok(n), Ok(p)) => Ok(n.powf(p)),
            },
            Expression::Exp(number) => number.to_f64(env).map(|v| v.exp()),
            Expression::Ln(number) => number.to_f64(env).map(|v| v.ln()),
            Expression::Sin(number) => number.to_f64(env).map(|v| v.sin()),
            Expression::Cos(number) => number.to_f64(env).map(|v| v.cos()),
            Expression::Tan(number) => number.to_f64(env).map(|v| v.tan()),
            Expression::Asn(number) => number.to_f64(env).map(|v| v.asin()),
            Expression::Acs(number) => number.to_f64(env).map(|v| v.acos()),
            Expression::Atn(number) => number.to_f64(env).map(|v| v.atan()),
            Expression::Rnd => panic!(),
            Expression::NumberArrayElement(array_name, indices) => {
                let mut indices_values = vec![];
                for index in indices {
                    indices_values.push(index.to_f64(env).unwrap());
                }
                match env.clone().number_arrays.get(array_name) {
                    None => Err(format!(
                        "No number array found with the name {}",
                        array_name
                    )),
                    Some(array) => Ok(array.data[indices_values[0] as usize - 1].unwrap_or(0.0)),
                }
            }
        }
    }

    fn to_bool(&self, env: &mut Env) -> Result<bool, String> {
        match self {
            Expression::Integer(value) => Err(value.to_string()),
            Expression::Text(value) => Err(value.to_string()),
            Expression::NumberVariable(value) => Err(value.to_string()),
            Expression::StringVariable(value) => Err(value.to_string()),
            Expression::Plus(_, _) => {
                Err("Cannot convert the result of a plus operation to boolean".to_string())
            }
            Expression::Subtract(_, _) => {
                Err("Cannot convert the result of a subtract operation to boolean".to_string())
            }
            Expression::Multiply(_, _) => {
                Err("Cannot convert the result of a multiply operation to boolean".to_string())
            }
            Expression::Divide(_, _) => {
                Err("Cannot convert the result of a divide operation to boolean".to_string())
            }
            Expression::AreEqual(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l == r),
                (_, _) => match (left.to_string(env), right.to_string(env)) {
                    (Ok(l), Ok(r)) => Ok(l == r),
                    (_, Err(e)) => Err(e.to_string()),
                    (Err(e), _) => Err(e.to_string()),
                },
            },
            Expression::LessThan(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l < r),
                (_, Err(e)) => Err(e.to_string()),
                (Err(e), _) => Err(e.to_string()),
            },
            Expression::GreaterThan(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l > r),
                (_, Err(e)) => Err(e.to_string()),
                (Err(e), _) => Err(e.to_string()),
            },
            Expression::Slice(_, _, _) => Err("Cannot convert a slice to bool".to_string()),
            Expression::Len(_) => Err("Cannot convert a length to bool".to_string()),
            Expression::Number(_) => Err("Cannot convert a number to bool".to_string()),
            Expression::Str(_) | Expression::StringArrayElement(_, _) => Err("Cannot convert a string to bool".to_string()),
            Expression::Sgn(_)
            | Expression::Abs(_)
            | Expression::Int(_)
            | Expression::Sqr(_)
            | Expression::RaiseTo(_, _)
            | Expression::Exp(_)
            | Expression::Ln(_)
            | Expression::Sin(_)
            | Expression::Cos(_)
            | Expression::Tan(_)
            | Expression::Asn(_)
            | Expression::Acs(_)
            | Expression::Atn(_)
            | Expression::Rnd
            | Expression::NumberArrayElement(_, _) => Err("Cannot convert a number to bool".to_string()),
            Expression::Fn(function_name, parameter_values) => {
                match env.clone().functions.get(function_name) {
                    None => Err("No function found".to_string()),
                    Some(f) => match Self::create_fn_env(env, parameter_values, &f) {
                        Ok(mut e) => f.body.to_bool(&mut e),
                        Err(m) => return Err(m),
                    },
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum UserInputReader {
    RealStdin,
    FakeStdin(String),
    FakeStdins(Vec<String>),
}

impl UserInputReader {
    fn next(&self) -> (String, UserInputReader) {
        match self {
            UserInputReader::RealStdin => todo!(),
            UserInputReader::FakeStdin(input) => (input.to_string(), self.clone()),
            UserInputReader::FakeStdins(inputs) => {
                let first = inputs.first();
                let mut others = inputs.clone();
                others.remove(0);
                (first.unwrap().to_string(), FakeStdins(others))
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Env {
    number_variables: HashMap<String, f64>,
    string_variables: HashMap<String, String>,
    user_input_reader: UserInputReader,
    loop_line_numbers: HashMap<String, usize>,
    current_line_number: usize,
    subroutines: Vec<usize>,
    data_by_line_number: BTreeMap<usize, Vec<String>>,
    current_data_line_number: usize,
    current_data_column: usize,
    functions: HashMap<String, Function>,
    seed: u64,
    number_arrays: HashMap<String, Array<f64>>,
    string_arrays: HashMap<String, Array<String>>,
}

impl Env {
    fn new(user_input_reader: UserInputReader, program: &Program) -> Env {
        Env {
            number_variables: HashMap::new(),
            string_variables: HashMap::new(),
            user_input_reader,
            loop_line_numbers: HashMap::new(),
            current_line_number: 0,
            subroutines: vec![],
            data_by_line_number: {
                let tuples: Vec<(usize, String)> = program
                    .lines
                    .iter()
                    .flat_map(|(line_number, command)| match command {
                        Command::Data(args) => {
                            let res: Vec<(usize, String)> = args
                                .iter()
                                .map(|expression| match expression {
                                    Expression::Integer(value) => (*line_number, value.to_string()),
                                    Expression::Number(value) => (*line_number, value.to_string()),
                                    Expression::Text(value) => (*line_number, value.to_string()),
                                    _ => panic!("Data not supported: {:#?}", expression),
                                })
                                .collect();
                            res
                        }
                        _ => vec![],
                    })
                    .collect();

                let mut map = BTreeMap::new();
                for (k, v) in tuples {
                    map.entry(k).or_insert_with(Vec::new).push(v.to_string());
                }
                map
            },
            current_data_line_number: 0,
            current_data_column: 0,
            functions: HashMap::new(),
            seed: 0,
            number_arrays: HashMap::new(),
            string_arrays: HashMap::new(),
        }
    }

    fn read_line(&self) -> (String, UserInputReader) {
        self.user_input_reader.next()
    }

    fn number_data(&mut self) -> Result<f64, String> {
        match self
            .data_by_line_number
            .range((Included(&self.current_data_line_number), Unbounded))
            .next()
        {
            None => Err(format!(
                "No number data available, current_data_line_number = {}",
                self.current_data_line_number
            )),
            Some((line_number, line)) => {
                self.current_data_line_number = *line_number;
                match line.get(self.current_data_column) {
                    None => Err("No data available at the expected column".to_string()),
                    Some(item) => {
                        if self.current_data_column + 1 == line.len() {
                            self.current_data_column = 0;
                            self.current_data_line_number = match self
                                .data_by_line_number
                                .range((Excluded(self.current_data_line_number), Unbounded))
                                .next()
                            {
                                None => 1000000,
                                Some((line_number, _)) => *line_number,
                            }
                        } else {
                            self.current_data_column += 1;
                        }
                        match item.parse() {
                            Ok(value) => Ok(value),
                            Err(parse_float_error) => Err(parse_float_error.to_string()),
                        }
                    }
                }
            }
        }
    }

    fn string_data(&mut self) -> Result<String, String> {
        match self
            .data_by_line_number
            .range((Included(&self.current_data_line_number), Unbounded))
            .next()
        {
            None => Err(format!(
                "No string data available, current_data_line_number = {}",
                self.current_data_line_number
            )),
            Some((line_number, line)) => {
                self.current_data_line_number = *line_number;
                match line.get(self.current_data_column) {
                    None => Err(format!("No data available at the expected column - line = {:#?}, current_data_column = {}",
                        *line, self.current_data_column)),
                    Some(item) => {
                        if self.current_data_column + 1 == line.len() {
                            self.current_data_column = 0;
                            self.current_data_line_number = match self.data_by_line_number.range((Excluded(self.current_data_line_number), Unbounded)).next() {
                                None => 1000000,
                                Some((line_number, _)) => *line_number
                            }
                        } else {
                            self.current_data_column += 1;
                        }
                        Ok(item.to_string())
                    }
                }
            }
        }
    }
}

enum CommandResult {
    Output(String),
    Jump(usize),
    Stop(String),
}

#[derive(Clone)]
enum Command {
    Let(Expression, Expression),
    Goto(usize),
    Print(Vec<Expression>),
    Input(Vec<Expression>),
    Rem(String),
    If(Expression, Vec<Command>),
    Stop,
    For(String, Expression, Expression, Expression),
    Next(String),
    Gosub(usize),
    Return,
    Read(Vec<Expression>),
    Data(Vec<Expression>),
    DefFn(String, Vec<String>, Expression),
    Randomize(Expression),
    Dim(Expression),
}

impl Command {
    fn let_equal(variable: Expression, value: Expression) -> Command {
        Command::Let(variable, value)
    }
    fn for_loop(
        variable_name: &str,
        begin: Expression,
        end: Expression,
        step: Expression,
    ) -> Command {
        Command::For(variable_name.to_string(), begin, end, step)
    }
    fn next(variable_name: &str) -> Command {
        Command::Next(variable_name.to_string())
    }
    fn def_fn(function_name: &str, parameter_names: Vec<&str>, body: &Expression) -> Command {
        Command::DefFn(
            function_name.to_string(),
            parameter_names.iter().map(|v| v.to_string()).collect(),
            body.clone(),
        )
    }
    fn run(&self, env: &mut Env) -> Result<CommandResult, String> {
        match self {
            Command::Let(Expression::StringVariable(variable_name), value) => {
                match value.to_string(env) {
                    Ok(v) => {
                        env.string_variables.insert(variable_name.clone(), v);
                        Ok(CommandResult::Output(String::new()))
                    }
                    Err(msg) => Err(msg.to_string()),
                }
            }
            Command::Let(Expression::NumberVariable(variable_name), value) => {
                match value.to_f64(env) {
                    Ok(number) => {
                        env.number_variables.insert(variable_name.clone(), number);
                        Ok(CommandResult::Output(String::new()))
                    }
                    Err(msg) => Err(msg.to_string()),
                }
            }
            Command::Let(Expression::NumberArrayElement(array_name, indices), value) => {
                let mut indices_values = vec![];
                for index in indices {
                    match index.to_f64(env) {
                        Ok(index) => indices_values.push(index),
                        Err(m) => return Err(m),
                    }
                }

                let mut array1 = env.number_arrays[array_name].clone();
                array1.data[indices_values[0] as usize - 1] = Some(value.to_f64(env).unwrap());
                env.number_arrays.insert(array_name.to_string(), array1);
                Ok(CommandResult::Output("".to_string()))
            }
            Command::Let(Expression::StringArrayElement(array_name, indices), value) => {
                let mut array1 = env.string_arrays[array_name].clone();
                array1.data[indices_values(indices, env)[0] as usize - 1] = Some(value.to_string(env).unwrap());
                env.string_arrays.insert(array_name.to_string(), array1);
                Ok(CommandResult::Output("".to_string()))
            }
            Command::Let(_, _) => panic!(),
            Command::Goto(line_number) => Ok(CommandResult::Jump(*line_number)),
            Command::Print(expressions) => Ok(CommandResult::Output(
                expressions
                    .iter()
                    .flat_map(|e| e.to_string(env))
                    .collect::<Vec<String>>()
                    .join(" ")
                    + "\n",
            )),
            Command::Input(expressions) => {
                let mut output = String::new();
                for expression in expressions {
                    match expression {
                        Expression::Text(text) => {
                            output.push_str(text);
                            output.push('\n');
                        }
                        Expression::NumberVariable(name) => {
                            let value = env.read_line();
                            env.user_input_reader = value.1.clone();
                            match value.0.parse::<f64>() {
                                Ok(v) => {
                                    env.number_variables.insert(name.to_owned(), v);
                                }
                                Err(msg) => {
                                    return Err(msg.to_string());
                                }
                            }
                        }
                        _ => {
                            return Err("Unexpected expression type found, and I don't know how to report which".to_string());
                        }
                    }
                }
                Ok(CommandResult::Output(output))
            }
            Command::Rem(_) => Ok(CommandResult::Output(String::new())),
            Command::If(test, then) => match test.to_bool(env) {
                Ok(false) => Ok(CommandResult::Output("".to_string())),
                Ok(true) => {
                    let mut output = String::new();
                    for command in then {
                        match &(command.run(env)) {
                            Ok(CommandResult::Output(text)) => output.push_str(text),
                            Ok(CommandResult::Jump(line_number)) => {
                                return Ok(CommandResult::Jump(*line_number))
                            }
                            Ok(CommandResult::Stop(text)) => {
                                output.push_str(text);
                                return Ok(CommandResult::Stop(output));
                            }
                            Err(msg) => return Err(msg.to_string()),
                        }
                    }
                    Ok(CommandResult::Output(output))
                }
                Err(e) => Err(e),
            },
            Command::Stop => Ok(CommandResult::Stop("".to_string())),
            Command::For(variable_name, begin, end, step) => {
                let begin = begin.to_f64(env);
                let end = end.to_f64(env);
                let step = step.to_f64(env);
                match (
                    env.number_variables.entry(variable_name.to_string()),
                    begin,
                    end,
                    step,
                ) {
                    (_, Err(e), _, _) => Err(e.to_string()),
                    (_, _, Err(e), _) => Err(e.to_string()),
                    (_, _, _, Err(e)) => Err(e.to_string()),
                    (Entry::Vacant(v), Ok(b), _, _) => {
                        v.insert(b);
                        env.loop_line_numbers
                            .insert(variable_name.to_string(), env.current_line_number);
                        Ok(CommandResult::Output("".to_string()))
                    }
                    (Entry::Occupied(mut o), Ok(_), Ok(e), Ok(s)) => {
                        o.insert(o.get() + s);
                        if (s > 0.0 && *o.get() >= e) || (s < 0.0 && *o.get() <= e) {
                            env.loop_line_numbers.remove(variable_name.as_str());
                        }
                        Ok(CommandResult::Output("".to_string()))
                    }
                }
            }
            Command::Next(variable_name) => match env.loop_line_numbers.get(variable_name) {
                None => {
                    env.number_variables.remove(variable_name);
                    Ok(CommandResult::Output("".to_string()))
                }
                Some(line_number) => Ok(CommandResult::Jump(*line_number)),
            },
            Command::Gosub(line_number) => {
                env.subroutines.push(env.current_line_number);
                Ok(CommandResult::Jump(*line_number))
            }
            Command::Return => match env.subroutines.pop() {
                None => Err("No matching gosub for return".to_string()),
                Some(line_number) => Ok(CommandResult::Jump(line_number + 1)),
            },
            Command::Read(variables) => {
                for v in variables {
                    match v {
                        Expression::NumberVariable(name) => match env.number_data() {
                            Ok(value) => {
                                env.number_variables.insert(name.to_string(), value);
                            }
                            Err(message) => return Err(message),
                        },
                        Expression::StringVariable(name) => match env.string_data() {
                            Ok(value) => {
                                env.string_variables.insert(name.to_string(), value);
                            }
                            Err(message) => return Err(message),
                        },
                        Expression::NumberArrayElement(name, indices) => {
                            let mut indices_values = vec![];
                            for index in indices {
                                indices_values.push(index.to_f64(env).unwrap());
                            }
                            match env.number_data() {
                                Ok(value) => match env.number_arrays.get_mut(name) {
                                    None => return Err(format!("No array named {} found", name)),
                                    Some(array) => {
                                        array.data[indices_values[0] as usize - 1] = Some(value)
                                    }
                                },
                                Err(m) => return Err(m),
                            }
                        }
                        Expression::StringArrayElement(name, indices) => {
                            let indices_values = indices_values(indices, env);
                            match env.string_data() {
                                Ok(value) => match env.string_arrays.get_mut(name) {
                                    None => return Err(format!("No array named {} found", name)),
                                    Some(array) => {
                                        array.data[indices_values[0] as usize - 1] = Some(value)
                                    }
                                },
                                Err(m) => return Err(m),
                            }
                        }
                        _ => return Err(format!("Can only read into variables, found {:#?}", v)),
                    }
                }
                Ok(CommandResult::Output("".to_string()))
            }
            Command::Data(_) => Ok(CommandResult::Output("".to_string())),
            Command::DefFn(function_name, parameter_names, body) => {
                env.functions.insert(
                    function_name.to_string(),
                    Function {
                        parameter_names: parameter_names.clone(),
                        body: body.clone(),
                    },
                );
                Ok(CommandResult::Output("".to_string()))
            }
            Command::Randomize(seed) => seed.to_f64(env).map(|seed| {
                env.seed = seed.to_bits();
                CommandResult::Output("".to_string())
            }),
            Command::Dim(expression) => match expression {
                Expression::NumberArrayElement(array_name, sizes) => {
                    let mut sizes_values = vec![];
                    for size in sizes {
                        sizes_values.push(size.to_f64(env).unwrap());
                    }
                    env.number_arrays
                        .insert(array_name.to_string(), Array::new(sizes_values[0] as usize));
                    Ok(CommandResult::Output("".to_string()))
                }
                Expression::StringArrayElement(array_name, sizes) => {
                    let indices_values = indices_values(sizes, env);
                    env.string_arrays
                        .insert(array_name.to_string(), Array::new(indices_values[0] as usize - 1));
                    Ok(CommandResult::Output("".to_string()))
                }
                _ => Err("DIM needs an array declaration".to_string()),
            },
        }
    }
}

fn indices_values(indices: &Vec<Expression>, env: &mut Env) -> Vec<f64> {
    let mut indices_values = vec!();
    for index in indices {
        indices_values.push(index.to_f64(env).unwrap());
    }
    indices_values
}

enum LinesLimit {
    NoLimit,
    Limit(i32),
}

impl LinesLimit {
    fn decrement(&self) -> LinesLimit {
        match self {
            LinesLimit::NoLimit => LinesLimit::NoLimit,
            LinesLimit::Limit(x) => LinesLimit::Limit(x - 1),
        }
    }
}

#[derive(Default)]
struct Program {
    lines: BTreeMap<usize, Command>,
}

impl Program {
    fn new() -> Program {
        Program {
            lines: BTreeMap::new(),
        }
    }

    fn add_line(&mut self, line_number: usize, command: Command) -> &mut Self {
        self.lines.insert(line_number, command);
        self
    }

    fn run(
        &self,
        lines_limit: LinesLimit,
        user_input_reader: UserInputReader,
    ) -> Result<ProgramResult, String> {
        let mut output = String::new();
        let mut env = Env::new(user_input_reader, &self);

        match self.run_helper(lines_limit, &mut output, &mut env, self.lines.clone()) {
            Ok(output) => Ok(ProgramResult {
                output,
                env
            }),
            Err(e) => Err(e),
        }
    }

    fn run_helper(
        &self,
        lines_limit: LinesLimit,
        output: &mut String,
        env: &mut Env,
        lines: BTreeMap<usize, Command>,
    ) -> Result<String, String> {
        for (line, command) in lines {
            env.current_line_number = line;
            match &(command.run(env)) {
                Ok(CommandResult::Output(text)) => output.push_str(text),
                Ok(CommandResult::Jump(line_number)) => {
                    match lines_limit {
                        LinesLimit::NoLimit => {}
                        LinesLimit::Limit(x) if x >= 1 => {}
                        LinesLimit::Limit(_) => return Ok(output.to_string()),
                    }

                    let mut child_lines: BTreeMap<usize, Command> = BTreeMap::new();
                    for (l, command) in &self.lines {
                        if l >= line_number {
                            let unboxed_command: Command = command.clone();
                            child_lines.insert(*l, unboxed_command);
                        }
                    }
                    return self.run_helper(
                        lines_limit.decrement(),
                        output,
                        env,
                        child_lines.clone(),
                    );
                }
                Ok(CommandResult::Stop(text)) => {
                    output.push_str(text);
                    return Ok(output.to_string());
                }
                Err(msg) => return Err(msg.to_string()),
            }
        }
        Ok(output.to_string())
    }
}

struct ProgramResult {
    output: String,
    env: Env,
}

#[derive(Clone, Debug)]
struct Function {
    parameter_names: Vec<String>,
    body: Expression,
}

#[derive(Clone, Debug)]
struct Array<T> {
    length: usize,
    data: Vec<Option<T>>,
}

impl<T: Clone> Array<T> {
    fn new(length: usize) -> Array<T> {
        Array {
            length,
            data: vec![None; length],
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::LinesLimit::NoLimit;
    use crate::UserInputReader::RealStdin;
    use crate::*;
    use Command::*;

    #[test]
    fn let_and_print() {
        let mut program = Program::new();
        program.add_line(20, Print(vec![Expression::number_variable("a")]));
        program.add_line(
            10,
            Let(Expression::number_variable("a"), Expression::Integer(10)),
        );
        let output = program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output;
        assert_eq!(output, "10\n");
    }

    // 10 LET a$="Hi"
    // 20 PRINT $a,$a
    #[test]
    fn let_string() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::let_equal(Expression::string_variable("a$"), Expression::text("Hi")),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::string_variable("a$"),
                Expression::string_variable("a$"),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "Hi Hi\n".to_string()
        );
    }

    #[test]
    fn print_adding_vars() {
        let mut program = Program::new();
        let number_variable = Expression::number_variable;
        program.add_line(20, Print(vec![number_variable("a") + number_variable("b")]));
        program.add_line(
            10,
            Let(Expression::number_variable("a"), Expression::Integer(10)),
        );
        program.add_line(
            15,
            Let(Expression::number_variable("b"), Expression::Integer(15)),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "25\n".to_string()
        );
    }

    #[test]
    fn print_with_comma() {
        let mut program = Program::new();
        program.add_line(
            10,
            Let(Expression::number_variable("a"), Expression::Integer(10)),
        );
        program.add_line(
            15,
            Let(Expression::number_variable("b"), Expression::Integer(15)),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::number_variable("a"),
                Expression::number_variable("b"),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "10 15\n".to_string()
        );
    }

    #[test]
    fn rem() {
        let mut program = Program::new();
        program.add_line(10, Rem(String::new()));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            String::new()
        );
    }

    #[test]
    fn print_with_no_expressions() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "\n".to_string()
        );
    }

    // PRINT 90-32
    #[test]
    fn subtract() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Integer(90) - Expression::Integer(32)]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "58\n".to_string()
        );
    }

    // PRINT 58*5
    #[test]
    fn multiply() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Integer(58) * Expression::Integer(5)]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "290\n".to_string()
        );
    }

    // 10 PRINT (41-32)*5/9
    #[test]
    fn print_arithmetic() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![
                (Expression::Integer(41) - Expression::Integer(32)) * Expression::Integer(5)
                    / Expression::Integer(9),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "5\n".to_string()
        );
    }

    // 10 PRINT "Hello, World!"
    // 20 GO TO 10
    #[test]
    fn goto() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::text("Hello, World!")]));
        program.add_line(20, Goto(10));

        assert_eq!(
            program.run(LinesLimit::Limit(1), UserInputReader::RealStdin).unwrap().output,
            "Hello, World!\nHello, World!\n".to_string()
        );
    }

    // 10 INPUT "Enter deg F", F
    // 20 PRINT "You gave ", F
    #[test]
    fn input() {
        let mut program = Program::new();
        program.add_line(
            10,
            Input(vec![
                Expression::text("Enter deg F"),
                Expression::number_variable("F"),
            ]),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::text("You gave"),
                Expression::number_variable("F"),
            ]),
        );

        assert_eq!(
            program.run(
                LinesLimit::NoLimit,
                UserInputReader::FakeStdin("42.0".to_string())
            ).unwrap().output,
            "Enter deg F\nYou gave 42\n".to_string()
        );
    }

    // 10 REM temperature conversion
    // 20 PRINT "deg F", "deg C"
    // 30 PRINT
    // 40 INPUT "Enter deg F", F
    // 50 PRINT F,(F-32)*5/9
    // 60 GO TO 40
    #[test]
    fn goto_plus_input() {
        let mut program = Program::new();
        program.add_line(10, Rem(String::from("temperature conversion")));
        program.add_line(
            20,
            Print(vec![Expression::text("deg F"), Expression::text("deg C")]),
        );
        program.add_line(30, Print(vec![]));
        program.add_line(
            40,
            Input(vec![
                Expression::text("Enter deg F"),
                Expression::number_variable("F"),
            ]),
        );
        program.add_line(
            50,
            Print(vec![
                Expression::number_variable("F"),
                (Expression::number_variable("F") - Expression::Integer(32))
                    * Expression::Integer(5)
                    / Expression::Integer(9),
            ]),
        );
        program.add_line(60, Goto(40));

        assert_eq!(
            program.run(LinesLimit::Limit(2), UserInputReader::FakeStdin("42.0".to_string())).unwrap().output,
            "deg F deg C\n\nEnter deg F\n42 5.555555555555555\nEnter deg F\n42 5.555555555555555\nEnter deg F\n42 5.555555555555555\n".to_string()
        );
    }

    // 26 INPUT a
    // 27 CLS
    // 30 INPUT ???Guess the number???, b
    // 40 IF b=a THEN PRINT ???That is correct???:,STOP
    // 56 IF b<a THEN PRINT ???That is too small, try again???
    // 66 IF b>a THEN PRINT ???That is too big, try again???
    // 70 GO TO 36
    #[test]
    fn guess_number() {
        let mut program = Program::new();
        program.add_line(26, Input(vec![Expression::number_variable("a")]));
        program.add_line(
            30,
            Input(vec![
                Expression::text("Guess the number"),
                Expression::number_variable("b"),
            ]),
        );
        program.add_line(
            40,
            If(
                Expression::number_variable("b").are_equal(&Expression::number_variable("a")),
                vec![Print(vec![Expression::text("That is correct")]), Stop],
            ),
        );
        program.add_line(
            56,
            If(
                Expression::number_variable("b").less_than(&Expression::number_variable("a")),
                vec![Print(vec![Expression::text(
                    "That is too small, try again",
                )])],
            ),
        );
        program.add_line(
            66,
            If(
                Expression::number_variable("b").greater_than(&Expression::number_variable("a")),
                vec![Print(vec![Expression::text("That is too big, try again")])],
            ),
        );
        program.add_line(70, Goto(36));

        assert_eq!(
            program.run(
                LinesLimit::NoLimit,
                UserInputReader::FakeStdin("42.0".to_string())
            ).unwrap().output,
            "Guess the number\nThat is correct\n".to_string()
        );
    }

    // 5 PRINT "Expected"
    // 10 IF 4 > 3 THEN STOP
    // 20 PRINT "Failed to stop"
    #[test]
    fn stop() {
        let mut program = Program::new();
        program.add_line(5, Print(vec![Expression::text("Expected")]));
        program.add_line(
            10,
            If(
                Expression::Integer(4).greater_than(&Expression::Integer(3)),
                vec![Stop],
            ),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "Expected\n".to_string()
        );
    }

    // 10 IF 4 > 3 THEN PRINT "Foo": PRINT "Bar"
    #[test]
    fn if_with_two_commands() {
        let mut program = Program::new();
        program.add_line(
            10,
            If(
                Expression::Integer(4).greater_than(&Expression::Integer(3)),
                vec![
                    Print(vec![Expression::text("Foo")]),
                    Print(vec![Expression::text("Bar")]),
                ],
            ),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "Foo\nBar\n".to_string()
        );
    }

    // 10 IF 4 > 3 THEN PRINT "Foo": STOP
    #[test]
    fn if_with_print_and_stop() {
        let mut program = Program::new();
        program.add_line(
            10,
            If(
                Expression::Integer(4).greater_than(&Expression::Integer(3)),
                vec![Print(vec![Expression::text("Foo")]), Stop],
            ),
        );
    }

    // 10 FOR n=1 TO 10
    // 20 PRINT n
    // 30 NEXT n"
    #[test]
    fn test_for_loop() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::for_loop(
                "n",
                Expression::Integer(1),
                Expression::Integer(10),
                Expression::Integer(1),
            ),
        );
        program.add_line(20, Print(vec![Expression::number_variable("n")]));
        program.add_line(30, Command::next("n"));

        assert_eq!(
            program.run(LinesLimit::Limit(100), RealStdin).unwrap().output,
            "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n".to_string()
        );
    }

    // 10 FOR n=10 TO 1 STEP -1
    // 20 PRINT n
    // 30 NEXT n
    #[test]
    fn for_with_step() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::for_loop(
                "n",
                Expression::Integer(10),
                Expression::Integer(1),
                Expression::Integer(-1),
            ),
        );
        program.add_line(20, Print(vec![Expression::number_variable("n")]));
        program.add_line(30, Command::next("n"));

        assert_eq!(
            program.run(LinesLimit::Limit(100), RealStdin).unwrap().output,
            "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n".to_string()
        );
    }

    // 5 LET n = 0
    // 10 FOR a = 1 TO 2
    // 20 FOR b = 2 TO 3
    // 30 LET c = a*b
    // 40 IF c>n THEN LET n = c
    // 50 NEXT b
    // 60 NEXT a
    // 70 PRINT n
    #[test]
    fn nested_for() {
        let mut program = Program::new();
        program.add_line(
            5,
            Command::let_equal(Expression::number_variable("n"), Expression::Integer(0)),
        );
        program.add_line(
            10,
            Command::for_loop(
                "a",
                Expression::Integer(1),
                Expression::Integer(2),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            20,
            Command::for_loop(
                "b",
                Expression::Integer(2),
                Expression::Integer(3),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            30,
            Command::let_equal(
                Expression::number_variable("c"),
                Expression::number_variable("a") * Expression::number_variable("b"),
            ),
        );
        program.add_line(
            40,
            If(
                Expression::number_variable("c").greater_than(&Expression::number_variable("n")),
                vec![Command::let_equal(
                    Expression::number_variable("n"),
                    Expression::number_variable("c"),
                )],
            ),
        );
        program.add_line(50, Command::next("b"));
        program.add_line(60, Command::next("a"));
        program.add_line(70, Print(vec![Expression::number_variable("n")]));
        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "6\n".to_string()
        );
    }

    // 10 FOR a=1 TO 2 STEP 1
    // 20 FOR b=3 TO 4 STEP 1
    // 30 PRINT a,b
    // 40 NEXT b
    // 50 NEXT a
    #[test]
    fn simple_nested_for() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::for_loop(
                "a",
                Expression::Integer(1),
                Expression::Integer(2),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            20,
            Command::for_loop(
                "b",
                Expression::Integer(3),
                Expression::Integer(4),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            30,
            Print(vec![
                Expression::number_variable("a"),
                Expression::number_variable("b"),
            ]),
        );
        program.add_line(40, Command::next("b"));
        program.add_line(50, Command::next("a"));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "1 3\n1 4\n2 3\n2 4\n".to_string()
        );
    }

    // 10 REM ???A rearranged guessing game???
    // 20 INPUT a:
    // 30 INPUT ???Guess the number ???,b
    // 40 IF a=b THEN PRINT ???Correct???: STOP
    // 50 IF a<b THEN GO SUB 100
    // 60 IF a>b THEN GO SUB 100
    // 70 GO TO 30
    // 100 PRINT ???Try again???
    // 110 RETURN
    #[test]
    fn gosub() {
        let mut program = Program::new();
        program.add_line(10, Rem("A rearranged guessing game".to_string()));
        let a = &Expression::number_variable("a");
        let b = &Expression::number_variable("b");
        program.add_line(20, Input(vec![a.clone()]));
        program.add_line(
            30,
            Input(vec![Expression::text("Guess the number "), b.clone()]),
        );
        program.add_line(
            40,
            If(
                a.are_equal(&b),
                vec![Print(vec![Expression::text("Correct")]), Stop],
            ),
        );
        program.add_line(50, If(a.less_than(&b), vec![Gosub(100)]));
        program.add_line(60, If(a.greater_than(&b), vec![Gosub(100)]));
        program.add_line(70, Goto(30));
        program.add_line(100, Print(vec![Expression::text("Try again")]));
        program.add_line(110, Return);

        assert_eq!(
            program.run(
                LinesLimit::NoLimit,
                UserInputReader::FakeStdins(vec!(
                    "5".to_string(),
                    "3".to_string(),
                    "5".to_string()
                ))
            ).unwrap().output,
            "Guess the number \nTry again\nGuess the number \nCorrect\n".to_string()
        );
    }

    // 10 GO SUB 30
    // 20 PRINT "20"
    // 25 STOP
    // 30 PRINT "30"
    // 40 RETURN
    #[test]
    fn simpler_gosub() {
        let mut program = Program::new();
        program.add_line(10, Gosub(30));
        program.add_line(20, Print(vec![Expression::text("20")]));
        program.add_line(25, Stop);
        program.add_line(30, Print(vec![Expression::text("30")]));
        program.add_line(40, Return);

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin).unwrap().output,
            "30\n20\n".to_string()
        );
    }

    // 10 READ d$
    // 20 PRINT ???The date is???,d$
    // 30 DATA ???June lst, 1982???
    // 40 STOP
    #[test]
    fn data() {
        let mut program = Program::new();
        program.add_line(10, Read(vec![Expression::string_variable("d$")]));
        program.add_line(
            20,
            Print(vec![
                Expression::text("The date is"),
                Expression::string_variable("d$"),
            ]),
        );
        program.add_line(30, Data(vec![Expression::text("June 1st, 1982")]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "The date is June 1st, 1982\n".to_string()
        );
    }

    // 10 READ a,b,c
    // 20 PRINT ???The numbers are???,a,b,c
    // 30 DATA 1234, 5678
    // 40 DATA 9101
    // 50 STOP
    #[test]
    fn multiple_data() {
        let mut program = Program::new();
        program.add_line(
            10,
            Read(vec![
                Expression::number_variable("a"),
                Expression::number_variable("b"),
                Expression::number_variable("c"),
            ]),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::text("The numbers are"),
                Expression::number_variable("a"),
                Expression::number_variable("b"),
                Expression::number_variable("c"),
            ]),
        );
        program.add_line(
            30,
            Data(vec![Expression::Integer(1234), Expression::Integer(5678)]),
        );
        program.add_line(40, Data(vec![Expression::Integer(9101)]));
        program.add_line(50, Stop);

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "The numbers are 1234 5678 9101\n".to_string()
        );
    }

    // 10 LET a$=???abcdef???
    // 20 FOR n=1 TO 6
    // 30 PRINT a$(n TO 6)
    // 40 NEXT n
    #[test]
    fn slice() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::let_equal(
                Expression::string_variable("a$"),
                Expression::text("abcdef"),
            ),
        );
        program.add_line(
            20,
            Command::for_loop(
                "n",
                Expression::Integer(1),
                Expression::Integer(6),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            30,
            Command::Print(vec![Expression::string_variable("a$").slice(
                Some(Expression::number_variable("n")),
                Some(Expression::Integer(6)),
            )]),
        );
        program.add_line(40, Command::next("n"));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "abcdef\nbcdef\ncdef\ndef\nef\nf\n".to_string()
        );
    }

    // 10 PRINT LEN "foo"
    #[test]
    fn len() {
        let mut program = Program::new();
        program.add_line(10, Command::Print(vec![Expression::text("foo").len()]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "3\n".to_string()
        );
    }

    // 10 PRINT LEN STR$ 100.0000
    #[test]
    fn str() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(100.0000).str().len()]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "3\n".to_string()
        );
    }

    // 10 PRINT SGN -10, SGN 0, SGN 10
    #[test]
    fn sgn() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![
                Expression::sgn(&Expression::Integer(-10)),
                Expression::sgn(&Expression::Integer(0)),
                Expression::sgn(&Expression::Integer(10)),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "-1 0 1\n".to_string()
        );
    }

    // 10 PRINT ABS -2.5
    #[test]
    fn abs() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(-2.5).abs()]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "2.5\n".to_string()
        );
    }

    // 10 PRINT INT 2.5
    #[test]
    fn int() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(2.5).int()]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "2\n".to_string()
        );
    }

    // 10 PRINT SQR 4096
    #[test]
    fn sqr() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(4096.0).sqr()]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "64\n".to_string()
        );
    }

    // 10 DEF FN s(x)=x*x
    // 20 PRINT FN s(4)
    #[test]
    fn def_fn() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::def_fn(
                "s",
                vec!["x"],
                &(Expression::number_variable("x") * Expression::number_variable("x")),
            ),
        );
        program.add_line(
            20,
            Print(vec![Expression::function(
                "s",
                vec![Expression::Number(4.0)],
            )]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "16\n".to_string()
        );
    }

    // On the Spectrum it's actually an up arrow, not a ^
    // 10 PRINT 3 ^ 5
    #[test]
    fn raise_to() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![
                Expression::Number(3.0).raise_to(Expression::Number(5.0))
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "243\n".to_string()
        );
    }

    // 10 PRINT EXP 2
    #[test]
    fn exp() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(2.0).exp()]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin).unwrap().output,
            "7.38905609893065\n".to_string()
        );
    }

    // 10 PRINT LN 5
    #[test]
    fn ln() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(5.0).ln()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "1.6094379124341003\n".to_string()
        );
    }

    // 10 PRINT SIN 2
    #[test]
    fn sin() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(2.0).sin()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "0.9092974268256817\n".to_string()
        );
    }

    // 10 PRINT COS 2
    #[test]
    fn cos() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(2.0).cos()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "-0.4161468365471424\n".to_string()
        );
    }

    // 10 PRINT TAN 2
    #[test]
    fn tan() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(2.0).tan()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "-2.185039863261519\n".to_string()
        );
    }

    // 10 PRINT ASN 1
    #[test]
    fn asn() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(1.0).asn()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "1.5707963267948966\n".to_string()
        );
    }

    // 10 PRINT ACS 0
    #[test]
    fn acs() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(0.0).acs()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "1.5707963267948966\n".to_string()
        );
    }

    // 10 PRINT ATN 2
    #[test]
    fn atn() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Number(2.0).atn()]));

        assert_eq!(
            program.run(NoLimit, RealStdin).unwrap().output,
            "1.1071487177940904\n".to_string()
        );
    }

    // 10 PRINT RND
    // 20 PRINT RND
    #[test]
    fn rnd() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::Rnd]));
        program.add_line(20, Print(vec![Expression::Rnd]));

        match program.run(NoLimit, RealStdin) {
            Ok(output) => {
                let lines: Vec<String> = output.output.split_whitespace().map(|v| v.to_string()).collect();
                assert_eq!(lines.len(), 2);
                assert_ne!(lines[0], lines[1]);
            }
            Err(m) => panic!("{}", m),
        }
    }

    // 10 RANDOMIZE 2
    // 20 PRINT RND
    // 30 RANDOMIZE 2
    // 40 PRINT RND
    #[test]
    fn randomize() {
        let mut program = Program::new();
        program.add_line(10, Command::Randomize(Expression::Number(2.0)));
        program.add_line(20, Print(vec![Expression::Rnd]));
        program.add_line(30, Command::Randomize(Expression::Number(2.0)));
        program.add_line(40, Print(vec![Expression::Rnd]));

        match program.run(NoLimit, RealStdin) {
            Ok(output) => {
                let lines: Vec<String> = output.output.split_whitespace().map(|v| v.to_string()).collect();
                assert_eq!(lines.len(), 2);
                assert_eq!(lines[0], lines[1]);
            }
            Err(m) => panic!("{}", m),
        }
    }

    // 5 DIM b(10)
    // 10 FOR n=l TO 10
    // 20 READ b(n)
    // 30 NEXT n
    // 40 PRINT b(10)
    // 50 DATA 10,2,5,19,16,3,11,1,0,6
    #[test]
    fn dim() {
        let mut program = Program::new();
        program.add_line(
            5,
            Dim(Expression::number_array_element(
                "b",
                vec![Expression::Number(10.0)],
            )),
        );
        program.add_line(
            10,
            Command::for_loop(
                "n",
                Expression::Number(1.0),
                Expression::Number(10.0),
                Expression::Number(1.0),
            ),
        );
        program.add_line(
            20,
            Read(vec![Expression::number_array_element(
                "b",
                vec![Expression::number_variable("n")],
            )]),
        );
        program.add_line(30, Command::next("n"));
        program.add_line(
            40,
            Print(vec![Expression::number_array_element(
                "b",
                vec![Expression::Number(10.0)],
            )]),
        );
        program.add_line(
            50,
            Data(vec![
                Expression::Number(10.0),
                Expression::Number(2.0),
                Expression::Number(5.0),
                Expression::Number(19.0),
                Expression::Number(16.0),
                Expression::Number(3.0),
                Expression::Number(11.0),
                Expression::Number(1.0),
                Expression::Number(0.0),
                Expression::Number(6.0),
            ]),
        );

        assert_eq!(program.run(NoLimit, RealStdin).unwrap().output, "6\n".to_string());
    }

    // 10 DIM a$(5,10)
    // 20 LET a$(2)="1234567890"
    // 30 PRINT a$(2),a$(2,7)
    // #[test]
    fn string_array() {
        let mut program = Program::new();
        program.add_line(
            10,
            Dim(Expression::string_array_element(
                "a$",
                vec![Expression::Number(5.0), Expression::Number(10.0)],
            )),
        );
        program.add_line(
            20,
            Let(
                Expression::string_array_element("a$", vec![Expression::Number(2.0)]),
                Expression::text("1234567890"),
            ),
        );
        program.add_line(
            30,
            Print(vec![
                Expression::string_array_element("a$", vec![Expression::Number(2.0)]),
                Expression::string_array_element(
                    "a$",
                    vec![Expression::Number(2.0), Expression::Number(7.0)],
                ),
            ]),
        );

        let result = program.run(NoLimit, RealStdin).unwrap();
        println!("Env: {:#?}", result.env);
        assert_eq!(result.output, "1234567890 7\n".to_string());
    }
}

fn main() {
    println!("Hello, world!");
}
