use std::collections::{BTreeMap, HashMap};
use std::io::stdin;
use std::result::Result;
use std::vec::Vec;

#[derive(Clone)]
enum Expression {
    Integer(i32),
    Text(String),
    NumberVariable(String),
    Plus(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
}
impl Expression {
    fn to_string(&self, env: &Env) -> Result<String, &str> {
        match self {
            Expression::Integer(value) => Ok(value.to_string()),
            Expression::Text(text) => Ok(text.to_string()),
            Expression::NumberVariable(number_variable_name) => Ok(env
                .number_variables
                .get(number_variable_name)
                .unwrap()
                .to_string()),
            Expression::Plus(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Subtract(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Multiply(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Divide(_, _) => self.to_f64(env).map(|v| v.to_string()),
        }
    }

    fn to_f64(&self, env: &Env) -> Result<f64, &str> {
        match self {
            Expression::Integer(value) => Ok(*value as f64),
            Expression::Text(text) => {
                Err("Expected a number, found something else, but I can't figure out how to include that in the error")
            }
            Expression::NumberVariable(number_variable_name) =>
                match env.number_variables.get(number_variable_name) {
                    Some(value) => Ok(*value),
                    None => Err("No such variable found, not sure how to say which.")
                },
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
        }
    }
}

enum UserInputReader {
    RealStdin,
    FakeStdin(String),
}
impl UserInputReader {
    fn next(&self) -> String {
        match self {
            UserInputReader::RealStdin => {
                let mut output = String::new();
                stdin().read_line(&mut output).unwrap();
                output
            },
            UserInputReader::FakeStdin(input) => input.to_string(),
        }
    }
}

struct Env {
    number_variables: HashMap<String, f64>,
    user_input_reader: UserInputReader,
}
impl Env {
    fn new(user_input_reader: UserInputReader) -> Env {
        Env {
            number_variables: HashMap::new(),
            user_input_reader: user_input_reader,
        }
    }

    fn read_line(&self) -> String {
        self.user_input_reader.next()
    }
}

enum CommandResult {
    Output(String),
    Jump(i32),
}

#[derive(Clone)]
enum Command {
    Let(String, Expression),
    Goto(i32),
    Print(Vec<Expression>),
    Input(Vec<Expression>),
    Rem(String),
}
impl Command {
    fn run(&self, env: &mut Env) -> Result<CommandResult, String> {
        match self {
            Command::Let(variable_name, value) => match value.to_f64(env) {
                Ok(number) => {
                    env.number_variables.insert(variable_name.clone(), number);
                    Ok(CommandResult::Output(String::new()))
                }
                Err(msg) => Err(msg.to_string()),
            },
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
                            match value.parse::<f64>() {
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
        }
    }
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
    lines: BTreeMap<i32, Command>,
}
impl Program {
    fn new() -> Program {
        Program {
            lines: BTreeMap::new(),
        }
    }

    fn add_line(&mut self, line_number: i32, command: Command) -> &mut Self {
        self.lines.insert(line_number, command);
        self
    }

    fn run(
        &self,
        lines_limit: LinesLimit,
        user_input_reader: UserInputReader,
    ) -> Result<String, String> {
        let mut output = String::new();
        let mut env = Env::new(user_input_reader);

        self.run_helper(
            lines_limit,
            &mut output,
            &mut env,
            self.lines.clone(),
        )
    }

    fn run_helper(
        &self,
        lines_limit: LinesLimit,
        output: &mut String,
        env: &mut Env,
        lines: BTreeMap<i32, Command>,
    ) -> Result<String, String> {
        for (_, command) in lines {
            match &(command.run(env)) {
                Ok(CommandResult::Output(text)) => output.push_str(text),
                Ok(CommandResult::Jump(line_number)) => {
                    match lines_limit {
                        LinesLimit::NoLimit => {}
                        LinesLimit::Limit(x) if x >= 1 => {}
                        LinesLimit::Limit(_) => return Ok(output.to_string()),
                    }

                    let mut child_lines: BTreeMap<i32, Command> = BTreeMap::new();
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
                Err(msg) => return Err(msg.to_string()),
            }
        }
        Ok(output.to_string())
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use Command::*;

    #[test]
    fn let_and_print() {
        let mut program = Program::new();
        program.add_line(
            20,
            Print(vec![Expression::NumberVariable("a".to_string())]),
        );
        program.add_line(
            10,
            Let("a".to_string(), Expression::Integer(10)),
        );
        let output = program.run(
            LinesLimit::NoLimit,
            UserInputReader::RealStdin
        );
        assert_eq!(output, Ok("10\n".to_string()));
    }

    #[test]
    fn print_adding_vars() {
        let mut program = Program::new();
        program.add_line(
            20,
            Print(vec![Expression::Plus(
                Box::new(Expression::NumberVariable("a".to_string())),
                Box::new(Expression::NumberVariable("b".to_string())),
            )]),
        );
        program.add_line(
            10,
            Let("a".to_string(), Expression::Integer(10)),
        );
        program.add_line(
            15,
            Let("b".to_string(), Expression::Integer(15)),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("25\n".to_string())
        );
    }

    #[test]
    fn print_with_comma() {
        let mut program = Program::new();
        program.add_line(
            10,
            Let("a".to_string(), Expression::Integer(10)),
        );
        program.add_line(
            15,
            Let("b".to_string(), Expression::Integer(15)),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::NumberVariable("a".to_string()),
                Expression::NumberVariable("b".to_string()),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("10 15\n".to_string())
        );
    }

    #[test]
    fn rem() {
        let mut program = Program::new();
        program.add_line(10, Rem(String::new()));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok(String::new())
        );
    }

    #[test]
    fn print_with_no_expressions() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("\n".to_string())
        );
    }

    // PRINT 90-32
    #[test]
    fn subtract() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Subtract(
                Box::new(Expression::Integer(90)),
                Box::new(Expression::Integer(32)),
            )]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("58\n".to_string())
        );
    }

    // PRINT 58*5
    #[test]
    fn multiply() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Multiply(
                Box::new(Expression::Integer(58)),
                Box::new(Expression::Integer(5)),
            )]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("290\n".to_string())
        );
    }

    // 10 PRINT (41-32)*5/9
    #[test]
    fn print_arithmetic() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Divide(
                Box::new(Expression::Multiply(
                    Box::new(Expression::Subtract(
                        Box::new(Expression::Integer(41)),
                        Box::new(Expression::Integer(32)),
                    )),
                    Box::new(Expression::Integer(5)),
                )),
                Box::new(Expression::Integer(9)),
            )]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("5\n".to_string())
        );
    }

    // 10 PRINT "Hello, World!"
    // 20 GO TO 10
    #[test]
    fn goto() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Text(
                "Hello, World!".to_string(),
            )]),
        );
        program.add_line(20, Goto(10));

        assert_eq!(
            program.run(LinesLimit::Limit(1), UserInputReader::RealStdin),
            Ok("Hello, World!\nHello, World!\n".to_string())
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
                Expression::Text("Enter deg F".to_string()),
                Expression::NumberVariable("F".to_string()),
            ]),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::Text("You gave".to_string()),
                Expression::NumberVariable("F".to_string()),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::FakeStdin("42.0".to_string())),
            Ok("Enter deg F\nYou gave 42\n".to_string())
        );
    }

    // 10 REM temperature conversion
    // 20 PRINT "deg F", "deg C"
    // 30 PRINT
    // 40 INPUT "Enter deg F", F
    // 50 PRINT F,(F-32)*5/9
    // 60 GO TO 40
}

fn main() {
    println!("Hello, world!");
}
