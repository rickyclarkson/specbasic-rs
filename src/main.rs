use std::collections::{BTreeMap, HashMap};
use std::vec::Vec;
use std::result::Result;

#[derive(Clone)]
enum Expression {
    IntExpression(i32),
    StringExpression(String),
    NumberVariableExpression(String),
    Plus(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
}
impl Expression {
    fn to_string(&self, env: &Env) -> Result<String, &str> {
        match self {
            Expression::IntExpression(value) => Ok(value.to_string()),
            Expression::StringExpression(text) => Ok(text.to_string()),
            Expression::NumberVariableExpression(number_variable_name) => Ok(env
                .number_variables
                .get(number_variable_name)
                .unwrap()
                .to_string()),
            Expression::Plus(_, _) => self.to_f64(&env).map(|v| v.to_string()),
            Expression::Subtract(_, _) => self.to_f64(&env).map(|v| v.to_string()),
            Expression::Multiply(_, _) => self.to_f64(&env).map(|v| v.to_string()),
            Expression::Divide(_, _) => self.to_f64(&env).map(|v| v.to_string()),
        }
    }

    fn to_f64(&self, env: &Env) -> Result<f64, &str> {
        match self {
            Expression::IntExpression(value) => Ok(*value as f64),
            Expression::StringExpression(text) => {
                Err("Expected a number, found something else, but I can't figure out how to include that in the error")
            }
            Expression::NumberVariableExpression(number_variable_name) =>
                match env.number_variables.get(number_variable_name) {
                    Some(value) => Ok(*value),
                    None => Err("No such variable found, not sure how to say which.")
                },
            Expression::Plus(left, right) => match (left.to_f64(&env), right.to_f64(&env)) {
                (Ok(l), Ok(r)) => Ok(l + r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Subtract(left, right) => match (left.to_f64(&env), right.to_f64(&env)) {
                (Ok(l), Ok(r)) => Ok(l - r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Multiply(left, right) => match (left.to_f64(&env), right.to_f64(&env)) {
                (Ok(l), Ok(r)) => Ok(l * r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Divide(left, right) => match (left.to_f64(&env), right.to_f64(&env)) {
                (Ok(l), Ok(r)) => Ok(l / r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
        }
    }
}

struct Env {
    number_variables: HashMap<String, f64>,
}
impl Env {
    fn new() -> Env {
        Env {
            number_variables: HashMap::new(),
        }
    }
}

enum CommandResult {
    Output(String),
    Jump(i32),
}

#[derive(Clone)]
enum Command {
    LetCommand(String, Expression),
    GotoCommand(i32),
    PrintCommand(Vec<Expression>),
    RemCommand(String),
}
impl Command {
    fn run(&self, env: &mut Env) -> Result<CommandResult, &str> {
        match self {
            Command::LetCommand(variable_name, value) => {
                match value.to_f64(&env) {
                    Ok(number) => {
                        env.number_variables.insert(variable_name.clone(), number);
                        Ok(CommandResult::Output(String::new()))
                    },
                    Err(msg) => Err(msg),
                }
            },
            Command::GotoCommand(line_number) => Ok(CommandResult::Jump(*line_number)),
            Command::PrintCommand(expressions) => Ok(CommandResult::Output(
                    expressions.iter().flat_map(|e| e.to_string(&env))
                    .collect::<Vec<String>>()
                    .join(", ")
                    + "\n")),
            Command::RemCommand(comment) => Ok(CommandResult::Output(String::new())),
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

    fn run(&self, linesLimit: LinesLimit) -> Result<String, String> {
        let mut output = String::new();
        let mut env = Env::new();

        self.run_helper(linesLimit, &mut output, &mut env, Box::new(self.lines.clone()))
    }

    fn run_helper(&self, linesLimit: LinesLimit, output: &mut String, env: &mut Env, lines: Box<BTreeMap<i32, Command>>) -> Result<String, String> {
        for (_, command) in *lines {
            match &(command.run(env)) {
                Ok(CommandResult::Output(text)) => output.push_str(text),
                Ok(CommandResult::Jump(line_number)) => {
                    match linesLimit {
                        LinesLimit::NoLimit => {},
                        LinesLimit::Limit(x) if x >= 1 => {},
                        LinesLimit::Limit(x) => return Ok(output.to_string()),
                    }

                    let mut child_lines: BTreeMap<i32, Command> = BTreeMap::new();
                    for (l, command) in &self.lines {
                        if l >= line_number {
                            let unboxed_command: Command = command.clone();
                            child_lines.insert(*l, unboxed_command);
                        }
                    }
                    return self.run_helper(linesLimit.decrement(), output, env, Box::new(child_lines.clone()));
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
            PrintCommand(vec![Expression::NumberVariableExpression("a".to_string())]));
        program.add_line(
            10,
            LetCommand("a".to_string(), Expression::IntExpression(10)));
        let output = program.run(LinesLimit::NoLimit);
        assert_eq!(output, Ok("10\n".to_string()));
    }

    #[test]
    fn print_adding_vars() {
        let mut program = Program::new();
        program.add_line(
            20,
            PrintCommand(vec![Expression::Plus(
                    Box::new(Expression::NumberVariableExpression("a".to_string())),
                    Box::new(Expression::NumberVariableExpression("b".to_string())),
                )]));
        program.add_line(
            10,
            LetCommand("a".to_string(), Expression::IntExpression(10)));
        program.add_line(
            15,
            LetCommand("b".to_string(), Expression::IntExpression(15)));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok("25\n".to_string()));
    }

    #[test]
    fn print_with_comma() {
        let mut program = Program::new();
        program.add_line(
            10,
            LetCommand("a".to_string(), Expression::IntExpression(10)));
        program.add_line(
            15,
            LetCommand("b".to_string(), Expression::IntExpression(15)));
        program.add_line(
            20,
            PrintCommand(vec![
                    Expression::NumberVariableExpression("a".to_string()),
                    Expression::NumberVariableExpression("b".to_string()),
                ]));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok("10, 15\n".to_string()));
    }

    #[test]
    fn rem() {
        let mut program = Program::new();
        program.add_line(
            10,
            RemCommand(String::new()));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok(String::new()));
    }

    #[test]
    fn print_with_no_expressions() {
        let mut program = Program::new();
        program.add_line(
            10,
            PrintCommand(vec![]));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok("\n".to_string()));
    }

    // PRINT 90-32
    #[test]
    fn subtract() {
        let mut program = Program::new();
        program.add_line(
            10,
            PrintCommand(vec![Expression::Subtract(
                    Box::new(Expression::IntExpression(90)),
                    Box::new(Expression::IntExpression(32)),
                )]));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok("58\n".to_string()));
    }

    // PRINT 58*5
    #[test]
    fn multiply() {
        let mut program = Program::new();
        program.add_line(
            10,
            PrintCommand(vec![Expression::Multiply(
                    Box::new(Expression::IntExpression(58)),
                    Box::new(Expression::IntExpression(5)),
                )]));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok("290\n".to_string()));
    }

    // 10 PRINT (41-32)*5/9
    #[test]
    fn print_arithmetic() {
        let mut program = Program::new();
        program.add_line(
            10,
            PrintCommand(
                vec![Expression::Divide(
                    Box::new(Expression::Multiply(
                        Box::new(Expression::Subtract(
                            Box::new(Expression::IntExpression(41)),
                            Box::new(Expression::IntExpression(32)),
                        )),
                        Box::new(Expression::IntExpression(5)),
                    )),
                    Box::new(Expression::IntExpression(9)),
                )]));

        assert_eq!(program.run(LinesLimit::NoLimit), Ok("5\n".to_string()));
    }

    // 10 PRINT "Hello, World!"
    // 20 GO TO 10
    #[test]
    fn goto() {
        let mut program = Program::new();
        program.add_line(
            10,
            PrintCommand(vec![Expression::StringExpression("Hello, World!".to_string())])
        );
        program.add_line(20, GotoCommand(10));

        assert_eq!(
            program.run(LinesLimit::Limit(1)),
            Ok("Hello, World!\nHello World\n".to_string())
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
