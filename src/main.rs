use syrup::{
    lang::{self, RewriteLang},
};
use egg::{Runner, Rewrite, RecExpr, rewrite};

fn main() {
    let start_expr: RecExpr<RewriteLang> = "(match (cons (var x) nil) (var hd) (var tl) 0 (var hd))".parse().unwrap();
    let goal_expr: RecExpr<RewriteLang> = "(var x)".parse().unwrap();

    let rules: Vec<Rewrite<RewriteLang, ()>> = vec![
        rewrite!("branch-true"; "(if true ?a ?b)" => "?a"),
        rewrite!("branch-false"; "(if false ?a ?b)" => "?b"),
        rewrite!("match-nil"; "(match nil (var ?x) (var ?y) ?a ?b)" => "?a"),
        rewrite!("match-cons"; 
            "(match (cons ?c ?d) (var ?x) (var ?y) ?a ?b)" 
            => "(let (var ?y) ?d (let (var ?x) ?c ?b))"),
        rewrite!("let-true"; "(let (var ?x) ?a true)" => "true"),
        rewrite!("let-false"; "(let (var ?x) ?a false)" => "false"),
        rewrite!("let-var-same"; "(let (var ?x) ?a (var ?x))" => "?a"),
        rewrite!("let-var-diff"; "(let (var ?x) ?a (var ?y))" => "(var ?y)" if lang::is_not_same_var(lang::var("?x"), lang::var("?y"))),
        rewrite!("let-nil"; "(let (var ?x) ?a nil)" => "nil"),
        rewrite!("let-cons"; "(let (var ?x) ?a (cons ?c ?d))" => "(cons (let (var ?x) ?a ?c) (let (var ?x) ?a ?d))"),
        rewrite!("let-if"; 
            "(let (var ?x) ?a (if ?b ?c ?d))" 
            => "(if (let (var ?x) ?a ?b) (let (var ?x) ?a ?c) (let (var ?x) ?a ?d))"),
    ];

    let mut runner = Runner::default().with_expr(&start_expr);
    runner.egraph.rebuild();

    let egraph = runner.run(&rules).egraph;
    let equivs = egraph.equivs(&start_expr, &goal_expr);

    println!("{:?}", egraph);
    println!("{}", egraph.dot());
    println!("{:?}", equivs);
}
