mod expand;
use std::io::Read;
fn main() {
    println!("{:?}", expand::brace_expand("{{abb,b}o{m,n}p,b}"));
    println!("{:?}", expand::brace_expand("qwe{a-bxc-ad,aaa-}zasd"));

    println!(
        "{:?}",
        expand::shell_brace_expansion("foo {1..5} bar \\\n\t\t{3,4} baz")
    );
    println!(
        "{:?}",
        expand::shell_brace_expansion(
            r" 123d vcxv uheuwh 3n4n4k3n5kjln lncbvklhnirehs6 453n jk6bnl \
{a 223} 3,a {2,45,754} {1..024} \\ \
                sdf "
        )
    );
    let mut stdin = std::io::stdin();
    let mut input = String::new();
    stdin.read_to_string(&mut input).unwrap();
    println!("{:?}", expand::shell_brace_expansion(&input));
}
