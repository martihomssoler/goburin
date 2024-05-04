type Parser<A> = Box<dyn FnOnce(String) -> Option<(A, String)>>;

fn parser<B, F>(f: F) -> Parser<B>
where
    F: 'static + FnOnce(String) -> Option<(B, String)>,
{
    Box::new(f)
}

pub fn result<A>(value: A) -> Parser<A>
where
    A: 'static,
{
    parser(move |input| Some((value, input)))
}

pub fn zero<A>() -> Parser<A> {
    parser(|_| None)
}

pub fn item() -> Parser<String> {
    parser(|input| match input.as_str() {
        "" => None,
        s => {
            let (idx, c) = s.char_indices().next().unwrap();
            let char_lenght = c.len_utf8();
            let (s, xs) = s.split_at(idx + char_lenght);
            Some((s.to_string(), xs.to_string()))
        }
    })
}

pub fn bind<A, B, F>(p: Parser<A>, f: F) -> Parser<B>
where
    A: 'static,
    B: 'static,
    F: 'static + FnOnce(A) -> Parser<B>,
{
    parser(move |input| match p(input) {
        Some((value, res)) => f(value)(res),
        None => None,
    })
}

pub fn seq(first: Parser<String>, second: Parser<String>) -> Parser<String>
where
{
    parser(|input| {
        bind(first, move |mut x| {
            bind(second, move |y| {
                x.push_str(y.as_str());
                result(x)
            })
        })(input)
    })
}

fn sat<F>(func: F) -> Parser<String>
where
    F: 'static + Fn(char) -> bool,
{
    parser(move |input| {
        bind(item(), move |x| {
            if func(x.chars().next().unwrap()) {
                result(x)
            } else {
                zero()
            }
        })(input)
    })
}

pub fn char(c: char) -> Parser<String> {
    parser(move |input| sat(move |x| x.eq(&c))(input))
}

pub fn digit() -> Parser<String> {
    parser(move |input| sat(move |x| x.is_ascii_digit())(input))
}

pub fn lowercase() -> Parser<String> {
    parser(move |input| sat(move |x| x.is_lowercase())(input))
}

pub fn uppercase() -> Parser<String> {
    parser(move |input| sat(move |x| x.is_uppercase())(input))
}
