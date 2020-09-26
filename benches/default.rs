use criterion::{black_box, criterion_group, criterion_main, Criterion};
use html5minify::*;

const HTML: &str =
    "<html> \n<link href=\"test.css\">\n<h2   id=\"id_one\"    >Hello\n</h2>    \n<p>\nWorld</p>";

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("minify", |b| {
        b.iter(|| {
            black_box(HTML).minify().expect("Failed to minify HTML");
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
