use std::fs;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glob::glob;
use html5minify::*;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("minify");
    group.sample_size(20);

    glob("testdata/*.html")
        .expect("Failed to read glob pattern")
        .for_each(|path| {
            let path = path.expect("Failed to get entry");

            if path.is_dir() {
                return;
            }

            let html = &fs::read_to_string(&path).expect("Failed to read HTML");
            let id = path.file_name().unwrap().to_str().unwrap();

            group.bench_function(id, |b| {
                b.iter(|| {
                    black_box(html).minify().expect("Failed to minify HTML");
                })
            });
        });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
