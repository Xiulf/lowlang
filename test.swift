apply(a, 3);
apply(b, "test");

func apply<A, R>(_ f: (A) -> R, _ a: A) -> R {
    return f(a);
}

func a(_ a: Int32) -> Bool {
    return a == 0;
}

func b<T>(_ a: T) -> Bool {
    return true;
}
