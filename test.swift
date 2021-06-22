class Test {
    private let abc: Int32 = 0;
}

func identity<T>(_ x: T) -> T {
    return x;
}

identity(22);
Test();
